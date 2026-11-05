module OpenSolid.Bisection
  ( Domain
  , Segment (children)
  , Tagged (Tagged)
  , subdomain
  , resolve
  , clusters
  , find
  , touching
  )
where

import OpenSolid.Bag (Bag)
import OpenSolid.Bag qualified as Bag
import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Queue (Queue)
import OpenSolid.Queue qualified as Queue
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set
import OpenSolid.UvBounds (UvBounds)

class (Set.Bounds domain, Intersects domain domain Unitless) => Domain domain where
  contains :: domain -> domain -> Bool

instance Domain (Interval Unitless) where
  contains = Interval.contains

instance Domain UvBounds where
  contains = Bounds2D.contains

instance (Domain domain1, Domain domain2) => Domain (domain1, domain2) where
  contains (b1, b2) (a1, a2) = contains b1 a1 && contains b2 a2

class (Bounded segment domain, Domain domain) => Segment segment domain | segment -> domain where
  children :: segment -> NonEmpty segment

data Tagged tag segment = Tagged tag segment

instance Segment segment domain => Bounded (Tagged tag segment) domain where
  {-# INLINE bounds #-}
  bounds (Tagged _ segment) = subdomain segment

instance
  (Segment segment1 domain1, Segment segment2 domain2) =>
  Segment (segment1, segment2) (domain1, domain2)
  where
  children (segment1, segment2) = NonEmpty.pairwise (,) (children segment1) (children segment2)

{-# INLINE subdomain #-}
subdomain :: Segment segment domain => segment -> domain
subdomain = Bounded.bounds

resolve ::
  Segment segment domain =>
  Bag domain existing ->
  (segment -> Fuzzy (Maybe tag)) ->
  segment ->
  Bag domain (Tagged tag segment)
resolve existing callback segment =
  if containedIn existing (subdomain segment)
    then Bag.empty
    else case callback segment of
      Resolved Nothing -> Bag.empty
      Resolved (Just tag) -> Bag.singleton $ Tagged tag segment
      Unresolved -> do
        let segmentChildren = NonEmpty.toList (children segment)
        Bag.group $ List.map (resolve existing callback) segmentChildren

containedIn :: forall domain existing. Domain domain => Bag domain existing -> domain -> Bool
containedIn existing candidate = existing & Bag.anyBounds (contains candidate)

touching ::
  Segment segment domain =>
  Bag domain existing ->
  Set domain (Tagged tag segment) ->
  Bool
touching Bag.Empty _ = False
touching (Bag.Full existing) cluster =
  Set.pairwiseAnyBounds (unitless (^)) existing cluster

clusters ::
  Segment segment domain =>
  Bag domain existing ->
  (segment -> Fuzzy (Maybe tag)) ->
  segment ->
  List (Set domain (Tagged tag segment))
clusters existing resolveFunction tree = do
  resolve existing resolveFunction tree
    & Bag.clusters (unitless (^)) (\_ _ -> True)
    & List.map Set.build
    & List.filter (not . touching existing)

find ::
  Segment segment domain =>
  (tag -> segment -> Fuzzy (Maybe solution)) ->
  Queue (Tagged tag segment) ->
  Maybe solution
find callback queue = do
  (Tagged tag segment, remaining) <- Queue.pop queue
  case callback tag segment of
    Resolved Nothing -> find callback remaining
    Resolved (Just solution) -> Just solution
    Unresolved -> do
      let updatedQueue =
            remaining & forEach (children segment) \child ->
              Queue.push (Tagged tag child)
      find callback updatedQueue
