module OpenSolid.Bisection
  ( Domain
  , Tree (Tree)
  , subdomain
  , segment
  , children
  , pairwise
  , clusters
  , find
  , touching
  )
where

import OpenSolid.Bag (Bag)
import OpenSolid.Bag qualified as Bag
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Queue (Queue)
import OpenSolid.Queue qualified as Queue
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)

class (Set.Bounds domain, Intersects domain domain (Tolerance Unitless)) => Domain domain where
  contains :: domain -> domain -> Bool

instance Domain (Interval Unitless) where
  contains = Interval.contains

instance Domain UvBounds where
  contains = Bounds2D.contains

instance (Domain domain1, Domain domain2) => Domain (domain1, domain2) where
  contains (b1, b2) (a1, a2) = contains b1 a1 && contains b2 a2

data Tree domain segment = Tree
  { subdomain :: domain
  , segment :: segment
  , children :: ~(NonEmpty (Tree domain segment))
  }

instance
  ( Units.Coercion domain1 domain2
  , Units.Coercion segment1 segment2
  ) =>
  Units.Coercion (Tree domain1 segment1) (Tree domain2 segment2)
  where
  coerce tree =
    Tree
      { subdomain = Units.coerce tree.subdomain
      , segment = Units.coerce tree.segment
      , children = NonEmpty.map Units.coerce tree.children
      }

pairwise ::
  Tree domain1 segment1 ->
  Tree domain2 segment2 ->
  Tree (domain1, domain2) (segment1, segment2)
pairwise tree1 tree2 = do
  let Tree subdomain1 segment1 children1 = tree1
  let Tree subdomain2 segment2 children2 = tree2
  let pairwiseChildren = NonEmpty.pairwise pairwise children1 children2
  Tree (subdomain1, subdomain2) (segment1, segment2) pairwiseChildren

subdomain :: forall domain segment. Tree domain segment -> domain
subdomain = (.subdomain)

segment :: forall domain segment. Tree domain segment -> segment
segment = (.segment)

children :: forall domain segment. Tree domain segment -> NonEmpty (Tree domain segment)
children = (.children)

resolve ::
  forall domain segment existing tag.
  Domain domain =>
  Bag domain existing ->
  (domain -> segment -> Fuzzy (Maybe tag)) ->
  Tree domain segment ->
  Bag domain (tag, Tree domain segment)
resolve existing callback tree =
  if containedIn existing tree.subdomain
    then Bag.empty
    else case callback tree.subdomain tree.segment of
      Resolved Nothing -> Bag.empty
      Resolved (Just tag) -> Bag.singleton tree.subdomain (tag, tree)
      Unresolved -> Bag.group (List.map (resolve existing callback) (NonEmpty.toList tree.children))

containedIn :: forall domain existing. Domain domain => Bag domain existing -> domain -> Bool
containedIn existing candidate = Bag.any (contains candidate) (const True) existing

touching ::
  forall domain segment existing tag.
  Domain domain =>
  Bag domain existing ->
  Set domain (tag, Tree domain segment) ->
  Bool
touching existing set = Bag.full set & Bag.pairwiseAny (^) (\_ _ -> True) existing

clusters ::
  forall domain segment existing tag.
  Domain domain =>
  Bag domain existing ->
  (domain -> segment -> Fuzzy (Maybe tag)) ->
  Tree domain segment ->
  List (Set domain (tag, Tree domain segment))
clusters existing resolveFunction tree = do
  resolve existing resolveFunction tree
    & Bag.clusters (^) (\_ _ -> True)
    & List.map (Set.build (subdomain . Pair.second))
    & List.filter (not . touching existing)

find ::
  forall domain segment tag solution.
  (tag -> domain -> segment -> Fuzzy (Maybe solution)) ->
  Set domain (tag, Tree domain segment) ->
  Maybe solution
find callback cluster = findImpl callback (Queue.fromNonEmpty (Set.toNonEmpty cluster))

findImpl ::
  forall domain segment tag solution.
  (tag -> domain -> segment -> Fuzzy (Maybe solution)) ->
  Queue (tag, Tree domain segment) ->
  Maybe solution
findImpl callback queue = do
  ((tag, subtree), remaining) <- Queue.pop queue
  case callback tag subtree.subdomain subtree.segment of
    Resolved Nothing -> findImpl callback remaining
    Resolved (Just solution) -> Just solution
    Unresolved -> do
      let updatedQueue = remaining & forEach subtree.children \child -> Queue.push (tag, child)
      findImpl callback updatedQueue
