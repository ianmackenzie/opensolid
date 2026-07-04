module OpenSolid.Bisection
  ( Tree (Tree)
  , subdomain
  , segment
  , children
  , pairwise
  , clusters
  , find
  )
where

import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Queue (Queue)
import OpenSolid.Queue qualified as Queue
import OpenSolid.SearchDomain qualified as SearchDomain
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set
import OpenSolid.Units qualified as Units

data Tree subdomain segment = Tree
  { subdomain :: subdomain
  , segment :: segment
  , children :: ~(NonEmpty (Tree subdomain segment))
  }

instance
  ( Units.Coercion subdomain1 subdomain2
  , Units.Coercion segment1 segment2
  ) =>
  Units.Coercion (Tree subdomain1 segment1) (Tree subdomain2 segment2)
  where
  coerce tree =
    Tree
      { subdomain = Units.coerce tree.subdomain
      , segment = Units.coerce tree.segment
      , children = NonEmpty.map Units.coerce tree.children
      }

pairwise ::
  Tree subdomain1 segment1 ->
  Tree subdomain2 segment2 ->
  Tree (subdomain1, subdomain2) (segment1, segment2)
pairwise tree1 tree2 = do
  let Tree subdomain1 segment1 children1 = tree1
  let Tree subdomain2 segment2 children2 = tree2
  let pairwiseChildren = NonEmpty.pairwise pairwise children1 children2
  Tree (subdomain1, subdomain2) (segment1, segment2) pairwiseChildren

subdomain :: forall subdomain segment. Tree subdomain segment -> subdomain
subdomain = (.subdomain)

segment :: forall subdomain segment. Tree subdomain segment -> segment
segment = (.segment)

children :: forall subdomain segment. Tree subdomain segment -> NonEmpty (Tree subdomain segment)
children = (.children)

resolve ::
  forall subdomain segment existing tag.
  SearchDomain.Bounds subdomain =>
  Maybe (Set subdomain existing) ->
  (subdomain -> segment -> Fuzzy (Maybe tag)) ->
  Tree subdomain segment ->
  Maybe (Set subdomain (tag, Tree subdomain segment))
resolve existing callback tree =
  if containedIn existing tree.subdomain
    then Nothing
    else case callback tree.subdomain tree.segment of
      Resolved Nothing -> Nothing
      Resolved (Just tag) -> Just (Set.leaf tree.subdomain (tag, tree))
      Unresolved ->
        case NonEmpty.filterMap (resolve existing callback) tree.children of
          NonEmpty resolvedChildren -> Just (Set.node resolvedChildren)
          [] -> Nothing

containedIn ::
  forall subdomain existing.
  SearchDomain.Bounds subdomain =>
  Maybe (Set subdomain existing) ->
  subdomain ->
  Bool
containedIn Nothing _ = False
containedIn (Just existing) candidate =
  Set.any (SearchDomain.contains candidate) (const True) existing

touching ::
  forall subdomain segment existing tag.
  SearchDomain.Bounds subdomain =>
  Maybe (Set subdomain existing) ->
  Set subdomain (tag, Tree subdomain segment) ->
  Bool
touching Nothing _ = False
touching (Just existing) set =
  Set.pairwiseAny SearchDomain.touching (\_ _ -> True) existing set

clusters ::
  forall subdomain segment existing tag.
  SearchDomain.Bounds subdomain =>
  Maybe (Set subdomain existing) ->
  (subdomain -> segment -> Fuzzy (Maybe tag)) ->
  Tree subdomain segment ->
  List (Set subdomain (tag, Tree subdomain segment))
clusters existing resolveFunction tree =
  case resolve existing resolveFunction tree of
    Nothing -> []
    Just resolved ->
      Set.clusters SearchDomain.touching (\_ _ -> True) resolved
        & NonEmpty.map (Set.build (subdomain . Pair.second))
        & NonEmpty.filter (not . touching existing)

find ::
  forall subdomain segment tag solution.
  (tag -> subdomain -> segment -> Fuzzy (Maybe solution)) ->
  Set subdomain (tag, Tree subdomain segment) ->
  Maybe solution
find callback cluster = findImpl callback (Queue.fromNonEmpty (Set.toNonEmpty cluster))

findImpl ::
  forall subdomain segment tag solution.
  (tag -> subdomain -> segment -> Fuzzy (Maybe solution)) ->
  Queue (tag, Tree subdomain segment) ->
  Maybe solution
findImpl callback queue = do
  ((tag, subtree), remaining) <- Queue.pop queue
  case callback tag subtree.subdomain subtree.segment of
    Resolved Nothing -> findImpl callback remaining
    Resolved (Just solution) -> Just solution
    Unresolved -> do
      let updatedQueue = remaining & forEach subtree.children \child -> Queue.push (tag, child)
      findImpl callback updatedQueue
