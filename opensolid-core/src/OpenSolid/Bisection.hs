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
  forall subdomain segment tag.
  Set.Bounds subdomain =>
  (subdomain -> segment -> Fuzzy (Maybe tag)) ->
  Tree subdomain segment ->
  Maybe (Set subdomain (tag, Tree subdomain segment))
resolve callback tree =
  case callback tree.subdomain tree.segment of
    Resolved Nothing -> Nothing
    Resolved (Just tag) -> Just (Set.leaf tree.subdomain (tag, tree))
    Unresolved ->
      case NonEmpty.filterMap (resolve callback) tree.children of
        NonEmpty resolvedChildren -> Just (Set.node resolvedChildren)
        [] -> Nothing

clusters ::
  forall subdomain segment tag.
  SearchDomain.Bounds subdomain =>
  (subdomain -> segment -> Fuzzy (Maybe tag)) ->
  (tag -> tag -> Bool) ->
  Tree subdomain segment ->
  List (NonEmpty (tag, Tree subdomain segment))
clusters resolveFunction tagPredicate tree =
  case resolve resolveFunction tree of
    Nothing -> []
    Just resolved -> do
      let itemPredicate (tag1, _) (tag2, _) = tagPredicate tag1 tag2
      Set.clusters SearchDomain.touching itemPredicate resolved
        & NonEmpty.toList

find ::
  forall subdomain segment tag solution.
  (tag -> subdomain -> segment -> Fuzzy (Maybe solution)) ->
  NonEmpty (tag, Tree subdomain segment) ->
  Maybe solution
find callback cluster = findImpl callback (Queue.fromNonEmpty cluster)

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
