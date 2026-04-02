module OpenSolid.Search
  ( primary
  , exclusive
  , isInterior
  )
where

import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.SearchDomain qualified as SearchDomain
import OpenSolid.SearchTree (SearchTree)
import OpenSolid.SearchTree qualified as SearchTree

primary ::
  SearchDomain.Bounds bounds =>
  (bounds -> value -> Fuzzy (Maybe solution)) ->
  SearchTree bounds value ->
  List (bounds, solution)
primary callback searchTree = collectPrimary callback searchTree []

collectPrimary ::
  SearchDomain.Bounds bounds =>
  (bounds -> value -> Fuzzy (Maybe solution)) ->
  SearchTree bounds value ->
  List (bounds, solution) ->
  List (bounds, solution)
collectPrimary callback searchTree accumulated = do
  let domainBounds = SearchTree.domainBounds searchTree
  if SearchDomain.isPrimary domainBounds
    then case callback domainBounds (SearchTree.value searchTree) of
      Resolved Nothing -> accumulated
      Resolved (Just solution) -> (domainBounds, solution) : accumulated
      Unresolved ->
        List.foldr (collectPrimary callback) accumulated (SearchTree.children searchTree)
    else accumulated

data SolutionTree bounds solution
  = Leaf bounds (Maybe solution)
  | Node bounds ~(List (SolutionTree bounds solution))

excluded :: SearchDomain.Bounds bounds => Int -> bounds -> SolutionTree bounds solution -> Bool
excluded _ bounds (Leaf leafBounds _) = SearchDomain.contains bounds leafBounds
excluded maxDepthIndex bounds (Node nodeBounds children)
  | maxDepthIndex == 0 = False -- Ensure we don't cause infinite recursion
  | not (SearchDomain.contains bounds nodeBounds) = False -- Stop early if possible
  | otherwise = List.any (excluded (maxDepthIndex - 1) bounds) children

exclusive ::
  SearchDomain.Bounds bounds =>
  (bounds -> value -> Fuzzy (Maybe solution)) ->
  ((bounds, solution) -> (bounds, solution) -> Bool) ->
  SearchTree bounds value ->
  List (bounds, solution)
exclusive callback duplicateCallback searchTree = do
  let domainBounds = SearchTree.domainBounds searchTree
  case callback domainBounds (SearchTree.value searchTree) of
    Resolved Nothing -> []
    Resolved (Just solution) -> [(domainBounds, solution)]
    Unresolved -> do
      let children = SearchTree.children searchTree
      let solutionTree = buildNode 0 callback domainBounds children solutionTree
      collectSolutions solutionTree [] & deduplicate duplicateCallback

buildNode ::
  SearchDomain.Bounds bounds =>
  Int -> -- always >= 0
  (bounds -> value -> Fuzzy (Maybe solution)) ->
  bounds ->
  List (SearchTree bounds value) ->
  SolutionTree bounds solution ->
  SolutionTree bounds solution
buildNode depthIndex callback domainBounds children builtSolutionTree =
  Node domainBounds $
    List.filterMap (buildSubtree (depthIndex + 1) callback builtSolutionTree) children

buildSubtree ::
  SearchDomain.Bounds bounds =>
  Int -> -- always >= 1
  (bounds -> value -> Fuzzy (Maybe solution)) ->
  SolutionTree bounds solution ->
  SearchTree bounds value ->
  Maybe (SolutionTree bounds solution)
buildSubtree depthIndex callback builtSolutionTree searchTree = do
  let domainBounds = SearchTree.domainBounds searchTree
  if excluded (depthIndex - 1) domainBounds builtSolutionTree
    then Nothing
    else case callback domainBounds (SearchTree.value searchTree) of
      Resolved maybeSolution -> Just (Leaf domainBounds maybeSolution)
      Unresolved -> do
        let children = SearchTree.children searchTree
        Just (buildNode depthIndex callback domainBounds children builtSolutionTree)

collectSolutions ::
  SolutionTree bounds solution ->
  List (bounds, solution) ->
  List (bounds, solution)
collectSolutions (Leaf _ Nothing) accumulated = accumulated
collectSolutions (Leaf bounds (Just solution)) accumulated = (bounds, solution) : accumulated
collectSolutions (Node _ children) accumulated = List.foldr collectSolutions accumulated children

deduplicate ::
  ((bounds, solution) -> (bounds, solution) -> Bool) ->
  List (bounds, solution) ->
  List (bounds, solution)
deduplicate callback solutions = deduplicateImpl callback solutions []

deduplicateImpl ::
  ((bounds, solution) -> (bounds, solution) -> Bool) ->
  List (bounds, solution) ->
  List (bounds, solution) ->
  List (bounds, solution)
deduplicateImpl _ [] accumulated = accumulated
deduplicateImpl isDuplicate (first : rest) accumulated
  | List.any (isDuplicate first) rest = deduplicateImpl isDuplicate rest accumulated
  | otherwise = deduplicateImpl isDuplicate rest (first : accumulated)

isInterior :: Number -> Interval Unitless -> Bool
isInterior value interval = Interval.inclusion value interval >= 0.125 * Interval.width interval
