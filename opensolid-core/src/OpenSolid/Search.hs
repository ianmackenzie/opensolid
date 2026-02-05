module OpenSolid.Search
  ( Domain (Domain)
  , Tree (Tree)
  , InfiniteRecursion (InfiniteRecursion)
  , curveDomain
  , surfaceDomain
  , tree
  , pairwise
  , exclusive
  )
where

import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Interval (Interval)
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Search.Domain (Domain (Domain), InfiniteRecursion (InfiniteRecursion))
import OpenSolid.Search.Domain qualified as Domain
import OpenSolid.UvBounds (UvBounds, pattern UvBounds)

data Tree bounds value where
  Tree ::
    Domain.Bounds bounds =>
    bounds ->
    value ->
    ~(List (Tree bounds value)) ->
    Tree bounds value

curveDomain :: Domain (Interval Unitless)
curveDomain = Domain.unitInterval

surfaceDomain :: Domain UvBounds
surfaceDomain = Domain.pairwise UvBounds Domain.unitInterval Domain.unitInterval

tree :: (bounds -> value) -> Domain bounds -> Tree bounds value
tree function (Domain domainBounds subdomains) =
  Tree domainBounds (function domainBounds) (List.map (tree function) subdomains)

pairwise :: Tree bounds1 value1 -> Tree bounds2 value2 -> Tree (bounds1, bounds2) (value1, value2)
pairwise (Tree domainBounds1 value1 children1) (Tree domainBounds2 value2 children2) =
  Tree (domainBounds1, domainBounds2) (value1, value2) $
    [pairwise child1 child2 | child1 <- children1, child2 <- children2]

data SolutionTree bounds solution
  = Leaf bounds (Maybe solution)
  | Node bounds ~(List (SolutionTree bounds solution))

excluded :: Domain.Bounds bounds => Int -> bounds -> SolutionTree bounds solution -> Bool
excluded _ bounds (Leaf leafBounds _) = Domain.contains bounds leafBounds
excluded maxDepthIndex bounds (Node nodeBounds children)
  | maxDepthIndex == 0 = False -- Ensure we don't cause infinite recursion
  | not (Domain.contains bounds nodeBounds) = False -- Stop early if possible
  | otherwise = List.anySatisfy (excluded (maxDepthIndex - 1) bounds) children

exclusive ::
  (bounds -> value -> Fuzzy (Maybe solution)) ->
  Tree bounds value ->
  List (bounds, solution)
exclusive callback (Tree bounds value children) =
  case callback bounds value of
    Resolved Nothing -> []
    Resolved (Just solution) -> [(bounds, solution)]
    Unresolved -> do
      let solutionTree = buildNode 0 callback bounds children solutionTree
      collectSolutions solutionTree []

buildNode ::
  Domain.Bounds bounds =>
  Int -> -- always >= 0
  (bounds -> value -> Fuzzy (Maybe solution)) ->
  bounds ->
  List (Tree bounds value) ->
  SolutionTree bounds solution ->
  SolutionTree bounds solution
buildNode depthIndex callback bounds children builtSolutionTree =
  Node bounds (List.filterMap (buildSubtree (depthIndex + 1) callback builtSolutionTree) children)

buildSubtree ::
  Int -> -- always >= 1
  (bounds -> value -> Fuzzy (Maybe solution)) ->
  SolutionTree bounds solution ->
  Tree bounds value ->
  Maybe (SolutionTree bounds solution)
buildSubtree depthIndex callback builtSolutionTree (Tree bounds value children)
  | excluded (depthIndex - 1) bounds builtSolutionTree = Nothing
  | otherwise = case callback bounds value of
      Resolved maybeSolution -> Just (Leaf bounds maybeSolution)
      Unresolved -> Just (buildNode depthIndex callback bounds children builtSolutionTree)

collectSolutions ::
  SolutionTree bounds solution ->
  List (bounds, solution) ->
  List (bounds, solution)
collectSolutions (Leaf _ Nothing) accumulated = accumulated
collectSolutions (Leaf bounds (Just solution)) accumulated = (bounds, solution) : accumulated
collectSolutions (Node _ children) accumulated = List.foldr collectSolutions accumulated children
