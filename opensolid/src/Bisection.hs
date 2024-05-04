module Bisection
  ( Tree
  , tree
  , solve
  , solve2
  )
where

import Float qualified
import OpenSolid
import Qty qualified
import Range (Range (Range))
import Range qualified

data Cache segment = Cache (Range Unitless) ~segment

data Tree segment
  = Tree (Cache segment) (Cache segment) ~(Tree segment) ~(Tree segment)

tree :: (Range Unitless -> segment) -> Tree segment
tree compute = buildTree compute Range.unit

expand :: Range Unitless -> Range Unitless
expand (Range low high) = do
  let expansion = 0.5 * (high - low)
  Range.unsafe (Float.max 0.0 (low - expansion)) (Float.min 1.0 (high + expansion))

buildTree :: (Range Unitless -> segment) -> Range Unitless -> Tree segment
buildTree compute domain = do
  let cache = Cache domain (compute domain)
  let neighborhoodDomain = expand domain
  let neighborhoodCache = Cache neighborhoodDomain (compute neighborhoodDomain)
  let (left, right) = Range.bisect domain
  Tree cache neighborhoodCache (buildTree compute left) (buildTree compute right)

solve ::
  (Range Unitless -> segment -> Bool) ->
  (Range Unitless -> segment -> Fuzzy resolution) ->
  (Range Unitless -> segment -> resolution -> Maybe solution) ->
  Tree segment ->
  (List solution, List (Range Unitless)) ->
  (List solution, List (Range Unitless))
solve isCandidate resolveNeighborhood findSolution segmentTree accumulated = do
  let Tree cache neighborhoodCache leftChild rightChild = segmentTree
  let (Cache domain segment) = cache
  let (Cache expandedDomain neighborhood) = neighborhoodCache
  let (solutions, exclusions) = accumulated
  let allowed = isAllowed domain exclusions
  let candidate = isCandidate domain segment
  let resolved = resolveNeighborhood expandedDomain neighborhood
  let recurse =
        accumulated
          |> solve isCandidate resolveNeighborhood findSolution rightChild
          |> solve isCandidate resolveNeighborhood findSolution leftChild
  case (allowed, candidate, resolved) of
    (Resolved False, _, _) -> accumulated
    (_, False, _) -> accumulated
    (Unresolved, True, _) -> recurse
    (Resolved True, True, Unresolved) -> recurse
    (Resolved True, True, Resolved resolution) ->
      case findSolution domain segment resolution of
        Just solution -> (solution : solutions, expandedDomain : exclusions)
        Nothing -> accumulated

isAllowed :: Range Unitless -> List (Range Unitless) -> Fuzzy Bool
isAllowed _ [] = Resolved True
isAllowed domain (first : rest)
  | Range.overlap domain first > Qty.zero =
      if Range.contains domain first
        then Resolved False
        else Unresolved
  | otherwise = isAllowed domain rest

solve2 ::
  Show solution =>
  (Range Unitless -> Range Unitless -> segment -> segment -> Bool) ->
  (Range Unitless -> Range Unitless -> segment -> segment -> Fuzzy resolution) ->
  (Range Unitless -> Range Unitless -> segment -> segment -> resolution -> Maybe solution) ->
  Tree segment ->
  Tree segment ->
  (List solution, List (Range Unitless, Range Unitless)) ->
  (List solution, List (Range Unitless, Range Unitless))
solve2 isCandidate resolveNeighborhood findSolution segmentTree1 segmentTree2 accumulated = do
  let (Tree cache1 neighborhoodCache1 leftChild1 rightChild1) = segmentTree1
  let (Tree cache2 neighborhoodCache2 leftChild2 rightChild2) = segmentTree2
  let (Cache domain1 segment1) = cache1
  let (Cache domain2 segment2) = cache2
  let (Cache expandedDomain1 neighborhood1) = neighborhoodCache1
  let (Cache expandedDomain2 neighborhood2) = neighborhoodCache2
  let (solutions, exclusions) = accumulated
  let allowed = isAllowed2 domain1 domain2 exclusions
  let candidate = isCandidate domain1 domain2 segment1 segment2
  let resolved = resolveNeighborhood expandedDomain1 expandedDomain2 neighborhood1 neighborhood2
  let recurse =
        accumulated
          |> solve2 isCandidate resolveNeighborhood findSolution rightChild1 rightChild2
          |> solve2 isCandidate resolveNeighborhood findSolution rightChild1 leftChild2
          |> solve2 isCandidate resolveNeighborhood findSolution leftChild1 rightChild2
          |> solve2 isCandidate resolveNeighborhood findSolution leftChild1 leftChild2
  case (allowed, candidate, resolved) of
    (Resolved False, _, _) -> accumulated
    (_, False, _) -> accumulated
    (Unresolved, True, _) -> recurse
    (Resolved True, True, Unresolved) -> recurse
    (Resolved True, True, Resolved resolution) ->
      case findSolution domain1 domain2 segment1 segment2 resolution of
        Just solution -> (solution : solutions, (expandedDomain1, expandedDomain2) : exclusions)
        Nothing -> accumulated

isAllowed2 :: Range Unitless -> Range Unitless -> List (Range Unitless, Range Unitless) -> Fuzzy Bool
isAllowed2 _ _ [] = Resolved True
isAllowed2 xDomain yDomain ((firstX, firstY) : rest)
  | Range.overlap firstX xDomain > Qty.zero && Range.overlap firstY yDomain > Qty.zero =
      if Range.contains xDomain firstX && Range.contains yDomain firstY
        then Resolved False
        else Unresolved
  | otherwise = isAllowed2 xDomain yDomain rest
