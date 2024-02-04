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
import T qualified

data Cache segment = Cache T.Bounds ~segment

data Tree segment
  = Tree (Cache segment) (Cache segment) ~(Tree segment) ~(Tree segment)

tree :: (T.Bounds -> segment) -> Tree segment
tree compute = buildTree compute T.domain

expand :: T.Bounds -> T.Bounds
expand (Range low high) =
  let expansion = 0.5 * (high - low)
   in Range.unsafe (Float.max 0.0 (low - expansion)) (Float.min 1.0 (high + expansion))

buildTree :: (T.Bounds -> segment) -> T.Bounds -> Tree segment
buildTree compute domain =
  let cache = Cache domain (compute domain)
      neighborhoodDomain = expand domain
      neighborhoodCache = Cache neighborhoodDomain (compute neighborhoodDomain)
      (left, right) = Range.bisect domain
   in Tree cache neighborhoodCache (buildTree compute left) (buildTree compute right)

solve ::
  (T.Bounds -> segment -> Bool) ->
  (T.Bounds -> segment -> Fuzzy resolution) ->
  (T.Bounds -> segment -> resolution -> Maybe solution) ->
  Tree segment ->
  (List solution, List T.Bounds) ->
  (List solution, List T.Bounds)
solve isCandidate resolveNeighborhood findSolution segmentTree accumulated =
  let allowed = isAllowed domain exclusions
      candidate = isCandidate domain segment
      resolved = resolveNeighborhood expandedDomain neighborhood
      recurse =
        accumulated
          |> solve isCandidate resolveNeighborhood findSolution rightChild
          |> solve isCandidate resolveNeighborhood findSolution leftChild
   in case (allowed, candidate, resolved) of
        (Resolved False, _, _) -> accumulated
        (_, False, _) -> accumulated
        (Unresolved, True, _) -> recurse
        (Resolved True, True, Unresolved) -> recurse
        (Resolved True, True, Resolved resolution) ->
          case findSolution domain segment resolution of
            Just solution -> (solution : solutions, expandedDomain : exclusions)
            Nothing -> accumulated
 where
  Tree cache neighborhoodCache leftChild rightChild = segmentTree
  (Cache domain segment) = cache
  (Cache expandedDomain neighborhood) = neighborhoodCache
  (solutions, exclusions) = accumulated

isAllowed :: T.Bounds -> List T.Bounds -> Fuzzy Bool
isAllowed _ [] = Resolved True
isAllowed domain (first : rest)
  | Range.overlap domain first > Qty.zero =
      if Range.contains domain first
        then Resolved False
        else Unresolved
  | otherwise = isAllowed domain rest

solve2 ::
  Show solution =>
  (T.Bounds -> T.Bounds -> segment -> segment -> Bool) ->
  (T.Bounds -> T.Bounds -> segment -> segment -> Fuzzy resolution) ->
  (T.Bounds -> T.Bounds -> segment -> segment -> resolution -> Maybe solution) ->
  Tree segment ->
  Tree segment ->
  (List solution, List (T.Bounds, T.Bounds)) ->
  (List solution, List (T.Bounds, T.Bounds))
solve2 isCandidate resolveNeighborhood findSolution segmentTree1 segmentTree2 accumulated =
  let allowed = isAllowed2 domain1 domain2 exclusions
      candidate = isCandidate domain1 domain2 segment1 segment2
      resolved = resolveNeighborhood expandedDomain1 expandedDomain2 neighborhood1 neighborhood2
      recurse =
        accumulated
          |> solve2 isCandidate resolveNeighborhood findSolution rightChild1 rightChild2
          |> solve2 isCandidate resolveNeighborhood findSolution rightChild1 leftChild2
          |> solve2 isCandidate resolveNeighborhood findSolution leftChild1 rightChild2
          |> solve2 isCandidate resolveNeighborhood findSolution leftChild1 leftChild2
   in case (allowed, candidate, resolved) of
        (Resolved False, _, _) -> accumulated
        (_, False, _) -> accumulated
        (Unresolved, True, _) -> recurse
        (Resolved True, True, Unresolved) -> recurse
        (Resolved True, True, Resolved resolution) ->
          case findSolution domain1 domain2 segment1 segment2 resolution of
            Just solution -> (solution : solutions, (expandedDomain1, expandedDomain2) : exclusions)
            Nothing -> accumulated
 where
  (Tree cache1 neighborhoodCache1 leftChild1 rightChild1) = segmentTree1
  (Tree cache2 neighborhoodCache2 leftChild2 rightChild2) = segmentTree2
  (Cache domain1 segment1) = cache1
  (Cache domain2 segment2) = cache2
  (Cache expandedDomain1 neighborhood1) = neighborhoodCache1
  (Cache expandedDomain2 neighborhood2) = neighborhoodCache2
  (solutions, exclusions) = accumulated

isAllowed2 :: T.Bounds -> T.Bounds -> List (T.Bounds, T.Bounds) -> Fuzzy Bool
isAllowed2 _ _ [] = Resolved True
isAllowed2 xDomain yDomain ((firstX, firstY) : rest)
  | Range.overlap firstX xDomain > Qty.zero && Range.overlap firstY yDomain > Qty.zero =
      if Range.contains xDomain firstX && Range.contains yDomain firstY
        then Resolved False
        else Unresolved
  | otherwise = isAllowed2 xDomain yDomain rest
