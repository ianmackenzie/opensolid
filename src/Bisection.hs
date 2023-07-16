module Bisection
  ( Domain
  , Tree
  , tree
  , solve
  , solve2
  )
where

import Domain (Domain)
import Domain qualified
import OpenSolid
import Range qualified

data Cache segment = Cache Domain ~segment

data Tree segment
  = Tree (Cache segment) (Cache segment) ~(Tree segment) ~(Tree segment)

tree :: (Domain -> segment) -> Tree segment
tree compute = buildTree compute Domain.unit

buildTree :: (Domain -> segment) -> Domain -> Tree segment
buildTree compute domain =
  let cache = Cache domain (compute domain)
      neighborhoodDomain = Domain.expand domain
      neighborhoodCache = Cache neighborhoodDomain (compute neighborhoodDomain)
      (left, right) = Range.bisect domain
   in Tree cache neighborhoodCache (buildTree compute left) (buildTree compute right)

solve ::
  (Domain -> segment -> Bool) ->
  (Domain -> segment -> Fuzzy resolution) ->
  (Domain -> segment -> resolution -> Maybe solution) ->
  Tree segment ->
  (List solution, List Domain) ->
  (List solution, List Domain)
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

isAllowed :: Domain -> List Domain -> Fuzzy Bool
isAllowed _ [] = Resolved True
isAllowed domain (first : rest)
  | Range.intersects domain first =
      if Range.contains domain first
        then Resolved False
        else Unresolved
  | otherwise = isAllowed domain rest

solve2 ::
  Show solution =>
  (Domain -> Domain -> segment -> segment -> Bool) ->
  (Domain -> Domain -> segment -> segment -> Fuzzy resolution) ->
  (Domain -> Domain -> segment -> segment -> resolution -> Maybe solution) ->
  Tree segment ->
  Tree segment ->
  (List solution, List (Domain, Domain)) ->
  (List solution, List (Domain, Domain))
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

isAllowed2 :: Domain -> Domain -> List (Domain, Domain) -> Fuzzy Bool
isAllowed2 _ _ [] = Resolved True
isAllowed2 xDomain yDomain ((firstX, firstY) : rest)
  | Range.intersects xDomain firstX && Range.intersects yDomain firstY =
      if Range.contains xDomain firstX && Range.contains yDomain firstY
        then Resolved False
        else Unresolved
  | otherwise = isAllowed2 xDomain yDomain rest
