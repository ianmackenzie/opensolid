module Solve2d
  ( Subdomain
  , domain
  , isAtomic
  , interior
  , bounds
  , overlaps
  , contains
  , Cache
  , init
  , SomeExclusions
  , NoExclusions
  , Exclusions (NoExclusions, SomeExclusions)
  , InfiniteRecursion (InfiniteRecursion)
  , search
  , Action
  , return
  , recurse
  , pass
  , newtonRaphson
  )
where

import Bounds2d qualified
import List qualified
import OpenSolid
import Qty qualified
import Queue (Queue)
import Queue qualified
import Solve1d qualified
import Uv qualified
import Vector2d qualified

data Subdomain = Subdomain Solve1d.Subdomain Solve1d.Subdomain

domain :: Subdomain
domain = Subdomain Solve1d.domain Solve1d.domain

isAtomic :: Subdomain -> Bool
isAtomic (Subdomain x y) = Solve1d.isAtomic x && Solve1d.isAtomic y

half :: Subdomain -> Subdomain
half (Subdomain x y) = Subdomain (Solve1d.half x) (Solve1d.half y)

bounds :: Subdomain -> Uv.Bounds
bounds (Subdomain x y) = Bounds2d.xy (Solve1d.bounds x) (Solve1d.bounds y)

interior :: Subdomain -> Uv.Bounds
interior (Subdomain x y) = Bounds2d.xy (Solve1d.interior x) (Solve1d.interior y)

overlaps :: Subdomain -> Subdomain -> Bool
overlaps (Subdomain x2 y2) (Subdomain x1 y1) = Solve1d.overlaps x2 x1 && Solve1d.overlaps y2 y1

contains :: Subdomain -> Subdomain -> Bool
contains (Subdomain x2 y2) (Subdomain x1 y1) = Solve1d.contains x2 x1 && Solve1d.contains y2 y1

data Cache cached
  = Tree Subdomain cached (Node cached)

data Node cached
  = Atomic
  | Central ~(Cache cached)
  | Horizontal ~(Cache cached) ~(Cache cached) ~(Cache cached)
  | Vertical ~(Cache cached) ~(Cache cached) ~(Cache cached)
  | Quadrant
      ~(Cache cached)
      ~(Cache cached)
      ~(Cache cached)
      ~(Cache cached)
      ~(Cache cached)
      ~(Cache cached)
      ~(Cache cached)
      ~(Cache cached)
      ~(Cache cached)

init :: (Uv.Bounds -> cached) -> Cache cached
init function = quadrant function domain

tree :: (Uv.Bounds -> cached) -> Subdomain -> Node cached -> Cache cached
tree function subdomain givenNode = do
  let cached = function (bounds subdomain)
  let node = if isAtomic subdomain then Atomic else givenNode
  Tree subdomain cached node

central :: (Uv.Bounds -> a) -> Subdomain -> Cache a
central function subdomain = do
  let child = central function (half subdomain)
  let node = Central child
  tree function subdomain node

horizontal :: (Uv.Bounds -> a) -> Subdomain -> Cache a
horizontal function subdomain = do
  let (Subdomain x y) = subdomain
  let (x1, x2) = Solve1d.bisect x
  let leftChild = horizontal function (Subdomain x1 y)
  let middleChild = central function (half subdomain)
  let rightChild = horizontal function (Subdomain x2 y)
  let node = Horizontal leftChild middleChild rightChild
  tree function subdomain node

vertical :: (Uv.Bounds -> a) -> Subdomain -> Cache a
vertical function subdomain = do
  let (Subdomain x y) = subdomain
  let (y1, y2) = Solve1d.bisect y
  let bottomChild = vertical function (Subdomain x y1)
  let middleChild = central function (half subdomain)
  let topChild = vertical function (Subdomain x y2)
  let node = Vertical bottomChild middleChild topChild
  tree function subdomain node

quadrant :: (Uv.Bounds -> a) -> Subdomain -> Cache a
quadrant function subdomain = do
  let (Subdomain x y) = subdomain
  let (x1, x2) = Solve1d.bisect x
  let (y1, y2) = Solve1d.bisect y
  let xMid = Solve1d.half x
  let yMid = Solve1d.half y
  let bottomLeftChild = quadrant function (Subdomain x1 y1)
  let bottomMiddleChild = vertical function (Subdomain xMid y1)
  let bottomRightChild = quadrant function (Subdomain x2 y1)
  let middleLeftChild = horizontal function (Subdomain x1 yMid)
  let middleChild = central function (Subdomain xMid yMid)
  let middleRightChild = horizontal function (Subdomain x2 yMid)
  let topLeftChild = quadrant function (Subdomain x1 y2)
  let topMiddleChild = vertical function (Subdomain xMid y2)
  let topRightChild = quadrant function (Subdomain x2 y2)
  let node =
        Quadrant
          bottomLeftChild
          bottomMiddleChild
          bottomRightChild
          middleLeftChild
          middleChild
          middleRightChild
          topLeftChild
          topMiddleChild
          topRightChild
  tree function subdomain node

data NoExclusions

data SomeExclusions

data Exclusions exclusions where
  NoExclusions :: Exclusions NoExclusions
  SomeExclusions :: NonEmpty Subdomain -> Exclusions SomeExclusions

data InfiniteRecursion = InfiniteRecursion deriving (Eq, Show, Error)

type Callback cached solution =
  forall exclusions.
  Subdomain ->
  cached ->
  Exclusions exclusions ->
  Action exclusions solution

search ::
  Callback cached solution ->
  Cache cached ->
  List solution ->
  List Subdomain ->
  Result InfiniteRecursion (List solution, List Subdomain)
search callback cache solutions exclusions =
  process callback (Queue.singleton cache) solutions exclusions

process ::
  forall cached solution.
  Callback cached solution ->
  Queue (Cache cached) ->
  List solution ->
  List Subdomain ->
  Result InfiniteRecursion (List solution, List Subdomain)
process callback queue solutions exclusions =
  case Queue.pop queue of
    Just (Tree subdomain cached node, remaining) -> do
      let filteredExclusions = List.filter (overlaps subdomain) exclusions
      if List.any (contains subdomain) filteredExclusions
        then process callback remaining solutions exclusions
        else case filteredExclusions of
          NonEmpty someExclusions ->
            case callback subdomain cached (SomeExclusions someExclusions) of
              Pass -> process callback remaining solutions exclusions
              Recurse -> recurseIntoChildrenOf node callback remaining solutions exclusions
          [] ->
            case callback subdomain cached NoExclusions of
              Pass -> process callback remaining solutions exclusions
              Recurse -> recurseIntoChildrenOf node callback remaining solutions exclusions
              Return solution ->
                process callback remaining (solution : solutions) (subdomain : exclusions)
    Nothing -> Ok (solutions, exclusions)

{-# INLINE recurseIntoChildrenOf #-}
recurseIntoChildrenOf ::
  forall cached solution.
  Node cached ->
  Callback cached solution ->
  Queue (Cache cached) ->
  List solution ->
  List Subdomain ->
  Result InfiniteRecursion (List solution, List Subdomain)
recurseIntoChildrenOf node callback queue solutions exclusions =
  case node of
    Atomic -> Error InfiniteRecursion
    Central child -> process callback (queue + child) solutions exclusions
    Horizontal left middle right -> do
      let updatedQueue = queue + middle + left + right
      process callback updatedQueue solutions exclusions
    Vertical bottom middle top -> do
      let updatedQueue = queue + middle + bottom + top
      process callback updatedQueue solutions exclusions
    Quadrant bottomLeft bottomMiddle bottomRight middleLeft middle middleRight topLeft topMiddle topRight -> do
      let updatedQueue =
            queue
              |> Queue.push middle
              |> Queue.push middleLeft
              |> Queue.push middleRight
              |> Queue.push bottomMiddle
              |> Queue.push topMiddle
              |> Queue.push bottomLeft
              |> Queue.push bottomRight
              |> Queue.push topLeft
              |> Queue.push topRight
      process callback updatedQueue solutions exclusions

data Action exclusions solution where
  Return :: solution -> Action NoExclusions solution
  Recurse :: Action exclusions solution
  Pass :: Action exclusions solution

return :: solution -> Action NoExclusions solution
return = Return

recurse :: Action exclusions solution
recurse = Recurse

pass :: Action exclusions solution
pass = Pass

data Divergence = Divergence deriving (Eq, Show, Error)

newtonRaphson ::
  Tolerance units =>
  (Uv.Point -> Qty units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Point -> Qty units) ->
  Uv.Bounds ->
  Uv.Point ->
  Qty units ->
  Qty units ->
  Int ->
  Result Divergence Uv.Point
newtonRaphson f fu fv g gu gv uvBounds p1 f1 g1 iterations =
  if iterations > 10 -- Check if we've entered an infinite loop
    then Error Divergence
    else do
      let fu1 = fu p1
      let fv1 = fv p1
      let gu1 = gu p1
      let gv1 = gv p1
      let d = fu1 .*. gv1 - fv1 .*. gu1
      let deltaU = (fv1 .*. g1 - gv1 .*. f1) / d
      let deltaV = (gu1 .*. f1 - fu1 .*. g1) / d
      let p2 = p1 + Vector2d.xy deltaU deltaV
      if not (Bounds2d.includes p2 uvBounds) -- Check if we stepped outside the given bounds
        then Error Divergence
        else do
          let f2 = f p2
          let g2 = g p2
          if Qty.hypot2 f2 g2 >= Qty.hypot2 f1 g1 -- Check if we've stopped converging
            then if f1 ~= Qty.zero && g1 ~= Qty.zero then Ok p1 else Error Divergence
            else -- We're still converging, so take another iteration
              newtonRaphson f fu fv g gu gv uvBounds p2 f2 g2 (iterations + 1)
