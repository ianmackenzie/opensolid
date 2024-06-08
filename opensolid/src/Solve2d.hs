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
  , InfiniteRecursion (InfiniteRecursion)
  , run
  , Action
  , recurse
  , return
  )
where

import Bounds2d qualified
import OpenSolid
import Queue (Queue)
import Queue qualified
import Solve1d qualified
import Uv qualified

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

data Cache a
  = Atomic Subdomain a
  | Central Subdomain a ~(Cache a)
  | Horizontal Subdomain a ~(Cache a) ~(Cache a) ~(Cache a)
  | Vertical Subdomain a ~(Cache a) ~(Cache a) ~(Cache a)
  | Quadrant Subdomain a ~(Cache a) ~(Cache a) ~(Cache a) ~(Cache a) ~(Cache a) ~(Cache a) ~(Cache a) ~(Cache a) ~(Cache a)

init :: (Uv.Bounds -> a) -> Cache a
init function = quadrant function domain

central :: (Uv.Bounds -> a) -> Subdomain -> Cache a
central function subdomain = do
  let cached = function (bounds subdomain)
  if isAtomic subdomain
    then Atomic subdomain cached
    else Central subdomain cached (central function (half subdomain))

horizontal :: (Uv.Bounds -> a) -> Subdomain -> Cache a
horizontal function subdomain = do
  let cached = function (bounds subdomain)
  if isAtomic subdomain
    then Atomic subdomain cached
    else do
      let (Subdomain x y) = subdomain
      let (x1, x2) = Solve1d.bisect x
      let leftChild = horizontal function (Subdomain x1 y)
      let middleChild = central function (half subdomain)
      let rightChild = horizontal function (Subdomain x2 y)
      Horizontal subdomain cached leftChild middleChild rightChild

vertical :: (Uv.Bounds -> a) -> Subdomain -> Cache a
vertical function subdomain = do
  let cached = function (bounds subdomain)
  if isAtomic subdomain
    then Atomic subdomain cached
    else do
      let (Subdomain x y) = subdomain
      let (y1, y2) = Solve1d.bisect y
      let bottomChild = vertical function (Subdomain x y1)
      let middleChild = central function (half subdomain)
      let topChild = vertical function (Subdomain x y2)
      Vertical subdomain cached bottomChild middleChild topChild

quadrant :: (Uv.Bounds -> a) -> Subdomain -> Cache a
quadrant function subdomain = do
  let cached = function (bounds subdomain)
  if isAtomic subdomain
    then Atomic subdomain cached
    else do
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
      Quadrant
        subdomain
        cached
        bottomLeftChild
        bottomMiddleChild
        bottomRightChild
        middleLeftChild
        middleChild
        middleRightChild
        topLeftChild
        topMiddleChild
        topRightChild

data InfiniteRecursion = InfiniteRecursion deriving (Eq, Show, Error)

run :: b -> (Subdomain -> a -> b -> Action b) -> Cache a -> Result InfiniteRecursion b
run initialState processSubdomain cache =
  process initialState processSubdomain (Queue.singleton cache)

process :: b -> (Subdomain -> a -> b -> Action b) -> Queue (Cache a) -> Result InfiniteRecursion b
process currentState processSubdomain queue =
  case Queue.pop queue of
    Just (first, remaining) -> case first of
      Atomic subdomain cached ->
        case processSubdomain subdomain cached currentState of
          Return updatedState -> process updatedState processSubdomain remaining
          Recurse _ -> Error InfiniteRecursion
      Central subdomain cached child ->
        case processSubdomain subdomain cached currentState of
          Return updatedState -> process updatedState processSubdomain remaining
          Recurse updatedState -> do
            let updatedQueue = remaining + child
            process updatedState processSubdomain updatedQueue
      Horizontal subdomain cached left middle right ->
        case processSubdomain subdomain cached currentState of
          Return updatedState -> process updatedState processSubdomain remaining
          Recurse updatedState -> do
            let updatedQueue = remaining + middle + left + right
            process updatedState processSubdomain updatedQueue
      Vertical subdomain cached bottom middle top ->
        case processSubdomain subdomain cached currentState of
          Return updatedState -> process updatedState processSubdomain remaining
          Recurse updatedState -> do
            let updatedQueue = remaining + middle + bottom + top
            process updatedState processSubdomain updatedQueue
      Quadrant subdomain cached bottomLeft bottomMiddle bottomRight middleLeft middle middleRight topLeft topMiddle topRight ->
        case processSubdomain subdomain cached currentState of
          Return updatedState -> process updatedState processSubdomain remaining
          Recurse updatedState -> do
            let updatedQueue =
                  remaining
                    |> Queue.push middle
                    |> Queue.push middleLeft
                    |> Queue.push middleRight
                    |> Queue.push bottomMiddle
                    |> Queue.push topMiddle
                    |> Queue.push bottomLeft
                    |> Queue.push bottomRight
                    |> Queue.push topLeft
                    |> Queue.push topRight
            process updatedState processSubdomain updatedQueue
    Nothing -> Ok currentState

data Action a
  = Return a
  | Recurse a

return :: a -> Action a
return = Return

recurse :: a -> Action a
recurse = Recurse
