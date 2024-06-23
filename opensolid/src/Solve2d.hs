module Solve2d
  ( Subdomain
  , Boundary
  , domain
  , isAtomic
  , coordinates
  , leftBoundary
  , rightBoundary
  , bottomBoundary
  , topBoundary
  , adjacent
  , contacts
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
  , unique
  , newtonRaphson
  )
where

import Bounds2d qualified
import List qualified
import Maybe qualified
import OpenSolid
import Pair qualified
import Point2d qualified
import Qty qualified
import Queue (Queue)
import Queue qualified
import Range (Range)
import Range qualified
import Result qualified
import Solve1d qualified
import Uv qualified
import Vector2d qualified

data Subdomain = Subdomain Solve1d.Subdomain Solve1d.Subdomain deriving (Show)

data Boundary
  = VerticalBoundary Solve1d.Endpoint Solve1d.Subdomain
  | HorizontalBoundary Solve1d.Subdomain Solve1d.Endpoint
  deriving (Show)

domain :: Subdomain
domain = Subdomain Solve1d.domain Solve1d.domain

isAtomic :: Subdomain -> Bool
isAtomic (Subdomain x y) = Solve1d.isAtomic x && Solve1d.isAtomic y

coordinates :: Subdomain -> (Solve1d.Subdomain, Solve1d.Subdomain)
coordinates (Subdomain x y) = (x, y)

leftBoundary :: Subdomain -> Boundary
leftBoundary (Subdomain x y) = VerticalBoundary (Solve1d.min x) y

rightBoundary :: Subdomain -> Boundary
rightBoundary (Subdomain x y) = VerticalBoundary (Solve1d.max x) y

bottomBoundary :: Subdomain -> Boundary
bottomBoundary (Subdomain x y) = HorizontalBoundary x (Solve1d.min y)

topBoundary :: Subdomain -> Boundary
topBoundary (Subdomain x y) = HorizontalBoundary x (Solve1d.max y)

adjacent :: Boundary -> Boundary -> Bool
adjacent (HorizontalBoundary{}) (VerticalBoundary{}) = False
adjacent (VerticalBoundary{}) (HorizontalBoundary{}) = False
adjacent (HorizontalBoundary x1 y1) (HorizontalBoundary x2 y2) = Solve1d.overlaps x1 x2 && y1 == y2
adjacent (VerticalBoundary x1 y1) (VerticalBoundary x2 y2) = x1 == x2 && Solve1d.overlaps y1 y2

contacts :: Subdomain -> Boundary -> Bool
contacts (Subdomain x y) boundary = case boundary of
  VerticalBoundary bx by -> Solve1d.overlaps by y && (bx == Solve1d.min x || bx == Solve1d.max x)
  HorizontalBoundary bx by -> Solve1d.overlaps bx x && (by == Solve1d.min y || by == Solve1d.max y)

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
  | Row ~(Cache cached) ~(Cache cached) ~(Cache cached)
  | Column ~(Cache cached) ~(Cache cached) ~(Cache cached)
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

row :: (Uv.Bounds -> a) -> Subdomain -> Cache a
row function subdomain = do
  let (Subdomain x y) = subdomain
  let (x1, x2) = Solve1d.bisect x
  let yMid = Solve1d.half y
  let leftChild = row function (Subdomain x1 yMid)
  let middleChild = central function (half subdomain)
  let rightChild = row function (Subdomain x2 yMid)
  let node = Row leftChild middleChild rightChild
  tree function subdomain node

column :: (Uv.Bounds -> a) -> Subdomain -> Cache a
column function subdomain = do
  let (Subdomain x y) = subdomain
  let xMid = Solve1d.half x
  let (y1, y2) = Solve1d.bisect y
  let bottomChild = column function (Subdomain xMid y1)
  let middleChild = central function (half subdomain)
  let topChild = column function (Subdomain xMid y2)
  let node = Column bottomChild middleChild topChild
  tree function subdomain node

quadrant :: (Uv.Bounds -> a) -> Subdomain -> Cache a
quadrant function subdomain = do
  let (Subdomain x y) = subdomain
  let (x1, x2) = Solve1d.bisect x
  let (y1, y2) = Solve1d.bisect y
  let xMid = Solve1d.half x
  let yMid = Solve1d.half y
  let bottomLeftChild = quadrant function (Subdomain x1 y1)
  let bottomMiddleChild = column function (Subdomain xMid y1)
  let bottomRightChild = quadrant function (Subdomain x2 y1)
  let middleLeftChild = row function (Subdomain x1 yMid)
  let middleChild = central function (Subdomain xMid yMid)
  let middleRightChild = row function (Subdomain x2 yMid)
  let topLeftChild = quadrant function (Subdomain x1 y2)
  let topMiddleChild = column function (Subdomain xMid y2)
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

deriving instance Show (Exclusions exclusions)

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
  List (solution, Subdomain) ->
  Result InfiniteRecursion (List (solution, Subdomain))
search callback cache accumulated =
  process callback (Queue.singleton cache) accumulated

process ::
  forall cached solution.
  Callback cached solution ->
  Queue (Cache cached) ->
  List (solution, Subdomain) ->
  Result InfiniteRecursion (List (solution, Subdomain))
process callback queue accumulated =
  case Queue.pop queue of
    Nothing -> Ok accumulated -- We're done! No more subdomains to process
    Just (Tree subdomain cached node, remaining) -> do
      -- TODO optimize to check for containment and overlapping subdomains in a single pass
      -- (maybe use the call stack to avoid even constructing the overlapping domain list
      -- if the subdomain is actually contained?)
      let filteredExclusions = List.filter (overlaps subdomain) (List.map Pair.second accumulated)
      if containedBy filteredExclusions subdomain
        then process callback remaining accumulated
        else do
          case filteredExclusions of
            NonEmpty someExclusions ->
              case callback subdomain cached (SomeExclusions someExclusions) of
                Pass -> process callback remaining accumulated
                Recurse -> recurseIntoChildrenOf node callback remaining accumulated
            [] ->
              case callback subdomain cached NoExclusions of
                Pass -> process callback remaining accumulated
                Recurse -> recurseIntoChildrenOf node callback remaining accumulated
                Return solution ->
                  process callback remaining ((solution, subdomain) : accumulated)

containedBy :: List Subdomain -> Subdomain -> Bool
containedBy exclusions subdomain =
  List.any (contains subdomain) exclusions || do
    let (Subdomain x y) = subdomain
    let (x1, x2) = Solve1d.endpoints (Solve1d.half x)
    let (y1, y2) = Solve1d.endpoints (Solve1d.half y)
    checkContainment exclusions x1 x2 y1 y2 False False False False

checkContainment ::
  List Subdomain ->
  Solve1d.Endpoint ->
  Solve1d.Endpoint ->
  Solve1d.Endpoint ->
  Solve1d.Endpoint ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  Bool
checkContainment exclusions x1 x2 y1 y2 contained11 contained12 contained21 contained22 =
  case exclusions of
    [] -> contained11 && contained12 && contained21 && contained22
    first : rest -> do
      let (Subdomain x y) = first
      let updated11 = contained11 || (Solve1d.includes x1 x && Solve1d.includes y1 y)
      let updated12 = contained12 || (Solve1d.includes x1 x && Solve1d.includes y2 y)
      let updated21 = contained21 || (Solve1d.includes x2 x && Solve1d.includes y1 y)
      let updated22 = contained22 || (Solve1d.includes x2 x && Solve1d.includes y2 y)
      checkContainment rest x1 x2 y1 y2 updated11 updated12 updated21 updated22

{-# INLINE recurseIntoChildrenOf #-}
recurseIntoChildrenOf ::
  forall cached solution.
  Node cached ->
  Callback cached solution ->
  Queue (Cache cached) ->
  List (solution, Subdomain) ->
  Result InfiniteRecursion (List (solution, Subdomain))
recurseIntoChildrenOf node callback queue accumulated = Result.do
  updatedQueue <- case node of
    Atomic -> Error InfiniteRecursion
    Central child -> Ok (queue + child)
    Row leftChild middleChild rightChild -> Ok (queue + middleChild + leftChild + rightChild)
    Column bottomChild middleChild topChild -> Ok (queue + middleChild + bottomChild + topChild)
    Quadrant bottomLeft bottomMiddle bottomRight middleLeft middle middleRight topLeft topMiddle topRight ->
      Ok (queue + middle + middleLeft + middleRight + bottomMiddle + topMiddle + bottomLeft + bottomRight + topLeft + topRight)
  process callback updatedQueue accumulated

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

unique ::
  Tolerance units =>
  (Uv.Bounds -> Range units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Bounds -> Range units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Point -> Qty units) ->
  Uv.Bounds ->
  Maybe Uv.Point
unique fBounds f fu fv gBounds g gu gv uvBounds =
  solveUnique uvBounds fBounds f fu fv gBounds g gu gv uvBounds

solveUnique ::
  Tolerance units =>
  Uv.Bounds ->
  (Uv.Bounds -> Range units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Bounds -> Range units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Point -> Qty units) ->
  (Uv.Point -> Qty units) ->
  Uv.Bounds ->
  Maybe Uv.Point
solveUnique localBounds fBounds f fu fv gBounds g gu gv globalBounds =
  -- First check if it's *possible* that there's a solution within localBounds
  if fBounds localBounds ^ Qty.zero && gBounds localBounds ^ Qty.zero
    then do
      let (uRange, vRange) = Bounds2d.coordinates localBounds
      let uMid = Range.midpoint uRange
      let vMid = Range.midpoint vRange
      let pMid = Point2d.xy uMid vMid
      let fMid = f pMid
      let gMid = g pMid
      -- First, try applying Newton-Raphson starting at the center point of localBounds
      -- to see if that converges to a root
      case newtonRaphson f fu fv g gu gv globalBounds pMid fMid gMid of
        Ok point -> Just point -- Newton-Raphson converged to a root, return it
        Error Divergence -- Newton-Raphson did not converge starting from pMid
          | Range.isAtomic uRange || Range.isAtomic vRange ->
              -- We can't bisect any further
              -- (Newton-Raphson somehow never converged),
              -- so check if we've found a root by bisection
              if fMid ~= Qty.zero && gMid ~= Qty.zero then Just pMid else Nothing
          | otherwise -> do
              -- Recurse into subdomains
              let (u1, u2) = Range.bisect uRange
              let (v1, v2) = Range.bisect vRange
              let solveRecursively uv = solveUnique uv fBounds f fu fv gBounds g gu gv globalBounds
              solveRecursively (Bounds2d.xy u1 v1)
                |> Maybe.orElse (solveRecursively (Bounds2d.xy u1 v2))
                |> Maybe.orElse (solveRecursively (Bounds2d.xy u2 v1))
                |> Maybe.orElse (solveRecursively (Bounds2d.xy u2 v2))
    else Nothing

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
  Result Divergence Uv.Point
newtonRaphson = solveNewtonRaphson 0

solveNewtonRaphson ::
  Tolerance units =>
  Int ->
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
  Result Divergence Uv.Point
solveNewtonRaphson iterations f fu fv g gu gv uvBounds p1 f1 g1 =
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
              solveNewtonRaphson (iterations + 1) f fu fv g gu gv uvBounds p2 f2 g2
