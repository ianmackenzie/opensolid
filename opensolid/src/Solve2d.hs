module Solve2d
  ( Cache
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

import OpenSolid
import Domain2d (Domain2d(Domain2d))
import Domain2d qualified
import List qualified
import Vector2d qualified
import Uv qualified
import Domain1d qualified
import Qty qualified
import Maybe qualified
import Pair qualified
import Bounds2d qualified
import Result qualified
import Point2d qualified
import Queue (Queue)
import Range qualified
import Range (Range)
import Queue qualified

data Cache cached
  = Tree Domain2d cached (Node cached)

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
init function = quadrant function Domain2d.unit

tree :: (Uv.Bounds -> cached) -> Domain2d -> Node cached -> Cache cached
tree function subdomain givenNode = do
  let cached = function (Domain2d.bounds subdomain)
  let node = if Domain2d.isAtomic subdomain then Atomic else givenNode
  Tree subdomain cached node

central :: (Uv.Bounds -> a) -> Domain2d -> Cache a
central function subdomain = do
  let child = central function (Domain2d.half subdomain)
  let node = Central child
  tree function subdomain node

row :: (Uv.Bounds -> a) -> Domain2d -> Cache a
row function subdomain = do
  let (Domain2d x y) = subdomain
  let (x1, x2) = Domain1d.bisect x
  let yMid = Domain1d.half y
  let leftChild = row function (Domain2d x1 yMid)
  let middleChild = central function (Domain2d.half subdomain)
  let rightChild = row function (Domain2d x2 yMid)
  let node = Row leftChild middleChild rightChild
  tree function subdomain node

column :: (Uv.Bounds -> a) -> Domain2d -> Cache a
column function subdomain = do
  let (Domain2d x y) = subdomain
  let xMid = Domain1d.half x
  let (y1, y2) = Domain1d.bisect y
  let bottomChild = column function (Domain2d xMid y1)
  let middleChild = central function (Domain2d.half subdomain)
  let topChild = column function (Domain2d xMid y2)
  let node = Column bottomChild middleChild topChild
  tree function subdomain node

quadrant :: (Uv.Bounds -> a) -> Domain2d -> Cache a
quadrant function subdomain = do
  let (Domain2d x y) = subdomain
  let (x1, x2) = Domain1d.bisect x
  let (y1, y2) = Domain1d.bisect y
  let xMid = Domain1d.half x
  let yMid = Domain1d.half y
  let bottomLeftChild = quadrant function (Domain2d x1 y1)
  let bottomMiddleChild = column function (Domain2d xMid y1)
  let bottomRightChild = quadrant function (Domain2d x2 y1)
  let middleLeftChild = row function (Domain2d x1 yMid)
  let middleChild = central function (Domain2d xMid yMid)
  let middleRightChild = row function (Domain2d x2 yMid)
  let topLeftChild = quadrant function (Domain2d x1 y2)
  let topMiddleChild = column function (Domain2d xMid y2)
  let topRightChild = quadrant function (Domain2d x2 y2)
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
  SomeExclusions :: NonEmpty Domain2d -> Exclusions SomeExclusions

deriving instance Show (Exclusions exclusions)

data InfiniteRecursion = InfiniteRecursion deriving (Eq, Show, Error)

type Callback cached solution =
  forall exclusions.
  Domain2d ->
  cached ->
  Exclusions exclusions ->
  Action exclusions solution

search ::
  Callback cached solution ->
  Cache cached ->
  List (solution, Domain2d) ->
  Result InfiniteRecursion (List (solution, Domain2d))
search callback cache accumulated =
  process callback (Queue.singleton cache) accumulated

process ::
  forall cached solution.
  Callback cached solution ->
  Queue (Cache cached) ->
  List (solution, Domain2d) ->
  Result InfiniteRecursion (List (solution, Domain2d))
process callback queue accumulated =
  case Queue.pop queue of
    Nothing -> Ok accumulated -- We're done! No more subdomains to process
    Just (Tree subdomain cached node, remaining) -> do
      -- TODO optimize to check for containment and overlapping subdomains in a single pass
      -- (maybe use the call stack to avoid even constructing the overlapping domain list
      -- if the subdomain is actually contained?)
      let filteredExclusions = 
            List.map Pair.second accumulated
              |> List.filter (Domain2d.overlaps subdomain)
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

containedBy :: List Domain2d -> Domain2d -> Bool
containedBy exclusions subdomain =
  List.any (Domain2d.contains subdomain) exclusions || do
    let (Domain2d x y) = subdomain
    let (x1, x2) = Domain1d.endpoints (Domain1d.half x)
    let (y1, y2) = Domain1d.endpoints (Domain1d.half y)
    checkContainment exclusions x1 x2 y1 y2 False False False False

checkContainment ::
  List Domain2d ->
  Domain1d.Boundary ->
  Domain1d.Boundary ->
  Domain1d.Boundary ->
  Domain1d.Boundary ->
  Bool ->
  Bool ->
  Bool ->
  Bool ->
  Bool
checkContainment exclusions x1 x2 y1 y2 contained11 contained12 contained21 contained22 =
  case exclusions of
    [] -> contained11 && contained12 && contained21 && contained22
    first : rest -> do
      let (Domain2d x y) = first
      let updated11 = contained11 || (Domain1d.includes x1 x && Domain1d.includes y1 y)
      let updated12 = contained12 || (Domain1d.includes x1 x && Domain1d.includes y2 y)
      let updated21 = contained21 || (Domain1d.includes x2 x && Domain1d.includes y1 y)
      let updated22 = contained22 || (Domain1d.includes x2 x && Domain1d.includes y2 y)
      checkContainment rest x1 x2 y1 y2 updated11 updated12 updated21 updated22

{-# INLINE recurseIntoChildrenOf #-}
recurseIntoChildrenOf ::
  forall cached solution.
  Node cached ->
  Callback cached solution ->
  Queue (Cache cached) ->
  List (solution, Domain2d) ->
  Result InfiniteRecursion (List (solution, Domain2d))
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
