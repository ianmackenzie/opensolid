module OpenSolid.Solve2d
  ( SomeExclusions
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

import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Domain2d (Domain2d (Domain2d))
import OpenSolid.Domain2d qualified as Domain2d
import OpenSolid.Error qualified as Error
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude hiding (return, (+), (-))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Queue (Queue)
import OpenSolid.Queue qualified as Queue
import OpenSolid.Result qualified as Result
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d)
import Prelude ((+), (-), (/))

data RecursionType
  = Central
  | Row
  | Column
  | Quadrant

data NoExclusions

data SomeExclusions

data Exclusions exclusions where
  NoExclusions :: Exclusions NoExclusions
  SomeExclusions :: Exclusions SomeExclusions

deriving instance Show (Exclusions exclusions)

data InfiniteRecursion = InfiniteRecursion deriving (Eq, Show, Error.Message)

type Callback context solution =
  forall exclusions.
  context ->
  Domain2d ->
  Exclusions exclusions ->
  Action exclusions context solution

search :: Callback context solution -> context -> Result InfiniteRecursion (List solution)
search callback initialContext =
  Result.map Pair.first $
    process callback (Queue.singleton (Domain2d.unit, initialContext, Quadrant)) [] []

process ::
  Callback context solution ->
  Queue (Domain2d, context, RecursionType) ->
  List solution ->
  List Domain2d ->
  Result InfiniteRecursion (List solution, List Domain2d)
process callback queue solutions exclusions =
  case Queue.pop queue of
    Nothing -> Success (solutions, exclusions) -- We're done! No more subdomains to process
    Just ((subdomain, context, recursionType), remaining) -> do
      -- TODO optimize to check for containment and overlapping subdomains in a single pass
      -- (maybe use the call stack to avoid even constructing the overlapping domain list
      -- if the subdomain is actually contained?)
      let filteredExclusions = List.filter (Domain2d.overlaps subdomain) exclusions
      let convergenceSubdomain = convergenceDomain subdomain recursionType
      if List.anySatisfy (Domain2d.contains convergenceSubdomain) filteredExclusions
        then process callback remaining solutions exclusions
        else do
          case filteredExclusions of
            [] -> case callback context subdomain NoExclusions of
              Pass -> process callback remaining solutions exclusions
              Recurse updatedContext -> do
                children <- recurseInto subdomain updatedContext recursionType
                process callback (remaining .+. children) solutions exclusions
              Return newSolution -> do
                let updatedSolutions = newSolution : solutions
                let updatedExclusions = subdomain : exclusions
                process callback remaining updatedSolutions updatedExclusions
            List.OneOrMore -> case callback context subdomain SomeExclusions of
              Pass -> process callback remaining solutions exclusions
              Recurse updatedContext -> do
                children <- recurseInto subdomain updatedContext recursionType
                process callback (remaining .+. children) solutions exclusions

convergenceDomain :: Domain2d -> RecursionType -> Domain2d
convergenceDomain subdomain recursionType = do
  let Domain2d xSubdomain ySubdomain = subdomain
  let midX = Domain1d.constant (Domain1d.midpoint xSubdomain)
  let midY = Domain1d.constant (Domain1d.midpoint ySubdomain)
  case recursionType of
    Quadrant -> subdomain
    Row -> Domain2d xSubdomain midY
    Column -> Domain2d midX ySubdomain
    Central -> Domain2d midX midY

recurseInto ::
  Domain2d ->
  context ->
  RecursionType ->
  Result InfiniteRecursion (List (Domain2d, context, RecursionType))
recurseInto subdomain context recursionType
  | Domain2d.isAtomic subdomain = Failure InfiniteRecursion
  | otherwise = Success $ case recursionType of
      Central ->
        [(Domain2d.half subdomain, context, Central)]
      Row -> do
        let Domain2d x y = subdomain
        let (x1, x2) = Domain1d.bisect x
        let xMid = Domain1d.half x
        let yMid = Domain1d.half y
        let left = (Domain2d x1 yMid, context, Row)
        let middle = (Domain2d xMid yMid, context, Central)
        let right = (Domain2d x2 yMid, context, Row)
        [left, middle, right]
      Column -> do
        let Domain2d x y = subdomain
        let xMid = Domain1d.half x
        let yMid = Domain1d.half y
        let (y1, y2) = Domain1d.bisect y
        let bottom = (Domain2d xMid y1, context, Column)
        let middle = (Domain2d xMid yMid, context, Central)
        let top = (Domain2d xMid y2, context, Column)
        [bottom, middle, top]
      Quadrant -> do
        let Domain2d x y = subdomain
        let (x1, x2) = Domain1d.bisect x
        let (y1, y2) = Domain1d.bisect y
        let xMid = Domain1d.half x
        let yMid = Domain1d.half y
        let lBot = (Domain2d x1 y1, context, Quadrant)
        let cBot = (Domain2d xMid y1, context, Column)
        let rBot = (Domain2d x2 y1, context, Quadrant)
        let lMid = (Domain2d x1 yMid, context, Row)
        let cMid = (Domain2d xMid yMid, context, Central)
        let rMid = (Domain2d x2 yMid, context, Row)
        let lTop = (Domain2d x1 y2, context, Quadrant)
        let cTop = (Domain2d xMid y2, context, Column)
        let rTop = (Domain2d x2 y2, context, Quadrant)
        [lBot, cBot, rBot, lMid, cMid, rMid, lTop, cTop, rTop]

data Action exclusions context solution where
  Return :: solution -> Action NoExclusions context solution
  Recurse :: context -> Action exclusions context solution
  Pass :: Action exclusions context solution

return :: solution -> Action NoExclusions context solution
return = Return

recurse :: context -> Action exclusions context solution
recurse = Recurse

pass :: Action exclusions context solution
pass = Pass

unique ::
  Tolerance units =>
  (UvBounds -> VectorBounds2d (space @ units)) ->
  (UvPoint -> Vector2d (space @ units)) ->
  (UvPoint -> Vector2d (space @ units)) ->
  (UvPoint -> Vector2d (space @ units)) ->
  UvBounds ->
  Maybe UvPoint
unique fBounds f fu fv uvBounds =
  solveUnique uvBounds fBounds f fu fv uvBounds

solveUnique ::
  Tolerance units =>
  UvBounds ->
  (UvBounds -> VectorBounds2d (space @ units)) ->
  (UvPoint -> Vector2d (space @ units)) ->
  (UvPoint -> Vector2d (space @ units)) ->
  (UvPoint -> Vector2d (space @ units)) ->
  UvBounds ->
  Maybe UvPoint
solveUnique localBounds fBounds f fu fv globalBounds =
  -- First check if it's *possible* that there's a solution within localBounds
  if fBounds localBounds ^ Vector2d.zero
    then do
      let Bounds2d uBounds vBounds = localBounds
      let uMid = Bounds.midpoint uBounds
      let vMid = Bounds.midpoint vBounds
      let pMid = Point2d uMid vMid
      let fMid = f pMid
      -- First, try applying Newton-Raphson starting at the center point of localBounds
      -- to see if that converges to a zero
      case newtonRaphson f fu fv globalBounds pMid fMid of
        Success point -> Just point -- Newton-Raphson converged to a zero, return it
        Failure Divergence -- Newton-Raphson did not converge starting from pMid
          | Bounds.isAtomic uBounds || Bounds.isAtomic vBounds ->
              -- We can't bisect any further
              -- (Newton-Raphson somehow never converged),
              -- so check if we've found a zero by bisection
              if fMid ~= Vector2d.zero then Just pMid else Nothing
          | otherwise -> do
              -- Recurse into subdomains
              let (u1, u2) = Bounds.bisect uBounds
              let (v1, v2) = Bounds.bisect vBounds
              let solveRecursively uv = solveUnique uv fBounds f fu fv globalBounds
              solveRecursively (Bounds2d u1 v1)
                |> Maybe.orElse (solveRecursively (Bounds2d u1 v2))
                |> Maybe.orElse (solveRecursively (Bounds2d u2 v1))
                |> Maybe.orElse (solveRecursively (Bounds2d u2 v2))
    else Nothing

data Divergence = Divergence deriving (Eq, Show, Error.Message)

newtonRaphson ::
  Tolerance units =>
  (UvPoint -> Vector2d (space @ units)) ->
  (UvPoint -> Vector2d (space @ units)) ->
  (UvPoint -> Vector2d (space @ units)) ->
  UvBounds ->
  UvPoint ->
  Vector2d (space @ units) ->
  Result Divergence UvPoint
newtonRaphson = solveNewtonRaphson 0

solveNewtonRaphson ::
  Tolerance units =>
  Int ->
  (UvPoint -> Vector2d (space @ units)) ->
  (UvPoint -> Vector2d (space @ units)) ->
  (UvPoint -> Vector2d (space @ units)) ->
  UvBounds ->
  UvPoint ->
  Vector2d (space @ units) ->
  Result Divergence UvPoint
solveNewtonRaphson iterations f fu fv uvBounds p1 f1 =
  if iterations > 10 -- Check if we've entered an infinite loop
    then Failure Divergence
    else do
      let Vector2d x1 y1 = f1
      let Vector2d xu1 yu1 = fu p1
      let Vector2d xv1 yv1 = fv p1
      let determinant = xu1 #*# yv1 .-. xv1 #*# yu1
      if determinant == Quantity.zero
        then Failure Divergence
        else do
          let deltaU = (xv1 #*# y1 .-. yv1 #*# x1) ./. determinant
          let deltaV = (yu1 #*# x1 .-. xu1 #*# y1) ./. determinant
          let p2 = boundedStep uvBounds p1 (p1 .+. Vector2d deltaU deltaV)
          let f2 = f p2
          if Vector2d.squaredMagnitude# f2 >= Vector2d.squaredMagnitude# f1
            then -- We've stopped converging, check if we've actually found a root
              if f1 ~= Vector2d.zero then Success p1 else Failure Divergence
            else -- We're still converging, so take another iteration
              solveNewtonRaphson (iterations + 1) f fu fv uvBounds p2 f2

boundedStep :: UvBounds -> UvPoint -> UvPoint -> UvPoint
boundedStep uvBounds p1 p2 =
  if Bounds2d.includes p2 uvBounds
    then p2 -- Stepped point is still within the given bounds, so we can use it
    else do
      -- Stepped point is outside the given bounds,
      -- pull it back in along the step direction
      let Bounds2d uBounds vBounds = uvBounds
      let Point2d u1 v1 = p1
      let Point2d u2 v2 = p2
      let clampedU = Quantity.clampTo uBounds u2
      let clampedV = Quantity.clampTo vBounds v2
      let uScale = if u1 == u2 then 1.0 else (clampedU - u1) / (u2 - u1)
      let vScale = if v1 == v2 then 1.0 else (clampedV - v1) / (v2 - v1)
      let scale = Quantity.min uScale vScale
      let Point2d u v = Point2d.interpolateFrom p1 p2 scale
      -- Perform a final clamping step
      -- in case numerical roundoff during interpolation
      -- left the point *slightly* outside uvBounds
      Point2d (Quantity.clampTo uBounds u) (Quantity.clampTo vBounds v)
