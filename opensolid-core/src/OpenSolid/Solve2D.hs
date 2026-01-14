module OpenSolid.Solve2D
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

import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Domain1D qualified as Domain1D
import OpenSolid.Domain2D (Domain2D (Domain2D))
import OpenSolid.Domain2D qualified as Domain2D
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Queue (Queue)
import OpenSolid.Queue qualified as Queue
import OpenSolid.Result qualified as Result
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorBounds2D (VectorBounds2D)

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

data InfiniteRecursion = InfiniteRecursion deriving (Eq, Show)

type Callback context solution =
  forall exclusions.
  context ->
  Domain2D ->
  Exclusions exclusions ->
  Action exclusions context solution

search :: Callback context solution -> context -> Result InfiniteRecursion (List solution)
search callback initialContext =
  Result.map Pair.first $
    process callback (Queue.singleton (Domain2D.unit, initialContext, Quadrant)) [] []

process ::
  Callback context solution ->
  Queue (Domain2D, context, RecursionType) ->
  List solution ->
  List Domain2D ->
  Result InfiniteRecursion (List solution, List Domain2D)
process callback queue solutions exclusions =
  case Queue.pop queue of
    Nothing -> Ok (solutions, exclusions) -- We're done! No more subdomains to process
    Just ((subdomain, context, recursionType), remaining) -> do
      -- TODO optimize to check for containment and overlapping subdomains in a single pass
      -- (maybe use the call stack to avoid even constructing the overlapping domain list
      -- if the subdomain is actually contained?)
      let filteredExclusions = List.filter (Domain2D.overlaps subdomain) exclusions
      let convergenceSubdomain = convergenceDomain subdomain recursionType
      if List.anySatisfy (Domain2D.contains convergenceSubdomain) filteredExclusions
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

convergenceDomain :: Domain2D -> RecursionType -> Domain2D
convergenceDomain subdomain recursionType = do
  let Domain2D xSubdomain ySubdomain = subdomain
  let midX = Domain1D.constant (Domain1D.midpoint xSubdomain)
  let midY = Domain1D.constant (Domain1D.midpoint ySubdomain)
  case recursionType of
    Quadrant -> subdomain
    Row -> Domain2D xSubdomain midY
    Column -> Domain2D midX ySubdomain
    Central -> Domain2D midX midY

recurseInto ::
  Domain2D ->
  context ->
  RecursionType ->
  Result InfiniteRecursion (List (Domain2D, context, RecursionType))
recurseInto subdomain context recursionType
  | Domain2D.isAtomic subdomain = Error InfiniteRecursion
  | otherwise = Ok $ case recursionType of
      Central ->
        [(Domain2D.half subdomain, context, Central)]
      Row -> do
        let Domain2D x y = subdomain
        let (x1, x2) = Domain1D.bisect x
        let xMid = Domain1D.half x
        let yMid = Domain1D.half y
        let left = (Domain2D x1 yMid, context, Row)
        let middle = (Domain2D xMid yMid, context, Central)
        let right = (Domain2D x2 yMid, context, Row)
        [left, middle, right]
      Column -> do
        let Domain2D x y = subdomain
        let xMid = Domain1D.half x
        let yMid = Domain1D.half y
        let (y1, y2) = Domain1D.bisect y
        let bottom = (Domain2D xMid y1, context, Column)
        let middle = (Domain2D xMid yMid, context, Central)
        let top = (Domain2D xMid y2, context, Column)
        [bottom, middle, top]
      Quadrant -> do
        let Domain2D x y = subdomain
        let (x1, x2) = Domain1D.bisect x
        let (y1, y2) = Domain1D.bisect y
        let xMid = Domain1D.half x
        let yMid = Domain1D.half y
        let lBot = (Domain2D x1 y1, context, Quadrant)
        let cBot = (Domain2D xMid y1, context, Column)
        let rBot = (Domain2D x2 y1, context, Quadrant)
        let lMid = (Domain2D x1 yMid, context, Row)
        let cMid = (Domain2D xMid yMid, context, Central)
        let rMid = (Domain2D x2 yMid, context, Row)
        let lTop = (Domain2D x1 y2, context, Quadrant)
        let cTop = (Domain2D xMid y2, context, Column)
        let rTop = (Domain2D x2 y2, context, Quadrant)
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
  (UvBounds -> VectorBounds2D units space) ->
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  UvBounds ->
  Maybe UvPoint
unique fBounds f fu fv uvBounds =
  solveUnique uvBounds fBounds f fu fv uvBounds

solveUnique ::
  Tolerance units =>
  UvBounds ->
  (UvBounds -> VectorBounds2D units space) ->
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  UvBounds ->
  Maybe UvPoint
solveUnique localBounds fBounds f fu fv globalBounds =
  -- First check if it's *possible* that there's a solution within localBounds
  if fBounds localBounds `intersects` Vector2D.zero
    then do
      let Bounds2D uBounds vBounds = localBounds
      let uMid = Interval.midpoint uBounds
      let vMid = Interval.midpoint vBounds
      let pMid = UvPoint uMid vMid
      let fMid = f pMid
      -- First, try applying Newton-Raphson starting at the center point of localBounds
      -- to see if that converges to a zero
      case newtonRaphson f fu fv globalBounds pMid fMid of
        Ok point -> Just point -- Newton-Raphson converged to a zero, return it
        Error Divergence -- Newton-Raphson did not converge starting from pMid
          | Interval.isAtomic uBounds || Interval.isAtomic vBounds ->
              -- We can't bisect any further
              -- (Newton-Raphson somehow never converged),
              -- so check if we've found a zero by bisection
              if fMid ~= Vector2D.zero then Just pMid else Nothing
          | otherwise -> do
              -- Recurse into subdomains
              let (u1, u2) = Interval.bisect uBounds
              let (v1, v2) = Interval.bisect vBounds
              let solveRecursively uv = solveUnique uv fBounds f fu fv globalBounds
              Maybe.oneOf
                [ solveRecursively (Bounds2D u1 v1)
                , solveRecursively (Bounds2D u1 v2)
                , solveRecursively (Bounds2D u2 v1)
                , solveRecursively (Bounds2D u2 v2)
                ]
    else Nothing

data Divergence = Divergence deriving (Eq, Show)

newtonRaphson ::
  Tolerance units =>
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  UvBounds ->
  UvPoint ->
  Vector2D units space ->
  Result Divergence UvPoint
newtonRaphson = solveNewtonRaphson 0

solveNewtonRaphson ::
  Tolerance units =>
  Int ->
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  (UvPoint -> Vector2D units space) ->
  UvBounds ->
  UvPoint ->
  Vector2D units space ->
  Result Divergence UvPoint
solveNewtonRaphson iterations f fu fv uvBounds p1 f1 =
  if iterations > 10 -- Check if we've entered an infinite loop
    then Error Divergence
    else do
      let Vector2D x1 y1 = f1
      let Vector2D xu1 yu1 = fu p1
      let Vector2D xv1 yv1 = fv p1
      let determinant = xu1 ?*? yv1 .-. xv1 ?*? yu1
      if determinant == Quantity.zero
        then Error Divergence
        else do
          let deltaU = (xv1 ?*? y1 .-. yv1 ?*? x1) ./. determinant
          let deltaV = (yu1 ?*? x1 .-. xu1 ?*? y1) ./. determinant
          let p2 = boundedStep uvBounds p1 (p1 .+. Vector2D deltaU deltaV)
          let f2 = f p2
          if Vector2D.squaredMagnitude_ f2 >= Vector2D.squaredMagnitude_ f1
            then -- We've stopped converging, check if we've actually found a root
              if f1 ~= Vector2D.zero then Ok p1 else Error Divergence
            else -- We're still converging, so take another iteration
              solveNewtonRaphson (iterations + 1) f fu fv uvBounds p2 f2

boundedStep :: UvBounds -> UvPoint -> UvPoint -> UvPoint
boundedStep uvBounds p1 p2 =
  if Bounds2D.includes p2 uvBounds
    then p2 -- Stepped point is still within the given bounds, so we can use it
    else do
      -- Stepped point is outside the given bounds,
      -- pull it back in along the step direction
      let Bounds2D uBounds vBounds = uvBounds
      let UvPoint u1 v1 = p1
      let UvPoint u2 v2 = p2
      let clampedU = Quantity.clampTo uBounds u2
      let clampedV = Quantity.clampTo vBounds v2
      let uScale = if u1 == u2 then 1 else (clampedU .-. u1) ./. (u2 .-. u1)
      let vScale = if v1 == v2 then 1 else (clampedV .-. v1) ./. (v2 .-. v1)
      let scale = min uScale vScale
      let UvPoint u v = Point2D.interpolateFrom p1 p2 scale
      -- Perform a final clamping step
      -- in case numerical roundoff during interpolation
      -- left the point *slightly* outside uvBounds
      UvPoint (Quantity.clampTo uBounds u) (Quantity.clampTo vBounds v)
