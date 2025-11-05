module OpenSolid.Bezier
  ( Vector
  , Constraints
  , derivative
  , hermite
  , syntheticStart
  , syntheticEnd
  )
where

import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.HasZero (HasZero (zero))
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude

type Vector vector =
  ( HasZero vector
  , Addition vector vector vector
  , Subtraction vector vector vector
  , Multiplication vector Number vector
  )

type Constraints point vector =
  ( Vector vector
  , Addition point vector point
  , Subtraction point point vector
  )

derivative :: Constraints point vector => NonEmpty point -> NonEmpty vector
derivative controlPoints = do
  let scale = fromIntegral (NonEmpty.length controlPoints - 1)
  let scaledDifference p1 p2 = (p2 - p1) .* scale
  let scaledDifferences = NonEmpty.successive scaledDifference controlPoints
  case scaledDifferences of
    [] -> NonEmpty.one zero
    NonEmpty derivativeControlPoints -> derivativeControlPoints

hermite ::
  Constraints point vector =>
  point ->
  List vector ->
  point ->
  List vector ->
  NonEmpty point
hermite startPoint startDerivatives endPoint endDerivatives = do
  let numStartDerivatives = startDerivatives.length
  let numEndDerivatives = endDerivatives.length
  let curveDegree = fromIntegral (1 + numStartDerivatives + numEndDerivatives)
  let scaledStartDerivatives = scaleDerivatives Positive 1.0 curveDegree startDerivatives
  let scaledEndDerivatives = scaleDerivatives Negative 1.0 curveDegree endDerivatives
  let startControlPoints =
        derivedControlPoints startPoint 1 (numStartDerivatives + 1) scaledStartDerivatives
  let endControlPoints =
        derivedControlPoints endPoint 1 (numEndDerivatives + 1) scaledEndDerivatives
          |> List.reverse
  startPoint :| List.concat [startControlPoints, endControlPoints, [endPoint]]

scaleDerivatives :: Vector vector => Sign -> Number -> Number -> List vector -> List vector
scaleDerivatives _ _ _ [] = []
scaleDerivatives sign scale n (first : rest) = do
  let updatedScale = sign * scale / n
  first * updatedScale : scaleDerivatives sign updatedScale (n .- 1.0) rest

derivedControlPoints :: Constraints point vector => point -> Int -> Int -> List vector -> List point
derivedControlPoints previousPoint i n qs
  | i < n = do
      let newPoint = previousPoint + offset i qs
      newPoint : derivedControlPoints newPoint (i + 1) n qs
  | otherwise = []

offset :: Vector vector => Int -> List vector -> vector
offset i scaledDerivatives =
  List.take i scaledDerivatives
    |> List.mapWithIndex (\j q -> q .* fromIntegral (Int.choose (i - 1) j))
    |> List.foldl (+) zero

segment :: Constraints point vector => Number -> Number -> NonEmpty point -> NonEmpty point
segment t1 t2 controlPoints = do
  let newControlPoint index _ = blossom controlPoints t1 t2 index
  NonEmpty.mapWithIndex newControlPoint controlPoints

blossom :: Constraints point vector => NonEmpty point -> Number -> Number -> Int -> point
blossom (point :| []) _ _ _ = point
blossom (first :| NonEmpty rest) t1 t2 n2 = do
  let t = if n2 > 0 then t2 else t1
  blossom (deCasteljau first rest t) t1 t2 (n2 - 1)

deCasteljau :: Constraints point vector => point -> NonEmpty point -> Number -> NonEmpty point
deCasteljau first rest t = case rest of
  second :| [] -> NonEmpty.one (lerp first second t)
  next :| NonEmpty remaining -> NonEmpty.push (lerp first next t) (deCasteljau next remaining t)

lerp :: Constraints point vector => point -> point -> Number -> point
lerp p1 p2 t = p1 + t * (p2 - p1)

syntheticStart ::
  Constraints point vector =>
  point ->
  vector ->
  point ->
  vector ->
  vector ->
  NonEmpty point
syntheticStart point0 firstDerivative0 pointT0 firstDerivativeT0 secondDerivativeT0 = do
  let t0 = Desingularization.t0
  let segmentDerivatives0 = [t0 * firstDerivative0]
  let segmentDerivativesT0 = [t0 * firstDerivativeT0, t0 * t0 * secondDerivativeT0]
  hermite point0 segmentDerivatives0 pointT0 segmentDerivativesT0 |> segment 0.0 (1.0 /. t0)

syntheticEnd ::
  Constraints point vector =>
  point ->
  vector ->
  vector ->
  point ->
  vector ->
  NonEmpty point
syntheticEnd pointT1 firstDerivativeT1 secondDerivativeT1 point1 firstDerivative1 = do
  let t0 = Desingularization.t0
  let t1 = Desingularization.t1
  let segmentDerivativesT1 = [t0 * firstDerivativeT1, t0 * t0 * secondDerivativeT1]
  let segmentDerivatives1 = [t0 * firstDerivative1]
  hermite pointT1 segmentDerivativesT1 point1 segmentDerivatives1 |> segment -(t1 / t0) 1.0
