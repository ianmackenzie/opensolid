module OpenSolid.Bezier (derivative, hermite, syntheticStart, syntheticEnd) where

import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Float qualified as Float
import OpenSolid.HasZero (HasZero (zero))
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Stream (Stream)
import OpenSolid.Stream qualified as Stream

type Vector vector =
  ( HasZero vector
  , Addition vector vector vector
  , Subtraction vector vector vector
  , Multiplication vector Float vector
  )

type Constraints point vector =
  ( Vector vector
  , Addition point vector point
  , Subtraction point point vector
  )

derivative :: Constraints point vector => NonEmpty point -> NonEmpty vector
derivative controlPoints = do
  let scale = Float.int (NonEmpty.length controlPoints - 1)
  let scaledDifference p1 p2 = (p2 - p1) * scale
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
  let curveDegree = Float.int (1 + numStartDerivatives + numEndDerivatives)
  let scaledStartDerivatives = scaleDerivatives Positive 1.0 curveDegree startDerivatives
  let scaledEndDerivatives = scaleDerivatives Negative 1.0 curveDegree endDerivatives
  let startControlPoints =
        derivedControlPoints startPoint 1 (numStartDerivatives + 1) scaledStartDerivatives
  let endControlPoints =
        derivedControlPoints endPoint 1 (numEndDerivatives + 1) scaledEndDerivatives
          |> List.reverse
  startPoint :| List.concat [startControlPoints, endControlPoints, [endPoint]]

scaleDerivatives :: Vector vector => Sign -> Float -> Float -> List vector -> List vector
scaleDerivatives _ _ _ [] = []
scaleDerivatives sign scale n (first : rest) = do
  let updatedScale = sign * scale / n
  first * updatedScale : scaleDerivatives sign updatedScale (n - 1.0) rest

derivedControlPoints :: Constraints point vector => point -> Int -> Int -> List vector -> List point
derivedControlPoints previousPoint i n qs
  | i < n = do
      let newPoint = previousPoint + offset i qs
      newPoint : derivedControlPoints newPoint (i + 1) n qs
  | otherwise = []

offset :: Vector vector => Int -> List vector -> vector
offset i scaledDerivatives =
  List.take i scaledDerivatives
    |> List.mapWithIndex (\j q -> q * Float.int (Int.choose (i - 1) j))
    |> List.foldl (+) zero

segment :: Constraints point vector => Float -> Float -> NonEmpty point -> NonEmpty point
segment t1 t2 controlPoints = do
  let newControlPoint index _ = blossom controlPoints t1 t2 index
  NonEmpty.mapWithIndex newControlPoint controlPoints

blossom :: Constraints point vector => NonEmpty point -> Float -> Float -> Int -> point
blossom (point :| []) _ _ _ = point
blossom (first :| NonEmpty rest) t1 t2 n2 = do
  let t = if n2 > 0 then t2 else t1
  blossom (deCasteljau first rest t) t1 t2 (n2 - 1)

deCasteljau :: Constraints point vector => point -> NonEmpty point -> Float -> NonEmpty point
deCasteljau first rest t = case rest of
  second :| [] -> NonEmpty.one (lerp first second t)
  next :| NonEmpty remaining -> NonEmpty.push (lerp first next t) (deCasteljau next remaining t)

lerp :: Constraints point vector => point -> point -> Float -> point
lerp p1 p2 t = p1 + t * (p2 - p1)

syntheticDerivativeScales :: Stream Float
syntheticDerivativeScales = Stream.iterate (* Desingularization.t0) Desingularization.t0

syntheticStart ::
  Constraints point vector =>
  point ->
  List vector ->
  point ->
  Stream vector ->
  (NonEmpty point, Stream (NonEmpty vector))
syntheticStart point0 derivatives0 pointT0 derivativesT0 = do
  let segmentDerivatives0 =
        List.map2 (*) derivatives0 (Stream.toList syntheticDerivativeScales)
  let segmentDerivatives1 = Stream.map2 (*) derivativesT0 syntheticDerivativeScales
  let baseCurve endDegree =
        hermite point0 segmentDerivatives0 pointT0 (Stream.take endDegree segmentDerivatives1)
          |> segment 0.0 (1.0 / Desingularization.t0)
  let derivativeCurve n = nthDerivative n (baseCurve (Desingularization.continuity + n))
  (baseCurve Desingularization.continuity, Stream.map derivativeCurve (Stream.from 1))

syntheticEnd ::
  Constraints point vector =>
  point ->
  Stream vector ->
  point ->
  List vector ->
  (NonEmpty point, Stream (NonEmpty vector))
syntheticEnd pointT1 derivativesT1 point1 derivatives1 = do
  let segmentDerivatives0 = Stream.map2 (*) derivativesT1 syntheticDerivativeScales
  let segmentDerivatives1 =
        List.map2 (*) derivatives1 (Stream.toList syntheticDerivativeScales)
  let baseCurve startDegree =
        hermite pointT1 (Stream.take startDegree segmentDerivatives0) point1 segmentDerivatives1
          |> segment -(Desingularization.t1 / Desingularization.t0) 1.0
  let derivativeCurve n = nthDerivative n (baseCurve (Desingularization.continuity + n))
  (baseCurve Desingularization.continuity, Stream.map derivativeCurve (Stream.from 1))

nthDerivative :: Constraints point vector => Int -> NonEmpty point -> NonEmpty vector
nthDerivative 0 _ = internalError "nthDerivative should always be called with n >= 1"
nthDerivative 1 controlPoints = derivative controlPoints
nthDerivative n controlPoints = derivative (nthDerivative (n - 1) controlPoints)
