module OpenSolid.Bezier (derivative, hermite) where

import OpenSolid.Float qualified as Float
import OpenSolid.HasZero (HasZero (zero))
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude

type Vector vector =
  ( HasZero vector
  , Addition vector vector vector
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
