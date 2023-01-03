module Curve2d (
    Curve2d (Curve2d),
    IsCurve2d (..),
    constant,
    parameterValues,
) where

import BoundingBox2d (BoundingBox2d)
import BoundingBox2d qualified
import Curve1d qualified
import Curve1d.Root qualified as Root
import List qualified
import OpenSolid
import Point2d (Point2d)
import Qty qualified
import Range (Range)
import Result qualified
import VectorCurve2d (IsVectorCurve2d, VectorCurve2d (VectorCurve2d))
import VectorCurve2d qualified

class
    ( Subtraction Point2d curve (VectorCurve2d Meters)
    , Subtraction curve Point2d (VectorCurve2d Meters)
    ) =>
    IsCurve2d curve
    where
    startPoint :: curve coordinates -> Point2d coordinates
    endPoint :: curve coordinates -> Point2d coordinates
    pointOn :: curve coordinates -> Float -> Point2d coordinates
    segmentBounds :: curve coordinates -> Range Unitless -> BoundingBox2d coordinates
    derivative :: curve coordinates -> VectorCurve2d Meters coordinates
    reverse :: curve coordinates -> curve coordinates
    bisect :: curve coordinates -> (curve coordinates, curve coordinates)
    boundingBox :: curve coordinates -> BoundingBox2d coordinates

data Curve2d coordinates = forall curve. IsCurve2d curve => Curve2d (curve coordinates)

instance IsCurve2d Curve2d where
    startPoint (Curve2d curve) = startPoint curve
    endPoint (Curve2d curve) = endPoint curve
    pointOn (Curve2d curve) t = pointOn curve t
    segmentBounds (Curve2d curve) t = segmentBounds curve t
    derivative (Curve2d curve) = derivative curve
    reverse (Curve2d curve) = Curve2d (reverse curve)
    bisect (Curve2d curve) =
        let (curve1, curve2) = bisect curve
         in (Curve2d curve1, Curve2d curve2)
    boundingBox (Curve2d curve) = boundingBox curve

newtype Constant coordinates = Constant (Point2d coordinates)

instance IsCurve2d Constant where
    startPoint (Constant point) = point
    endPoint (Constant point) = point
    pointOn (Constant point) _ = point
    segmentBounds (Constant point) _ = BoundingBox2d.constant point
    derivative _ = VectorCurve2d.zero
    reverse = identity
    bisect point = (point, point)
    boundingBox (Constant point) = BoundingBox2d.constant point

instance Subtraction Constant Point2d (VectorCurve2d Meters) where
    Constant p1 - p2 = VectorCurve2d.constant (p1 - p2)

instance Subtraction Point2d Constant (VectorCurve2d Meters) where
    p1 - Constant p2 = VectorCurve2d.constant (p1 - p2)

data PointCurveDifference coordinates = PointCurveDifference (Point2d coordinates) (Curve2d coordinates)

instance IsVectorCurve2d (PointCurveDifference coordinates) Meters coordinates where
    pointOn (PointCurveDifference point curve) t = point - pointOn curve t
    segmentBounds (PointCurveDifference point curve) t = point - segmentBounds curve t
    derivative (PointCurveDifference _ curve) = -(derivative curve)

instance Subtraction Point2d Curve2d (VectorCurve2d Meters) where
    point - curve = VectorCurve2d (PointCurveDifference point curve)

data CurvePointDifference coordinates = CurvePointDifference (Curve2d coordinates) (Point2d coordinates)

instance IsVectorCurve2d (CurvePointDifference coordinates) Meters coordinates where
    pointOn (CurvePointDifference curve point) t = pointOn curve t - point
    segmentBounds (CurvePointDifference curve point) t = segmentBounds curve t - point
    derivative (CurvePointDifference curve _) = derivative curve

instance Subtraction Curve2d Point2d (VectorCurve2d Meters) where
    curve - point = VectorCurve2d (CurvePointDifference curve point)

constant :: Point2d coordinates -> Curve2d coordinates
constant point = Curve2d (Constant point)

data IsCoincidentWithPoint = IsCoincidentWithPoint deriving (Eq, Show)

parameterValues :: IsCurve2d curve => Length -> Point2d coordinates -> curve coordinates -> Result IsCoincidentWithPoint (List Float)
parameterValues tolerance point curve =
    VectorCurve2d.squaredMagnitude (curve - point)
        |> Curve1d.roots (Qty.squared tolerance)
        |> Result.map (List.map Root.value)
        |> Result.orErr IsCoincidentWithPoint
