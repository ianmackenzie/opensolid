module Curve2d (
    Curve2d (Curve2d),
    IsCurve2d (..),
    constant,
    parameterValue,
) where

import BoundingBox2d (BoundingBox2d (BoundingBox2d))
import BoundingBox2d qualified
import Control.Applicative ((<|>))
import OpenSolid
import Point2d (Point2d (Point2d))
import Point2d qualified
import Qty qualified
import Range (Range)
import Range qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

class IsCurve2d curve where
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

instance IsCurve2d Point2d where
    startPoint = identity
    endPoint = identity
    pointOn = always
    segmentBounds point _ = BoundingBox2d.constant point
    derivative _ = VectorCurve2d.zero
    reverse = identity
    bisect point = (point, point)
    boundingBox = BoundingBox2d.constant

constant :: Point2d coordinates -> Curve2d coordinates
constant = Curve2d

parameterValue :: IsCurve2d curve => Length -> Point2d coordinates -> curve coordinates -> Maybe Float
parameterValue tolerance point curve
    | Point2d.distanceFrom point (startPoint curve) <= tolerance = Just 0.0
    | Point2d.distanceFrom point (endPoint curve) <= tolerance = Just 1.0
    | otherwise = find tolerance point curve (derivative curve) (Range.from 0.0 1.0)

find :: IsCurve2d curve => Length -> Point2d coordinates -> curve coordinates -> VectorCurve2d Meters coordinates -> Range Unitless -> Maybe Float
find tolerance point curve curveDerivative domain
    | outside bounds tolerance point = Nothing
    | not ((point - bounds) <> derivativeBounds |> Range.contains Qty.zero) = Nothing
    | Range.isAtomic domain = Just (Range.minValue domain)
    | otherwise =
        let (leftDomain, rightDomain) = Range.bisect domain
         in find tolerance point curve curveDerivative leftDomain <|> find tolerance point curve curveDerivative rightDomain
  where
    bounds = segmentBounds curve domain
    derivativeBounds = VectorCurve2d.segmentBounds curveDerivative domain

outside :: BoundingBox2d coordinates -> Length -> Point2d coordinates -> Bool
outside (BoundingBox2d bx by) tolerance (Point2d px py)
    | px > Range.maxValue bx + tolerance = True
    | px < Range.minValue bx - tolerance = True
    | py > Range.maxValue by + tolerance = True
    | py < Range.minValue by - tolerance = True
    | otherwise = False
