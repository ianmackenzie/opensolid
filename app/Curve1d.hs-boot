module Curve1d (Curve1d) where

import OpenSolid
import Range (Range)

class IsCurve1d curve units | curve -> units where
    pointOn :: curve -> Float -> Qty units
    segmentBounds :: curve -> Range Unitless -> Range units
    derivative :: curve -> Curve1d units

data Curve1d units = forall curve. IsCurve1d curve units => Curve1d curve
