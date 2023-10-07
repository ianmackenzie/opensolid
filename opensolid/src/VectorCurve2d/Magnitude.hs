module VectorCurve2d.Magnitude (unsafe) where

import Curve1d (Curve1d (Curve1d), IsCurve1d (..))
import OpenSolid
import Units qualified
import Vector2d qualified
import VectorBox2d qualified
import {-# SOURCE #-} VectorCurve2d (VectorCurve2d)
import {-# SOURCE #-} VectorCurve2d qualified

newtype Magnitude (coordinateSystem :: CoordinateSystem)
  = Magnitude (VectorCurve2d coordinateSystem)

deriving instance Show (Magnitude (space @ units))

instance IsCurve1d (Magnitude (space @ units)) units where
  evaluateAtImpl t (Magnitude curve) =
    Vector2d.magnitude (VectorCurve2d.evaluateAt t curve)

  segmentBoundsImpl t (Magnitude curve) =
    VectorBox2d.magnitude (VectorCurve2d.segmentBounds t curve)

  derivativeImpl (Magnitude curve) =
    let curve' = Units.generalize curve
        derivative' = (VectorCurve2d.derivative curve' <> curve') / Curve1d (Magnitude curve')
     in Units.specialize derivative'

unsafe :: VectorCurve2d (space @ units) -> Curve1d units
unsafe curve = Curve1d (Magnitude curve)
