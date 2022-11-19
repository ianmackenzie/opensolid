module VectorCurve3d (
    VectorCurve3d,
    constant,
) where

import {-# SOURCE #-} Curve1d (Curve1d)
import OpenSolid
import Range (Range)
import Vector3d (Vector3d)
import VectorBox3d (VectorBox3d)

class IsVectorCurve3d curve units coordinates | curve -> units, curve -> coordinates where
    pointOn :: curve -> Float -> Vector3d units coordinates
    segmentBounds :: curve -> Range Unitless -> VectorBox3d units coordinates
    derivative :: curve -> VectorCurve3d units coordinates

data VectorCurve3d units coordinates = forall curve. IsVectorCurve3d curve units coordinates => VectorCurve3d curve

constant :: Vector3d units coordinates -> VectorCurve3d units coordinates

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (VectorCurve3d units1 coordinates) (Curve1d units2) (VectorCurve3d units3 coordinates)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (VectorCurve3d units2 coordinates) (VectorCurve3d units3 coordinates)
