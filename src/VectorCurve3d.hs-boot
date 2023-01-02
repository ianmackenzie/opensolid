module VectorCurve3d (
    VectorCurve3d,
    constant,
) where

import {-# SOURCE #-} Curve1d (Curve1d)
import Data.Kind (Type)
import OpenSolid
import Vector3d (Vector3d)

type role VectorCurve3d nominal nominal

type VectorCurve3d :: Type -> Type -> Type
data VectorCurve3d units coordinates

constant :: Vector3d units coordinates -> VectorCurve3d units coordinates

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (VectorCurve3d units1 coordinates) (Curve1d units2) (VectorCurve3d units3 coordinates)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (VectorCurve3d units2 coordinates) (VectorCurve3d units3 coordinates)
