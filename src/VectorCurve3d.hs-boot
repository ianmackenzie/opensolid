module VectorCurve3d
  ( VectorCurve3d
  , constant
  )
where

import {-# SOURCE #-} Curve1d (Curve1d)
import OpenSolid
import Vector3d (Vector3d)

type role VectorCurve3d nominal nominal

type VectorCurve3d :: Type -> Type -> Type
data VectorCurve3d coordinates units

constant :: Vector3d coordinates units -> VectorCurve3d coordinates units

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (VectorCurve3d coordinates units1) (Curve1d units2) (VectorCurve3d coordinates units3)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (VectorCurve3d coordinates units2) (VectorCurve3d coordinates units3)
