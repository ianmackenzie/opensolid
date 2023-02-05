module VectorCurve2d
  ( VectorCurve2d
  , constant
  )
where

import {-# SOURCE #-} Curve1d (Curve1d)
import OpenSolid
import Vector2d (Vector2d)

type role VectorCurve2d nominal nominal

type VectorCurve2d :: Type -> Type -> Type
data VectorCurve2d units coordinates

constant :: Vector2d units coordinates -> VectorCurve2d units coordinates

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (VectorCurve2d units2 coordinates) (VectorCurve2d units3 coordinates)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (VectorCurve2d units1 coordinates) (Curve1d units2) (VectorCurve2d units3 coordinates)

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (VectorCurve2d units1 coordinates) (Curve1d units2) (VectorCurve2d units3 coordinates)
