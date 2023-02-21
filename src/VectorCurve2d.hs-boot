module VectorCurve2d
  ( VectorCurve2d
  , constant
  )
where

import {-# SOURCE #-} Curve1d (Curve1d)
import OpenSolid
import Units qualified
import Vector2d (Vector2d)

type role VectorCurve2d nominal nominal

type VectorCurve2d :: Type -> Type -> Type
data VectorCurve2d coordinates units

constant :: Vector2d coordinates units -> VectorCurve2d coordinates units

instance Units.Product units1 units2 units3 => Multiplication (Curve1d units1) (VectorCurve2d coordinates units2) (VectorCurve2d coordinates units3)

instance Units.Product units1 units2 units3 => Multiplication (VectorCurve2d coordinates units1) (Curve1d units2) (VectorCurve2d coordinates units3)

instance Units.Quotient units1 units2 units3 => Division (VectorCurve2d coordinates units1) (Curve1d units2) (VectorCurve2d coordinates units3)
