module VectorCurve2d
  ( IsVectorCurve2d (..)
  , VectorCurve2d
  , constant
  , wrap
  , evaluateAt
  , segmentBounds
  , derivative
  )
where

import {-# SOURCE #-} Curve1d (Curve1d)
import Domain (Domain)
import OpenSolid
import Units qualified
import Vector2d (Vector2d)
import VectorBox2d (VectorBox2d)

class
  (Show curve) =>
  IsVectorCurve2d curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  evaluateAtImpl :: Float -> curve -> Vector2d coordinateSystem
  segmentBoundsImpl :: Domain -> curve -> VectorBox2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem

type role VectorCurve2d nominal

data VectorCurve2d (coordinateSystem :: CoordinateSystem)

instance Show (VectorCurve2d (space @ units))

instance Negation (VectorCurve2d (space @ units))

instance
  Multiplication
    Sign
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space @ units))

instance
  Multiplication
    (VectorCurve2d (space @ units))
    Sign
    (VectorCurve2d (space @ units))

instance
  (units1 ~ units1', units2 ~ units2', space ~ space') =>
  Units.Coercion
    units1
    units2
    (VectorCurve2d (space @ units1'))
    (VectorCurve2d (space' @ units2'))

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (Curve1d units1)
    (VectorCurve2d (space @ units2))
    (VectorCurve2d (space @ units3))

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (VectorCurve2d (space @ units1))
    (Curve1d units2)
    (VectorCurve2d (space @ units3))

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (VectorCurve2d (space @ units1))
    (Curve1d units2)
    (VectorCurve2d (space @ units3))

instance
  ( Units.Product units1 units2 units3
  , space ~ space'
  ) =>
  DotProduct
    (VectorCurve2d (space @ units1))
    (VectorCurve2d (space' @ units2))
    (Curve1d units3)

constant :: Vector2d (space @ units) -> VectorCurve2d (space @ units)
wrap :: (IsVectorCurve2d curve (space @ units)) => curve -> VectorCurve2d (space @ units)
evaluateAt :: Float -> VectorCurve2d (space @ units) -> Vector2d (space @ units)
segmentBounds :: Domain -> VectorCurve2d (space @ units) -> VectorBox2d (space @ units)
derivative :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
