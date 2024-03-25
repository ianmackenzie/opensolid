module VectorCurve2d
  ( Interface (..)
  , VectorCurve2d
  , constant
  , wrap
  , evaluateAt
  , segmentBounds
  , derivative
  )
where

import {-# SOURCE #-} Curve1d (Curve1d)
import OpenSolid
import Range (Range)
import Units qualified
import Vector2d (Vector2d)
import VectorBounds2d (VectorBounds2d)

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  evaluateAtImpl :: Float -> curve -> Vector2d coordinateSystem
  segmentBoundsImpl :: Range Unitless -> curve -> VectorBounds2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem

type role VectorCurve2d nominal

data VectorCurve2d (coordinateSystem :: CoordinateSystem)

instance Show (VectorCurve2d (space @ units))

instance Negation (VectorCurve2d (space @ units))

instance Multiplication Sign (VectorCurve2d (space @ units))

instance Product Sign (VectorCurve2d (space @ units)) (VectorCurve2d (space @ units))

instance Multiplication (VectorCurve2d (space @ units)) Sign

instance Product (VectorCurve2d (space @ units)) Sign (VectorCurve2d (space @ units))

instance
  space ~ space' =>
  Units.Coercion (VectorCurve2d (space @ units1)) (VectorCurve2d (space' @ units2))

instance Multiplication (Curve1d units1) (VectorCurve2d (space @ units2))

instance
  Units.Product units1 units2 units3 =>
  Product (Curve1d units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3))

instance Multiplication (VectorCurve2d (space @ units1)) (Curve1d units2)

instance
  Units.Product units1 units2 units3 =>
  Product (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance Division (VectorCurve2d (space @ units1)) (Curve1d units2)

instance
  Units.Quotient units1 units2 units3 =>
  Quotient (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance
  space ~ space' =>
  DotMultiplication
    (VectorCurve2d (space @ units1))
    (VectorCurve2d (space' @ units2))

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct (VectorCurve2d (space @ units1)) (VectorCurve2d (space' @ units2)) (Curve1d units3)

constant :: Vector2d (space @ units) -> VectorCurve2d (space @ units)
wrap :: Interface curve (space @ units) => curve -> VectorCurve2d (space @ units)
evaluateAt :: Float -> VectorCurve2d (space @ units) -> Vector2d (space @ units)
segmentBounds :: Range Unitless -> VectorCurve2d (space @ units) -> VectorBounds2d (space @ units)
derivative :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
