-- Allow typeclass instances to be declared here
-- even though the type is actually defined in the Functions module
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.VectorCurve2d
  ( VectorCurve2d
  , constant
  , new
  , evaluate
  , evaluateBounds
  , quotient
  , quotient'
  , unsafeMagnitude
  , transformBy
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import OpenSolid.Functions (VectorCurve2d (..))
import OpenSolid.Prelude
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.VectorBounds2d (VectorBounds2d)

instance HasField "compiled" (VectorCurve2d (space @ units)) (Compiled (space @ units))

instance HasField "derivative" (VectorCurve2d (space @ units)) (VectorCurve2d (space @ units))

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    Float
    (Vector2d coordinateSystem)
    (Bounds Unitless)
    (VectorBounds2d coordinateSystem)

instance HasUnits (VectorCurve2d (space @ units)) units

instance Negation (VectorCurve2d (space @ units))

instance Multiplication Sign (VectorCurve2d (space @ units)) (VectorCurve2d (space @ units))

instance Multiplication (VectorCurve2d (space @ units)) Sign (VectorCurve2d (space @ units))

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve2d (space1 @ unitsA)) (VectorCurve2d (space2 @ unitsB))

instance
  Multiplication'
    (Curve units1)
    (VectorCurve2d (space @ units2))
    (VectorCurve2d (space @ (units1 :*: units2)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3))

instance
  Multiplication'
    (VectorCurve2d (space @ units1))
    (Curve units2)
    (VectorCurve2d (space @ (units1 :*: units2)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2d (space @ units1)) (Curve units2) (VectorCurve2d (space @ units3))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve (units1 :*: units2))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve units3)

constant :: Vector2d (space @ units) -> VectorCurve2d (space @ units)
new :: Compiled (space @ units) -> VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
evaluate :: VectorCurve2d (space @ units) -> Float -> Vector2d (space @ units)
evaluateBounds :: VectorCurve2d (space @ units) -> Bounds Unitless -> VectorBounds2d (space @ units)
quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve2d (space @ units1) ->
  Curve units2 ->
  VectorCurve2d (space @ units3)
quotient' ::
  Tolerance units2 =>
  VectorCurve2d (space @ units1) ->
  Curve units2 ->
  VectorCurve2d (space @ (units1 :/: units2))
unsafeMagnitude :: VectorCurve2d (space @ units) -> Curve units
transformBy ::
  Transform2d tag (space @ translationUnits) ->
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
