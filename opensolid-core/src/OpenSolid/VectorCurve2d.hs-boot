-- Allow typeclass instances to be declared here
-- even though the type is actually defined in the Functions module
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.VectorCurve2d
  ( VectorCurve2d (compiled, derivative)
  , Compiled
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
import {-# SOURCE #-} OpenSolid.Curve (Curve)
import OpenSolid.Functions (VectorCurve2d (..), VectorCurve2dCompiled)
import OpenSolid.Prelude
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.VectorBounds2d (VectorBounds2d)

type Compiled coordinateSystem = VectorCurve2dCompiled coordinateSystem

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
