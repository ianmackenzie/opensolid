module OpenSolid.VectorCurve2d.Function
  ( Interface (..)
  , Function
  , constant
  , new
  , evaluate
  , evaluateBounds
  , derivative
  , unsafeMagnitude
  , transformBy
  )
where

import OpenSolid.CoordinateSystem (Space)
import {-# SOURCE #-} OpenSolid.Curve1d.Function qualified as Curve1d.Function
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.VectorBounds2d (VectorBounds2d)

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Float -> Vector2d coordinateSystem
  evaluateBoundsImpl :: function -> Range Unitless -> VectorBounds2d coordinateSystem
  derivativeImpl :: function -> Function coordinateSystem
  transformByImpl ::
    Transform2d tag (Space coordinateSystem @ translationUnits) ->
    function ->
    Function coordinateSystem

type role Function nominal

data Function (coordinateSystem :: CoordinateSystem)

instance Show (Function (space @ units))

instance Negation (Function (space @ units))

instance Multiplication' Sign (Function (space @ units))

instance Multiplication Sign (Function (space @ units)) (Function (space @ units))

instance Multiplication' (Function (space @ units)) Sign

instance Multiplication (Function (space @ units)) Sign (Function (space @ units))

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ unitsA)) (Function (space2 @ unitsB))

instance Multiplication' (Curve1d.Function.Function units1) (Function (space @ units2))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Curve1d.Function.Function units1)
    (Function (space @ units2))
    (Function (space @ units3))

instance Multiplication' (Function (space @ units1)) (Curve1d.Function.Function units2)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function (space @ units1))
    (Curve1d.Function.Function units2)
    (Function (space @ units3))

instance Division' (Function (space @ units1)) (Curve1d.Function.Function units2)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function (space @ units1))
    (Curve1d.Function.Function units2)
    (Function (space @ units3))

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Function (space1 @ units1))
    (Function (space2 @ units2))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Curve1d.Function.Function units3)

constant :: Vector2d (space @ units) -> Function (space @ units)
new :: Interface curve (space @ units) => curve -> Function (space @ units)
evaluate :: Function (space @ units) -> Float -> Vector2d (space @ units)
evaluateBounds :: Function (space @ units) -> Range Unitless -> VectorBounds2d (space @ units)
derivative :: Function (space @ units) -> Function (space @ units)
unsafeMagnitude :: Function (space @ units) -> Curve1d.Function.Function units
transformBy ::
  Transform2d tag (space @ translationUnits) ->
  Function (space @ units) ->
  Function (space @ units)
