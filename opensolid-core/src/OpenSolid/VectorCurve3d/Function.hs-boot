module OpenSolid.VectorCurve3d.Function
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
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorBounds3d (VectorBounds3d)

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Float -> Vector3d coordinateSystem
  evaluateBoundsImpl :: function -> Range Unitless -> VectorBounds3d coordinateSystem
  derivativeImpl :: function -> Function coordinateSystem
  transformByImpl ::
    Transform3d tag (Space coordinateSystem @ translationUnits) ->
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

constant :: Vector3d (space @ units) -> Function (space @ units)
new :: Interface function (space @ units) => function -> Function (space @ units)
evaluate :: Function (space @ units) -> Float -> Vector3d (space @ units)
evaluateBounds :: Function (space @ units) -> Range Unitless -> VectorBounds3d (space @ units)
derivative :: Function (space @ units) -> Function (space @ units)
unsafeMagnitude :: Function (space @ units) -> Curve1d.Function.Function units
transformBy ::
  Transform3d tag (space @ translationUnits) ->
  Function (space @ units) ->
  Function (space @ units)
