module OpenSolid.VectorSurface2d.Function
  ( Interface (..)
  , Function (Parametric)
  , new
  , constant
  )
where

import OpenSolid.CoordinateSystem (Space)
import OpenSolid.Expression (Expression)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Surface1d.Function qualified as Surface1d.Function
import OpenSolid.SurfaceParameter (SurfaceParameter, UvBounds, UvPoint)
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.VectorBounds2d (VectorBounds2d)

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> UvPoint -> Vector2d coordinateSystem
  evaluateBoundsImpl :: function -> UvBounds -> VectorBounds2d coordinateSystem
  derivativeImpl :: SurfaceParameter -> function -> Function coordinateSystem
  transformByImpl ::
    Transform2d tag (Space coordinateSystem @ translationUnits) ->
    function ->
    Function coordinateSystem

type role Function nominal

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  Parametric ::
    Expression UvPoint (Vector2d (space @ units)) ->
    Function (space @ units)
  XY ::
    Surface1d.Function.Function units ->
    Surface1d.Function.Function units ->
    Function (space @ units)
  Negated ::
    Function (space @ units) ->
    Function (space @ units)
  Sum ::
    Function (space @ units) ->
    Function (space @ units) ->
    Function (space @ units)
  Difference ::
    Function (space @ units) ->
    Function (space @ units) ->
    Function (space @ units)
  Product1d2d' ::
    Surface1d.Function.Function units1 ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))
  Product2d1d' ::
    Function (space @ units1) ->
    Surface1d.Function.Function units2 ->
    Function (space @ (units1 :*: units2))
  Quotient' ::
    Function (space @ units1) ->
    Surface1d.Function.Function units2 ->
    Function (space @ (units1 :/: units2))
  Transformed ::
    Transform2d.Affine (space @ Unitless) ->
    Function (space @ units) ->
    Function (space @ units)

instance HasUnits (Function (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ units1)) (Function (space2 @ units2))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Surface1d.Function.Function units1)
    (Function (space @ units2))
    (Function (space @ units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function (space @ units1))
    (Surface1d.Function.Function units2)
    (Function (space @ units3))

instance
  Multiplication'
    (Surface1d.Function.Function units1)
    (Function (space @ units2))
    (Function (space @ (units1 :*: units2)))

instance
  Multiplication'
    (Function (space @ units1))
    (Surface1d.Function.Function units2)
    (Function (space @ (units1 :*: units2)))

new :: Interface function (space @ units) => function -> Function (space @ units)
constant :: Vector2d (space @ units) -> Function (space @ units)
