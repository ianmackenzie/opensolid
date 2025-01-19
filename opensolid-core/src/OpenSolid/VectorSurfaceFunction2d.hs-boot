module OpenSolid.VectorSurfaceFunction2d
  ( Interface (..)
  , VectorSurfaceFunction2d (Parametric)
  , new
  , constant
  )
where

import OpenSolid.CoordinateSystem (Space)
import OpenSolid.Expression (Expression)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
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
  derivativeImpl :: SurfaceParameter -> function -> VectorSurfaceFunction2d coordinateSystem
  transformByImpl ::
    Transform2d tag (Space coordinateSystem @ translationUnits) ->
    function ->
    VectorSurfaceFunction2d coordinateSystem

type role VectorSurfaceFunction2d nominal

data VectorSurfaceFunction2d (coordinateSystem :: CoordinateSystem) where
  VectorSurfaceFunction2d ::
    Interface function (space @ units) =>
    function ->
    VectorSurfaceFunction2d (space @ units)
  Coerce ::
    VectorSurfaceFunction2d (space @ units1) ->
    VectorSurfaceFunction2d (space @ units2)
  Parametric ::
    Expression UvPoint (Vector2d (space @ units)) ->
    VectorSurfaceFunction2d (space @ units)
  XY ::
    SurfaceFunction units ->
    SurfaceFunction units ->
    VectorSurfaceFunction2d (space @ units)
  Negated ::
    VectorSurfaceFunction2d (space @ units) ->
    VectorSurfaceFunction2d (space @ units)
  Sum ::
    VectorSurfaceFunction2d (space @ units) ->
    VectorSurfaceFunction2d (space @ units) ->
    VectorSurfaceFunction2d (space @ units)
  Difference ::
    VectorSurfaceFunction2d (space @ units) ->
    VectorSurfaceFunction2d (space @ units) ->
    VectorSurfaceFunction2d (space @ units)
  Product1d2d' ::
    SurfaceFunction units1 ->
    VectorSurfaceFunction2d (space @ units2) ->
    VectorSurfaceFunction2d (space @ (units1 :*: units2))
  Product2d1d' ::
    VectorSurfaceFunction2d (space @ units1) ->
    SurfaceFunction units2 ->
    VectorSurfaceFunction2d (space @ (units1 :*: units2))
  Quotient' ::
    VectorSurfaceFunction2d (space @ units1) ->
    SurfaceFunction units2 ->
    VectorSurfaceFunction2d (space @ (units1 :/: units2))
  Transformed ::
    Transform2d.Affine (space @ Unitless) ->
    VectorSurfaceFunction2d (space @ units) ->
    VectorSurfaceFunction2d (space @ units)

instance
  HasUnits
    (VectorSurfaceFunction2d (space @ units))
    units
    (VectorSurfaceFunction2d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (VectorSurfaceFunction2d (space1 @ units1)) (VectorSurfaceFunction2d (space2 @ units2))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d (space @ units2))
    (VectorSurfaceFunction2d (space @ units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction2d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (space @ units3))

instance
  Multiplication'
    (SurfaceFunction units1)
    (VectorSurfaceFunction2d (space @ units2))
    (VectorSurfaceFunction2d (space @ (units1 :*: units2)))

instance
  Multiplication'
    (VectorSurfaceFunction2d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (space @ (units1 :*: units2)))

new :: Interface function (space @ units) => function -> VectorSurfaceFunction2d (space @ units)
constant :: Vector2d (space @ units) -> VectorSurfaceFunction2d (space @ units)
