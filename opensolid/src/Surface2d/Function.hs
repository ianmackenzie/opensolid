-- Needed for CurveSurfaceComposition
{-# OPTIONS_GHC -Wno-orphans #-}

module Surface2d.Function
  ( Function (Constant)
  , Interface (..)
  , new
  , constant
  , xy
  , evaluate
  , bounds
  , derivative
  )
where

import Bounds2d (Bounds2d)
import Bounds2d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Surface1d qualified
import Surface1d.Function qualified
import Units qualified
import Uv (Parameter)
import Uv qualified
import Vector2d (Vector2d)
import Vector2d qualified
import VectorSurface2d qualified
import VectorSurface2d.Function qualified

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Uv.Point -> Point2d coordinateSystem
  boundsImpl :: function -> Uv.Bounds -> Bounds2d coordinateSystem
  derivativeImpl :: Parameter -> function -> VectorSurface2d.Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  Constant ::
    Point2d (space @ units) ->
    Function (space @ units)
  XY ::
    Surface1d.Function units ->
    Surface1d.Function units ->
    Function (space @ units)
  Sum ::
    Function (space @ units) ->
    VectorSurface2d.Function (space @ units) ->
    Function (space @ units)
  Difference ::
    Function (space @ units) ->
    VectorSurface2d.Function (space @ units) ->
    Function (space @ units)

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type Units (Function (space @ units)) = units
  type Erase (Function (space @ units)) = Function (space @ Unitless)

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ unitsA)) (Function (space2 @ unitsB))
  where
  coerce function = case function of
    Constant v -> Constant (Units.coerce v)
    Coerce f -> Coerce f
    _ -> Coerce function

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Function (space1 @ units1))
    (VectorSurface2d.Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  f1 + VectorSurface2d.Function.Constant v | v == Vector2d.zero = f1
  f1 + f2 = Sum f1 f2

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Function (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Function (space1 @ units1))
  where
  f + v = f + VectorSurface2d.Function.constant v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Function (space1 @ units1))
    (VectorSurface2d.Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  f1 - VectorSurface2d.Function.Constant v | v == Vector2d.zero = f1
  f1 - f2 = Difference f1 f2

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Function (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Function (space1 @ units1))
  where
  f - v = f - VectorSurface2d.Function.constant v

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

constant :: Point2d (space @ units) -> Function (space @ units)
constant = Constant

xy ::
  Surface1d.Function units ->
  Surface1d.Function units ->
  Function (space @ units)
xy = XY

evaluate :: Function (space @ units) -> Uv.Point -> Point2d (space @ units)
evaluate function uv = case function of
  Function f -> evaluateImpl f uv
  Coerce f -> Units.coerce (evaluate f uv)
  Constant v -> v
  XY x y ->
    Point2d.xy
      (Surface1d.Function.evaluate x uv)
      (Surface1d.Function.evaluate y uv)
  Sum f1 f2 -> evaluate f1 uv + VectorSurface2d.Function.evaluate f2 uv
  Difference f1 f2 -> evaluate f1 uv - VectorSurface2d.Function.evaluate f2 uv

bounds :: Function (space @ units) -> Uv.Bounds -> Bounds2d (space @ units)
bounds function uv = case function of
  Function f -> boundsImpl f uv
  Coerce f -> Units.coerce (bounds f uv)
  Constant v -> Bounds2d.constant v
  XY x y ->
    Bounds2d.xy
      (Surface1d.Function.bounds x uv)
      (Surface1d.Function.bounds y uv)
  Sum f1 f2 -> bounds f1 uv + VectorSurface2d.Function.bounds f2 uv
  Difference f1 f2 -> bounds f1 uv - VectorSurface2d.Function.bounds f2 uv

derivative :: Uv.Parameter -> Function (space @ units) -> VectorSurface2d.Function (space @ units)
derivative parameter function = case function of
  Function f -> derivativeImpl parameter f
  Coerce f -> Units.coerce (derivative parameter f)
  Constant _ -> VectorSurface2d.Function.zero
  XY x y ->
    VectorSurface2d.Function.xy
      (Surface1d.Function.derivative parameter x)
      (Surface1d.Function.derivative parameter y)
  Sum f1 f2 -> derivative parameter f1 + VectorSurface2d.Function.derivative parameter f2
  Difference f1 f2 -> derivative parameter f1 - VectorSurface2d.Function.derivative parameter f2

data SurfaceCurveComposition (coordinateSystem :: CoordinateSystem) where
  SurfaceCurveComposition ::
    Surface1d.Function Unitless ->
    Curve2d (space @ units) ->
    SurfaceCurveComposition (space @ units)

deriving instance Show (SurfaceCurveComposition coordinateSystem)

instance
  Composition
    (Surface1d.Function Unitless)
    (Curve2d (space @ units))
    (Function (space @ units))
  where
  curve . function = new (SurfaceCurveComposition function curve)

instance Interface (SurfaceCurveComposition (space @ units)) (space @ units) where
  evaluateImpl (SurfaceCurveComposition function curve) uv =
    Curve2d.pointOn curve (Surface1d.Function.evaluate function uv)

  boundsImpl (SurfaceCurveComposition function curve) uv =
    Curve2d.segmentBounds curve (Surface1d.Function.bounds function uv)

  derivativeImpl parameter (SurfaceCurveComposition function curve) =
    (Curve2d.derivative curve . function) * Surface1d.Function.derivative parameter function
