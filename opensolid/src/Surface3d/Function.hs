-- Needed for CurveSurfaceComposition
{-# OPTIONS_GHC -Wno-orphans #-}

module Surface3d.Function
  ( Function (Constant)
  , Interface (..)
  , new
  , constant
  , xyz
  , evaluate
  , bounds
  , derivative
  )
where

import Bounds3d (Bounds3d)
import Bounds3d qualified
import Curve3d (Curve3d)
import Curve3d qualified
import OpenSolid
import Point3d (Point3d)
import Point3d qualified
import Surface1d qualified
import Surface1d.Function qualified
import Units qualified
import Uv (Parameter)
import Uv qualified
import Vector3d (Vector3d)
import Vector3d qualified
import VectorSurface3d qualified
import VectorSurface3d.Function qualified

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Uv.Point -> Point3d coordinateSystem
  boundsImpl :: function -> Uv.Bounds -> Bounds3d coordinateSystem
  derivativeImpl :: Parameter -> function -> VectorSurface3d.Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  Constant ::
    Point3d (space @ units) ->
    Function (space @ units)
  XYZ ::
    Surface1d.Function units ->
    Surface1d.Function units ->
    Surface1d.Function units ->
    Function (space @ units)
  Sum ::
    Function (space @ units) ->
    VectorSurface3d.Function (space @ units) ->
    Function (space @ units)
  Difference ::
    Function (space @ units) ->
    VectorSurface3d.Function (space @ units) ->
    Function (space @ units)

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type Units (Function (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ unitsA)) (Function (space2 @ unitsB))
  where
  coerce function = case function of
    Constant v -> Constant (Units.coerce v)
    Coerce f -> Coerce f
    _ -> Coerce function

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Function (space @ units))
    (VectorSurface3d.Function (space_ @ units_))
    (Function (space @ units))
  where
  f1 + VectorSurface3d.Function.Constant v | v == Vector3d.zero = f1
  f1 + f2 = Sum f1 f2

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Function (space @ units))
    (Vector3d (space_ @ units_))
    (Function (space @ units))
  where
  f + v = f + VectorSurface3d.Function.constant v

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Function (space @ units))
    (VectorSurface3d.Function (space_ @ units_))
    (Function (space @ units))
  where
  f1 - VectorSurface3d.Function.Constant v | v == Vector3d.zero = f1
  f1 - f2 = Difference f1 f2

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Function (space @ units))
    (Vector3d (space_ @ units_))
    (Function (space @ units))
  where
  f - v = f - VectorSurface3d.Function.constant v

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

constant :: Point3d (space @ units) -> Function (space @ units)
constant = Constant

xyz ::
  Surface1d.Function units ->
  Surface1d.Function units ->
  Surface1d.Function units ->
  Function (space @ units)
xyz = XYZ

evaluate :: Function (space @ units) -> Uv.Point -> Point3d (space @ units)
evaluate function uv = case function of
  Function f -> evaluateImpl f uv
  Coerce f -> Units.coerce (evaluate f uv)
  Constant v -> v
  XYZ x y z ->
    Point3d.xyz
      (Surface1d.Function.evaluate x uv)
      (Surface1d.Function.evaluate y uv)
      (Surface1d.Function.evaluate z uv)
  Sum f1 f2 -> evaluate f1 uv + VectorSurface3d.Function.evaluate f2 uv
  Difference f1 f2 -> evaluate f1 uv - VectorSurface3d.Function.evaluate f2 uv

bounds :: Function (space @ units) -> Uv.Bounds -> Bounds3d (space @ units)
bounds function uv = case function of
  Function f -> boundsImpl f uv
  Coerce f -> Units.coerce (bounds f uv)
  Constant v -> Bounds3d.constant v
  XYZ x y z ->
    Bounds3d.xyz
      (Surface1d.Function.bounds x uv)
      (Surface1d.Function.bounds y uv)
      (Surface1d.Function.bounds z uv)
  Sum f1 f2 -> bounds f1 uv + VectorSurface3d.Function.bounds f2 uv
  Difference f1 f2 -> bounds f1 uv - VectorSurface3d.Function.bounds f2 uv

derivative :: Uv.Parameter -> Function (space @ units) -> VectorSurface3d.Function (space @ units)
derivative parameter function = case function of
  Function f -> derivativeImpl parameter f
  Coerce f -> Units.coerce (derivative parameter f)
  Constant _ -> VectorSurface3d.Function.zero
  XYZ x y z ->
    VectorSurface3d.Function.xyz
      (Surface1d.Function.derivative parameter x)
      (Surface1d.Function.derivative parameter y)
      (Surface1d.Function.derivative parameter z)
  Sum f1 f2 -> derivative parameter f1 + VectorSurface3d.Function.derivative parameter f2
  Difference f1 f2 -> derivative parameter f1 - VectorSurface3d.Function.derivative parameter f2

data SurfaceCurveComposition (coordinateSystem :: CoordinateSystem) where
  SurfaceCurveComposition ::
    Surface1d.Function Unitless ->
    Curve3d (space @ units) ->
    SurfaceCurveComposition (space @ units)

deriving instance Show (SurfaceCurveComposition coordinateSystem)

instance
  Composition
    (Surface1d.Function Unitless)
    (Curve3d (space @ units))
    (Function (space @ units))
  where
  curve . function = new (SurfaceCurveComposition function curve)

instance Interface (SurfaceCurveComposition (space @ units)) (space @ units) where
  evaluateImpl (SurfaceCurveComposition function curve) uv =
    Curve3d.pointOn curve (Surface1d.Function.evaluate function uv)

  boundsImpl (SurfaceCurveComposition function curve) uv =
    Curve3d.segmentBounds curve (Surface1d.Function.bounds function uv)

  derivativeImpl parameter (SurfaceCurveComposition function curve) =
    (Curve3d.derivative curve . function) * Surface1d.Function.derivative parameter function
