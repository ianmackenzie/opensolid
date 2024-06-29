module Surface3d.Function
  ( Function
  , Interface (..)
  , wrap
  , constant
  , xyz
  , evaluate
  , bounds
  , derivative
  )
where

import Bounds3d (Bounds3d)
import Bounds3d qualified
import OpenSolid
import Point3d (Point3d)
import Point3d qualified
import Surface1d qualified
import Surface1d.Function qualified
import Units qualified
import Uv (Parameter)
import Uv qualified
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

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type Units (Function (space @ units)) = units
  type Erase (Function (space @ units)) = Function (space @ Unitless)

instance
  space ~ space_ =>
  Units.Coercion (Function (space @ units1)) (Function (space_ @ units2))
  where
  coerce function = case function of
    Constant v -> Constant (Units.coerce v)
    Coerce f -> Coerce f
    _ -> Coerce function

wrap :: Interface function (space @ units) => function -> Function (space @ units)
wrap = Function

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
