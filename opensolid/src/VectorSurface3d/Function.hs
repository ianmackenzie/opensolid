module VectorSurface3d.Function
  ( Function
  , Interface (..)
  , wrap
  , evaluate
  , bounds
  )
where

import OpenSolid
import Surface1d qualified
import Surface1d.Function qualified
import Units (Erase)
import Units qualified
import Uv (Parameter)
import Uv qualified
import Vector3d (Vector3d)
import Vector3d qualified
import VectorBounds3d (VectorBounds3d)
import VectorBounds3d qualified

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Uv.Point -> Vector3d coordinateSystem
  boundsImpl :: function -> Uv.Bounds -> VectorBounds3d coordinateSystem
  derivativeImpl :: Parameter -> function -> Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  Constant ::
    Vector3d (space @ units) ->
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
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce curve) = Coerce curve
  coerce curve = Coerce curve

wrap :: Interface function (space @ units) => function -> Function (space @ units)
wrap = Function

evaluate :: Function (space @ units) -> Uv.Point -> Vector3d (space @ units)
evaluate function uv = case function of
  Function f -> evaluateImpl f uv
  Coerce f -> Units.coerce (evaluate f uv)
  Constant v -> v
  XYZ x y z ->
    Vector3d.xyz
      (Surface1d.Function.evaluateAt uv x)
      (Surface1d.Function.evaluateAt uv y)
      (Surface1d.Function.evaluateAt uv z)

bounds :: Function (space @ units) -> Uv.Bounds -> VectorBounds3d (space @ units)
bounds function uv = case function of
  Function f -> boundsImpl f uv
  Coerce f -> Units.coerce (bounds f uv)
  Constant v -> VectorBounds3d.constant v
  XYZ x y z ->
    VectorBounds3d.xyz
      (Surface1d.Function.segmentBounds uv x)
      (Surface1d.Function.segmentBounds uv y)
      (Surface1d.Function.segmentBounds uv z)
