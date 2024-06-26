module Surface3d.Function
  ( Function
  , Interface (..)
  , wrap
  )
where

import Bounds3d (Bounds3d)
import OpenSolid
import Point3d (Point3d)
import Uv (Parameter)
import Uv qualified
import VectorSurface3d qualified

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

wrap :: Interface function (space @ units) => function -> Function (space @ units)
wrap = Function
