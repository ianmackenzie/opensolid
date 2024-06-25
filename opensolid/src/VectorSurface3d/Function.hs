module VectorSurface3d.Function
  ( Function
  , Interface (..)
  , wrap
  )
where

import OpenSolid
import Uv (Parameter)
import Uv qualified
import Vector3d (Vector3d)
import VectorBounds3d (VectorBounds3d)

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateAtImpl :: Uv.Point -> function -> Vector3d coordinateSystem
  segmentBoundsImpl :: Uv.Bounds -> function -> VectorBounds3d coordinateSystem
  derivativeImpl :: Parameter -> function -> Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)

wrap :: Interface function (space @ units) => function -> Function (space @ units)
wrap = Function
