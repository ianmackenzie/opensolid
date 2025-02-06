module OpenSolid.VectorSurfaceFunction3d
  ( Interface (..)
  , VectorSurfaceFunction3d
  , new
  , constant
  )
where

import OpenSolid.CoordinateSystem (Space)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceParameter (SurfaceParameter, UvBounds, UvPoint)
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorBounds3d (VectorBounds3d)

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> UvPoint -> Vector3d coordinateSystem
  evaluateBoundsImpl :: function -> UvBounds -> VectorBounds3d coordinateSystem
  derivativeImpl :: SurfaceParameter -> function -> VectorSurfaceFunction3d coordinateSystem
  transformByImpl ::
    Transform3d tag (Space coordinateSystem @ translationUnits) ->
    function ->
    VectorSurfaceFunction3d coordinateSystem

type role VectorSurfaceFunction3d nominal

data VectorSurfaceFunction3d (coordinateSystem :: CoordinateSystem)

instance
  HasUnits
    (VectorSurfaceFunction3d (space @ units))
    units
    (VectorSurfaceFunction3d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion
    (VectorSurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))

new :: Interface function (space @ units) => function -> VectorSurfaceFunction3d (space @ units)
constant :: Vector3d (space @ units) -> VectorSurfaceFunction3d (space @ units)

instance
  Multiplication'
    (SurfaceFunction units1)
    (VectorSurfaceFunction3d (space @ units2))
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))

instance
  Multiplication'
    (VectorSurfaceFunction3d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (VectorSurfaceFunction3d (space @ units2))
    (VectorSurfaceFunction3d (space @ units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorSurfaceFunction3d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d (space @ units3))
