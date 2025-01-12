module OpenSolid.VectorSurface3d.Function
  ( Interface (..)
  , Function
  , new
  , constant
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Surface.Function qualified as Surface.Function
import OpenSolid.SurfaceParameter (SurfaceParameter, UvBounds, UvPoint)
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
  derivativeImpl :: SurfaceParameter -> function -> Function coordinateSystem

type role Function nominal

data Function (coordinateSystem :: CoordinateSystem)

instance HasUnits (Function (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ units1)) (Function (space2 @ units2))

new :: Interface function (space @ units) => function -> Function (space @ units)
constant :: Vector3d (space @ units) -> Function (space @ units)

instance
  Multiplication'
    (Surface.Function.Function units1)
    (Function (space @ units2))
    (Function (space @ (units1 :*: units2)))

instance
  Multiplication'
    (Function (space @ units1))
    (Surface.Function.Function units2)
    (Function (space @ (units1 :*: units2)))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Surface.Function.Function units1)
    (Function (space @ units2))
    (Function (space @ units3))

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function (space @ units1))
    (Surface.Function.Function units2)
    (Function (space @ units3))
