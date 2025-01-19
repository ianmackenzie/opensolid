module OpenSolid.SurfaceFunction3d
  ( SurfaceFunction3d (Parametric)
  , Interface (..)
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Expression (Expression)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceParameter (SurfaceParameter, UvBounds, UvPoint)
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> UvPoint -> Point3d coordinateSystem
  evaluateBoundsImpl :: function -> UvBounds -> Bounds3d coordinateSystem
  derivativeImpl :: SurfaceParameter -> function -> VectorSurfaceFunction3d coordinateSystem

type role SurfaceFunction3d nominal

data SurfaceFunction3d (coordinateSystem :: CoordinateSystem) where
  SurfaceFunction3d ::
    Interface function (space @ units) =>
    function ->
    SurfaceFunction3d (space @ units)
  Coerce ::
    SurfaceFunction3d (space @ units1) ->
    SurfaceFunction3d (space @ units2)
  Parametric ::
    Expression UvPoint (Point3d (space @ units)) ->
    SurfaceFunction3d (space @ units)
  XYZ ::
    SurfaceFunction units ->
    SurfaceFunction units ->
    SurfaceFunction units ->
    SurfaceFunction3d (space @ units)
  Sum ::
    SurfaceFunction3d (space @ units) ->
    VectorSurfaceFunction3d (space @ units) ->
    SurfaceFunction3d (space @ units)
  Difference ::
    SurfaceFunction3d (space @ units) ->
    VectorSurfaceFunction3d (space @ units) ->
    SurfaceFunction3d (space @ units)

instance Show (SurfaceFunction3d (space @ units))

instance HasUnits (SurfaceFunction3d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (SurfaceFunction3d (space1 @ unitsA)) (SurfaceFunction3d (space2 @ unitsB))

evaluate :: SurfaceFunction3d (space @ units) -> UvPoint -> Point3d (space @ units)
evaluateBounds :: SurfaceFunction3d (space @ units) -> UvBounds -> Bounds3d (space @ units)
derivative ::
  SurfaceParameter ->
  SurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units)
