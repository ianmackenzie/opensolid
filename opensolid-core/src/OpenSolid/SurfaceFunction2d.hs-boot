module OpenSolid.SurfaceFunction2d
  ( SurfaceFunction2d (Parametric)
  , Interface (..)
  , new
  )
where

import OpenSolid.Bounds2d (Bounds2d)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Expression (Expression)
import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceParameter (SurfaceParameter, UvBounds, UvCoordinates, UvPoint)
import OpenSolid.Transform2d qualified as Transform2d
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> UvPoint -> Point2d coordinateSystem
  evaluateBoundsImpl :: function -> UvBounds -> Bounds2d coordinateSystem
  derivativeImpl :: SurfaceParameter -> function -> VectorSurfaceFunction2d coordinateSystem

data SurfaceFunction2d (coordinateSystem :: CoordinateSystem) where
  SurfaceFunction2d ::
    Interface function (space @ units) =>
    function ->
    SurfaceFunction2d (space @ units)
  Coerce ::
    SurfaceFunction2d (space @ units1) ->
    SurfaceFunction2d (space @ units2)
  Parametric ::
    Expression UvPoint (Point2d (space @ units)) ->
    SurfaceFunction2d (space @ units)
  XY ::
    SurfaceFunction units ->
    SurfaceFunction units ->
    SurfaceFunction2d (space @ units)
  Addition ::
    SurfaceFunction2d (space @ units) ->
    VectorSurfaceFunction2d (space @ units) ->
    SurfaceFunction2d (space @ units)
  Subtraction ::
    SurfaceFunction2d (space @ units) ->
    VectorSurfaceFunction2d (space @ units) ->
    SurfaceFunction2d (space @ units)
  Transformed ::
    Transform2d.Affine (space @ units) ->
    SurfaceFunction2d (space @ units) ->
    SurfaceFunction2d (space @ units)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (SurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction2d (space1 @ units1))

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction2d (space1 @ units1))
    (SurfaceFunction2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (Curve2d uvCoordinates)
    (SurfaceFunction2d (space @ units))
    (Curve2d (space @ units))

new :: Interface function (space @ units) => function -> SurfaceFunction2d (space @ units)
