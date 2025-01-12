module OpenSolid.Surface2d.Function
  ( Function (Parametric)
  , Interface (..)
  , new
  )
where

import OpenSolid.Bounds2d (Bounds2d)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Expression (Expression)
import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Surface.Function qualified as Surface.Function
import OpenSolid.SurfaceParameter (SurfaceParameter, UvBounds, UvCoordinates, UvPoint)
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import {-# SOURCE #-} OpenSolid.VectorSurface2d.Function qualified as VectorSurface2d.Function

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> UvPoint -> Point2d coordinateSystem
  evaluateBoundsImpl :: function -> UvBounds -> Bounds2d coordinateSystem
  derivativeImpl :: SurfaceParameter -> function -> VectorSurface2d.Function.Function coordinateSystem
  transformByImpl :: Transform2d tag coordinateSystem -> function -> Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  Parametric ::
    Expression UvPoint (Point2d (space @ units)) ->
    Function (space @ units)
  XY ::
    Surface.Function.Function units ->
    Surface.Function.Function units ->
    Function (space @ units)
  Addition ::
    Function (space @ units) ->
    VectorSurface2d.Function.Function (space @ units) ->
    Function (space @ units)
  Subtraction ::
    Function (space @ units) ->
    VectorSurface2d.Function.Function (space @ units) ->
    Function (space @ units)
  Transformed ::
    Transform2d.Affine (space @ units) ->
    Function (space @ units) ->
    Function (space @ units)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function (space1 @ units1))
    (VectorSurface2d.Function.Function (space2 @ units2))
    (Function (space1 @ units1))

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (VectorSurface2d.Function.Function (space1 @ units1))

instance
  Composition
    (Curve2d UvCoordinates)
    (Function (space @ units))
    (Curve2d (space @ units))

new :: Interface function (space @ units) => function -> Function (space @ units)
