-- Allow typeclass instances to be declared here
-- even though the type is actually defined in the Functions module
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.SurfaceFunction2d
  ( SurfaceFunction2d (compiled)
  , Compiled
  , new
  , recursive
  , constant
  , uv
  , xy
  , evaluate
  , evaluateBounds
  , derivative
  , distanceAlong
  , xCoordinate
  , yCoordinate
  , coordinates
  , transformBy
  )
where

import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Surface2d qualified as Expression.Surface2d
import OpenSolid.Functions (SurfaceFunction2d (..), SurfaceFunction2dCompiled)
import OpenSolid.Functions qualified as Functions
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d

type Compiled coordinateSystem = SurfaceFunction2dCompiled coordinateSystem

instance HasUnits (SurfaceFunction2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (SurfaceFunction2d (space1 @ unitsA)) (SurfaceFunction2d (space2 @ unitsB))
  where
  coerce (SurfaceFunction2d c du dv) =
    SurfaceFunction2d (Units.coerce c) (Units.coerce du) (Units.coerce dv)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (SurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction2d (space1 @ units1))
  where
  lhs + rhs =
    new
      @ lhs.compiled + rhs.compiled
      @ \p -> derivative p lhs + VectorSurfaceFunction2d.derivative p rhs

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (SurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (SurfaceFunction2d (space1 @ units1))
  where
  f + v = f + VectorSurfaceFunction2d.constant v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (SurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction2d (space1 @ units1))
  where
  lhs - rhs =
    new
      @ lhs.compiled - rhs.compiled
      @ \p -> derivative p lhs - VectorSurfaceFunction2d.derivative p rhs

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (SurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (SurfaceFunction2d (space1 @ units1))
  where
  f - v = f - VectorSurfaceFunction2d.constant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction2d (space1 @ units1))
    (SurfaceFunction2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  lhs - rhs =
    VectorSurfaceFunction2d.new
      @ lhs.compiled - rhs.compiled
      @ \p -> derivative p lhs - derivative p rhs

new ::
  Compiled (space @ units) ->
  (SurfaceParameter -> VectorSurfaceFunction2d (space @ units)) ->
  SurfaceFunction2d (space @ units)
new = Functions.surfaceFunction2dNew

recursive ::
  Compiled (space @ units) ->
  ( SurfaceFunction2d (space @ units) ->
    SurfaceParameter ->
    VectorSurfaceFunction2d (space @ units)
  ) ->
  SurfaceFunction2d (space @ units)
recursive givenCompiled derivativeFunction =
  let self = new givenCompiled (derivativeFunction self) in self

constant :: Point2d (space @ units) -> SurfaceFunction2d (space @ units)
constant value = new (CompiledFunction.constant value) (always VectorSurfaceFunction2d.zero)

uv :: SurfaceFunction2d UvCoordinates
uv = xy SurfaceFunction.u SurfaceFunction.v

xy ::
  SurfaceFunction units ->
  SurfaceFunction units ->
  SurfaceFunction2d (space @ units)
xy x y =
  new
    @ CompiledFunction.map2
      Expression.xy
      Point2d
      Bounds2d
      x.compiled
      y.compiled
    @ \p ->
      VectorSurfaceFunction2d.xy
        @ SurfaceFunction.derivative p x
        @ SurfaceFunction.derivative p y

{-# INLINE evaluate #-}
evaluate :: SurfaceFunction2d (space @ units) -> UvPoint -> Point2d (space @ units)
evaluate = Functions.surfaceFunction2dEvaluate

{-# INLINE evaluateBounds #-}
evaluateBounds :: SurfaceFunction2d (space @ units) -> UvBounds -> Bounds2d (space @ units)
evaluateBounds = Functions.surfaceFunction2dEvaluateBounds

derivative ::
  SurfaceParameter ->
  SurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units)
derivative = Functions.surfaceFunction2dDerivative

transformBy ::
  Transform2d tag (space @ units) ->
  SurfaceFunction2d (space @ units) ->
  SurfaceFunction2d (space @ units)
transformBy transform function =
  new
    @ CompiledFunction.map
      (Expression.Surface2d.transformBy transform)
      (Point2d.transformBy transform)
      (Bounds2d.transformBy transform)
      function.compiled
    @ \p -> VectorSurfaceFunction2d.transformBy transform (derivative p function)

distanceAlong ::
  Axis2d (space @ units) ->
  SurfaceFunction2d (space @ units) ->
  SurfaceFunction units
distanceAlong axis function =
  (function - constant (Axis2d.originPoint axis)) `dot` Axis2d.direction axis

xCoordinate :: SurfaceFunction2d (space @ units) -> SurfaceFunction units
xCoordinate = Functions.surfaceFunction2dXCoordinate

yCoordinate :: SurfaceFunction2d (space @ units) -> SurfaceFunction units
yCoordinate = Functions.surfaceFunction2dYCoordinate

coordinates :: SurfaceFunction2d (space @ units) -> (SurfaceFunction units, SurfaceFunction units)
coordinates = Functions.surfaceFunction2dCoordinates
