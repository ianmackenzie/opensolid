module OpenSolid.SurfaceFunction3d
  ( SurfaceFunction3d
  , Compiled
  , compiled
  , new
  , constant
  , xyz
  , evaluate
  , evaluateBounds
  , derivative
  , placeIn
  , relativeTo
  , transformBy
  )
where

import OpenSolid.Bounds3d (Bounds3d (Bounds3d))
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Surface3d qualified as Expression.Surface3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Point3d (Point3d (Point3d))
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Region2d (Region2d)
import {-# SOURCE #-} OpenSolid.Surface3d (Surface3d)
import {-# SOURCE #-} OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction2d (SurfaceFunction2d)
import OpenSolid.SurfaceFunction2d qualified as SurfaceFunction2d
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvCoordinates, UvPoint)
import OpenSolid.Text qualified as Text
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d
import Prelude qualified

data SurfaceFunction3d (coordinateSystem :: CoordinateSystem) where
  SurfaceFunction3d ::
    Compiled (space @ units) ->
    ~(VectorSurfaceFunction3d (space @ units)) ->
    ~(VectorSurfaceFunction3d (space @ units)) ->
    SurfaceFunction3d (space @ units)

type Compiled coordinateSystem =
  CompiledFunction
    UvPoint
    (Point3d coordinateSystem)
    UvBounds
    (Bounds3d coordinateSystem)

instance Show (SurfaceFunction3d (space @ units)) where
  show _ = Text.unpack "SurfaceFunction3d"

instance HasUnits (SurfaceFunction3d (space @ units)) units (SurfaceFunction3d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (SurfaceFunction3d (space1 @ unitsA)) (SurfaceFunction3d (space2 @ unitsB))
  where
  coerce (SurfaceFunction3d c du dv) =
    SurfaceFunction3d (Units.coerce c) (Units.coerce du) (Units.coerce dv)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (SurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (SurfaceFunction3d (space1 @ units1))
  where
  lhs + rhs =
    new
      (compiled lhs + VectorSurfaceFunction3d.compiled rhs)
      (\p -> derivative p lhs + VectorSurfaceFunction3d.derivative p rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (SurfaceFunction3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (SurfaceFunction3d (space1 @ units1))
  where
  f + v = f + VectorSurfaceFunction3d.constant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (SurfaceFunction3d (space1 @ units1))
  where
  lhs - rhs =
    new
      (compiled lhs - VectorSurfaceFunction3d.compiled rhs)
      (\p -> derivative p lhs - VectorSurfaceFunction3d.derivative p rhs)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (SurfaceFunction3d (space1 @ units1))
  where
  f - v = f - VectorSurfaceFunction3d.constant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction3d (space1 @ units1))
    (SurfaceFunction3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ units1))
  where
  f1 - f2 =
    VectorSurfaceFunction3d.new
      (compiled f1 - compiled f2)
      (\p -> derivative p f1 - derivative p f2)

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (Region2d uvCoordinates)
    (SurfaceFunction3d (space @ units))
    (Surface3d (space @ units))
  where
  function . domain = Surface3d.parametric function domain

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (SurfaceFunction2d uvCoordinates)
    (SurfaceFunction3d (space @ units))
    (SurfaceFunction3d (space @ units))
  where
  outer . inner = do
    let duOuter = derivative U outer . inner
    let dvOuter = derivative V outer . inner
    new
      & compiled outer . SurfaceFunction2d.compiled inner
      & \p -> do
        let dInner = SurfaceFunction2d.derivative p inner
        let dU = VectorSurfaceFunction2d.xComponent dInner
        let dV = VectorSurfaceFunction2d.yComponent dInner
        duOuter * dU + dvOuter * dV

{-# INLINE compiled #-}
compiled :: SurfaceFunction3d (space @ units) -> Compiled (space @ units)
compiled (SurfaceFunction3d c _ _) = c

new ::
  Compiled (space @ units) ->
  (SurfaceParameter -> VectorSurfaceFunction3d (space @ units)) ->
  SurfaceFunction3d (space @ units)
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  let dv' =
        VectorSurfaceFunction3d.new (VectorSurfaceFunction3d.compiled dv) $
          \p -> case p of
            U -> VectorSurfaceFunction3d.derivative V du
            V -> VectorSurfaceFunction3d.derivative V dv
  SurfaceFunction3d c du dv'

constant :: Point3d (space @ units) -> SurfaceFunction3d (space @ units)
constant value = new (CompiledFunction.constant value) (always VectorSurfaceFunction3d.zero)

xyz ::
  SurfaceFunction units ->
  SurfaceFunction units ->
  SurfaceFunction units ->
  SurfaceFunction3d (space @ units)
xyz x y z =
  new
    & CompiledFunction.map3
      Expression.xyz
      Point3d
      Bounds3d
      (SurfaceFunction.compiled x)
      (SurfaceFunction.compiled y)
      (SurfaceFunction.compiled z)
    & \p ->
      VectorSurfaceFunction3d.xyz
        (SurfaceFunction.derivative p x)
        (SurfaceFunction.derivative p y)
        (SurfaceFunction.derivative p z)

evaluate :: SurfaceFunction3d (space @ units) -> UvPoint -> Point3d (space @ units)
evaluate function uvPoint = CompiledFunction.evaluate (compiled function) uvPoint

evaluateBounds :: SurfaceFunction3d (space @ units) -> UvBounds -> Bounds3d (space @ units)
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds (compiled function) uvBounds

derivative ::
  SurfaceParameter ->
  SurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units)
derivative U (SurfaceFunction3d _ du _) = du
derivative V (SurfaceFunction3d _ _ dv) = dv

transformBy ::
  Transform3d tag (space @ units) ->
  SurfaceFunction3d (space @ units) ->
  SurfaceFunction3d (space @ units)
transformBy transform function =
  new
    & CompiledFunction.map
      (Expression.Surface3d.transformBy transform)
      (Point3d.transformBy transform)
      (Bounds3d.transformBy transform)
      (compiled function)
    & \p -> VectorSurfaceFunction3d.transformBy transform (derivative p function)

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  SurfaceFunction3d (local @ units) ->
  SurfaceFunction3d (global @ units)
placeIn frame function =
  new
    & CompiledFunction.map
      (Expression.Surface3d.placeIn frame)
      (Point3d.placeIn frame)
      (Bounds3d.placeIn frame)
      (compiled function)
    & \p -> VectorSurfaceFunction3d.placeIn (Frame3d.basis frame) (derivative p function)

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  SurfaceFunction3d (global @ units) ->
  SurfaceFunction3d (local @ units)
relativeTo frame = placeIn (Frame3d.inverse frame)
