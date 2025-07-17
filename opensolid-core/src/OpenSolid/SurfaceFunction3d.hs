module OpenSolid.SurfaceFunction3d
  ( SurfaceFunction3d
  , Compiled
  , new
  , constant
  , evaluate
  , evaluateBounds
  , derivative
  , placeIn
  , relativeTo
  , transformBy
  )
where

import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Expression.Surface3d qualified as Expression.Surface3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Region2d (Region2d)
import {-# SOURCE #-} OpenSolid.Surface3d (Surface3d)
import {-# SOURCE #-} OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction2d (SurfaceFunction2d)
import OpenSolid.SurfaceFunction2d qualified as SurfaceFunction2d
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d

data SurfaceFunction3d (coordinateSystem :: CoordinateSystem) where
  SurfaceFunction3d ::
    Compiled (space @ units) ->
    ~(VectorSurfaceFunction3d (space @ units)) ->
    ~(VectorSurfaceFunction3d (space @ units)) ->
    SurfaceFunction3d (space @ units)

instance
  HasField
    "du"
    (SurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space @ units))
  where
  getField (SurfaceFunction3d _ du _) = du

instance
  HasField
    "dv"
    (SurfaceFunction3d (space @ units))
    (VectorSurfaceFunction3d (space @ units))
  where
  getField (SurfaceFunction3d _ _ dv) = dv

type Compiled coordinateSystem =
  CompiledFunction
    UvPoint
    (Point3d coordinateSystem)
    UvBounds
    (Bounds3d coordinateSystem)

instance HasUnits (SurfaceFunction3d (space @ units)) units

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
      @ lhs.compiled + rhs.compiled
      @ \parameter -> derivative parameter lhs + VectorSurfaceFunction3d.derivative parameter rhs

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
      @ lhs.compiled - rhs.compiled
      @ \parameter -> derivative parameter lhs - VectorSurfaceFunction3d.derivative parameter rhs

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
  lhs - rhs =
    VectorSurfaceFunction3d.new
      @ lhs.compiled - rhs.compiled
      @ \parameter -> derivative parameter lhs - derivative parameter rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction3d (space1 @ units1))
    (Point3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ units1))
  where
  function - point = function - constant point

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point3d (space1 @ units1))
    (SurfaceFunction3d (space2 @ units2))
    (VectorSurfaceFunction3d (space1 @ units1))
  where
  point - function = constant point - function

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
    let duOuter = outer.du . inner
    let dvOuter = outer.dv . inner
    new
      @ outer.compiled . inner.compiled
      @ \parameter -> do
        let innerDerivative = SurfaceFunction2d.derivative parameter inner
        let (dU, dV) = innerDerivative.components
        duOuter * dU + dvOuter * dV

instance HasField "compiled" (SurfaceFunction3d (space @ units)) (Compiled (space @ units)) where
  getField (SurfaceFunction3d c _ _) = c

new ::
  Compiled (space @ units) ->
  (SurfaceParameter -> VectorSurfaceFunction3d (space @ units)) ->
  SurfaceFunction3d (space @ units)
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  let dv' = VectorSurfaceFunction3d.new dv.compiled (\case U -> du.dv; V -> dv.dv)
  SurfaceFunction3d c du dv'

constant :: Point3d (space @ units) -> SurfaceFunction3d (space @ units)
constant value = new (CompiledFunction.constant value) (always VectorSurfaceFunction3d.zero)

evaluate :: SurfaceFunction3d (space @ units) -> UvPoint -> Point3d (space @ units)
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

evaluateBounds :: SurfaceFunction3d (space @ units) -> UvBounds -> Bounds3d (space @ units)
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

derivative ::
  SurfaceParameter ->
  SurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units)
derivative U = (.du)
derivative V = (.dv)

transformBy ::
  Transform3d tag (space @ units) ->
  SurfaceFunction3d (space @ units) ->
  SurfaceFunction3d (space @ units)
transformBy transform function =
  new
    @ CompiledFunction.map
      (Expression.Surface3d.transformBy transform)
      (Point3d.transformBy transform)
      (Bounds3d.transformBy transform)
      function.compiled
    @ \parameter -> VectorSurfaceFunction3d.transformBy transform (derivative parameter function)

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  SurfaceFunction3d (local @ units) ->
  SurfaceFunction3d (global @ units)
placeIn frame function =
  new
    @ CompiledFunction.map
      (Expression.Surface3d.placeIn frame)
      (Point3d.placeIn frame)
      (Bounds3d.placeIn frame)
      function.compiled
    @ \parameter -> VectorSurfaceFunction3d.placeIn frame (derivative parameter function)

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  SurfaceFunction3d (global @ units) ->
  SurfaceFunction3d (local @ units)
relativeTo frame = placeIn (Frame3d.inverse frame)
