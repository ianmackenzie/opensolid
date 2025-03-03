module OpenSolid.SurfaceFunction3d
  ( SurfaceFunction3d (Parametric)
  , Interface (..)
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

import OpenSolid.Arithmetic qualified as Arithmetic
import OpenSolid.Bounds3d (Bounds3d (Bounds3d))
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.Composition
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Surface3d qualified as Expression.Surface3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Point3d (Point3d)
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
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> UvPoint -> Point3d coordinateSystem
  evaluateBoundsImpl :: function -> UvBounds -> Bounds3d coordinateSystem
  derivativeImpl :: SurfaceParameter -> function -> VectorSurfaceFunction3d coordinateSystem

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
  PlaceIn ::
    Frame3d (global @ units) (Defines local) ->
    SurfaceFunction3d (local @ units) ->
    SurfaceFunction3d (global @ units)
  Transformed ::
    Transform3d tag (space @ units) ->
    SurfaceFunction3d (space @ units) ->
    SurfaceFunction3d (space @ units)

deriving instance Show (SurfaceFunction3d (space @ units))

instance HasUnits (SurfaceFunction3d (space @ units)) units (SurfaceFunction3d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (SurfaceFunction3d (space1 @ unitsA)) (SurfaceFunction3d (space2 @ unitsB))
  where
  coerce function = case function of
    Parametric expression -> Parametric (Units.coerce expression)
    Coerce f -> Coerce f
    _ -> Coerce function

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (SurfaceFunction3d (space1 @ units1))
    (VectorSurfaceFunction3d (space2 @ units2))
    (SurfaceFunction3d (space1 @ units1))
  where
  Parametric lhs + VectorSurfaceFunction3d.Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Sum lhs rhs

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
  Parametric lhs - VectorSurfaceFunction3d.Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Difference lhs rhs

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
  Parametric expression1 - Parametric expression2 = VectorSurfaceFunction3d.Parametric (expression1 - expression2)
  f1 - f2 = VectorSurfaceFunction3d.new (Arithmetic.Difference f1 f2)

instance
  VectorSurfaceFunction3d.Interface
    (Arithmetic.Difference (SurfaceFunction3d (space @ units)) (SurfaceFunction3d (space @ units)))
    (space @ units)
  where
  evaluateImpl (Arithmetic.Difference f1 f2) uvPoint =
    evaluate f1 uvPoint - evaluate f2 uvPoint

  evaluateBoundsImpl (Arithmetic.Difference f1 f2) uvBounds =
    evaluateBounds f1 uvBounds - evaluateBounds f2 uvBounds

  derivativeImpl parameter (Arithmetic.Difference f1 f2) =
    derivative parameter f1 - derivative parameter f2

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
  Parametric outer . SurfaceFunction2d.Parametric inner = Parametric (outer . inner)
  outer . inner = new (outer :.: inner)

instance
  uvCoordinates ~ UvCoordinates =>
  Interface
    (SurfaceFunction3d (space @ units) :.: SurfaceFunction2d uvCoordinates)
    (space @ units)
  where
  evaluateImpl (outer :.: inner) uvValue =
    evaluate outer (SurfaceFunction2d.evaluate inner uvValue)

  evaluateBoundsImpl (outer :.: inner) uvBounds =
    evaluateBounds outer (SurfaceFunction2d.evaluateBounds inner uvBounds)

  derivativeImpl varyingParameter (outer :.: inner) = do
    let duOuter = derivative U outer . inner
    let dvOuter = derivative V outer . inner
    let dInner = SurfaceFunction2d.derivative varyingParameter inner
    duOuter * VectorSurfaceFunction2d.xComponent dInner + dvOuter * VectorSurfaceFunction2d.yComponent dInner

new :: Interface function (space @ units) => function -> SurfaceFunction3d (space @ units)
new = SurfaceFunction3d

constant :: Point3d (space @ units) -> SurfaceFunction3d (space @ units)
constant = Parametric . Expression.constant

xyz ::
  SurfaceFunction units ->
  SurfaceFunction units ->
  SurfaceFunction units ->
  SurfaceFunction3d (space @ units)
xyz
  (SurfaceFunction.Parametric x)
  (SurfaceFunction.Parametric y)
  (SurfaceFunction.Parametric z) =
    Parametric (Expression.xyz x y z)
xyz x y z = XYZ x y z

evaluate :: SurfaceFunction3d (space @ units) -> UvPoint -> Point3d (space @ units)
evaluate function uvPoint = case function of
  SurfaceFunction3d f -> evaluateImpl f uvPoint
  Coerce f -> Units.coerce (evaluate f uvPoint)
  Parametric expression -> Expression.evaluate expression uvPoint
  XYZ x y z ->
    Point3d.xyz
      (SurfaceFunction.evaluate x uvPoint)
      (SurfaceFunction.evaluate y uvPoint)
      (SurfaceFunction.evaluate z uvPoint)
  Sum f1 f2 -> evaluate f1 uvPoint + VectorSurfaceFunction3d.evaluate f2 uvPoint
  Difference f1 f2 -> evaluate f1 uvPoint - VectorSurfaceFunction3d.evaluate f2 uvPoint
  PlaceIn frame f -> Point3d.placeIn frame (evaluate f uvPoint)
  Transformed transform f -> Point3d.transformBy transform (evaluate f uvPoint)

evaluateBounds :: SurfaceFunction3d (space @ units) -> UvBounds -> Bounds3d (space @ units)
evaluateBounds function uvBounds = case function of
  SurfaceFunction3d f -> evaluateBoundsImpl f uvBounds
  Coerce f -> Units.coerce (evaluateBounds f uvBounds)
  Parametric expression -> Expression.evaluateBounds expression uvBounds
  XYZ x y z ->
    Bounds3d
      (SurfaceFunction.evaluateBounds x uvBounds)
      (SurfaceFunction.evaluateBounds y uvBounds)
      (SurfaceFunction.evaluateBounds z uvBounds)
  Sum f1 f2 ->
    evaluateBounds f1 uvBounds + VectorSurfaceFunction3d.evaluateBounds f2 uvBounds
  Difference f1 f2 ->
    evaluateBounds f1 uvBounds - VectorSurfaceFunction3d.evaluateBounds f2 uvBounds
  PlaceIn frame f -> Bounds3d.placeIn frame (evaluateBounds f uvBounds)
  Transformed transform f -> Bounds3d.transformBy transform (evaluateBounds f uvBounds)

derivative ::
  SurfaceParameter ->
  SurfaceFunction3d (space @ units) ->
  VectorSurfaceFunction3d (space @ units)
derivative parameter function = case function of
  SurfaceFunction3d f -> derivativeImpl parameter f
  Coerce f -> Units.coerce (derivative parameter f)
  Parametric expression ->
    VectorSurfaceFunction3d.Parametric (Expression.surfaceDerivative parameter expression)
  XYZ x y z ->
    VectorSurfaceFunction3d.xyz
      (SurfaceFunction.derivative parameter x)
      (SurfaceFunction.derivative parameter y)
      (SurfaceFunction.derivative parameter z)
  Sum f1 f2 -> derivative parameter f1 + VectorSurfaceFunction3d.derivative parameter f2
  Difference f1 f2 -> derivative parameter f1 - VectorSurfaceFunction3d.derivative parameter f2
  PlaceIn frame f -> VectorSurfaceFunction3d.placeIn (Frame3d.basis frame) (derivative parameter f)
  Transformed transform f -> VectorSurfaceFunction3d.transformBy transform (derivative parameter f)

transformBy ::
  Transform3d tag (space @ units) ->
  SurfaceFunction3d (space @ units) ->
  SurfaceFunction3d (space @ units)
transformBy transform function = case function of
  Coerce f -> Coerce (transformBy (Units.coerce transform) f)
  Parametric expression -> Parametric (Expression.Surface3d.transformBy transform expression)
  Sum f1 f2 -> transformBy transform f1 + VectorSurfaceFunction3d.transformBy transform f2
  Difference f1 f2 -> transformBy transform f1 - VectorSurfaceFunction3d.transformBy transform f2
  Transformed existing f ->
    Transformed (Transform3d.toAffine transform . Transform3d.toAffine existing) f
  _ -> Transformed transform function

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  SurfaceFunction3d (local @ units) ->
  SurfaceFunction3d (global @ units)
placeIn frame f = case f of
  Parametric expression -> Parametric (Expression.Surface3d.placeIn frame expression)
  Coerce function -> Coerce (placeIn (Units.coerce frame) function)
  function -> PlaceIn frame function

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  SurfaceFunction3d (global @ units) ->
  SurfaceFunction3d (local @ units)
relativeTo frame = placeIn (Frame3d.inverse frame)
