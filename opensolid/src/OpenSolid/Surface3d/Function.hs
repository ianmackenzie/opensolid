-- Needed for CurveSurfaceComposition
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.Surface3d.Function
  ( Function (Parametric)
  , Interface (..)
  , new
  , constant
  , xyz
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import Bounds3d (Bounds3d)
import Bounds3d qualified
import Composition
import OpenSolid.Prelude
import OpenSolid.Curve3d (Curve3d)
import OpenSolid.Curve3d qualified as Curve3d
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Surface1d qualified as Surface1d
import OpenSolid.Surface1d.Function qualified as Surface1d.Function
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorSurface3d qualified as VectorSurface3d
import OpenSolid.VectorSurface3d.Function qualified as VectorSurface3d.Function
import SurfaceParameter (SurfaceParameter, UvBounds, UvPoint)
import Units qualified

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> UvPoint -> Point3d coordinateSystem
  evaluateBoundsImpl :: function -> UvBounds -> Bounds3d coordinateSystem
  derivativeImpl :: SurfaceParameter -> function -> VectorSurface3d.Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  Parametric ::
    Expression UvPoint (Point3d (space @ units)) ->
    Function (space @ units)
  XYZ ::
    Surface1d.Function units ->
    Surface1d.Function units ->
    Surface1d.Function units ->
    Function (space @ units)
  Sum ::
    Function (space @ units) ->
    VectorSurface3d.Function (space @ units) ->
    Function (space @ units)
  Difference ::
    Function (space @ units) ->
    VectorSurface3d.Function (space @ units) ->
    Function (space @ units)

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type UnitsOf (Function (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ unitsA)) (Function (space2 @ unitsB))
  where
  coerce function = case function of
    Parametric expression -> Parametric (Units.coerce expression)
    Coerce f -> Coerce f
    _ -> Coerce function

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Function (space @ units))
    (VectorSurface3d.Function (space_ @ units_))
    (Function (space @ units))
  where
  Parametric lhs + VectorSurface3d.Function.Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Sum lhs rhs

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Function (space @ units))
    (Vector3d (space_ @ units_))
    (Function (space @ units))
  where
  f + v = f + VectorSurface3d.Function.constant v

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Function (space @ units))
    (VectorSurface3d.Function (space_ @ units_))
    (Function (space @ units))
  where
  Parametric lhs - VectorSurface3d.Function.Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Difference lhs rhs

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Function (space @ units))
    (Vector3d (space_ @ units_))
    (Function (space @ units))
  where
  f - v = f - VectorSurface3d.Function.constant v

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

constant :: Point3d (space @ units) -> Function (space @ units)
constant = Parametric . Expression.constant

xyz ::
  Surface1d.Function units ->
  Surface1d.Function units ->
  Surface1d.Function units ->
  Function (space @ units)
xyz
  (Surface1d.Function.Parametric x)
  (Surface1d.Function.Parametric y)
  (Surface1d.Function.Parametric z) =
    Parametric (Expression.xyz x y z)
xyz x y z = XYZ x y z

evaluate :: Function (space @ units) -> UvPoint -> Point3d (space @ units)
evaluate function uvPoint = case function of
  Function f -> evaluateImpl f uvPoint
  Coerce f -> Units.coerce (evaluate f uvPoint)
  Parametric expression -> Expression.evaluate expression uvPoint
  XYZ x y z ->
    Point3d.xyz
      (Surface1d.Function.evaluate x uvPoint)
      (Surface1d.Function.evaluate y uvPoint)
      (Surface1d.Function.evaluate z uvPoint)
  Sum f1 f2 -> evaluate f1 uvPoint + VectorSurface3d.Function.evaluate f2 uvPoint
  Difference f1 f2 -> evaluate f1 uvPoint - VectorSurface3d.Function.evaluate f2 uvPoint

evaluateBounds :: Function (space @ units) -> UvBounds -> Bounds3d (space @ units)
evaluateBounds function uvBounds = case function of
  Function f -> evaluateBoundsImpl f uvBounds
  Coerce f -> Units.coerce (evaluateBounds f uvBounds)
  Parametric expression -> Expression.evaluateBounds expression uvBounds
  XYZ x y z ->
    Bounds3d.xyz
      (Surface1d.Function.evaluateBounds x uvBounds)
      (Surface1d.Function.evaluateBounds y uvBounds)
      (Surface1d.Function.evaluateBounds z uvBounds)
  Sum f1 f2 ->
    evaluateBounds f1 uvBounds + VectorSurface3d.Function.evaluateBounds f2 uvBounds
  Difference f1 f2 ->
    evaluateBounds f1 uvBounds - VectorSurface3d.Function.evaluateBounds f2 uvBounds

derivative ::
  SurfaceParameter ->
  Function (space @ units) ->
  VectorSurface3d.Function (space @ units)
derivative parameter function = case function of
  Function f -> derivativeImpl parameter f
  Coerce f -> Units.coerce (derivative parameter f)
  Parametric expression ->
    VectorSurface3d.Function.Parametric (Expression.surfaceDerivative parameter expression)
  XYZ x y z ->
    VectorSurface3d.Function.xyz
      (Surface1d.Function.derivative parameter x)
      (Surface1d.Function.derivative parameter y)
      (Surface1d.Function.derivative parameter z)
  Sum f1 f2 -> derivative parameter f1 + VectorSurface3d.Function.derivative parameter f2
  Difference f1 f2 -> derivative parameter f1 - VectorSurface3d.Function.derivative parameter f2

data SurfaceCurveComposition (coordinateSystem :: CoordinateSystem) where
  SurfaceCurveComposition ::
    Surface1d.Function Unitless ->
    Curve3d (space @ units) ->
    SurfaceCurveComposition (space @ units)

deriving instance Show (SurfaceCurveComposition coordinateSystem)

instance
  Composition
    (Surface1d.Function Unitless)
    (Curve3d (space @ units))
    (Function (space @ units))
  where
  Curve3d.Parametric curve . Surface1d.Function.Parametric function = Parametric (curve . function)
  curve . function = new (curve :.: function)

instance Interface (Curve3d (space @ units) :.: Surface1d.Function Unitless) (space @ units) where
  evaluateImpl (curve :.: function) uvPoint =
    Curve3d.evaluate curve (Surface1d.Function.evaluate function uvPoint)

  evaluateBoundsImpl (curve :.: function) uvBounds =
    Curve3d.evaluateBounds curve (Surface1d.Function.evaluateBounds function uvBounds)

  derivativeImpl parameter (curve :.: function) =
    (Curve3d.derivative curve . function) * Surface1d.Function.derivative parameter function
