-- Needed for CurveSurfaceComposition
{-# OPTIONS_GHC -Wno-orphans #-}

module Surface2d.Function
  ( Function (Parametric)
  , Interface (..)
  , new
  , constant
  , xy
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import Arithmetic qualified
import Bounds2d (Bounds2d)
import Bounds2d qualified
import Composition
import {-# SOURCE #-} Curve2d (Curve2d)
import {-# SOURCE #-} Curve2d qualified
import Expression (Expression)
import Expression qualified
import Expression.Surface2d qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Surface1d.Function qualified
import SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvCoordinates, UvPoint)
import Transform2d (Transform2d)
import Transform2d qualified
import Units qualified
import Vector2d (Vector2d)
import VectorCurve2d qualified
import VectorSurface2d.Function qualified

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
    Surface1d.Function.Function units ->
    Surface1d.Function.Function units ->
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
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Function (space1 @ units1))
    (VectorSurface2d.Function.Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  Parametric lhs + VectorSurface2d.Function.Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Addition lhs rhs

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Function (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Function (space1 @ units1))
  where
  f + v = f + VectorSurface2d.Function.constant v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Function (space1 @ units1))
    (VectorSurface2d.Function.Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  Parametric lhs - VectorSurface2d.Function.Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Subtraction lhs rhs

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Function (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Function (space1 @ units1))
  where
  f - v = f - VectorSurface2d.Function.constant v

instance
  VectorSurface2d.Function.Interface
    (Arithmetic.Difference (Function (space @ units)) (Function (space @ units)))
    (space @ units)
  where
  evaluateImpl (Arithmetic.Difference f1 f2) uvPoint =
    evaluate f1 uvPoint - evaluate f2 uvPoint

  evaluateBoundsImpl (Arithmetic.Difference f1 f2) uvBounds =
    evaluateBounds f1 uvBounds - evaluateBounds f2 uvBounds

  derivativeImpl parameter (Arithmetic.Difference f1 f2) =
    derivative parameter f1 - derivative parameter f2

  transformByImpl transform (Arithmetic.Difference f1 f2) =
    VectorSurface2d.Function.new $
      Arithmetic.Difference
        -- Note the slight hack here:
        -- the definition of VectorCurve2d.Interface states that the units of the transform
        -- do *not* have to match the units of the vector curve,
        -- because vectors and vector curves ignore translation
        -- (and the units of the transform are just the units of its translation part).
        -- This would in general mean that we couldn't apply the given transform to a Point2d or Curve2d,
        -- but in this case it's safe since any translation will cancel out
        -- when the point and curve are subtracted from each other.
        (transformBy (Units.coerce transform) f1)
        (transformBy (Units.coerce transform) f2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (VectorSurface2d.Function.Function (space1 @ units1))
  where
  Parametric lhs - Parametric rhs = VectorSurface2d.Function.Parametric (lhs - rhs)
  lhs - rhs = VectorSurface2d.Function.new (Arithmetic.Difference lhs rhs)

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

constant :: Point2d (space @ units) -> Function (space @ units)
constant = Parametric . Expression.constant

xy ::
  Surface1d.Function.Function units ->
  Surface1d.Function.Function units ->
  Function (space @ units)
xy (Surface1d.Function.Parametric x) (Surface1d.Function.Parametric y) =
  Parametric (Expression.xy x y)
xy x y = XY x y

evaluate :: Function (space @ units) -> UvPoint -> Point2d (space @ units)
evaluate function uv = case function of
  Function f -> evaluateImpl f uv
  Coerce f -> Units.coerce (evaluate f uv)
  Parametric expression -> Expression.evaluate expression uv
  XY x y ->
    Point2d.xy
      (Surface1d.Function.evaluate x uv)
      (Surface1d.Function.evaluate y uv)
  Addition f1 f2 -> evaluate f1 uv + VectorSurface2d.Function.evaluate f2 uv
  Subtraction f1 f2 -> evaluate f1 uv - VectorSurface2d.Function.evaluate f2 uv
  Transformed transform f -> Point2d.transformBy transform (evaluate f uv)

evaluateBounds :: Function (space @ units) -> UvBounds -> Bounds2d (space @ units)
evaluateBounds function uv = case function of
  Function f -> evaluateBoundsImpl f uv
  Coerce f -> Units.coerce (evaluateBounds f uv)
  Parametric expression -> Expression.evaluateBounds expression uv
  XY x y ->
    Bounds2d.xy
      (Surface1d.Function.evaluateBounds x uv)
      (Surface1d.Function.evaluateBounds y uv)
  Addition f1 f2 -> evaluateBounds f1 uv + VectorSurface2d.Function.evaluateBounds f2 uv
  Subtraction f1 f2 -> evaluateBounds f1 uv - VectorSurface2d.Function.evaluateBounds f2 uv
  Transformed transform f -> Bounds2d.transformBy transform (evaluateBounds f uv)

derivative :: SurfaceParameter -> Function (space @ units) -> VectorSurface2d.Function.Function (space @ units)
derivative parameter function = case function of
  Function f -> derivativeImpl parameter f
  Coerce f -> Units.coerce (derivative parameter f)
  Parametric expression ->
    VectorSurface2d.Function.Parametric (Expression.surfaceDerivative parameter expression)
  XY x y ->
    VectorSurface2d.Function.xy
      (Surface1d.Function.derivative parameter x)
      (Surface1d.Function.derivative parameter y)
  Addition f1 f2 -> derivative parameter f1 + VectorSurface2d.Function.derivative parameter f2
  Subtraction f1 f2 -> derivative parameter f1 - VectorSurface2d.Function.derivative parameter f2
  Transformed transform f -> VectorSurface2d.Function.transformBy transform (derivative parameter f)

transformBy ::
  Transform2d tag (space @ units) ->
  Function (space @ units) ->
  Function (space @ units)
transformBy transform function = case function of
  Function f -> transformByImpl transform f
  Coerce c -> Units.coerce (transformBy (Units.coerce transform) c)
  Parametric expression -> Parametric (Expression.Surface2d.transformBy transform expression)
  XY{} -> Transformed (Transform2d.toAffine transform) function
  Addition f1 f2 -> transformBy transform f1 + VectorSurface2d.Function.transformBy transform f2
  Subtraction c v -> transformBy transform c - VectorSurface2d.Function.transformBy transform v
  Transformed current f -> Transformed (Transform2d.toAffine transform . current) f

instance
  Composition
    (Surface1d.Function.Function Unitless)
    (Curve2d (space @ units))
    (Function (space @ units))
  where
  Curve2d.Parametric outer . Surface1d.Function.Parametric inner = Parametric (outer . inner)
  curve . function = new (curve :.: function)

instance
  Interface
    (Curve2d (space @ units) :.: Surface1d.Function.Function Unitless)
    (space @ units)
  where
  evaluateImpl (curve :.: function) uvPoint =
    Curve2d.evaluate curve (Surface1d.Function.evaluate function uvPoint)

  evaluateBoundsImpl (curve :.: function) uvBounds =
    Curve2d.evaluateBounds curve (Surface1d.Function.evaluateBounds function uvBounds)

  derivativeImpl parameter (curve :.: function) =
    (Curve2d.derivative curve . function) * Surface1d.Function.derivative parameter function

  transformByImpl transform (curve :.: function) =
    Curve2d.transformBy transform curve . function

instance
  Composition
    (Curve2d UvCoordinates)
    (Function (space @ units))
    (Curve2d (space @ units))
  where
  Parametric function . Curve2d.Parametric curve = Curve2d.Parametric (function . curve)
  function . curve = Curve2d.new (function :.: curve)

instance
  Curve2d.Interface
    (Function (space @ units) :.: Curve2d UvCoordinates)
    (space @ units)
  where
  evaluateImpl (function :.: curve) tValue =
    evaluate function (Curve2d.evaluate curve tValue)

  evaluateBoundsImpl (function :.: curve) tRange =
    evaluateBounds function (Curve2d.evaluateBounds curve tRange)

  boundsImpl (function :.: curve) =
    evaluateBounds function (Curve2d.bounds curve)

  derivativeImpl (function :.: curve) = do
    let curveDerivative = Curve2d.derivative curve
    let dudt = VectorCurve2d.xComponent curveDerivative
    let dvdt = VectorCurve2d.yComponent curveDerivative
    (derivative U function . curve) * dudt + (derivative V function . curve) * dvdt

  reverseImpl (function :.: curve) =
    function :.: Curve2d.reverse curve

  transformByImpl transform (function :.: curve) =
    transformBy transform function . curve
