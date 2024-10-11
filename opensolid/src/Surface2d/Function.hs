-- Needed for CurveSurfaceComposition
{-# OPTIONS_GHC -Wno-orphans #-}

module Surface2d.Function
  ( Function (Constant)
  , Interface (..)
  , new
  , constant
  , xy
  , evaluate
  , bounds
  , derivative
  , expression
  )
where

import Bounds2d (Bounds2d)
import Bounds2d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Expression (Expression)
import Expression.Point2d qualified
import Maybe qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Surface1d qualified
import Surface1d.Function qualified
import Units qualified
import Uv (Parameter)
import Uv qualified
import Vector2d (Vector2d)
import Vector2d qualified
import VectorSurface2d qualified
import VectorSurface2d.Function qualified

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Uv.Point -> Point2d coordinateSystem
  boundsImpl :: function -> Uv.Bounds -> Bounds2d coordinateSystem
  derivativeImpl :: Parameter -> function -> VectorSurface2d.Function coordinateSystem
  expressionImpl :: function -> Maybe (Expression Uv.Point (Point2d coordinateSystem))

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  Constant ::
    Point2d (space @ units) ->
    Function (space @ units)
  XY ::
    Surface1d.Function units ->
    Surface1d.Function units ->
    Function (space @ units)
  Addition ::
    Function (space @ units) ->
    VectorSurface2d.Function (space @ units) ->
    Function (space @ units)
  Subtraction ::
    Function (space @ units) ->
    VectorSurface2d.Function (space @ units) ->
    Function (space @ units)

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type UnitsOf (Function (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ unitsA)) (Function (space2 @ unitsB))
  where
  coerce function = case function of
    Constant v -> Constant (Units.coerce v)
    Coerce f -> Coerce f
    _ -> Coerce function

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Function (space1 @ units1))
    (VectorSurface2d.Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  f1 + VectorSurface2d.Function.Constant v | v == Vector2d.zero = f1
  f1 + f2 = Addition f1 f2

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
    (VectorSurface2d.Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  f1 - VectorSurface2d.Function.Constant v | v == Vector2d.zero = f1
  f1 - f2 = Subtraction f1 f2

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

data Difference (coordinateSystem :: CoordinateSystem) where
  Difference ::
    Function (space @ units) ->
    Function (space @ units) ->
    Difference (space @ units)

deriving instance Show (Difference (space @ units))

instance VectorSurface2d.Function.Interface (Difference (space @ units)) (space @ units) where
  evaluateImpl (Difference f1 f2) uv = evaluate f1 uv - evaluate f2 uv
  boundsImpl (Difference f1 f2) uv = bounds f1 uv - bounds f2 uv
  derivativeImpl parameter (Difference f1 f2) = derivative parameter f1 - derivative parameter f2
  expressionImpl (Difference f1 f2) = Maybe.map2 (-) (expression f1) (expression f2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (VectorSurface2d.Function (space1 @ units1))
  where
  Constant p1 - Constant p2 = VectorSurface2d.Function.Constant (p1 - p2)
  f1 - f2 = VectorSurface2d.Function.new (Difference f1 f2)

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

constant :: Point2d (space @ units) -> Function (space @ units)
constant = Constant

xy ::
  Surface1d.Function units ->
  Surface1d.Function units ->
  Function (space @ units)
xy = XY

evaluate :: Function (space @ units) -> Uv.Point -> Point2d (space @ units)
evaluate function uv = case function of
  Function f -> evaluateImpl f uv
  Coerce f -> Units.coerce (evaluate f uv)
  Constant v -> v
  XY x y ->
    Point2d.xy
      (Surface1d.Function.evaluate x uv)
      (Surface1d.Function.evaluate y uv)
  Addition f1 f2 -> evaluate f1 uv + VectorSurface2d.Function.evaluate f2 uv
  Subtraction f1 f2 -> evaluate f1 uv - VectorSurface2d.Function.evaluate f2 uv

bounds :: Function (space @ units) -> Uv.Bounds -> Bounds2d (space @ units)
bounds function uv = case function of
  Function f -> boundsImpl f uv
  Coerce f -> Units.coerce (bounds f uv)
  Constant v -> Bounds2d.constant v
  XY x y ->
    Bounds2d.xy
      (Surface1d.Function.bounds x uv)
      (Surface1d.Function.bounds y uv)
  Addition f1 f2 -> bounds f1 uv + VectorSurface2d.Function.bounds f2 uv
  Subtraction f1 f2 -> bounds f1 uv - VectorSurface2d.Function.bounds f2 uv

derivative :: Uv.Parameter -> Function (space @ units) -> VectorSurface2d.Function (space @ units)
derivative parameter function = case function of
  Function f -> derivativeImpl parameter f
  Coerce f -> Units.coerce (derivative parameter f)
  Constant _ -> VectorSurface2d.Function.zero
  XY x y ->
    VectorSurface2d.Function.xy
      (Surface1d.Function.derivative parameter x)
      (Surface1d.Function.derivative parameter y)
  Addition f1 f2 -> derivative parameter f1 + VectorSurface2d.Function.derivative parameter f2
  Subtraction f1 f2 -> derivative parameter f1 - VectorSurface2d.Function.derivative parameter f2

data SurfaceCurveComposition (coordinateSystem :: CoordinateSystem) where
  SurfaceCurveComposition ::
    Surface1d.Function Unitless ->
    Curve2d (space @ units) ->
    SurfaceCurveComposition (space @ units)

deriving instance Show (SurfaceCurveComposition (space @ units))

instance
  Composition
    (Surface1d.Function Unitless)
    (Curve2d (space @ units))
    (Function (space @ units))
  where
  curve . function = new (SurfaceCurveComposition function curve)

instance Interface (SurfaceCurveComposition (space @ units)) (space @ units) where
  evaluateImpl (SurfaceCurveComposition function curve) uv =
    Curve2d.pointOn curve (Surface1d.Function.evaluate function uv)

  boundsImpl (SurfaceCurveComposition function curve) uv =
    Curve2d.segmentBounds curve (Surface1d.Function.bounds function uv)

  derivativeImpl parameter (SurfaceCurveComposition function curve) =
    (Curve2d.derivative curve . function) * Surface1d.Function.derivative parameter function

  expressionImpl (SurfaceCurveComposition function curve) =
    Maybe.map2 (.) (Curve2d.expression curve) (Surface1d.Function.expression function)

expression :: Function (space @ units) -> Maybe (Expression Uv.Point (Point2d (space @ units)))
expression function = case function of
  Function f -> expressionImpl f
  Coerce f -> Units.coerce (expression f)
  Constant p -> Just (Expression.Point2d.constant p)
  XY x y -> Maybe.map2 Expression.Point2d.xy (Surface1d.Function.expression x) (Surface1d.Function.expression y)
  Addition f1 f2 -> Maybe.map2 (+) (expression f1) (VectorSurface2d.Function.expression f2)
  Subtraction f1 f2 -> Maybe.map2 (-) (expression f1) (VectorSurface2d.Function.expression f2)
