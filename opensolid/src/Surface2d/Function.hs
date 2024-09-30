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
  , toAst
  )
where

import Bounds2d (Bounds2d)
import Bounds2d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Jit.Expression qualified as Expression
import Jit.Expression2d (Expression2d)
import Jit.Expression2d qualified as Expression2d
import Maybe qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import Surface1d qualified
import Surface1d.Function qualified
import Typeable qualified
import Units qualified
import Uv (Parameter)
import Uv qualified
import Vector2d (Vector2d)
import Vector2d qualified
import VectorSurface2d qualified
import VectorSurface2d.Function qualified

class
  Known function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Uv.Point -> Point2d coordinateSystem
  boundsImpl :: function -> Uv.Bounds -> Bounds2d coordinateSystem
  derivativeImpl :: Parameter -> function -> VectorSurface2d.Function coordinateSystem
  toAstImpl :: function -> Maybe (Expression2d Expression.Surface)

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Coerce ::
    (Known units1, Known units2) =>
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

instance (Known space, Known units) => Eq (Function (space @ units)) where
  function1 == function2 = case function1 of
    Function f1 | Function f2 <- function2 -> Typeable.equal f1 f2 | otherwise -> False
    Coerce f1 | Coerce f2 <- function2 -> Typeable.equal f1 f2 | otherwise -> False
    Constant x | Constant y <- function2 -> x == y | otherwise -> False
    XY x1 y1 | XY x2 y2 <- function2 -> x1 == x2 && y1 == y2 | otherwise -> False
    Addition lhs1 rhs1 | Addition lhs2 rhs2 <- function2 -> lhs1 == lhs2 && rhs1 == rhs2 | otherwise -> False
    Subtraction lhs1 rhs1 | Subtraction lhs2 rhs2 <- function2 -> lhs1 == lhs2 && rhs1 == rhs2 | otherwise -> False

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type UnitsOf (Function (space @ units)) = units

instance
  (Known space1, Known space2, Known unitsA, Known unitsB, space1 ~ space2) =>
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

deriving instance (Known space, Known units) => Eq (Difference (space @ units))

deriving instance Show (Difference (space @ units))

instance
  (Known space, Known units) =>
  VectorSurface2d.Function.Interface (Difference (space @ units)) (space @ units)
  where
  evaluateImpl (Difference f1 f2) uv = evaluate f1 uv - evaluate f2 uv
  boundsImpl (Difference f1 f2) uv = bounds f1 uv - bounds f2 uv
  derivativeImpl parameter (Difference f1 f2) = derivative parameter f1 - derivative parameter f2
  toAstImpl (Difference f1 f2) = Maybe.map2 (-) (toAst f1) (toAst f2)

instance
  (Known space1, Known space2, Known units1, Known units2, space1 ~ space2, units1 ~ units2) =>
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

derivative ::
  (Known space, Known units) =>
  Uv.Parameter ->
  Function (space @ units) ->
  VectorSurface2d.Function (space @ units)
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

deriving instance (Known space, Known units) => Eq (SurfaceCurveComposition (space @ units))

instance
  (Known space, Known units) =>
  Composition
    (Surface1d.Function Unitless)
    (Curve2d (space @ units))
    (Function (space @ units))
  where
  curve . function = new (SurfaceCurveComposition function curve)

instance
  (Known space, Known units) =>
  Interface (SurfaceCurveComposition (space @ units)) (space @ units)
  where
  evaluateImpl (SurfaceCurveComposition function curve) uv =
    Curve2d.pointOn curve (Surface1d.Function.evaluate function uv)

  boundsImpl (SurfaceCurveComposition function curve) uv =
    Curve2d.segmentBounds curve (Surface1d.Function.bounds function uv)

  derivativeImpl parameter (SurfaceCurveComposition function curve) =
    (Curve2d.derivative curve . function) * Surface1d.Function.derivative parameter function

  toAstImpl (SurfaceCurveComposition function curve) =
    Maybe.map2 (.) (Curve2d.toAst curve) (Surface1d.Function.toAst function)

toAst :: Known space => Function (space @ units) -> Maybe (Expression2d Expression.Surface)
toAst function = case function of
  Function f -> toAstImpl f
  Coerce f -> toAst f
  Constant p -> Just (Expression2d.constant p)
  XY x y -> Maybe.map2 Expression2d.xy (Surface1d.Function.toAst x) (Surface1d.Function.toAst y)
  Addition f1 f2 -> Maybe.map2 (+) (toAst f1) (VectorSurface2d.Function.toAst f2)
  Subtraction f1 f2 -> Maybe.map2 (-) (toAst f1) (VectorSurface2d.Function.toAst f2)
