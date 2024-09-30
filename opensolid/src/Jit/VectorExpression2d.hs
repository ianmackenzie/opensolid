-- Needed for 'Expression * Vector2d = VectorExpression2d'
-- and 'Vector2d * Expression = VectorExpression2d' instances
{-# OPTIONS_GHC -Wno-orphans #-}

module Jit.VectorExpression2d
  ( VectorExpression2d (VectorExpression2d)
  , Curve
  , Surface
  , constant
  , xy
  , squaredMagnitude
  , magnitude
  , interpolateFrom
  , placeIn
  , placeInBasis
  , transformBy
  )
where

import Basis2d (Basis2d)
import Basis2d qualified
import Direction2d (Direction2d (Direction2d))
import Frame2d (Frame2d)
import Frame2d qualified
import Jit.Expression (Expression)
import Jit.Expression qualified as Expression
import {-# SOURCE #-} Jit.Expression2d (Expression2d (Expression2d))
import OpenSolid
import Point2d (Point2d (Point2d))
import Transform2d (Transform2d (Transform2d))
import Units qualified
import Vector2d (Vector2d (Vector2d))

data VectorExpression2d parameterization
  = VectorExpression2d (Expression parameterization) (Expression parameterization)

type Curve = VectorExpression2d Expression.Curve

type Surface = VectorExpression2d Expression.Surface

instance HasUnits (VectorExpression2d parameterization) where
  type UnitsOf (VectorExpression2d parameterization) = Unitless

instance
  parameterization1 ~ parameterization2 =>
  Units.Coercion
    (VectorExpression2d parameterization1)
    (VectorExpression2d parameterization2)
  where
  coerce = identity

instance Negation (VectorExpression2d parameterization) where
  negate (VectorExpression2d vx vy) = VectorExpression2d (negate vx) (negate vy)

instance Multiplication' Sign (VectorExpression2d parameterization) where
  type Sign .*. VectorExpression2d parameterization = VectorExpression2d parameterization
  sign .*. VectorExpression2d vx vy = VectorExpression2d (sign .*. vx) (sign .*. vy)

instance Multiplication' (VectorExpression2d parameterization) Sign where
  type VectorExpression2d parameterization .*. Sign = VectorExpression2d parameterization
  VectorExpression2d vx vy .*. sign = VectorExpression2d (vx .*. sign) (vy .*. sign)

instance
  Multiplication
    Sign
    (VectorExpression2d parameterization)
    (VectorExpression2d parameterization)

instance
  Multiplication
    (VectorExpression2d parameterization)
    Sign
    (VectorExpression2d parameterization)

instance
  parameterization1 ~ parameterization2 =>
  Addition
    (VectorExpression2d parameterization1)
    (VectorExpression2d parameterization2)
    (VectorExpression2d parameterization1)
  where
  VectorExpression2d x1 y1 + VectorExpression2d x2 y2 = VectorExpression2d (x1 + x2) (y1 + y2)

instance
  Addition
    (VectorExpression2d parameterization)
    (Vector2d (space @ units))
    (VectorExpression2d parameterization)
  where
  curve + vector = curve + constant vector

instance
  Addition
    (Vector2d (space @ units))
    (VectorExpression2d parameterization)
    (VectorExpression2d parameterization)
  where
  vector + curve = constant vector + curve

instance
  Addition
    (Point2d (space @ units))
    (VectorExpression2d parameterization)
    (Expression2d parameterization)
  where
  Point2d px py + VectorExpression2d vx vy = Expression2d (px + vx) (py + vy)

instance
  parameterization1 ~ parameterization2 =>
  Subtraction
    (VectorExpression2d parameterization1)
    (VectorExpression2d parameterization2)
    (VectorExpression2d parameterization1)
  where
  VectorExpression2d x1 y1 - VectorExpression2d x2 y2 = VectorExpression2d (x1 - x2) (y1 - y2)

instance
  Subtraction
    (VectorExpression2d parameterization)
    (Vector2d (space @ units))
    (VectorExpression2d parameterization)
  where
  curve - vector = curve - constant vector

instance
  Subtraction
    (Vector2d (space @ units))
    (VectorExpression2d parameterization)
    (VectorExpression2d parameterization)
  where
  vector - curve = constant vector - curve

instance
  parameterization1 ~ parameterization2 =>
  Multiplication' (Expression parameterization1) (VectorExpression2d parameterization2)
  where
  type
    Expression parameterization1 .*. VectorExpression2d parameterization2 =
      VectorExpression2d parameterization1
  scale .*. VectorExpression2d vx vy = VectorExpression2d (scale .*. vx) (scale .*. vy)

instance
  parameterization1 ~ parameterization2 =>
  Multiplication' (VectorExpression2d parameterization1) (Expression parameterization2)
  where
  type
    VectorExpression2d parameterization1 .*. Expression parameterization2 =
      VectorExpression2d parameterization1
  VectorExpression2d vx vy .*. scale = VectorExpression2d (vx .*. scale) (vy .*. scale)

instance
  parameterization1 ~ parameterization2 =>
  Multiplication
    (Expression parameterization1)
    (VectorExpression2d parameterization2)
    (VectorExpression2d parameterization1)

instance
  parameterization1 ~ parameterization2 =>
  Multiplication
    (VectorExpression2d parameterization1)
    (Expression parameterization2)
    (VectorExpression2d parameterization1)

instance Multiplication' (Expression parameterization) (Vector2d (space @ units)) where
  type
    Expression parameterization .*. Vector2d (space @ units) =
      VectorExpression2d parameterization
  expression .*. Vector2d vx vy = VectorExpression2d (expression .*. vx) (expression .*. vy)

instance Multiplication' (Vector2d (space @ units)) (Expression parameterization) where
  type
    Vector2d (space @ units) .*. Expression parameterization =
      VectorExpression2d parameterization
  Vector2d vx vy .*. expression = VectorExpression2d (vx .*. expression) (vy .*. expression)

instance
  Multiplication
    (Expression parameterization)
    (Vector2d (space @ units))
    (VectorExpression2d parameterization)

instance
  Multiplication
    (Vector2d (space @ units))
    (Expression parameterization)
    (VectorExpression2d parameterization)

instance
  parameterization1 ~ parameterization2 =>
  Division' (VectorExpression2d parameterization1) (Expression parameterization2)
  where
  type
    VectorExpression2d parameterization1 ./. Expression parameterization2 =
      VectorExpression2d parameterization1
  VectorExpression2d vx vy ./. scale = VectorExpression2d (vx ./. scale) (vy ./. scale)

instance
  parameterization1 ~ parameterization2 =>
  Division
    (VectorExpression2d parameterization1)
    (Expression parameterization2)
    (VectorExpression2d parameterization1)

instance
  parameterization1 ~ parameterization2 =>
  DotMultiplication'
    (VectorExpression2d parameterization1)
    (VectorExpression2d parameterization2)
  where
  type
    VectorExpression2d parameterization1 .<>. VectorExpression2d parameterization2 =
      Expression parameterization1
  VectorExpression2d x1 y1 .<>. VectorExpression2d x2 y2 = x1 .*. x2 + y1 .*. y2

instance
  parameterization1 ~ parameterization2 =>
  DotMultiplication
    (VectorExpression2d parameterization1)
    (VectorExpression2d parameterization2)
    (Expression parameterization1)

instance
  parameterization1 ~ parameterization2 =>
  CrossMultiplication'
    (VectorExpression2d parameterization1)
    (VectorExpression2d parameterization2)
  where
  type
    VectorExpression2d parameterization1 .><. VectorExpression2d parameterization2 =
      Expression parameterization1
  VectorExpression2d x1 y1 .><. VectorExpression2d x2 y2 = x1 .*. y2 - y1 .*. x2

instance
  parameterization1 ~ parameterization2 =>
  CrossMultiplication
    (VectorExpression2d parameterization1)
    (VectorExpression2d parameterization2)
    (Expression parameterization1)

instance
  Composition
    (Expression parameterization)
    (VectorExpression2d Expression.Curve)
    (VectorExpression2d parameterization)
  where
  VectorExpression2d vx vy . expression = VectorExpression2d (vx . expression) (vy . expression)

constant :: Vector2d (space @ units) -> VectorExpression2d parameterization
constant (Vector2d vx vy) = VectorExpression2d (Expression.constant vx) (Expression.constant vy)

xy ::
  Expression parameterization ->
  Expression parameterization ->
  VectorExpression2d parameterization
xy = VectorExpression2d

squaredMagnitude :: VectorExpression2d parameterization -> Expression parameterization
squaredMagnitude (VectorExpression2d x y) = Expression.squared x + Expression.squared y

magnitude :: VectorExpression2d parameterization -> Expression parameterization
magnitude = Expression.sqrt . squaredMagnitude

interpolateFrom ::
  VectorExpression2d parameterization ->
  VectorExpression2d parameterization ->
  Expression parameterization ->
  VectorExpression2d parameterization
interpolateFrom start end t = start + t * (end - start)

placeIn ::
  Frame2d (space @ units) defines ->
  VectorExpression2d parameterization ->
  VectorExpression2d parameterization
placeIn frame expression = placeInBasis (Frame2d.basis frame) expression

placeInBasis ::
  Basis2d space defines ->
  VectorExpression2d parameterization ->
  VectorExpression2d parameterization
placeInBasis basis (VectorExpression2d x y) = do
  let i = Vector2d.unit (Basis2d.xDirection basis)
  let j = Vector2d.unit (Basis2d.yDirection basis)
  x * i + y * j

transformBy ::
  Transform2d a (space @ units) ->
  VectorExpression2d parameterization ->
  VectorExpression2d parameterization
transformBy (Transform2d _ i j) (VectorExpression2d x y) = x * i + y * j
