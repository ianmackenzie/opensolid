module OpenSolid.DirectionBounds2d
  ( DirectionBounds2d
  , unsafe
  , unwrap
  , constant
  , xComponent
  , yComponent
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d)
import OpenSolid.VectorBounds2d qualified as VectorBounds2d

newtype DirectionBounds2d space
  = DirectionBounds2d (VectorBounds2d (space @ Unitless))
  deriving (Show)

instance HasUnits (DirectionBounds2d space) Unitless

instance
  space1 ~ space2 =>
  Units.Coercion (DirectionBounds2d space1) (DirectionBounds2d space2)
  where
  coerce = id

instance Negation (DirectionBounds2d space) where
  negate (DirectionBounds2d vectorBounds) = DirectionBounds2d (negate vectorBounds)

instance Multiplication Sign (DirectionBounds2d space) (DirectionBounds2d space) where
  Positive * directionBounds = directionBounds
  Negative * directionBounds = -directionBounds

instance Multiplication' Sign (DirectionBounds2d space) (DirectionBounds2d space) where
  Positive ~*~ directionBounds = directionBounds
  Negative ~*~ directionBounds = -directionBounds

instance Multiplication (DirectionBounds2d space) Sign (DirectionBounds2d space) where
  directionBounds * Positive = directionBounds
  directionBounds * Negative = -directionBounds

instance Multiplication' (DirectionBounds2d space) Sign (DirectionBounds2d space) where
  directionBounds ~*~ Positive = directionBounds
  directionBounds ~*~ Negative = -directionBounds

instance
  Multiplication
    (Quantity units)
    (DirectionBounds2d space)
    (VectorBounds2d (space @ units))
  where
  value * DirectionBounds2d vectorBounds = value * vectorBounds

instance
  Multiplication
    (DirectionBounds2d space)
    (Quantity units)
    (VectorBounds2d (space @ units))
  where
  DirectionBounds2d vectorBounds * value = vectorBounds * value

instance
  Multiplication
    (Bounds units)
    (DirectionBounds2d space)
    (VectorBounds2d (space @ units))
  where
  bounds * DirectionBounds2d vectorBounds = bounds * vectorBounds

instance
  Multiplication
    (DirectionBounds2d space)
    (Bounds units)
    (VectorBounds2d (space @ units))
  where
  DirectionBounds2d vectorBounds * bounds = vectorBounds * bounds

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2d space1) (DirectionBounds2d space2) (Bounds Unitless)
  where
  DirectionBounds2d vectorBounds1 `dot` DirectionBounds2d vectorBounds2 =
    vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2d space1) (VectorBounds2d (space2 @ units)) (Bounds units)
  where
  DirectionBounds2d vectorBounds1 `dot` vectorBounds2 = vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (VectorBounds2d (space1 @ units)) (DirectionBounds2d space2) (Bounds units)
  where
  vectorBounds1 `dot` DirectionBounds2d vectorBounds2 = vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2d space1) (Direction2d space2) (Bounds Unitless)
  where
  DirectionBounds2d vectorBounds `dot` direction = vectorBounds `dot` direction

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (DirectionBounds2d space2) (Bounds Unitless)
  where
  direction `dot` DirectionBounds2d vectorBounds = direction `dot` vectorBounds

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2d space1) (Vector2d (space2 @ units)) (Bounds units)
  where
  DirectionBounds2d vectorBounds `dot` vector = vectorBounds `dot` vector

instance
  space1 ~ space2 =>
  DotMultiplication (Vector2d (space1 @ units)) (DirectionBounds2d space2) (Bounds units)
  where
  vector `dot` DirectionBounds2d vectorBounds = vector `dot` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2d space1) (DirectionBounds2d space2) (Bounds Unitless)
  where
  DirectionBounds2d vectorBounds1 `cross` DirectionBounds2d vectorBounds2 =
    vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2d space1) (VectorBounds2d (space2 @ units)) (Bounds units)
  where
  DirectionBounds2d vectorBounds1 `cross` vectorBounds2 = vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorBounds2d (space1 @ units)) (DirectionBounds2d space2) (Bounds units)
  where
  vectorBounds1 `cross` DirectionBounds2d vectorBounds2 = vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2d space1) (Direction2d space2) (Bounds Unitless)
  where
  DirectionBounds2d vectorBounds `cross` direction = vectorBounds `cross` direction

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (DirectionBounds2d space2) (Bounds Unitless)
  where
  direction `cross` DirectionBounds2d vectorBounds = direction `cross` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2d space1) (Vector2d (space2 @ units)) (Bounds units)
  where
  DirectionBounds2d vectorBounds `cross` vector = vectorBounds `cross` vector

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector2d (space1 @ units)) (DirectionBounds2d space2) (Bounds units)
  where
  vector `cross` DirectionBounds2d vectorBounds = vector `cross` vectorBounds

unsafe :: VectorBounds2d (space @ Unitless) -> DirectionBounds2d space
unsafe = DirectionBounds2d

unwrap :: DirectionBounds2d space -> VectorBounds2d (space @ Unitless)
unwrap (DirectionBounds2d vectorBounds) = vectorBounds

constant :: Direction2d space -> DirectionBounds2d space
constant direction = DirectionBounds2d (VectorBounds2d.constant (Vector2d.unit direction))

xComponent :: DirectionBounds2d space -> Bounds Unitless
xComponent (DirectionBounds2d vectorBounds) = VectorBounds2d.xComponent vectorBounds

yComponent :: DirectionBounds2d space -> Bounds Unitless
yComponent (DirectionBounds2d vectorBounds) = VectorBounds2d.yComponent vectorBounds
