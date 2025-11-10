module OpenSolid.DirectionBounds3d
  ( DirectionBounds3d
  , unsafe
  , unwrap
  , constant
  , xComponent
  , yComponent
  , zComponent
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds3d (VectorBounds3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d

newtype DirectionBounds3d space
  = DirectionBounds3d (VectorBounds3d (space @ Unitless))
  deriving (Show)

instance HasUnits (DirectionBounds3d space) Unitless

instance
  space1 ~ space2 =>
  Units.Coercion (DirectionBounds3d space1) (DirectionBounds3d space2)
  where
  coerce = id

instance Negation (DirectionBounds3d space) where
  negative (DirectionBounds3d vectorBounds) = DirectionBounds3d (negative vectorBounds)

instance Multiplication Sign (DirectionBounds3d space) (DirectionBounds3d space) where
  Positive .*. directionBounds = directionBounds
  Negative .*. directionBounds = negative directionBounds

instance Multiplication# Sign (DirectionBounds3d space) (DirectionBounds3d space) where
  Positive #*# directionBounds = directionBounds
  Negative #*# directionBounds = negative directionBounds

instance Multiplication (DirectionBounds3d space) Sign (DirectionBounds3d space) where
  directionBounds .*. Positive = directionBounds
  directionBounds .*. Negative = negative directionBounds

instance Multiplication# (DirectionBounds3d space) Sign (DirectionBounds3d space) where
  directionBounds #*# Positive = directionBounds
  directionBounds #*# Negative = negative directionBounds

instance
  Multiplication
    (Quantity units)
    (DirectionBounds3d space)
    (VectorBounds3d (space @ units))
  where
  value .*. DirectionBounds3d vectorBounds = value .*. vectorBounds

instance
  Multiplication
    (DirectionBounds3d space)
    (Quantity units)
    (VectorBounds3d (space @ units))
  where
  DirectionBounds3d vectorBounds .*. value = vectorBounds .*. value

instance
  Multiplication
    (Bounds units)
    (DirectionBounds3d space)
    (VectorBounds3d (space @ units))
  where
  bounds .*. DirectionBounds3d vectorBounds = bounds .*. vectorBounds

instance
  Multiplication
    (DirectionBounds3d space)
    (Bounds units)
    (VectorBounds3d (space @ units))
  where
  DirectionBounds3d vectorBounds .*. bounds = vectorBounds .*. bounds

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds3d space1) (DirectionBounds3d space2) (Bounds Unitless)
  where
  DirectionBounds3d vectorBounds1 `dot` DirectionBounds3d vectorBounds2 =
    vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds3d space1) (VectorBounds3d (space2 @ units)) (Bounds units)
  where
  DirectionBounds3d vectorBounds1 `dot` vectorBounds2 = vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (VectorBounds3d (space1 @ units)) (DirectionBounds3d space2) (Bounds units)
  where
  vectorBounds1 `dot` DirectionBounds3d vectorBounds2 = vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds3d space1) (Direction3d space2) (Bounds Unitless)
  where
  DirectionBounds3d vectorBounds `dot` direction = vectorBounds `dot` direction

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3d space1) (DirectionBounds3d space2) (Bounds Unitless)
  where
  direction `dot` DirectionBounds3d vectorBounds = direction `dot` vectorBounds

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds3d space1) (Vector3d (space2 @ units)) (Bounds units)
  where
  DirectionBounds3d vectorBounds `dot` vector = vectorBounds `dot` vector

instance
  space1 ~ space2 =>
  DotMultiplication (Vector3d (space1 @ units)) (DirectionBounds3d space2) (Bounds units)
  where
  vector `dot` DirectionBounds3d vectorBounds = vector `dot` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionBounds3d space1)
    (DirectionBounds3d space2)
    (VectorBounds3d (space1 @ Unitless))
  where
  DirectionBounds3d vectorBounds1 `cross` DirectionBounds3d vectorBounds2 =
    vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionBounds3d space1)
    (VectorBounds3d (space2 @ units))
    (VectorBounds3d (space1 @ units))
  where
  DirectionBounds3d vectorBounds1 `cross` vectorBounds2 = vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorBounds3d (space1 @ units))
    (DirectionBounds3d space2)
    (VectorBounds3d (space1 @ units))
  where
  vectorBounds1 `cross` DirectionBounds3d vectorBounds2 = vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionBounds3d space1)
    (Direction3d space2)
    (VectorBounds3d (space1 @ Unitless))
  where
  DirectionBounds3d vectorBounds `cross` direction = vectorBounds `cross` direction

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (DirectionBounds3d space2)
    (VectorBounds3d (space1 @ Unitless))
  where
  direction `cross` DirectionBounds3d vectorBounds = direction `cross` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionBounds3d space1)
    (Vector3d (space2 @ units))
    (VectorBounds3d (space1 @ units))
  where
  DirectionBounds3d vectorBounds `cross` vector = vectorBounds `cross` vector

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Vector3d (space1 @ units))
    (DirectionBounds3d space2)
    (VectorBounds3d (space1 @ units))
  where
  vector `cross` DirectionBounds3d vectorBounds = vector `cross` vectorBounds

unsafe :: VectorBounds3d (space @ Unitless) -> DirectionBounds3d space
unsafe = DirectionBounds3d

unwrap :: DirectionBounds3d space -> VectorBounds3d (space @ Unitless)
unwrap (DirectionBounds3d vectorBounds) = vectorBounds

constant :: Direction3d space -> DirectionBounds3d space
constant direction = DirectionBounds3d (VectorBounds3d.constant (Vector3d.unit direction))

xComponent :: DirectionBounds3d space -> Bounds Unitless
xComponent (DirectionBounds3d vectorBounds) = VectorBounds3d.xComponent vectorBounds

yComponent :: DirectionBounds3d space -> Bounds Unitless
yComponent (DirectionBounds3d vectorBounds) = VectorBounds3d.yComponent vectorBounds

zComponent :: DirectionBounds3d space -> Bounds Unitless
zComponent (DirectionBounds3d vectorBounds) = VectorBounds3d.zComponent vectorBounds
