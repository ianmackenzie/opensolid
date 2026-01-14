module OpenSolid.DirectionBounds3D
  ( DirectionBounds3D
  , unsafe
  , unwrap
  , constant
  , xComponent
  , yComponent
  , zComponent
  )
where

import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D

newtype DirectionBounds3D space
  = UnitBounds3D (VectorBounds3D Unitless space)
  deriving (Show)

instance
  space1 ~ space2 =>
  Units.Coercion (DirectionBounds3D space1) (DirectionBounds3D space2)
  where
  coerce = id

instance Negation (DirectionBounds3D space) where
  negative (UnitBounds3D vectorBounds) = UnitBounds3D (negative vectorBounds)

instance Multiplication Sign (DirectionBounds3D space) (DirectionBounds3D space) where
  Positive .*. directionBounds = directionBounds
  Negative .*. directionBounds = negative directionBounds

instance Multiplication_ Sign (DirectionBounds3D space) (DirectionBounds3D space) where
  Positive ?*? directionBounds = directionBounds
  Negative ?*? directionBounds = negative directionBounds

instance Multiplication (DirectionBounds3D space) Sign (DirectionBounds3D space) where
  directionBounds .*. Positive = directionBounds
  directionBounds .*. Negative = negative directionBounds

instance Multiplication_ (DirectionBounds3D space) Sign (DirectionBounds3D space) where
  directionBounds ?*? Positive = directionBounds
  directionBounds ?*? Negative = negative directionBounds

instance
  Multiplication
    (Quantity units)
    (DirectionBounds3D space)
    (VectorBounds3D units space)
  where
  value .*. UnitBounds3D vectorBounds = value .*. vectorBounds

instance
  Multiplication
    (DirectionBounds3D space)
    (Quantity units)
    (VectorBounds3D units space)
  where
  UnitBounds3D vectorBounds .*. value = vectorBounds .*. value

instance
  Multiplication
    (Interval units)
    (DirectionBounds3D space)
    (VectorBounds3D units space)
  where
  interval .*. UnitBounds3D vectorBounds = interval .*. vectorBounds

instance
  Multiplication
    (DirectionBounds3D space)
    (Interval units)
    (VectorBounds3D units space)
  where
  UnitBounds3D vectorBounds .*. interval = vectorBounds .*. interval

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds3D space1) (DirectionBounds3D space2) (Interval Unitless)
  where
  UnitBounds3D vectorBounds1 `dot` UnitBounds3D vectorBounds2 =
    vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds3D space1) (VectorBounds3D units space2) (Interval units)
  where
  UnitBounds3D vectorBounds1 `dot` vectorBounds2 = vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (VectorBounds3D units space1) (DirectionBounds3D space2) (Interval units)
  where
  vectorBounds1 `dot` UnitBounds3D vectorBounds2 = vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds3D space1) (Direction3D space2) (Interval Unitless)
  where
  UnitBounds3D vectorBounds `dot` direction = vectorBounds `dot` direction

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3D space1) (DirectionBounds3D space2) (Interval Unitless)
  where
  direction `dot` UnitBounds3D vectorBounds = direction `dot` vectorBounds

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds3D space1) (Vector3D units space2) (Interval units)
  where
  UnitBounds3D vectorBounds `dot` vector = vectorBounds `dot` vector

instance
  space1 ~ space2 =>
  DotMultiplication (Vector3D units space1) (DirectionBounds3D space2) (Interval units)
  where
  vector `dot` UnitBounds3D vectorBounds = vector `dot` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionBounds3D space1)
    (DirectionBounds3D space2)
    (VectorBounds3D Unitless space1)
  where
  UnitBounds3D vectorBounds1 `cross` UnitBounds3D vectorBounds2 =
    vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionBounds3D space1)
    (VectorBounds3D units space2)
    (VectorBounds3D units space1)
  where
  UnitBounds3D vectorBounds1 `cross` vectorBounds2 = vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorBounds3D units space1)
    (DirectionBounds3D space2)
    (VectorBounds3D units space1)
  where
  vectorBounds1 `cross` UnitBounds3D vectorBounds2 = vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionBounds3D space1)
    (Direction3D space2)
    (VectorBounds3D Unitless space1)
  where
  UnitBounds3D vectorBounds `cross` direction = vectorBounds `cross` direction

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3D space1)
    (DirectionBounds3D space2)
    (VectorBounds3D Unitless space1)
  where
  direction `cross` UnitBounds3D vectorBounds = direction `cross` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionBounds3D space1)
    (Vector3D units space2)
    (VectorBounds3D units space1)
  where
  UnitBounds3D vectorBounds `cross` vector = vectorBounds `cross` vector

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Vector3D units space1)
    (DirectionBounds3D space2)
    (VectorBounds3D units space1)
  where
  vector `cross` UnitBounds3D vectorBounds = vector `cross` vectorBounds

unsafe :: VectorBounds3D Unitless space -> DirectionBounds3D space
unsafe = UnitBounds3D

unwrap :: DirectionBounds3D space -> VectorBounds3D Unitless space
unwrap (UnitBounds3D vectorBounds) = vectorBounds

constant :: Direction3D space -> DirectionBounds3D space
constant direction = UnitBounds3D (VectorBounds3D.constant (Vector3D.unit direction))

xComponent :: DirectionBounds3D space -> Interval Unitless
xComponent (UnitBounds3D vectorBounds) = VectorBounds3D.xComponent vectorBounds

yComponent :: DirectionBounds3D space -> Interval Unitless
yComponent (UnitBounds3D vectorBounds) = VectorBounds3D.yComponent vectorBounds

zComponent :: DirectionBounds3D space -> Interval Unitless
zComponent (UnitBounds3D vectorBounds) = VectorBounds3D.zComponent vectorBounds
