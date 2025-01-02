module OpenSolid.DirectionBounds2d
  ( DirectionBounds2d
  , unsafe
  , unwrap
  , constant
  , xComponent
  , yComponent
  )
where

import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)
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
  coerce = identity

instance Negation (DirectionBounds2d space) where
  negate (DirectionBounds2d vectorBounds) = DirectionBounds2d (negate vectorBounds)

instance Multiplication Sign (DirectionBounds2d space) (DirectionBounds2d space) where
  Positive * directionBounds = directionBounds
  Negative * directionBounds = -directionBounds

instance Multiplication' Sign (DirectionBounds2d space) (DirectionBounds2d space) where
  Positive .*. directionBounds = directionBounds
  Negative .*. directionBounds = -directionBounds

instance Multiplication (DirectionBounds2d space) Sign (DirectionBounds2d space) where
  directionBounds * Positive = directionBounds
  directionBounds * Negative = -directionBounds

instance Multiplication' (DirectionBounds2d space) Sign (DirectionBounds2d space) where
  directionBounds .*. Positive = directionBounds
  directionBounds .*. Negative = -directionBounds

instance
  Multiplication
    (Qty units)
    (DirectionBounds2d space)
    (VectorBounds2d (space @ units))
  where
  value * DirectionBounds2d vectorBounds = value * vectorBounds

instance
  Multiplication'
    (Qty units)
    (DirectionBounds2d space)
    (VectorBounds2d (space @ (units :*: Unitless)))
  where
  value .*. DirectionBounds2d vectorBounds = value .*. vectorBounds

instance
  Multiplication
    (DirectionBounds2d space)
    (Qty units)
    (VectorBounds2d (space @ units))
  where
  DirectionBounds2d vectorBounds * value = vectorBounds * value

instance
  Multiplication'
    (DirectionBounds2d space)
    (Qty units)
    (VectorBounds2d (space @ (Unitless :*: units)))
  where
  DirectionBounds2d vectorBounds .*. value = vectorBounds .*. value

instance
  Multiplication
    (Range units)
    (DirectionBounds2d space)
    (VectorBounds2d (space @ units))
  where
  range * DirectionBounds2d vectorBounds = range * vectorBounds

instance
  Multiplication'
    (Range units)
    (DirectionBounds2d space)
    (VectorBounds2d (space @ (units :*: Unitless)))
  where
  range .*. DirectionBounds2d vectorBounds = range .*. vectorBounds

instance
  Multiplication
    (DirectionBounds2d space)
    (Range units)
    (VectorBounds2d (space @ units))
  where
  DirectionBounds2d vectorBounds * range = vectorBounds * range

instance
  Multiplication'
    (DirectionBounds2d space)
    (Range units)
    (VectorBounds2d (space @ (Unitless :*: units)))
  where
  DirectionBounds2d vectorBounds .*. range = vectorBounds .*. range

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2d space1) (DirectionBounds2d space2) (Range Unitless)
  where
  DirectionBounds2d vectorBounds1 <> DirectionBounds2d vectorBounds2 =
    vectorBounds1 <> vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication'
    (DirectionBounds2d space1)
    (DirectionBounds2d space2)
    (Range (Unitless :*: Unitless))
  where
  DirectionBounds2d vectorBounds1 .<>. DirectionBounds2d vectorBounds2 =
    vectorBounds1 .<>. vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2d space1) (VectorBounds2d (space2 @ units)) (Range units)
  where
  DirectionBounds2d vectorBounds1 <> vectorBounds2 = vectorBounds1 <> vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication'
    (DirectionBounds2d space1)
    (VectorBounds2d (space2 @ units))
    (Range (Unitless :*: units))
  where
  DirectionBounds2d vectorBounds1 .<>. vectorBounds2 = vectorBounds1 .<>. vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (VectorBounds2d (space1 @ units)) (DirectionBounds2d space2) (Range units)
  where
  vectorBounds1 <> DirectionBounds2d vectorBounds2 = vectorBounds1 <> vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorBounds2d (space1 @ units))
    (DirectionBounds2d space2)
    (Range (units :*: Unitless))
  where
  vectorBounds1 .<>. DirectionBounds2d vectorBounds2 = vectorBounds1 .<>. vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2d space1) (Direction2d space2) (Range Unitless)
  where
  DirectionBounds2d vectorBounds <> direction = vectorBounds <> direction

instance
  space1 ~ space2 =>
  DotMultiplication'
    (DirectionBounds2d space1)
    (Direction2d space2)
    (Range (Unitless :*: Unitless))
  where
  DirectionBounds2d vectorBounds .<>. direction = vectorBounds .<>. direction

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (DirectionBounds2d space2) (Range Unitless)
  where
  direction <> DirectionBounds2d vectorBounds = direction <> vectorBounds

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Direction2d space1)
    (DirectionBounds2d space2)
    (Range (Unitless :*: Unitless))
  where
  direction .<>. DirectionBounds2d vectorBounds = direction .<>. vectorBounds

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2d space1) (Vector2d (space2 @ units)) (Range units)
  where
  DirectionBounds2d vectorBounds <> vector = vectorBounds <> vector

instance
  space1 ~ space2 =>
  DotMultiplication'
    (DirectionBounds2d space1)
    (Vector2d (space2 @ units))
    (Range (Unitless :*: units))
  where
  DirectionBounds2d vectorBounds .<>. vector = vectorBounds .<>. vector

instance
  space1 ~ space2 =>
  DotMultiplication (Vector2d (space1 @ units)) (DirectionBounds2d space2) (Range units)
  where
  vector <> DirectionBounds2d vectorBounds = vector <> vectorBounds

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units))
    (DirectionBounds2d space2)
    (Range (units :*: Unitless))
  where
  vector .<>. DirectionBounds2d vectorBounds = vector .<>. vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2d space1) (DirectionBounds2d space2) (Range Unitless)
  where
  DirectionBounds2d vectorBounds1 >< DirectionBounds2d vectorBounds2 =
    vectorBounds1 >< vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (DirectionBounds2d space1)
    (DirectionBounds2d space2)
    (Range (Unitless :*: Unitless))
  where
  DirectionBounds2d vectorBounds1 .><. DirectionBounds2d vectorBounds2 =
    vectorBounds1 .><. vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2d space1) (VectorBounds2d (space2 @ units)) (Range units)
  where
  DirectionBounds2d vectorBounds1 >< vectorBounds2 = vectorBounds1 >< vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (DirectionBounds2d space1)
    (VectorBounds2d (space2 @ units))
    (Range (Unitless :*: units))
  where
  DirectionBounds2d vectorBounds1 .><. vectorBounds2 = vectorBounds1 .><. vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorBounds2d (space1 @ units)) (DirectionBounds2d space2) (Range units)
  where
  vectorBounds1 >< DirectionBounds2d vectorBounds2 = vectorBounds1 >< vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorBounds2d (space1 @ units))
    (DirectionBounds2d space2)
    (Range (units :*: Unitless))
  where
  vectorBounds1 .><. DirectionBounds2d vectorBounds2 = vectorBounds1 .><. vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2d space1) (Direction2d space2) (Range Unitless)
  where
  DirectionBounds2d vectorBounds >< direction = vectorBounds >< direction

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (DirectionBounds2d space1)
    (Direction2d space2)
    (Range (Unitless :*: Unitless))
  where
  DirectionBounds2d vectorBounds .><. direction = vectorBounds .><. direction

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (DirectionBounds2d space2) (Range Unitless)
  where
  direction >< DirectionBounds2d vectorBounds = direction >< vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Direction2d space1)
    (DirectionBounds2d space2)
    (Range (Unitless :*: Unitless))
  where
  direction .><. DirectionBounds2d vectorBounds = direction .><. vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2d space1) (Vector2d (space2 @ units)) (Range units)
  where
  DirectionBounds2d vectorBounds >< vector = vectorBounds >< vector

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (DirectionBounds2d space1)
    (Vector2d (space2 @ units))
    (Range (Unitless :*: units))
  where
  DirectionBounds2d vectorBounds .><. vector = vectorBounds .><. vector

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector2d (space1 @ units)) (DirectionBounds2d space2) (Range units)
  where
  vector >< DirectionBounds2d vectorBounds = vector >< vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units))
    (DirectionBounds2d space2)
    (Range (units :*: Unitless))
  where
  vector .><. DirectionBounds2d vectorBounds = vector .><. vectorBounds

unsafe :: VectorBounds2d (space @ Unitless) -> DirectionBounds2d space
unsafe = DirectionBounds2d

unwrap :: DirectionBounds2d space -> VectorBounds2d (space @ Unitless)
unwrap (DirectionBounds2d vectorBounds) = vectorBounds

constant :: Direction2d space -> DirectionBounds2d space
constant direction = DirectionBounds2d (VectorBounds2d.constant (Vector2d.unit direction))

xComponent :: DirectionBounds2d space -> Range Unitless
xComponent (DirectionBounds2d vectorBounds) = VectorBounds2d.xComponent vectorBounds

yComponent :: DirectionBounds2d space -> Range Unitless
yComponent (DirectionBounds2d vectorBounds) = VectorBounds2d.yComponent vectorBounds
