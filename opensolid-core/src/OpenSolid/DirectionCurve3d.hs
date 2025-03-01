module OpenSolid.DirectionCurve3d
  ( DirectionCurve3d
  , unsafe
  , unwrap
  , startValue
  , endValue
  , evaluate
  , evaluateBounds
  , derivative
  , constant
  , reverse
  , xComponent
  , yComponent
  , zComponent
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Curve (Curve)
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.DirectionBounds3d (DirectionBounds3d)
import OpenSolid.DirectionBounds3d qualified as DirectionBounds3d
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorCurve3d (VectorCurve3d)
import OpenSolid.VectorCurve3d qualified as VectorCurve3d

newtype DirectionCurve3d space = DirectionCurve3d (VectorCurve3d (space @ Unitless))
  deriving (Show)

instance HasUnits (DirectionCurve3d space) Unitless (DirectionCurve3d space)

instance
  space1 ~ space2 =>
  Units.Coercion (DirectionCurve3d space1) (DirectionCurve3d space2)
  where
  coerce = identity

unsafe :: VectorCurve3d (space @ Unitless) -> DirectionCurve3d space
unsafe = DirectionCurve3d

unwrap :: DirectionCurve3d space -> VectorCurve3d (space @ Unitless)
unwrap (DirectionCurve3d vectorCurve) = vectorCurve

startValue :: DirectionCurve3d space -> Direction3d space
startValue curve = evaluate curve 0.0

endValue :: DirectionCurve3d space -> Direction3d space
endValue curve = evaluate curve 1.0

evaluate :: DirectionCurve3d space -> Float -> Direction3d space
evaluate (DirectionCurve3d vectorCurve) tValue =
  Direction3d.unsafe (VectorCurve3d.evaluate vectorCurve tValue)

evaluateBounds :: DirectionCurve3d space -> Range Unitless -> DirectionBounds3d space
evaluateBounds (DirectionCurve3d vectorCurve) tRange =
  DirectionBounds3d.unsafe (VectorCurve3d.evaluateBounds vectorCurve tRange)

derivative :: DirectionCurve3d space -> VectorCurve3d (space @ Unitless)
derivative (DirectionCurve3d vectorCurve) = VectorCurve3d.derivative vectorCurve

constant :: Direction3d space -> DirectionCurve3d space
constant direction = DirectionCurve3d (VectorCurve3d.constant (Vector3d.unit direction))

reverse :: DirectionCurve3d space -> DirectionCurve3d space
reverse (DirectionCurve3d vectorCurve) = DirectionCurve3d (VectorCurve3d.reverse vectorCurve)

instance Negation (DirectionCurve3d space) where
  negate (DirectionCurve3d vectorCurve) = DirectionCurve3d (negate vectorCurve)

instance Multiplication Sign (DirectionCurve3d space) (DirectionCurve3d space) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication' Sign (DirectionCurve3d space) (DirectionCurve3d space) where
  Positive .*. curve = curve
  Negative .*. curve = -curve

instance Multiplication (DirectionCurve3d space) Sign (DirectionCurve3d space) where
  curve * Positive = curve
  curve * Negative = -curve

instance Multiplication' (DirectionCurve3d space) Sign (DirectionCurve3d space) where
  curve .*. Positive = curve
  curve .*. Negative = -curve

instance Multiplication (Qty units) (DirectionCurve3d space) (VectorCurve3d (space @ units)) where
  value * DirectionCurve3d vectorCurve = value * vectorCurve

instance Multiplication (DirectionCurve3d space) (Qty units) (VectorCurve3d (space @ units)) where
  DirectionCurve3d vectorCurve * value = vectorCurve * value

instance
  Multiplication
    (Curve units)
    (DirectionCurve3d space)
    (VectorCurve3d (space @ units))
  where
  scalarCurve * DirectionCurve3d vectorCurve = scalarCurve * vectorCurve

instance
  Multiplication
    (DirectionCurve3d space)
    (Curve units)
    (VectorCurve3d (space @ units))
  where
  DirectionCurve3d vectorCurve * scalarCurve = vectorCurve * scalarCurve

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3d space1) (DirectionCurve3d space2) (Curve Unitless)
  where
  DirectionCurve3d lhs <> DirectionCurve3d rhs = lhs <> rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3d space1) (VectorCurve3d (space2 @ units)) (Curve units)
  where
  DirectionCurve3d lhs <> rhs = lhs <> rhs

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve3d (space1 @ units)) (DirectionCurve3d space2) (Curve units)
  where
  lhs <> DirectionCurve3d rhs = lhs <> rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3d space1) (Direction3d space2) (Curve Unitless)
  where
  DirectionCurve3d lhs <> rhs = lhs <> rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3d space1) (DirectionCurve3d space2) (Curve Unitless)
  where
  lhs <> DirectionCurve3d rhs = lhs <> rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3d space1) (Vector3d (space2 @ units)) (Curve units)
  where
  DirectionCurve3d lhs <> rhs = lhs <> rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Vector3d (space1 @ units)) (DirectionCurve3d space2) (Curve units)
  where
  lhs <> DirectionCurve3d rhs = lhs <> rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3d space1)
    (DirectionCurve3d space2)
    (VectorCurve3d (space1 @ Unitless))
  where
  DirectionCurve3d lhs >< DirectionCurve3d rhs = lhs >< rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3d space1)
    (VectorCurve3d (space2 @ units))
    (VectorCurve3d (space1 @ units))
  where
  DirectionCurve3d lhs >< rhs = lhs >< rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units))
    (DirectionCurve3d space2)
    (VectorCurve3d (space1 @ units))
  where
  lhs >< DirectionCurve3d rhs = lhs >< rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3d space1)
    (Direction3d space2)
    (VectorCurve3d (space1 @ Unitless))
  where
  DirectionCurve3d lhs >< rhs = lhs >< rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (DirectionCurve3d space2)
    (VectorCurve3d (space1 @ Unitless))
  where
  lhs >< DirectionCurve3d rhs = lhs >< rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3d space1)
    (Vector3d (space2 @ units))
    (VectorCurve3d (space1 @ units))
  where
  DirectionCurve3d lhs >< rhs = lhs >< rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Vector3d (space1 @ units))
    (DirectionCurve3d space2)
    (VectorCurve3d (space1 @ units))
  where
  lhs >< DirectionCurve3d rhs = lhs >< rhs

instance
  unitless ~ Unitless =>
  Composition (Curve unitless) (DirectionCurve3d space) (DirectionCurve3d space)
  where
  curve1d >> DirectionCurve3d curve = DirectionCurve3d (curve1d >> curve)

xComponent :: DirectionCurve3d space -> Curve Unitless
xComponent curve = curve <> Direction3d.x

yComponent :: DirectionCurve3d space -> Curve Unitless
yComponent curve = curve <> Direction3d.y

zComponent :: DirectionCurve3d space -> Curve Unitless
zComponent curve = curve <> Direction3d.z

placeIn ::
  Basis3d global (Defines local) ->
  DirectionCurve3d local ->
  DirectionCurve3d global
placeIn basis (DirectionCurve3d curve) =
  DirectionCurve3d (VectorCurve3d.placeIn basis curve)

relativeTo ::
  Basis3d global (Defines local) ->
  DirectionCurve3d global ->
  DirectionCurve3d local
relativeTo basis = placeIn (Basis3d.inverse basis)
