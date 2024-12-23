module DirectionCurve3d
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
  , placeInBasis
  , relativeTo
  , relativeToBasis
  )
where

import Basis3d (Basis3d)
import Basis3d qualified
import Curve1d (Curve1d)
import Direction3d (Direction3d)
import Direction3d qualified
import DirectionBounds3d (DirectionBounds3d)
import DirectionBounds3d qualified
import Frame3d (Frame3d)
import Frame3d qualified
import OpenSolid
import Range (Range)
import Units qualified
import Vector3d (Vector3d)
import Vector3d qualified
import VectorCurve3d (VectorCurve3d)
import VectorCurve3d qualified

newtype DirectionCurve3d space = DirectionCurve3d (VectorCurve3d (space @ Unitless))
  deriving (Show)

instance HasUnits (DirectionCurve3d space) where
  type UnitsOf (DirectionCurve3d space) = Unitless

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

instance Multiplication Sign (DirectionCurve3d space) (DirectionCurve3d space)

instance Multiplication' Sign (DirectionCurve3d space) where
  type Sign .*. DirectionCurve3d space = DirectionCurve3d space
  Positive .*. curve = curve
  Negative .*. curve = -curve

instance Multiplication (DirectionCurve3d space) Sign (DirectionCurve3d space)

instance Multiplication' (DirectionCurve3d space) Sign where
  type DirectionCurve3d space .*. Sign = DirectionCurve3d space
  curve .*. Positive = curve
  curve .*. Negative = -curve

instance Multiplication (Qty units) (DirectionCurve3d space) (VectorCurve3d (space @ units))

instance Multiplication' (Qty units) (DirectionCurve3d space) where
  type Qty units .*. DirectionCurve3d space = VectorCurve3d (space @ (units :*: Unitless))
  value .*. DirectionCurve3d vectorCurve = value .*. vectorCurve

instance Multiplication (DirectionCurve3d space) (Qty units) (VectorCurve3d (space @ units))

instance Multiplication' (DirectionCurve3d space) (Qty units) where
  type DirectionCurve3d space .*. Qty units = VectorCurve3d (space @ (Unitless :*: units))
  DirectionCurve3d vectorCurve .*. value = vectorCurve .*. value

instance Multiplication (Curve1d units) (DirectionCurve3d space) (VectorCurve3d (space @ units))

instance Multiplication' (Curve1d units) (DirectionCurve3d space) where
  type Curve1d units .*. DirectionCurve3d space = VectorCurve3d (space @ (units :*: Unitless))
  scalarCurve .*. DirectionCurve3d vectorCurve = scalarCurve .*. vectorCurve

instance Multiplication (DirectionCurve3d space) (Curve1d units) (VectorCurve3d (space @ units))

instance Multiplication' (DirectionCurve3d space) (Curve1d units) where
  type DirectionCurve3d space .*. Curve1d units = VectorCurve3d (space @ (Unitless :*: units))
  DirectionCurve3d vectorCurve .*. scalarCurve = vectorCurve .*. scalarCurve

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3d space1) (DirectionCurve3d space2) (Curve1d Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (DirectionCurve3d space1) (DirectionCurve3d space2)
  where
  type DirectionCurve3d space1 .<>. DirectionCurve3d space2 = Curve1d (Unitless :*: Unitless)
  DirectionCurve3d curve1 .<>. DirectionCurve3d curve2 = curve1 .<>. curve2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3d space1) (VectorCurve3d (space2 @ units)) (Curve1d units)

instance
  space1 ~ space2 =>
  DotMultiplication' (DirectionCurve3d space1) (VectorCurve3d (space2 @ units))
  where
  type DirectionCurve3d space1 .<>. VectorCurve3d (space2 @ units) = Curve1d (Unitless :*: units)
  DirectionCurve3d curve1 .<>. curve2 = curve1 .<>. curve2

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve3d (space1 @ units)) (DirectionCurve3d space2) (Curve1d units)

instance
  space1 ~ space2 =>
  DotMultiplication' (VectorCurve3d (space1 @ units)) (DirectionCurve3d space2)
  where
  type VectorCurve3d (space1 @ units) .<>. DirectionCurve3d space2 = Curve1d (units :*: Unitless)
  curve1 .<>. DirectionCurve3d curve2 = curve1 .<>. curve2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3d space1) (Direction3d space2) (Curve1d Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (DirectionCurve3d space1) (Direction3d space2)
  where
  type DirectionCurve3d space1 .<>. Direction3d space2 = Curve1d (Unitless :*: Unitless)
  DirectionCurve3d curve .<>. direction = curve .<>. direction

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3d space1) (DirectionCurve3d space2) (Curve1d Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (Direction3d space1) (DirectionCurve3d space2)
  where
  type Direction3d space1 .<>. DirectionCurve3d space2 = Curve1d (Unitless :*: Unitless)
  direction .<>. DirectionCurve3d curve = direction .<>. curve

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve3d space1) (Vector3d (space2 @ units)) (Curve1d units)

instance
  space1 ~ space2 =>
  DotMultiplication' (DirectionCurve3d space1) (Vector3d (space2 @ units))
  where
  type DirectionCurve3d space1 .<>. Vector3d (space2 @ units) = Curve1d (Unitless :*: units)
  DirectionCurve3d curve .<>. vector = curve .<>. vector

instance
  space1 ~ space2 =>
  DotMultiplication (Vector3d (space1 @ units)) (DirectionCurve3d space2) (Curve1d units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Vector3d (space1 @ units)) (DirectionCurve3d space2)
  where
  type Vector3d (space1 @ units) .<>. DirectionCurve3d space2 = Curve1d (units :*: Unitless)
  vector .<>. DirectionCurve3d curve = vector .<>. curve

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3d space1)
    (DirectionCurve3d space2)
    (VectorCurve3d (space1 @ Unitless))

instance
  space1 ~ space2 =>
  CrossMultiplication' (DirectionCurve3d space1) (DirectionCurve3d space2)
  where
  type
    DirectionCurve3d space1 .><. DirectionCurve3d space2 =
      VectorCurve3d (space1 @ (Unitless :*: Unitless))
  DirectionCurve3d curve1 .><. DirectionCurve3d curve2 = curve1 .><. curve2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3d space1)
    (VectorCurve3d (space2 @ units))
    (VectorCurve3d (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (DirectionCurve3d space1) (VectorCurve3d (space2 @ units))
  where
  type
    DirectionCurve3d space1 .><. VectorCurve3d (space2 @ units) =
      VectorCurve3d (space1 @ (Unitless :*: units))
  DirectionCurve3d curve1 .><. curve2 = curve1 .><. curve2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units))
    (DirectionCurve3d space2)
    (VectorCurve3d (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (VectorCurve3d (space1 @ units)) (DirectionCurve3d space2)
  where
  type
    VectorCurve3d (space1 @ units) .><. DirectionCurve3d space2 =
      VectorCurve3d (space1 @ (units :*: Unitless))
  curve1 .><. DirectionCurve3d curve2 = curve1 .><. curve2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3d space1)
    (Direction3d space2)
    (VectorCurve3d (space1 @ Unitless))

instance
  space1 ~ space2 =>
  CrossMultiplication' (DirectionCurve3d space1) (Direction3d space2)
  where
  type
    DirectionCurve3d space1 .><. Direction3d space2 =
      VectorCurve3d (space1 @ (Unitless :*: Unitless))
  DirectionCurve3d curve .><. direction = curve .><. direction

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (DirectionCurve3d space2)
    (VectorCurve3d (space1 @ Unitless))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction3d space1) (DirectionCurve3d space2)
  where
  type
    Direction3d space1 .><. DirectionCurve3d space2 =
      VectorCurve3d (space1 @ (Unitless :*: Unitless))
  direction .><. DirectionCurve3d curve = direction .><. curve

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionCurve3d space1)
    (Vector3d (space2 @ units))
    (VectorCurve3d (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (DirectionCurve3d space1) (Vector3d (space2 @ units))
  where
  type
    DirectionCurve3d space1 .><. Vector3d (space2 @ units) =
      VectorCurve3d (space1 @ (Unitless :*: units))
  DirectionCurve3d curve .><. vector = curve .><. vector

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Vector3d (space1 @ units))
    (DirectionCurve3d space2)
    (VectorCurve3d (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Vector3d (space1 @ units)) (DirectionCurve3d space2)
  where
  type
    Vector3d (space1 @ units) .><. DirectionCurve3d space2 =
      VectorCurve3d (space1 @ (units :*: Unitless))
  vector .><. DirectionCurve3d curve = vector .><. curve

instance Composition (Curve1d Unitless) (DirectionCurve3d space) (DirectionCurve3d space) where
  curve1d >> DirectionCurve3d curve = DirectionCurve3d (curve1d >> curve)

xComponent :: DirectionCurve3d space -> Curve1d Unitless
xComponent curve = curve <> Direction3d.x

yComponent :: DirectionCurve3d space -> Curve1d Unitless
yComponent curve = curve <> Direction3d.y

zComponent :: DirectionCurve3d space -> Curve1d Unitless
zComponent curve = curve <> Direction3d.z

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  DirectionCurve3d local ->
  DirectionCurve3d global
placeIn frame = placeInBasis (Frame3d.basis frame)

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  DirectionCurve3d global ->
  DirectionCurve3d local
relativeTo frame = relativeToBasis (Frame3d.basis frame)

placeInBasis ::
  Basis3d global (Defines local) ->
  DirectionCurve3d local ->
  DirectionCurve3d global
placeInBasis basis (DirectionCurve3d curve) =
  DirectionCurve3d (VectorCurve3d.placeInBasis basis curve)

relativeToBasis ::
  Basis3d global (Defines local) ->
  DirectionCurve3d global ->
  DirectionCurve3d local
relativeToBasis basis = placeInBasis (Basis3d.inverse basis)
