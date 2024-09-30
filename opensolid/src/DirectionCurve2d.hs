module DirectionCurve2d
  ( DirectionCurve2d
  , unsafe
  , unwrap
  , evaluateAt
  , segmentBounds
  , derivative
  , constant
  , arc
  , reverse
  , xComponent
  , yComponent
  , placeIn
  , placeInBasis
  , relativeTo
  , relativeToBasis
  )
where

import Basis2d (Basis2d)
import Basis2d qualified
import Curve1d (Curve1d)
import Direction2d (Direction2d)
import Direction2d qualified
import DirectionBounds2d (DirectionBounds2d)
import DirectionBounds2d qualified
import Frame2d (Frame2d)
import Frame2d qualified
import OpenSolid
import Range (Range)
import Units qualified
import Vector2d (Vector2d)
import Vector2d qualified
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

newtype DirectionCurve2d space = DirectionCurve2d (VectorCurve2d (space @ Unitless))
  deriving (Show)

instance HasUnits (DirectionCurve2d space) where
  type UnitsOf (DirectionCurve2d space) = Unitless

instance
  space1 ~ space2 =>
  Units.Coercion (DirectionCurve2d space1) (DirectionCurve2d space2)
  where
  coerce = identity

unsafe :: VectorCurve2d (space @ Unitless) -> DirectionCurve2d space
unsafe = DirectionCurve2d

unwrap :: DirectionCurve2d space -> VectorCurve2d (space @ Unitless)
unwrap (DirectionCurve2d vectorCurve) = vectorCurve

evaluateAt :: Float -> DirectionCurve2d space -> Direction2d space
evaluateAt t (DirectionCurve2d vectorCurve) =
  Direction2d.unsafe (VectorCurve2d.evaluateAt t vectorCurve)

segmentBounds :: Range Unitless -> DirectionCurve2d space -> DirectionBounds2d space
segmentBounds t (DirectionCurve2d vectorCurve) =
  DirectionBounds2d.unsafe (VectorCurve2d.segmentBounds t vectorCurve)

derivative :: DirectionCurve2d space -> VectorCurve2d (space @ Unitless)
derivative (DirectionCurve2d vectorCurve) = VectorCurve2d.derivative vectorCurve

constant :: Direction2d space -> DirectionCurve2d space
constant direction = DirectionCurve2d (VectorCurve2d.constant (Vector2d.unit direction))

arc :: Angle -> Angle -> DirectionCurve2d space
arc a b = DirectionCurve2d (VectorCurve2d.arc (Vector2d.xy 1.0 0.0) (Vector2d.xy 0.0 1.0) a b)

reverse :: DirectionCurve2d space -> DirectionCurve2d space
reverse (DirectionCurve2d vectorCurve) = DirectionCurve2d (VectorCurve2d.reverse vectorCurve)

instance Negation (DirectionCurve2d space) where
  negate (DirectionCurve2d vectorCurve) = DirectionCurve2d (negate vectorCurve)

instance Multiplication Sign (DirectionCurve2d space) (DirectionCurve2d space)

instance Multiplication' Sign (DirectionCurve2d space) where
  type Sign .*. DirectionCurve2d space = DirectionCurve2d space
  Positive .*. curve = curve
  Negative .*. curve = -curve

instance Multiplication (DirectionCurve2d space) Sign (DirectionCurve2d space)

instance Multiplication' (DirectionCurve2d space) Sign where
  type DirectionCurve2d space .*. Sign = DirectionCurve2d space
  curve .*. Positive = curve
  curve .*. Negative = -curve

instance Multiplication (Qty units) (DirectionCurve2d space) (VectorCurve2d (space @ units))

instance Multiplication' (Qty units) (DirectionCurve2d space) where
  type Qty units .*. DirectionCurve2d space = VectorCurve2d (space @ (units :*: Unitless))
  value .*. DirectionCurve2d vectorCurve = value .*. vectorCurve

instance Multiplication (DirectionCurve2d space) (Qty units) (VectorCurve2d (space @ units))

instance Multiplication' (DirectionCurve2d space) (Qty units) where
  type DirectionCurve2d space .*. Qty units = VectorCurve2d (space @ (Unitless :*: units))
  DirectionCurve2d vectorCurve .*. value = vectorCurve .*. value

instance Multiplication (Curve1d units) (DirectionCurve2d space) (VectorCurve2d (space @ units))

instance Multiplication' (Curve1d units) (DirectionCurve2d space) where
  type Curve1d units .*. DirectionCurve2d space = VectorCurve2d (space @ (units :*: Unitless))
  scalarCurve .*. DirectionCurve2d vectorCurve = scalarCurve .*. vectorCurve

instance Multiplication (DirectionCurve2d space) (Curve1d units) (VectorCurve2d (space @ units))

instance Multiplication' (DirectionCurve2d space) (Curve1d units) where
  type DirectionCurve2d space .*. Curve1d units = VectorCurve2d (space @ (Unitless :*: units))
  DirectionCurve2d vectorCurve .*. scalarCurve = vectorCurve .*. scalarCurve

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve2d space1) (DirectionCurve2d space2) (Curve1d Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (DirectionCurve2d space1) (DirectionCurve2d space2)
  where
  type DirectionCurve2d space1 .<>. DirectionCurve2d space2 = Curve1d (Unitless :*: Unitless)
  DirectionCurve2d curve1 .<>. DirectionCurve2d curve2 = curve1 .<>. curve2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve2d space1) (VectorCurve2d (space2 @ units)) (Curve1d units)

instance
  space1 ~ space2 =>
  DotMultiplication' (DirectionCurve2d space1) (VectorCurve2d (space2 @ units))
  where
  type DirectionCurve2d space1 .<>. VectorCurve2d (space2 @ units) = Curve1d (Unitless :*: units)
  DirectionCurve2d curve1 .<>. curve2 = curve1 .<>. curve2

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve2d (space1 @ units)) (DirectionCurve2d space2) (Curve1d units)

instance
  space1 ~ space2 =>
  DotMultiplication' (VectorCurve2d (space1 @ units)) (DirectionCurve2d space2)
  where
  type VectorCurve2d (space1 @ units) .<>. DirectionCurve2d space2 = Curve1d (units :*: Unitless)
  curve1 .<>. DirectionCurve2d curve2 = curve1 .<>. curve2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve2d space1) (Direction2d space2) (Curve1d Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (DirectionCurve2d space1) (Direction2d space2)
  where
  type DirectionCurve2d space1 .<>. Direction2d space2 = Curve1d (Unitless :*: Unitless)
  DirectionCurve2d curve .<>. direction = curve .<>. direction

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (DirectionCurve2d space2) (Curve1d Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (Direction2d space1) (DirectionCurve2d space2)
  where
  type Direction2d space1 .<>. DirectionCurve2d space2 = Curve1d (Unitless :*: Unitless)
  direction .<>. DirectionCurve2d curve = direction .<>. curve

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve2d space1) (Vector2d (space2 @ units)) (Curve1d units)

instance
  space1 ~ space2 =>
  DotMultiplication' (DirectionCurve2d space1) (Vector2d (space2 @ units))
  where
  type DirectionCurve2d space1 .<>. Vector2d (space2 @ units) = Curve1d (Unitless :*: units)
  DirectionCurve2d curve .<>. vector = curve .<>. vector

instance
  space1 ~ space2 =>
  DotMultiplication (Vector2d (space1 @ units)) (DirectionCurve2d space2) (Curve1d units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Vector2d (space1 @ units)) (DirectionCurve2d space2)
  where
  type Vector2d (space1 @ units) .<>. DirectionCurve2d space2 = Curve1d (units :*: Unitless)
  vector .<>. DirectionCurve2d curve = vector .<>. curve

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionCurve2d space1) (DirectionCurve2d space2) (Curve1d Unitless)

instance
  space1 ~ space2 =>
  CrossMultiplication' (DirectionCurve2d space1) (DirectionCurve2d space2)
  where
  type DirectionCurve2d space1 .><. DirectionCurve2d space2 = Curve1d (Unitless :*: Unitless)
  DirectionCurve2d curve1 .><. DirectionCurve2d curve2 = curve1 .><. curve2

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionCurve2d space1) (VectorCurve2d (space2 @ units)) (Curve1d units)

instance
  space1 ~ space2 =>
  CrossMultiplication' (DirectionCurve2d space1) (VectorCurve2d (space2 @ units))
  where
  type DirectionCurve2d space1 .><. VectorCurve2d (space2 @ units) = Curve1d (Unitless :*: units)
  DirectionCurve2d curve1 .><. curve2 = curve1 .><. curve2

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorCurve2d (space1 @ units)) (DirectionCurve2d space2) (Curve1d units)

instance
  space1 ~ space2 =>
  CrossMultiplication' (VectorCurve2d (space1 @ units)) (DirectionCurve2d space2)
  where
  type VectorCurve2d (space1 @ units) .><. DirectionCurve2d space2 = Curve1d (units :*: Unitless)
  curve1 .><. DirectionCurve2d curve2 = curve1 .><. curve2

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionCurve2d space1) (Direction2d space2) (Curve1d Unitless)

instance
  space1 ~ space2 =>
  CrossMultiplication' (DirectionCurve2d space1) (Direction2d space2)
  where
  type DirectionCurve2d space1 .><. Direction2d space2 = Curve1d (Unitless :*: Unitless)
  DirectionCurve2d curve .><. direction = curve .><. direction

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (DirectionCurve2d space2) (Curve1d Unitless)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction2d space1) (DirectionCurve2d space2)
  where
  type Direction2d space1 .><. DirectionCurve2d space2 = Curve1d (Unitless :*: Unitless)
  direction .><. DirectionCurve2d curve = direction .><. curve

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionCurve2d space1) (Vector2d (space2 @ units)) (Curve1d units)

instance
  space1 ~ space2 =>
  CrossMultiplication' (DirectionCurve2d space1) (Vector2d (space2 @ units))
  where
  type DirectionCurve2d space1 .><. Vector2d (space2 @ units) = Curve1d (Unitless :*: units)
  DirectionCurve2d curve .><. vector = curve .><. vector

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector2d (space1 @ units)) (DirectionCurve2d space2) (Curve1d units)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Vector2d (space1 @ units)) (DirectionCurve2d space2)
  where
  type Vector2d (space1 @ units) .><. DirectionCurve2d space2 = Curve1d (units :*: Unitless)
  vector .><. DirectionCurve2d curve = vector .><. curve

instance Composition (Curve1d Unitless) (DirectionCurve2d space) (DirectionCurve2d space) where
  curve1d >> DirectionCurve2d curve = DirectionCurve2d (curve1d >> curve)

xComponent :: DirectionCurve2d space -> Curve1d Unitless
xComponent curve = curve <> Direction2d.x

yComponent :: DirectionCurve2d space -> Curve1d Unitless
yComponent curve = curve <> Direction2d.y

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  DirectionCurve2d local ->
  DirectionCurve2d global
placeIn frame = placeInBasis (Frame2d.basis frame)

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  DirectionCurve2d global ->
  DirectionCurve2d local
relativeTo frame = relativeToBasis (Frame2d.basis frame)

placeInBasis ::
  Basis2d global (Defines local) ->
  DirectionCurve2d local ->
  DirectionCurve2d global
placeInBasis basis (DirectionCurve2d curve) = DirectionCurve2d (VectorCurve2d.placeInBasis basis curve)

relativeToBasis ::
  Basis2d global (Defines local) ->
  DirectionCurve2d global ->
  DirectionCurve2d local
relativeToBasis basis = placeInBasis (Basis2d.inverse basis)
