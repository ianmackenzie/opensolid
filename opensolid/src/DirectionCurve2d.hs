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
import Direction2d (Direction2d (Direction2d))
import Direction2d qualified
import DirectionBounds2d (DirectionBounds2d)
import DirectionBounds2d qualified
import Frame2d (Frame2d)
import Frame2d qualified
import OpenSolid
import T qualified
import Units qualified
import Vector2d (Vector2d)
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

newtype DirectionCurve2d space = DirectionCurve2d (VectorCurve2d (space @ Unitless))
  deriving (Show)

type instance Units (DirectionCurve2d space) = Unitless

instance space ~ space' => Units.Coercion (DirectionCurve2d space) (DirectionCurve2d space') where
  coerce = identity

unsafe :: VectorCurve2d (space @ Unitless) -> DirectionCurve2d space
unsafe = DirectionCurve2d

unwrap :: DirectionCurve2d space -> VectorCurve2d (space @ Unitless)
unwrap (DirectionCurve2d vectorCurve) = vectorCurve

evaluateAt :: Float -> DirectionCurve2d space -> Direction2d space
evaluateAt t (DirectionCurve2d vectorCurve) =
  Direction2d.unsafe (VectorCurve2d.evaluateAt t vectorCurve)

segmentBounds :: T.Bounds -> DirectionCurve2d space -> DirectionBounds2d space
segmentBounds t (DirectionCurve2d vectorCurve) =
  DirectionBounds2d.unsafe (VectorCurve2d.segmentBounds t vectorCurve)

derivative :: DirectionCurve2d space -> VectorCurve2d (space @ Unitless)
derivative (DirectionCurve2d vectorCurve) = VectorCurve2d.derivative vectorCurve

constant :: Direction2d space -> DirectionCurve2d space
constant (Direction2d vector) = DirectionCurve2d (VectorCurve2d.constant vector)

arc :: Angle -> Angle -> DirectionCurve2d space
arc a b = DirectionCurve2d (VectorCurve2d.arc 1.0 a b)

reverse :: DirectionCurve2d space -> DirectionCurve2d space
reverse (DirectionCurve2d vectorCurve) = DirectionCurve2d (VectorCurve2d.reverse vectorCurve)

instance Negation (DirectionCurve2d space) where
  negate (DirectionCurve2d vectorCurve) = DirectionCurve2d (negate vectorCurve)

instance Product Sign (DirectionCurve2d space) (DirectionCurve2d space)

instance Multiplication Sign (DirectionCurve2d space) where
  type Sign .*. DirectionCurve2d space = DirectionCurve2d space
  Positive .*. curve = curve
  Negative .*. curve = -curve

instance Product (DirectionCurve2d space) Sign (DirectionCurve2d space)

instance Multiplication (DirectionCurve2d space) Sign where
  type DirectionCurve2d space .*. Sign = DirectionCurve2d space
  curve .*. Positive = curve
  curve .*. Negative = -curve

instance Product (Qty units) (DirectionCurve2d space) (VectorCurve2d (space @ units))

instance Multiplication (Qty units) (DirectionCurve2d space) where
  type Qty units .*. DirectionCurve2d space = VectorCurve2d (space @ (units :*: Unitless))
  value .*. DirectionCurve2d vectorCurve = value .*. vectorCurve

instance Product (DirectionCurve2d space) (Qty units) (VectorCurve2d (space @ units))

instance Multiplication (DirectionCurve2d space) (Qty units) where
  type DirectionCurve2d space .*. Qty units = VectorCurve2d (space @ (Unitless :*: units))
  DirectionCurve2d vectorCurve .*. value = vectorCurve .*. value

instance Product (Curve1d units) (DirectionCurve2d space) (VectorCurve2d (space @ units))

instance Multiplication (Curve1d units) (DirectionCurve2d space) where
  type Curve1d units .*. DirectionCurve2d space = VectorCurve2d (space @ (units :*: Unitless))
  scalarCurve .*. DirectionCurve2d vectorCurve = scalarCurve .*. vectorCurve

instance Product (DirectionCurve2d space) (Curve1d units) (VectorCurve2d (space @ units))

instance Multiplication (DirectionCurve2d space) (Curve1d units) where
  type DirectionCurve2d space .*. Curve1d units = VectorCurve2d (space @ (Unitless :*: units))
  DirectionCurve2d vectorCurve .*. scalarCurve = vectorCurve .*. scalarCurve

instance
  space ~ space' =>
  DotProduct (DirectionCurve2d space) (DirectionCurve2d space') (Curve1d Unitless)

instance
  space ~ space' =>
  DotMultiplication (DirectionCurve2d space) (DirectionCurve2d space')
  where
  type DirectionCurve2d space .<>. DirectionCurve2d space' = Curve1d (Unitless :*: Unitless)
  DirectionCurve2d curve1 .<>. DirectionCurve2d curve2 = curve1 .<>. curve2

instance
  space ~ space' =>
  DotProduct (DirectionCurve2d space) (VectorCurve2d (space' @ units)) (Curve1d units)

instance
  space ~ space' =>
  DotMultiplication (DirectionCurve2d space) (VectorCurve2d (space' @ units))
  where
  type DirectionCurve2d space .<>. VectorCurve2d (space' @ units) = Curve1d (Unitless :*: units)
  DirectionCurve2d curve1 .<>. curve2 = curve1 .<>. curve2

instance
  space ~ space' =>
  DotProduct (VectorCurve2d (space @ units)) (DirectionCurve2d space') (Curve1d units)

instance
  space ~ space' =>
  DotMultiplication (VectorCurve2d (space @ units)) (DirectionCurve2d space')
  where
  type VectorCurve2d (space @ units) .<>. DirectionCurve2d space' = Curve1d (units :*: Unitless)
  curve1 .<>. DirectionCurve2d curve2 = curve1 .<>. curve2

instance
  space ~ space' =>
  DotProduct (DirectionCurve2d space) (Direction2d space') (Curve1d Unitless)

instance
  space ~ space' =>
  DotMultiplication (DirectionCurve2d space) (Direction2d space')
  where
  type DirectionCurve2d space .<>. Direction2d space' = Curve1d (Unitless :*: Unitless)
  DirectionCurve2d curve .<>. direction = curve .<>. direction

instance
  space ~ space' =>
  DotProduct (Direction2d space) (DirectionCurve2d space') (Curve1d Unitless)

instance
  space ~ space' =>
  DotMultiplication (Direction2d space) (DirectionCurve2d space')
  where
  type Direction2d space .<>. DirectionCurve2d space' = Curve1d (Unitless :*: Unitless)
  direction .<>. DirectionCurve2d curve = direction .<>. curve

instance
  space ~ space' =>
  DotProduct (DirectionCurve2d space) (Vector2d (space' @ units)) (Curve1d units)

instance
  space ~ space' =>
  DotMultiplication (DirectionCurve2d space) (Vector2d (space' @ units))
  where
  type DirectionCurve2d space .<>. Vector2d (space' @ units) = Curve1d (Unitless :*: units)
  DirectionCurve2d curve .<>. vector = curve .<>. vector

instance
  space ~ space' =>
  DotProduct (Vector2d (space @ units)) (DirectionCurve2d space') (Curve1d units)

instance
  space ~ space' =>
  DotMultiplication (Vector2d (space @ units)) (DirectionCurve2d space')
  where
  type Vector2d (space @ units) .<>. DirectionCurve2d space' = Curve1d (units :*: Unitless)
  vector .<>. DirectionCurve2d curve = vector .<>. curve

instance
  space ~ space' =>
  CrossProduct (DirectionCurve2d space) (DirectionCurve2d space') (Curve1d Unitless)

instance
  space ~ space' =>
  CrossMultiplication (DirectionCurve2d space) (DirectionCurve2d space')
  where
  type DirectionCurve2d space .><. DirectionCurve2d space' = Curve1d (Unitless :*: Unitless)
  DirectionCurve2d curve1 .><. DirectionCurve2d curve2 = curve1 .><. curve2

instance
  space ~ space' =>
  CrossProduct (DirectionCurve2d space) (VectorCurve2d (space' @ units)) (Curve1d units)

instance
  space ~ space' =>
  CrossMultiplication (DirectionCurve2d space) (VectorCurve2d (space' @ units))
  where
  type DirectionCurve2d space .><. VectorCurve2d (space' @ units) = Curve1d (Unitless :*: units)
  DirectionCurve2d curve1 .><. curve2 = curve1 .><. curve2

instance
  space ~ space' =>
  CrossProduct (VectorCurve2d (space @ units)) (DirectionCurve2d space') (Curve1d units)

instance
  space ~ space' =>
  CrossMultiplication (VectorCurve2d (space @ units)) (DirectionCurve2d space')
  where
  type VectorCurve2d (space @ units) .><. DirectionCurve2d space' = Curve1d (units :*: Unitless)
  curve1 .><. DirectionCurve2d curve2 = curve1 .><. curve2

instance
  space ~ space' =>
  CrossProduct (DirectionCurve2d space) (Direction2d space') (Curve1d Unitless)

instance
  space ~ space' =>
  CrossMultiplication (DirectionCurve2d space) (Direction2d space')
  where
  type DirectionCurve2d space .><. Direction2d space' = Curve1d (Unitless :*: Unitless)
  DirectionCurve2d curve .><. direction = curve .><. direction

instance
  space ~ space' =>
  CrossProduct (Direction2d space) (DirectionCurve2d space') (Curve1d Unitless)

instance
  space ~ space' =>
  CrossMultiplication (Direction2d space) (DirectionCurve2d space')
  where
  type Direction2d space .><. DirectionCurve2d space' = Curve1d (Unitless :*: Unitless)
  direction .><. DirectionCurve2d curve = direction .><. curve

instance
  space ~ space' =>
  CrossProduct (DirectionCurve2d space) (Vector2d (space' @ units)) (Curve1d units)

instance
  space ~ space' =>
  CrossMultiplication (DirectionCurve2d space) (Vector2d (space' @ units))
  where
  type DirectionCurve2d space .><. Vector2d (space' @ units) = Curve1d (Unitless :*: units)
  DirectionCurve2d curve .><. vector = curve .><. vector

instance
  space ~ space' =>
  CrossProduct (Vector2d (space @ units)) (DirectionCurve2d space') (Curve1d units)

instance
  space ~ space' =>
  CrossMultiplication (Vector2d (space @ units)) (DirectionCurve2d space')
  where
  type Vector2d (space @ units) .><. DirectionCurve2d space' = Curve1d (units :*: Unitless)
  vector .><. DirectionCurve2d curve = vector .><. curve

xComponent :: DirectionCurve2d space -> Curve1d Unitless
xComponent curve = curve <> Direction2d.x

yComponent :: DirectionCurve2d space -> Curve1d Unitless
yComponent curve = curve <> Direction2d.y

placeIn :: Frame2d (global @ units) (Defines local) -> DirectionCurve2d local -> DirectionCurve2d global
placeIn frame = placeInBasis (Frame2d.basis frame)

relativeTo :: Frame2d (global @ units) (Defines local) -> DirectionCurve2d global -> DirectionCurve2d local
relativeTo frame = relativeToBasis (Frame2d.basis frame)

placeInBasis :: Basis2d global (Defines local) -> DirectionCurve2d local -> DirectionCurve2d global
placeInBasis basis (DirectionCurve2d curve) = DirectionCurve2d (VectorCurve2d.placeInBasis basis curve)

relativeToBasis :: Basis2d global (Defines local) -> DirectionCurve2d global -> DirectionCurve2d local
relativeToBasis basis = placeInBasis (Basis2d.inverse basis)
