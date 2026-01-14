module OpenSolid.DirectionBounds2D
  ( DirectionBounds2D (DirectionBounds2D)
  , unsafe
  , unwrap
  , constant
  , xComponent
  , yComponent
  , relativeTo
  , relativeToOrientation
  , placeIn
  , placeInOrientation
  , placeOn
  , placeOnOrientation
  )
where

import OpenSolid.Direction2D (Direction2D)
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.DirectionBounds3D qualified as DirectionBounds3D
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Interval (Interval)
import OpenSolid.Orientation2D (Orientation2D)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.PlaneOrientation3D (PlaneOrientation3D)
import OpenSolid.Prelude
import OpenSolid.Primitives qualified as Primitives
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorBounds2D (VectorBounds2D (VectorBounds2D))
import OpenSolid.VectorBounds2D qualified as VectorBounds2D

newtype DirectionBounds2D space
  = UnitBounds2D (VectorBounds2D Unitless space)
  deriving (Show)

instance
  space1 ~ space2 =>
  Units.Coercion (DirectionBounds2D space1) (DirectionBounds2D space2)
  where
  coerce = id

{-# COMPLETE DirectionBounds2D #-}

pattern DirectionBounds2D :: Interval Unitless -> Interval Unitless -> DirectionBounds2D space
pattern DirectionBounds2D dx dy <- UnitBounds2D (VectorBounds2D dx dy)

instance Negation (DirectionBounds2D space) where
  negative (UnitBounds2D vectorBounds) = UnitBounds2D (negative vectorBounds)

instance Multiplication Sign (DirectionBounds2D space) (DirectionBounds2D space) where
  Positive .*. directionBounds = directionBounds
  Negative .*. directionBounds = negative directionBounds

instance Multiplication_ Sign (DirectionBounds2D space) (DirectionBounds2D space) where
  Positive ?*? directionBounds = directionBounds
  Negative ?*? directionBounds = negative directionBounds

instance Multiplication (DirectionBounds2D space) Sign (DirectionBounds2D space) where
  directionBounds .*. Positive = directionBounds
  directionBounds .*. Negative = negative directionBounds

instance Multiplication_ (DirectionBounds2D space) Sign (DirectionBounds2D space) where
  directionBounds ?*? Positive = directionBounds
  directionBounds ?*? Negative = negative directionBounds

instance
  Multiplication
    (Quantity units)
    (DirectionBounds2D space)
    (VectorBounds2D units space)
  where
  value .*. UnitBounds2D vectorBounds = value .*. vectorBounds

instance
  Multiplication
    (DirectionBounds2D space)
    (Quantity units)
    (VectorBounds2D units space)
  where
  UnitBounds2D vectorBounds .*. value = vectorBounds .*. value

instance
  Multiplication
    (Interval units)
    (DirectionBounds2D space)
    (VectorBounds2D units space)
  where
  interval .*. UnitBounds2D vectorBounds = interval .*. vectorBounds

instance
  Multiplication
    (DirectionBounds2D space)
    (Interval units)
    (VectorBounds2D units space)
  where
  UnitBounds2D vectorBounds .*. interval = vectorBounds .*. interval

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2D space1) (DirectionBounds2D space2) (Interval Unitless)
  where
  UnitBounds2D vectorBounds1 `dot` UnitBounds2D vectorBounds2 =
    vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2D space1) (VectorBounds2D units space2) (Interval units)
  where
  UnitBounds2D vectorBounds1 `dot` vectorBounds2 = vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (VectorBounds2D units space1) (DirectionBounds2D space2) (Interval units)
  where
  vectorBounds1 `dot` UnitBounds2D vectorBounds2 = vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2D space1) (Direction2D space2) (Interval Unitless)
  where
  UnitBounds2D vectorBounds `dot` direction = vectorBounds `dot` direction

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2D space1) (DirectionBounds2D space2) (Interval Unitless)
  where
  direction `dot` UnitBounds2D vectorBounds = direction `dot` vectorBounds

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2D space1) (Vector2D units space2) (Interval units)
  where
  UnitBounds2D vectorBounds `dot` vector = vectorBounds `dot` vector

instance
  space1 ~ space2 =>
  DotMultiplication (Vector2D units space1) (DirectionBounds2D space2) (Interval units)
  where
  vector `dot` UnitBounds2D vectorBounds = vector `dot` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2D space1) (DirectionBounds2D space2) (Interval Unitless)
  where
  UnitBounds2D vectorBounds1 `cross` UnitBounds2D vectorBounds2 =
    vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2D space1) (VectorBounds2D units space2) (Interval units)
  where
  UnitBounds2D vectorBounds1 `cross` vectorBounds2 = vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorBounds2D units space1) (DirectionBounds2D space2) (Interval units)
  where
  vectorBounds1 `cross` UnitBounds2D vectorBounds2 = vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2D space1) (Direction2D space2) (Interval Unitless)
  where
  UnitBounds2D vectorBounds `cross` direction = vectorBounds `cross` direction

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2D space1) (DirectionBounds2D space2) (Interval Unitless)
  where
  direction `cross` UnitBounds2D vectorBounds = direction `cross` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2D space1) (Vector2D units space2) (Interval units)
  where
  UnitBounds2D vectorBounds `cross` vector = vectorBounds `cross` vector

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector2D units space1) (DirectionBounds2D space2) (Interval units)
  where
  vector `cross` UnitBounds2D vectorBounds = vector `cross` vectorBounds

unsafe :: VectorBounds2D Unitless space -> DirectionBounds2D space
unsafe = UnitBounds2D

unwrap :: DirectionBounds2D space -> VectorBounds2D Unitless space
unwrap (UnitBounds2D vectorBounds) = vectorBounds

{-# INLINE lift #-}
lift ::
  (VectorBounds2D Unitless space1 -> VectorBounds2D Unitless space2) ->
  DirectionBounds2D space1 ->
  DirectionBounds2D space2
lift function (UnitBounds2D vectorBounds) = UnitBounds2D (function vectorBounds)

constant :: Direction2D space -> DirectionBounds2D space
constant direction = UnitBounds2D (VectorBounds2D.constant (Vector2D.unit direction))

xComponent :: DirectionBounds2D space -> Interval Unitless
xComponent (UnitBounds2D vectorBounds) = VectorBounds2D.xComponent vectorBounds

yComponent :: DirectionBounds2D space -> Interval Unitless
yComponent (UnitBounds2D vectorBounds) = VectorBounds2D.yComponent vectorBounds

placeIn ::
  Frame2D frameUnits global local ->
  DirectionBounds2D local ->
  DirectionBounds2D global
placeIn frame = placeInOrientation frame.orientation

placeInOrientation :: Orientation2D global -> DirectionBounds2D local -> DirectionBounds2D global
placeInOrientation orientation = lift (VectorBounds2D.placeInOrientation orientation)

relativeTo ::
  Frame2D frameUnits global local ->
  DirectionBounds2D global ->
  DirectionBounds2D local
relativeTo frame = relativeToOrientation frame.orientation

relativeToOrientation :: Orientation2D global -> DirectionBounds2D global -> DirectionBounds2D local
relativeToOrientation orientation = lift (VectorBounds2D.relativeToOrientation orientation)

{-| Convert a 2D direction to 3D direction by placing it on a plane.

Given a 2D direction defined within a plane's coordinate system,
this returns the corresponding 3D direction.
-}
placeOn ::
  Plane3D global local ->
  DirectionBounds2D local ->
  DirectionBounds3D global
placeOn plane = placeOnOrientation plane.orientation

placeOnOrientation ::
  PlaneOrientation3D global ->
  DirectionBounds2D local ->
  DirectionBounds3D global
placeOnOrientation orientation (UnitBounds2D vector) =
  DirectionBounds3D.unsafe (VectorBounds2D.placeOnOrientation orientation vector)
