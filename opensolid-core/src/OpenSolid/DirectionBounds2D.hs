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
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorBounds2D (VectorBounds2D (VectorBounds2D))
import OpenSolid.VectorBounds2D qualified as VectorBounds2D

newtype DirectionBounds2D = UnitBounds2D (VectorBounds2D Unitless) deriving (Show)

{-# COMPLETE DirectionBounds2D #-}

pattern DirectionBounds2D :: Interval Unitless -> Interval Unitless -> DirectionBounds2D
pattern DirectionBounds2D dx dy <- UnitBounds2D (VectorBounds2D dx dy)

instance Negation DirectionBounds2D where
  negate (UnitBounds2D vectorBounds) = UnitBounds2D (negate vectorBounds)

instance Multiplication Sign DirectionBounds2D DirectionBounds2D where
  Positive * directionBounds = directionBounds
  Negative * directionBounds = -directionBounds

instance Multiplication_ Sign DirectionBounds2D DirectionBounds2D where
  Positive ?*? directionBounds = directionBounds
  Negative ?*? directionBounds = -directionBounds

instance Multiplication DirectionBounds2D Sign DirectionBounds2D where
  directionBounds * Positive = directionBounds
  directionBounds * Negative = -directionBounds

instance Multiplication_ DirectionBounds2D Sign DirectionBounds2D where
  directionBounds ?*? Positive = directionBounds
  directionBounds ?*? Negative = -directionBounds

instance Multiplication (Quantity units) DirectionBounds2D (VectorBounds2D units) where
  value * UnitBounds2D vectorBounds = value * vectorBounds

instance Multiplication DirectionBounds2D (Quantity units) (VectorBounds2D units) where
  UnitBounds2D vectorBounds * value = vectorBounds * value

instance Multiplication (Interval units) DirectionBounds2D (VectorBounds2D units) where
  interval * UnitBounds2D vectorBounds = interval * vectorBounds

instance Multiplication DirectionBounds2D (Interval units) (VectorBounds2D units) where
  UnitBounds2D vectorBounds * interval = vectorBounds * interval

instance DotMultiplication DirectionBounds2D DirectionBounds2D (Interval Unitless) where
  UnitBounds2D vectorBounds1 `dot` UnitBounds2D vectorBounds2 =
    vectorBounds1 `dot` vectorBounds2

instance DotMultiplication DirectionBounds2D (VectorBounds2D units) (Interval units) where
  UnitBounds2D vectorBounds1 `dot` vectorBounds2 = vectorBounds1 `dot` vectorBounds2

instance DotMultiplication (VectorBounds2D units) DirectionBounds2D (Interval units) where
  vectorBounds1 `dot` UnitBounds2D vectorBounds2 = vectorBounds1 `dot` vectorBounds2

instance DotMultiplication DirectionBounds2D Direction2D (Interval Unitless) where
  UnitBounds2D vectorBounds `dot` direction = vectorBounds `dot` direction

instance DotMultiplication Direction2D DirectionBounds2D (Interval Unitless) where
  direction `dot` UnitBounds2D vectorBounds = direction `dot` vectorBounds

instance DotMultiplication DirectionBounds2D (Vector2D units) (Interval units) where
  UnitBounds2D vectorBounds `dot` vector = vectorBounds `dot` vector

instance DotMultiplication (Vector2D units) DirectionBounds2D (Interval units) where
  vector `dot` UnitBounds2D vectorBounds = vector `dot` vectorBounds

instance CrossMultiplication DirectionBounds2D DirectionBounds2D (Interval Unitless) where
  UnitBounds2D vectorBounds1 `cross` UnitBounds2D vectorBounds2 =
    vectorBounds1 `cross` vectorBounds2

instance CrossMultiplication DirectionBounds2D (VectorBounds2D units) (Interval units) where
  UnitBounds2D vectorBounds1 `cross` vectorBounds2 = vectorBounds1 `cross` vectorBounds2

instance CrossMultiplication (VectorBounds2D units) DirectionBounds2D (Interval units) where
  vectorBounds1 `cross` UnitBounds2D vectorBounds2 = vectorBounds1 `cross` vectorBounds2

instance CrossMultiplication DirectionBounds2D Direction2D (Interval Unitless) where
  UnitBounds2D vectorBounds `cross` direction = vectorBounds `cross` direction

instance CrossMultiplication Direction2D DirectionBounds2D (Interval Unitless) where
  direction `cross` UnitBounds2D vectorBounds = direction `cross` vectorBounds

instance CrossMultiplication DirectionBounds2D (Vector2D units) (Interval units) where
  UnitBounds2D vectorBounds `cross` vector = vectorBounds `cross` vector

instance CrossMultiplication (Vector2D units) DirectionBounds2D (Interval units) where
  vector `cross` UnitBounds2D vectorBounds = vector `cross` vectorBounds

unsafe :: VectorBounds2D Unitless -> DirectionBounds2D
unsafe = UnitBounds2D

unwrap :: DirectionBounds2D -> VectorBounds2D Unitless
unwrap (UnitBounds2D vectorBounds) = vectorBounds

{-# INLINE lift #-}
lift ::
  (VectorBounds2D Unitless -> VectorBounds2D Unitless) ->
  DirectionBounds2D ->
  DirectionBounds2D
lift function (UnitBounds2D vectorBounds) = UnitBounds2D (function vectorBounds)

constant :: Direction2D -> DirectionBounds2D
constant direction = UnitBounds2D (VectorBounds2D.constant (Vector2D.unit direction))

xComponent :: DirectionBounds2D -> Interval Unitless
xComponent (UnitBounds2D vectorBounds) = VectorBounds2D.xComponent vectorBounds

yComponent :: DirectionBounds2D -> Interval Unitless
yComponent (UnitBounds2D vectorBounds) = VectorBounds2D.yComponent vectorBounds

placeIn :: Frame2D frameUnits -> DirectionBounds2D -> DirectionBounds2D
placeIn frame = placeInOrientation frame.orientation

placeInOrientation :: Orientation2D -> DirectionBounds2D -> DirectionBounds2D
placeInOrientation orientation = lift (VectorBounds2D.placeInOrientation orientation)

relativeTo :: Frame2D frameUnits -> DirectionBounds2D -> DirectionBounds2D
relativeTo frame = relativeToOrientation frame.orientation

relativeToOrientation :: Orientation2D -> DirectionBounds2D -> DirectionBounds2D
relativeToOrientation orientation = lift (VectorBounds2D.relativeToOrientation orientation)

{-| Convert a 2D direction to 3D direction by placing it on a plane.

Given a 2D direction defined within a plane's coordinate system,
this returns the corresponding 3D direction.
-}
placeOn :: Plane3D space -> DirectionBounds2D -> DirectionBounds3D space
placeOn plane = placeOnOrientation plane.orientation

placeOnOrientation :: PlaneOrientation3D space -> DirectionBounds2D -> DirectionBounds3D space
placeOnOrientation orientation (UnitBounds2D vector) =
  DirectionBounds3D.unsafe (VectorBounds2D.placeOnOrientation orientation vector)
