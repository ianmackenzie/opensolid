module OpenSolid.DirectionBounds2d
  ( DirectionBounds2d (DirectionBounds2d)
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

import OpenSolid.Bounds (Bounds)
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.DirectionBounds3d (DirectionBounds3d)
import OpenSolid.DirectionBounds3d qualified as DirectionBounds3d
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Orientation2d (Orientation2d)
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.PlaneOrientation3d (PlaneOrientation3d)
import OpenSolid.Prelude
import OpenSolid.Primitives qualified as Primitives
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds2d qualified as VectorBounds2d

newtype DirectionBounds2d space
  = UnitBounds2d (VectorBounds2d space Unitless)
  deriving (Show)

instance
  space1 ~ space2 =>
  Units.Coercion (DirectionBounds2d space1) (DirectionBounds2d space2)
  where
  coerce = id

{-# COMPLETE DirectionBounds2d #-}

pattern DirectionBounds2d :: Bounds Unitless -> Bounds Unitless -> DirectionBounds2d space
pattern DirectionBounds2d dx dy <- UnitBounds2d (VectorBounds2d dx dy)

instance Negation (DirectionBounds2d space) where
  negative (UnitBounds2d vectorBounds) = UnitBounds2d (negative vectorBounds)

instance Multiplication Sign (DirectionBounds2d space) (DirectionBounds2d space) where
  Positive .*. directionBounds = directionBounds
  Negative .*. directionBounds = negative directionBounds

instance Multiplication_ Sign (DirectionBounds2d space) (DirectionBounds2d space) where
  Positive ?*? directionBounds = directionBounds
  Negative ?*? directionBounds = negative directionBounds

instance Multiplication (DirectionBounds2d space) Sign (DirectionBounds2d space) where
  directionBounds .*. Positive = directionBounds
  directionBounds .*. Negative = negative directionBounds

instance Multiplication_ (DirectionBounds2d space) Sign (DirectionBounds2d space) where
  directionBounds ?*? Positive = directionBounds
  directionBounds ?*? Negative = negative directionBounds

instance
  Multiplication
    (Quantity units)
    (DirectionBounds2d space)
    (VectorBounds2d space units)
  where
  value .*. UnitBounds2d vectorBounds = value .*. vectorBounds

instance
  Multiplication
    (DirectionBounds2d space)
    (Quantity units)
    (VectorBounds2d space units)
  where
  UnitBounds2d vectorBounds .*. value = vectorBounds .*. value

instance
  Multiplication
    (Bounds units)
    (DirectionBounds2d space)
    (VectorBounds2d space units)
  where
  bounds .*. UnitBounds2d vectorBounds = bounds .*. vectorBounds

instance
  Multiplication
    (DirectionBounds2d space)
    (Bounds units)
    (VectorBounds2d space units)
  where
  UnitBounds2d vectorBounds .*. bounds = vectorBounds .*. bounds

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2d space1) (DirectionBounds2d space2) (Bounds Unitless)
  where
  UnitBounds2d vectorBounds1 `dot` UnitBounds2d vectorBounds2 =
    vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2d space1) (VectorBounds2d space2 units) (Bounds units)
  where
  UnitBounds2d vectorBounds1 `dot` vectorBounds2 = vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (VectorBounds2d space1 units) (DirectionBounds2d space2) (Bounds units)
  where
  vectorBounds1 `dot` UnitBounds2d vectorBounds2 = vectorBounds1 `dot` vectorBounds2

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2d space1) (Direction2d space2) (Bounds Unitless)
  where
  UnitBounds2d vectorBounds `dot` direction = vectorBounds `dot` direction

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (DirectionBounds2d space2) (Bounds Unitless)
  where
  direction `dot` UnitBounds2d vectorBounds = direction `dot` vectorBounds

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionBounds2d space1) (Vector2d space2 units) (Bounds units)
  where
  UnitBounds2d vectorBounds `dot` vector = vectorBounds `dot` vector

instance
  space1 ~ space2 =>
  DotMultiplication (Vector2d space1 units) (DirectionBounds2d space2) (Bounds units)
  where
  vector `dot` UnitBounds2d vectorBounds = vector `dot` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2d space1) (DirectionBounds2d space2) (Bounds Unitless)
  where
  UnitBounds2d vectorBounds1 `cross` UnitBounds2d vectorBounds2 =
    vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2d space1) (VectorBounds2d space2 units) (Bounds units)
  where
  UnitBounds2d vectorBounds1 `cross` vectorBounds2 = vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorBounds2d space1 units) (DirectionBounds2d space2) (Bounds units)
  where
  vectorBounds1 `cross` UnitBounds2d vectorBounds2 = vectorBounds1 `cross` vectorBounds2

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2d space1) (Direction2d space2) (Bounds Unitless)
  where
  UnitBounds2d vectorBounds `cross` direction = vectorBounds `cross` direction

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (DirectionBounds2d space2) (Bounds Unitless)
  where
  direction `cross` UnitBounds2d vectorBounds = direction `cross` vectorBounds

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionBounds2d space1) (Vector2d space2 units) (Bounds units)
  where
  UnitBounds2d vectorBounds `cross` vector = vectorBounds `cross` vector

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector2d space1 units) (DirectionBounds2d space2) (Bounds units)
  where
  vector `cross` UnitBounds2d vectorBounds = vector `cross` vectorBounds

unsafe :: VectorBounds2d space Unitless -> DirectionBounds2d space
unsafe = UnitBounds2d

unwrap :: DirectionBounds2d space -> VectorBounds2d space Unitless
unwrap (UnitBounds2d vectorBounds) = vectorBounds

{-# INLINE lift #-}
lift ::
  (VectorBounds2d space1 Unitless -> VectorBounds2d space2 Unitless) ->
  DirectionBounds2d space1 ->
  DirectionBounds2d space2
lift function (UnitBounds2d vectorBounds) = UnitBounds2d (function vectorBounds)

constant :: Direction2d space -> DirectionBounds2d space
constant direction = UnitBounds2d (VectorBounds2d.constant (Vector2d.unit direction))

xComponent :: DirectionBounds2d space -> Bounds Unitless
xComponent (UnitBounds2d vectorBounds) = VectorBounds2d.xComponent vectorBounds

yComponent :: DirectionBounds2d space -> Bounds Unitless
yComponent (UnitBounds2d vectorBounds) = VectorBounds2d.yComponent vectorBounds

placeIn ::
  Frame2d global frameUnits local ->
  DirectionBounds2d local ->
  DirectionBounds2d global
placeIn frame = placeInOrientation frame.orientation

placeInOrientation :: Orientation2d global -> DirectionBounds2d local -> DirectionBounds2d global
placeInOrientation orientation = lift (VectorBounds2d.placeInOrientation orientation)

relativeTo ::
  Frame2d global frameUnits local ->
  DirectionBounds2d global ->
  DirectionBounds2d local
relativeTo frame = relativeToOrientation frame.orientation

relativeToOrientation :: Orientation2d global -> DirectionBounds2d global -> DirectionBounds2d local
relativeToOrientation orientation = lift (VectorBounds2d.relativeToOrientation orientation)

{-| Convert a 2D direction to 3D direction by placing it on a plane.

Given a 2D direction defined within a plane's coordinate system,
this returns the corresponding 3D direction.
-}
placeOn ::
  Plane3d global local ->
  DirectionBounds2d local ->
  DirectionBounds3d global
placeOn plane = placeOnOrientation plane.orientation

placeOnOrientation ::
  PlaneOrientation3d global ->
  DirectionBounds2d local ->
  DirectionBounds3d global
placeOnOrientation orientation (UnitBounds2d vector) =
  DirectionBounds3d.unsafe (VectorBounds2d.placeOnOrientation orientation vector)
