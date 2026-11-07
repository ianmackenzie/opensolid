{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.VectorBounds2D
  ( VectorBounds2D (VectorBounds2D)
  , constant
  , coerce
  , erase
  , unerase
  , aggregate2
  , aggregate3
  , aggregate
  , aggregateOf
  , hull2
  , hull3
  , hull4
  , hull
  , hullOf
  , polar
  , xComponent
  , yComponent
  , components
  , center
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  , maxMagnitude
  , maxSquaredMagnitude
  , maxSquaredMagnitude_
  , diameter
  , isResolved
  , areDistinct
  , areIndependent
  , direction
  , normalize
  , exclusion
  , inclusion
  , member
  , contains
  , isContainedIn
  , separation
  , overlap
  , intersection
  , interpolate
  , relativeTo
  , relativeToOrientation
  , placeIn
  , placeInOrientation
  , placeOn
  , placeOnOrientation
  , convert
  , unconvert
  , transformBy
  )
where

import Data.Coerce qualified
import {-# SOURCE #-} OpenSolid.DirectionBounds2D (DirectionBounds2D)
import {-# SOURCE #-} OpenSolid.DirectionBounds2D qualified as DirectionBounds2D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction2D (Direction2D)
  , Direction3D (Direction3D)
  , Frame2D
  , Orientation2D (Orientation2D)
  , Plane3D
  , PlaneOrientation3D (PlaneOrientation3D)
  , Vector2D (V2D#)
  , Vector3D (Vector3D)
  , VectorBounds2D (VB2D#, VectorBounds2D)
  , VectorBounds3D (VectorBounds3D)
  , VectorTransform2D (VectorTransform2D)
  )
import OpenSolid.Primitives qualified as Primitives
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D

constant :: Vector2D units -> VectorBounds2D units
constant (Vector2D x y) = VectorBounds2D (Interval.constant x) (Interval.constant y)

{-# INLINE coerce #-}
coerce :: VectorBounds2D units1 -> VectorBounds2D units2
coerce = Data.Coerce.coerce

{-# INLINE erase #-}
erase :: VectorBounds2D units -> VectorBounds2D Unitless
erase = coerce

{-# INLINE unerase #-}
unerase :: VectorBounds2D Unitless -> VectorBounds2D units
unerase = coerce

hull2 :: Vector2D units -> Vector2D units -> VectorBounds2D units
hull2 (Vector2D x1 y1) (Vector2D x2 y2) =
  VectorBounds2D (Interval x1 x2) (Interval y1 y2)

hull3 :: Vector2D units -> Vector2D units -> Vector2D units -> VectorBounds2D units
hull3 (V2D# (Vector2D# x1# y1#)) (V2D# (Vector2D# x2# y2#)) (V2D# (Vector2D# x3# y3#)) =
  VB2D# (VectorBounds2D# (hull3# x1# x2# x3#) (hull3# y1# y2# y3#))

hull4 ::
  Vector2D units ->
  Vector2D units ->
  Vector2D units ->
  Vector2D units ->
  VectorBounds2D units
hull4 v1 v2 v3 v4 = do
  let !(V2D# (Vector2D# x1# y1#)) = v1
  let !(V2D# (Vector2D# x2# y2#)) = v2
  let !(V2D# (Vector2D# x3# y3#)) = v3
  let !(V2D# (Vector2D# x4# y4#)) = v4
  VB2D# (VectorBounds2D# (hull4# x1# x2# x3# x4#) (hull4# y1# y2# y3# y4#))

hull :: NonEmpty (Vector2D units) -> VectorBounds2D units
hull (V2D# (Vector2D# x# y#) :| rest) = hullImpl x# x# y# y# rest

hullImpl :: Double# -> Double# -> Double# -> Double# -> List (Vector2D units) -> VectorBounds2D units
hullImpl xLow# xHigh# yLow# yHigh# vectors = case vectors of
  [] -> VB2D# (VectorBounds2D# (Interval# xLow# xHigh#) (Interval# yLow# yHigh#))
  V2D# (Vector2D# x# y#) : remaining ->
    hullImpl (min# xLow# x#) (max# xHigh# x#) (min# yLow# y#) (max# yHigh# y#) remaining

hullOf :: (a -> Vector2D units) -> NonEmpty a -> VectorBounds2D units
hullOf function nonEmpty = hull (NonEmpty.map function nonEmpty)

aggregate2 ::
  VectorBounds2D units ->
  VectorBounds2D units ->
  VectorBounds2D units
aggregate2 (VectorBounds2D x1 y1) (VectorBounds2D x2 y2) =
  VectorBounds2D (Interval.aggregate2 x1 x2) (Interval.aggregate2 y1 y2)

aggregate3 ::
  VectorBounds2D units ->
  VectorBounds2D units ->
  VectorBounds2D units ->
  VectorBounds2D units
aggregate3 (VectorBounds2D x1 y1) (VectorBounds2D x2 y2) (VectorBounds2D x3 y3) =
  VectorBounds2D (Interval.aggregate3 x1 x2 x3) (Interval.aggregate3 y1 y2 y3)

-- | Construct a vector bounding box containing all vector bounding boxes in the given list.
aggregate :: NonEmpty (VectorBounds2D units) -> VectorBounds2D units
aggregate (first :| rest) = do
  let !(VB2D# (VectorBounds2D# x# y#)) = first
  let !(Interval# xLow# xHigh#) = x#
  let !(Interval# yLow# yHigh#) = y#
  aggregateImpl xLow# xHigh# yLow# yHigh# rest

aggregateImpl ::
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  List (VectorBounds2D units) ->
  VectorBounds2D units
aggregateImpl xLow# xHigh# yLow# yHigh# list = case list of
  [] -> VB2D# (VectorBounds2D# (Interval# xLow# xHigh#) (Interval# yLow# yHigh#))
  VB2D# (VectorBounds2D# xNext# yNext#) : remaining -> do
    let !(Interval# xLowNext# xHighNext#) = xNext#
    let !(Interval# yLowNext# yHighNext#) = yNext#
    aggregateImpl
      (min# xLow# xLowNext#)
      (max# xHigh# xHighNext#)
      (min# yLow# yLowNext#)
      (max# yHigh# yHighNext#)
      remaining

aggregateOf :: (a -> VectorBounds2D units) -> NonEmpty a -> VectorBounds2D units
aggregateOf function nonEmpty = aggregate (NonEmpty.map function nonEmpty)

polar :: Interval units -> Interval Radians -> VectorBounds2D units
polar r theta = VectorBounds2D (r * Interval.cos theta) (r * Interval.sin theta)

xComponent :: VectorBounds2D units -> Interval units
xComponent (VectorBounds2D vx _) = vx

yComponent :: VectorBounds2D units -> Interval units
yComponent (VectorBounds2D _ vy) = vy

components :: VectorBounds2D units -> (Interval units, Interval units)
components (VectorBounds2D vx vy) = (vx, vy)

center :: VectorBounds2D units -> Vector2D units
center (VectorBounds2D vx vy) = Vector2D (Interval.midpoint vx) (Interval.midpoint vy)

squaredMagnitude :: Units.Squared units1 units2 => VectorBounds2D units1 -> Interval units2
squaredMagnitude = Units.specialize . squaredMagnitude_

squaredMagnitude_ :: VectorBounds2D units -> Interval (units ?*? units)
squaredMagnitude_ (VectorBounds2D x y) = Interval.squared_ x + Interval.squared_ y

magnitude :: VectorBounds2D units -> Interval units
magnitude (VectorBounds2D x y) = Interval.hypot2 x y

maxMagnitude :: VectorBounds2D units -> Quantity units
maxMagnitude (VectorBounds2D (Interval minX maxX) (Interval minY maxY)) = do
  let xMagnitude = max (Quantity.abs minX) (Quantity.abs maxX)
  let yMagnitude = max (Quantity.abs minY) (Quantity.abs maxY)
  Quantity.hypot2 xMagnitude yMagnitude

maxSquaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorBounds2D units1 ->
  Quantity units2
maxSquaredMagnitude = Units.specialize . maxSquaredMagnitude_

maxSquaredMagnitude_ :: VectorBounds2D units -> Quantity (units ?*? units)
maxSquaredMagnitude_ (VectorBounds2D (Interval minX maxX) (Interval minY maxY)) = do
  let xMagnitude = max (Quantity.abs minX) (Quantity.abs maxX)
  let yMagnitude = max (Quantity.abs minY) (Quantity.abs maxY)
  Quantity.squared_ xMagnitude + Quantity.squared_ yMagnitude

diameter :: VectorBounds2D units -> Quantity units
diameter (VectorBounds2D x y) = Quantity.hypot2 (Interval.width x) (Interval.width y)

isResolved :: VectorBounds2D units -> Bool
isResolved (VectorBounds2D x y) = Interval.isResolved x || Interval.isResolved y

areDistinct :: VectorBounds2D units -> VectorBounds2D units -> Bool
areDistinct (VectorBounds2D x1 y1) (VectorBounds2D x2 y2) =
  Interval.areDistinct x1 x2 || Interval.areDistinct y1 y2

areIndependent :: VectorBounds2D units -> VectorBounds2D units -> Bool
areIndependent bounds1 bounds2 = Interval.isResolved (bounds1 `cross_` bounds2)

direction :: VectorBounds2D units -> DirectionBounds2D
direction vectorBounds = DirectionBounds2D.unsafe (normalize vectorBounds)

normalize :: VectorBounds2D units -> VectorBounds2D Unitless
normalize vectorBounds = do
  let VectorBounds2D x y = vectorBounds / magnitude vectorBounds
  let nx = clampNormalized x
  let ny = clampNormalized y
  VectorBounds2D nx ny

normalizedBounds :: Interval Unitless
normalizedBounds = Interval -1.0 1.0

clampNormalized :: Interval Unitless -> Interval Unitless
clampNormalized (Interval low high) =
  Interval (Number.clampTo normalizedBounds low) (Number.clampTo normalizedBounds high)

exclusion :: Vector2D units -> VectorBounds2D units -> Quantity units
exclusion (V2D# vector#) (VB2D# bounds#) = Q# (exclusion# vector# bounds#)

inclusion :: Vector2D units -> VectorBounds2D units -> Quantity units
inclusion (V2D# vector#) (VB2D# bounds#) = Q# (inclusion# vector# bounds#)

member :: Vector2D units -> VectorBounds2D units -> Bool
member (Vector2D vx vy) (VectorBounds2D x y) = Interval.member vx x && Interval.member vy y

contains :: VectorBounds2D units -> VectorBounds2D units -> Bool
contains (VectorBounds2D x2 y2) (VectorBounds2D x1 y1) =
  Interval.contains x2 x1 && Interval.contains y2 y1

isContainedIn :: VectorBounds2D units -> VectorBounds2D units -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

separation :: VectorBounds2D units -> VectorBounds2D units -> Quantity units
separation (VB2D# bounds1#) (VB2D# bounds2#) = Q# (separation# bounds1# bounds2#)

overlap :: VectorBounds2D units -> VectorBounds2D units -> Quantity units
overlap (VB2D# first#) (VB2D# second#) = Q# (overlap# first# second#)

intersection :: VectorBounds2D units -> VectorBounds2D units -> Maybe (VectorBounds2D units)
intersection (VectorBounds2D x1 y1) (VectorBounds2D x2 y2) =
  Maybe.map2 VectorBounds2D (Interval.intersection x1 x2) (Interval.intersection y1 y2)

interpolate :: VectorBounds2D units -> Number -> Number -> Vector2D units
interpolate (VectorBounds2D x y) u v = Vector2D (x @ u) (y @ v)

placeIn :: Frame2D frameUnits -> VectorBounds2D units -> VectorBounds2D units
placeIn frame = placeInOrientation frame.orientation

placeInOrientation :: Orientation2D -> VectorBounds2D units -> VectorBounds2D units
placeInOrientation orientation (VectorBounds2D x y) = do
  let xMid = Interval.midpoint x
  let yMid = Interval.midpoint y
  let xWidth = Interval.width x
  let yWidth = Interval.width y
  let Vector2D x0 y0 = Vector2D.placeInOrientation orientation (Vector2D xMid yMid)
  let Orientation2D i j = orientation
  let Direction2D ix iy = i
  let Direction2D jx jy = j
  let rx = 0.5 * xWidth * Number.abs ix + 0.5 * yWidth * Number.abs jx
  let ry = 0.5 * xWidth * Number.abs iy + 0.5 * yWidth * Number.abs jy
  VectorBounds2D (Interval (x0 - rx) (x0 + rx)) (Interval (y0 - ry) (y0 + ry))

relativeTo :: Frame2D frameUnits -> VectorBounds2D units -> VectorBounds2D units
relativeTo frame = relativeToOrientation frame.orientation

relativeToOrientation :: Orientation2D -> VectorBounds2D units -> VectorBounds2D units
relativeToOrientation orientation (VectorBounds2D x y) = do
  let xMid = Interval.midpoint x
  let yMid = Interval.midpoint y
  let xWidth = Interval.width x
  let yWidth = Interval.width y
  let Vector2D x0 y0 = Vector2D.relativeToOrientation orientation (Vector2D xMid yMid)
  let Orientation2D i j = orientation
  let Direction2D ix iy = i
  let Direction2D jx jy = j
  let rx = 0.5 * xWidth * Number.abs ix + 0.5 * yWidth * Number.abs iy
  let ry = 0.5 * xWidth * Number.abs jx + 0.5 * yWidth * Number.abs jy
  VectorBounds2D (Interval (x0 - rx) (x0 + rx)) (Interval (y0 - ry) (y0 + ry))

placeOn :: Plane3D space -> VectorBounds2D units -> VectorBounds3D units space
placeOn plane = placeOnOrientation plane.orientation

placeOnOrientation :: PlaneOrientation3D space -> VectorBounds2D units -> VectorBounds3D units space
placeOnOrientation orientation (VectorBounds2D x y) = do
  let xMid = Interval.midpoint x
  let yMid = Interval.midpoint y
  let xWidth = Interval.width x
  let yWidth = Interval.width y
  let Vector3D x0 y0 z0 = Vector2D.placeOnOrientation orientation (Vector2D xMid yMid)
  let PlaneOrientation3D i j _ = orientation
  let Direction3D ix iy iz = i
  let Direction3D jx jy jz = j
  let rx = 0.5 * xWidth * Number.abs ix + 0.5 * yWidth * Number.abs jx
  let ry = 0.5 * xWidth * Number.abs iy + 0.5 * yWidth * Number.abs jy
  let rz = 0.5 * xWidth * Number.abs iz + 0.5 * yWidth * Number.abs jz
  VectorBounds3D
    (Interval (x0 - rx) (x0 + rx))
    (Interval (y0 - ry) (y0 + ry))
    (Interval (z0 - rz) (z0 + rz))

convert :: Quantity (units2 ?/? units1) -> VectorBounds2D units1 -> VectorBounds2D units2
convert factor vectorBounds = Units.simplify (vectorBounds ?*? factor)

unconvert :: Quantity (units2 ?/? units1) -> VectorBounds2D units2 -> VectorBounds2D units1
unconvert factor vectorBounds = Units.simplify (vectorBounds ?/? factor)

transformBy :: VectorTransform2D tag -> VectorBounds2D units -> VectorBounds2D units
transformBy transform (VectorBounds2D x y) = do
  let xMid = Interval.midpoint x
  let yMid = Interval.midpoint y
  let xWidth = Interval.width x
  let yWidth = Interval.width y
  let Vector2D x0 y0 = Vector2D.transformBy transform (Vector2D xMid yMid)
  let VectorTransform2D i j = transform
  let Vector2D ix iy = i
  let Vector2D jx jy = j
  let rx = 0.5 * Number.abs ix * xWidth + 0.5 * Number.abs jx * yWidth
  let ry = 0.5 * Number.abs iy * xWidth + 0.5 * Number.abs jy * yWidth
  VectorBounds2D (Interval (x0 - rx) (x0 + rx)) (Interval (y0 - ry) (y0 + ry))
