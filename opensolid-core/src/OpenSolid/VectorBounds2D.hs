{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.VectorBounds2D
  ( VectorBounds2D (VectorBounds2D)
  , constant
  , coerce
  , aggregate2
  , aggregate3
  , aggregateN
  , hull2
  , hull3
  , hull4
  , hullN
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
  , direction
  , normalize
  , exclusion
  , exclusion#
  , inclusion
  , inclusion#
  , includes
  , contains
  , isContainedIn
  , separation
  , separation#
  , overlap
  , overlap#
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

import {-# SOURCE #-} OpenSolid.DirectionBounds2D (DirectionBounds2D)
import {-# SOURCE #-} OpenSolid.DirectionBounds2D qualified as DirectionBounds2D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction2D (Direction2D)
  , Direction3D (Direction3D)
  , Frame2D
  , Orientation2D (Orientation2D)
  , Plane3D
  , PlaneOrientation3D (PlaneOrientation3D)
  , Vector3D (Vector3D)
  , VectorBounds2D (VectorBounds2D)
  , VectorBounds3D (VectorBounds3D)
  )
import OpenSolid.Primitives qualified as Primitives
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Transform2D (Transform2D (Transform2D))
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D

constant :: Vector2D units space -> VectorBounds2D units space
constant (Vector2D x y) = VectorBounds2D (Interval.constant x) (Interval.constant y)

{-# INLINE coerce #-}
coerce :: VectorBounds2D units1 space1 -> VectorBounds2D units2 space2
coerce (VectorBounds2D x y) = VectorBounds2D (Interval.coerce x) (Interval.coerce y)

hull2 :: Vector2D units space -> Vector2D units space -> VectorBounds2D units space
hull2 (Vector2D x1 y1) (Vector2D x2 y2) =
  VectorBounds2D (Interval x1 x2) (Interval y1 y2)

hull3 ::
  Vector2D units space ->
  Vector2D units space ->
  Vector2D units space ->
  VectorBounds2D units space
hull3 (Vector2D x1 y1) (Vector2D x2 y2) (Vector2D x3 y3) = do
  let minX = min (min x1 x2) x3
  let maxX = max (max x1 x2) x3
  let minY = min (min y1 y2) y3
  let maxY = max (max y1 y2) y3
  VectorBounds2D (Interval minX maxX) (Interval minY maxY)

hull4 ::
  Vector2D units space ->
  Vector2D units space ->
  Vector2D units space ->
  Vector2D units space ->
  VectorBounds2D units space
hull4 (Vector2D x1 y1) (Vector2D x2 y2) (Vector2D x3 y3) (Vector2D x4 y4) = do
  let minX = min (min (min x1 x2) x3) x4
  let maxX = max (max (max x1 x2) x3) x4
  let minY = min (min (min y1 y2) y3) y4
  let maxY = max (max (max y1 y2) y3) y4
  VectorBounds2D (Interval minX maxX) (Interval minY maxY)

hullN :: NonEmpty (Vector2D units space) -> VectorBounds2D units space
hullN (Vector2D x0 y0 :| rest) = go x0 x0 y0 y0 rest
 where
  go ::
    Quantity units ->
    Quantity units ->
    Quantity units ->
    Quantity units ->
    List (Vector2D units space) ->
    VectorBounds2D units space
  go xLow xHigh yLow yHigh [] = VectorBounds2D (Interval xLow xHigh) (Interval yLow yHigh)
  go xLow xHigh yLow yHigh (Vector2D x y : remaining) =
    go (min xLow x) (max xHigh x) (min yLow y) (max yHigh y) remaining

aggregate2 ::
  VectorBounds2D units space ->
  VectorBounds2D units space ->
  VectorBounds2D units space
aggregate2 (VectorBounds2D x1 y1) (VectorBounds2D x2 y2) =
  VectorBounds2D (Interval.aggregate2 x1 x2) (Interval.aggregate2 y1 y2)

aggregate3 ::
  VectorBounds2D units space ->
  VectorBounds2D units space ->
  VectorBounds2D units space ->
  VectorBounds2D units space
aggregate3 (VectorBounds2D x1 y1) (VectorBounds2D x2 y2) (VectorBounds2D x3 y3) =
  VectorBounds2D (Interval.aggregate3 x1 x2 x3) (Interval.aggregate3 y1 y2 y3)

-- | Construct a vector bounding box containing all vector bounding boxes in the given list.
aggregateN :: NonEmpty (VectorBounds2D units space) -> VectorBounds2D units space
aggregateN (VectorBounds2D (Interval xLow0 xHigh0) (Interval yLow0 yHigh0) :| rest) =
  aggregateImpl xLow0 xHigh0 yLow0 yHigh0 rest

aggregateImpl ::
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Quantity units ->
  List (VectorBounds2D units space) ->
  VectorBounds2D units space
aggregateImpl xLow xHigh yLow yHigh [] = VectorBounds2D (Interval xLow xHigh) (Interval yLow yHigh)
aggregateImpl xLow xHigh yLow yHigh (next : remaining) = do
  let VectorBounds2D (Interval xLowNext xHighNext) (Interval yLowNext yHighNext) = next
  aggregateImpl
    (min xLow xLowNext)
    (max xHigh xHighNext)
    (min yLow yLowNext)
    (max yHigh yHighNext)
    remaining

polar :: Interval units -> Interval Radians -> VectorBounds2D units space
polar r theta = VectorBounds2D (r .*. Interval.cos theta) (r .*. Interval.sin theta)

xComponent :: VectorBounds2D units space -> Interval units
xComponent (VectorBounds2D vx _) = vx

yComponent :: VectorBounds2D units space -> Interval units
yComponent (VectorBounds2D _ vy) = vy

components :: VectorBounds2D units space -> (Interval units, Interval units)
components (VectorBounds2D vx vy) = (vx, vy)

center :: VectorBounds2D units space -> Vector2D units space
center (VectorBounds2D vx vy) = Vector2D (Interval.midpoint vx) (Interval.midpoint vy)

squaredMagnitude :: Units.Squared units1 units2 => VectorBounds2D units1 space -> Interval units2
squaredMagnitude = Units.specialize . squaredMagnitude_

squaredMagnitude_ :: VectorBounds2D units space -> Interval (units ?*? units)
squaredMagnitude_ (VectorBounds2D x y) = Interval.squared_ x .+. Interval.squared_ y

magnitude :: VectorBounds2D units space -> Interval units
magnitude (VectorBounds2D x y) = Interval.hypot2 x y

maxMagnitude :: VectorBounds2D units space -> Quantity units
maxMagnitude (VectorBounds2D (Interval minX maxX) (Interval minY maxY)) = do
  let xMagnitude = max (Quantity.abs minX) (Quantity.abs maxX)
  let yMagnitude = max (Quantity.abs minY) (Quantity.abs maxY)
  Quantity.hypot2 xMagnitude yMagnitude

maxSquaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorBounds2D units1 space ->
  Quantity units2
maxSquaredMagnitude = Units.specialize . maxSquaredMagnitude_

maxSquaredMagnitude_ :: VectorBounds2D units space -> Quantity (units ?*? units)
maxSquaredMagnitude_ (VectorBounds2D (Interval minX maxX) (Interval minY maxY)) = do
  let xMagnitude = max (Quantity.abs minX) (Quantity.abs maxX)
  let yMagnitude = max (Quantity.abs minY) (Quantity.abs maxY)
  Quantity.squared_ xMagnitude .+. Quantity.squared_ yMagnitude

direction :: VectorBounds2D units space -> DirectionBounds2D space
direction vectorBounds = DirectionBounds2D.unsafe (normalize vectorBounds)

normalize :: VectorBounds2D units space -> VectorBounds2D Unitless space
normalize vectorBounds = do
  let VectorBounds2D x y = vectorBounds ./. magnitude vectorBounds
  let nx = clampNormalized x
  let ny = clampNormalized y
  VectorBounds2D nx ny

normalizedBounds :: Interval Unitless
normalizedBounds = Interval -1 1

clampNormalized :: Interval Unitless -> Interval Unitless
clampNormalized (Interval low high) =
  Interval (Quantity.clampTo normalizedBounds low) (Quantity.clampTo normalizedBounds high)

exclusion :: Vector2D units space -> VectorBounds2D units space -> Quantity units
exclusion vector bounds = Quantity# (exclusion# vector bounds)

{-# INLINEABLE exclusion# #-}
exclusion# :: Vector2D units space -> VectorBounds2D units space -> Double#
exclusion# (Vector2D (Quantity# vx#) (Quantity# vy#)) (VectorBounds2D bx by) = do
  let exclusionX# = Interval.exclusion# vx# bx
  let exclusionY# = Interval.exclusion# vy# by
  let positiveX# = exclusionX# >=# 0.0##
  let positiveY# = exclusionY# >=# 0.0##
  case (# positiveX#, positiveY# #) of
    (# 1#, 1# #) -> hypot2# exclusionX# exclusionY#
    (# 1#, _ #) -> exclusionX#
    (# _, 1# #) -> exclusionY#
    (# _, _ #) -> max# exclusionX# exclusionY#

inclusion :: Vector2D units space -> VectorBounds2D units space -> Quantity units
inclusion vector box = Quantity# (inclusion# vector box)

{-# INLINE inclusion# #-}
inclusion# :: Vector2D units space -> VectorBounds2D units space -> Double#
inclusion# vector box = negate# (exclusion# vector box)

includes :: Vector2D units space -> VectorBounds2D units space -> Bool
includes (Vector2D vx vy) (VectorBounds2D x y) = Interval.includes vx x && Interval.includes vy y

contains :: VectorBounds2D units space -> VectorBounds2D units space -> Bool
contains (VectorBounds2D x2 y2) (VectorBounds2D x1 y1) =
  Interval.contains x2 x1 && Interval.contains y2 y1

isContainedIn :: VectorBounds2D units space -> VectorBounds2D units space -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

separation :: VectorBounds2D units space -> VectorBounds2D units space -> Quantity units
separation bounds1 bounds2 = Quantity# (separation# bounds1 bounds2)

{-# INLINEABLE separation# #-}
separation# :: VectorBounds2D units space -> VectorBounds2D units space -> Double#
separation# (VectorBounds2D x1 y1) (VectorBounds2D x2 y2) = do
  let separationX# = Interval.separation# x1 x2
  let separationY# = Interval.separation# y1 y2
  let positiveX# = separationX# >=# 0.0##
  let positiveY# = separationY# >=# 0.0##
  case (# positiveX#, positiveY# #) of
    (# 1#, 1# #) -> hypot2# separationX# separationY#
    (# 1#, _ #) -> separationX#
    (# _, 1# #) -> separationY#
    (# _, _ #) -> max# separationX# separationY#

overlap :: VectorBounds2D units space -> VectorBounds2D units space -> Quantity units
overlap first second = Quantity# (overlap# first second)

{-# INLINE overlap# #-}
overlap# :: VectorBounds2D units space -> VectorBounds2D units space -> Double#
overlap# first second = negate# (separation# first second)

intersection ::
  VectorBounds2D units space ->
  VectorBounds2D units space ->
  Maybe (VectorBounds2D units space)
intersection (VectorBounds2D x1 y1) (VectorBounds2D x2 y2) =
  Maybe.map2 VectorBounds2D (Interval.intersection x1 x2) (Interval.intersection y1 y2)

interpolate :: VectorBounds2D units space -> Number -> Number -> Vector2D units space
interpolate (VectorBounds2D x y) u v =
  Vector2D (Interval.interpolate x u) (Interval.interpolate y v)

placeIn ::
  Frame2D frameUnits global local ->
  VectorBounds2D units local ->
  VectorBounds2D units global
placeIn frame = placeInOrientation frame.orientation

placeInOrientation ::
  Orientation2D global ->
  VectorBounds2D units local ->
  VectorBounds2D units global
placeInOrientation orientation (VectorBounds2D x y) = do
  let xMid = Interval.midpoint x
  let yMid = Interval.midpoint y
  let xWidth = Interval.width x
  let yWidth = Interval.width y
  let Vector2D x0 y0 = Vector2D.placeInOrientation orientation (Vector2D xMid yMid)
  let Orientation2D i j = orientation
  let Direction2D ix iy = i
  let Direction2D jx jy = j
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs jx
  let ry = 0.5 *. xWidth .*. Number.abs iy .+. 0.5 *. yWidth .*. Number.abs jy
  VectorBounds2D (Interval (x0 .-. rx) (x0 .+. rx)) (Interval (y0 .-. ry) (y0 .+. ry))

relativeTo ::
  Frame2D frameUnits global local ->
  VectorBounds2D units global ->
  VectorBounds2D units local
relativeTo frame = relativeToOrientation frame.orientation

relativeToOrientation ::
  Orientation2D global ->
  VectorBounds2D units global ->
  VectorBounds2D units local
relativeToOrientation orientation (VectorBounds2D x y) = do
  let xMid = Interval.midpoint x
  let yMid = Interval.midpoint y
  let xWidth = Interval.width x
  let yWidth = Interval.width y
  let Vector2D x0 y0 = Vector2D.relativeToOrientation orientation (Vector2D xMid yMid)
  let Orientation2D i j = orientation
  let Direction2D ix iy = i
  let Direction2D jx jy = j
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs iy
  let ry = 0.5 *. xWidth .*. Number.abs jx .+. 0.5 *. yWidth .*. Number.abs jy
  VectorBounds2D (Interval (x0 .-. rx) (x0 .+. rx)) (Interval (y0 .-. ry) (y0 .+. ry))

placeOn :: Plane3D global local -> VectorBounds2D units local -> VectorBounds3D units global
placeOn plane = placeOnOrientation plane.orientation

placeOnOrientation ::
  PlaneOrientation3D global ->
  VectorBounds2D units local ->
  VectorBounds3D units global
placeOnOrientation orientation (VectorBounds2D x y) = do
  let xMid = Interval.midpoint x
  let yMid = Interval.midpoint y
  let xWidth = Interval.width x
  let yWidth = Interval.width y
  let Vector3D x0 y0 z0 = Vector2D.placeOnOrientation orientation (Vector2D xMid yMid)
  let PlaneOrientation3D i j = orientation
  let Direction3D ix iy iz = i
  let Direction3D jx jy jz = j
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs jx
  let ry = 0.5 *. xWidth .*. Number.abs iy .+. 0.5 *. yWidth .*. Number.abs jy
  let rz = 0.5 *. xWidth .*. Number.abs iz .+. 0.5 *. yWidth .*. Number.abs jz
  VectorBounds3D
    (Interval (x0 .-. rx) (x0 .+. rx))
    (Interval (y0 .-. ry) (y0 .+. ry))
    (Interval (z0 .-. rz) (z0 .+. rz))

convert ::
  Quantity (units2 ?/? units1) ->
  VectorBounds2D units1 space ->
  VectorBounds2D units2 space
convert factor vectorBounds = Units.simplify (vectorBounds ?*? factor)

unconvert ::
  Quantity (units2 ?/? units1) ->
  VectorBounds2D units2 space ->
  VectorBounds2D units1 space
unconvert factor vectorBounds = Units.simplify (vectorBounds ?/? factor)

transformBy ::
  Transform2D tag units1 space ->
  VectorBounds2D units2 space ->
  VectorBounds2D units2 space
transformBy transform (VectorBounds2D x y) = do
  let xMid = Interval.midpoint x
  let yMid = Interval.midpoint y
  let xWidth = Interval.width x
  let yWidth = Interval.width y
  let Vector2D x0 y0 = Vector2D.transformBy transform (Vector2D xMid yMid)
  let Transform2D _ i j = transform
  let Vector2D ix iy = i
  let Vector2D jx jy = j
  let rx = 0.5 *. Number.abs ix .*. xWidth .+. 0.5 *. Number.abs jx .*. yWidth
  let ry = 0.5 *. Number.abs iy .*. xWidth .+. 0.5 *. Number.abs jy .*. yWidth
  VectorBounds2D (Interval (x0 .-. rx) (x0 .+. rx)) (Interval (y0 .-. ry) (y0 .+. ry))
