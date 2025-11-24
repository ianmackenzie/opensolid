{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.VectorBounds2d
  ( VectorBounds2d (VectorBounds2d)
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
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  , maxMagnitude
  , maxSquaredMagnitude
  , maxSquaredMagnitude_
  , direction
  , normalize
  , exclusion
  , exclusion##
  , inclusion
  , inclusion##
  , includes
  , contains
  , isContainedIn
  , separation
  , separation##
  , overlap
  , overlap##
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

import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import {-# SOURCE #-} OpenSolid.DirectionBounds2d (DirectionBounds2d)
import {-# SOURCE #-} OpenSolid.DirectionBounds2d qualified as DirectionBounds2d
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction2d (Direction2d)
  , Direction3d (Direction3d)
  , Frame2d
  , Orientation2d (Orientation2d)
  , Plane3d
  , PlaneOrientation3d (PlaneOrientation3d)
  , Vector3d (Vector3d)
  , VectorBounds2d (VectorBounds2d)
  , VectorBounds3d (VectorBounds3d)
  )
import OpenSolid.Quantity (Quantity (Quantity##))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d

constant :: Vector2d space units -> VectorBounds2d space units
constant (Vector2d x y) = VectorBounds2d (Bounds.constant x) (Bounds.constant y)

{-# INLINE coerce #-}
coerce :: VectorBounds2d space1 units1 -> VectorBounds2d space2 units2
coerce (VectorBounds2d x y) = VectorBounds2d (Bounds.coerce x) (Bounds.coerce y)

hull2 :: Vector2d space units -> Vector2d space units -> VectorBounds2d space units
hull2 (Vector2d x1 y1) (Vector2d x2 y2) =
  VectorBounds2d (Bounds x1 x2) (Bounds y1 y2)

hull3 ::
  Vector2d space units ->
  Vector2d space units ->
  Vector2d space units ->
  VectorBounds2d space units
hull3 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) = do
  let minX = min (min x1 x2) x3
  let maxX = max (max x1 x2) x3
  let minY = min (min y1 y2) y3
  let maxY = max (max y1 y2) y3
  VectorBounds2d (Bounds minX maxX) (Bounds minY maxY)

hull4 ::
  Vector2d space units ->
  Vector2d space units ->
  Vector2d space units ->
  Vector2d space units ->
  VectorBounds2d space units
hull4 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) (Vector2d x4 y4) = do
  let minX = min (min (min x1 x2) x3) x4
  let maxX = max (max (max x1 x2) x3) x4
  let minY = min (min (min y1 y2) y3) y4
  let maxY = max (max (max y1 y2) y3) y4
  VectorBounds2d (Bounds minX maxX) (Bounds minY maxY)

hullN :: NonEmpty (Vector2d space units) -> VectorBounds2d space units
hullN (Vector2d x0 y0 :| rest) = go x0 x0 y0 y0 rest
 where
  go ::
    Quantity units ->
    Quantity units ->
    Quantity units ->
    Quantity units ->
    List (Vector2d space units) ->
    VectorBounds2d space units
  go xLow xHigh yLow yHigh [] = VectorBounds2d (Bounds xLow xHigh) (Bounds yLow yHigh)
  go xLow xHigh yLow yHigh (Vector2d x y : remaining) =
    go (min xLow x) (max xHigh x) (min yLow y) (max yHigh y) remaining

aggregate2 ::
  VectorBounds2d space units ->
  VectorBounds2d space units ->
  VectorBounds2d space units
aggregate2 (VectorBounds2d x1 y1) (VectorBounds2d x2 y2) =
  VectorBounds2d (Bounds.aggregate2 x1 x2) (Bounds.aggregate2 y1 y2)

aggregate3 ::
  VectorBounds2d space units ->
  VectorBounds2d space units ->
  VectorBounds2d space units ->
  VectorBounds2d space units
aggregate3 (VectorBounds2d x1 y1) (VectorBounds2d x2 y2) (VectorBounds2d x3 y3) =
  VectorBounds2d (Bounds.aggregate3 x1 x2 x3) (Bounds.aggregate3 y1 y2 y3)

-- | Construct a vector bounding box containing all vector bounding boxes in the given list.
aggregateN :: NonEmpty (VectorBounds2d space units) -> VectorBounds2d space units
aggregateN (VectorBounds2d (Bounds xLow0 xHigh0) (Bounds yLow0 yHigh0) :| rest) =
  aggregateImpl xLow0 xHigh0 yLow0 yHigh0 rest

aggregateImpl ::
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Quantity units ->
  List (VectorBounds2d space units) ->
  VectorBounds2d space units
aggregateImpl xLow xHigh yLow yHigh [] = VectorBounds2d (Bounds xLow xHigh) (Bounds yLow yHigh)
aggregateImpl xLow xHigh yLow yHigh (next : remaining) = do
  let VectorBounds2d (Bounds xLowNext xHighNext) (Bounds yLowNext yHighNext) = next
  aggregateImpl
    (min xLow xLowNext)
    (max xHigh xHighNext)
    (min yLow yLowNext)
    (max yHigh yHighNext)
    remaining

polar :: Bounds units -> Bounds Radians -> VectorBounds2d space units
polar r theta = VectorBounds2d (r .*. Bounds.cos theta) (r .*. Bounds.sin theta)

xComponent :: VectorBounds2d space units -> Bounds units
xComponent (VectorBounds2d vx _) = vx

yComponent :: VectorBounds2d space units -> Bounds units
yComponent (VectorBounds2d _ vy) = vy

components :: VectorBounds2d space units -> (Bounds units, Bounds units)
components (VectorBounds2d vx vy) = (vx, vy)

squaredMagnitude :: Units.Squared units1 units2 => VectorBounds2d space units1 -> Bounds units2
squaredMagnitude = Units.specialize . squaredMagnitude_

squaredMagnitude_ :: VectorBounds2d space units -> Bounds (units ?*? units)
squaredMagnitude_ (VectorBounds2d x y) = Bounds.squared_ x .+. Bounds.squared_ y

magnitude :: VectorBounds2d space units -> Bounds units
magnitude (VectorBounds2d x y) = Bounds.hypot2 x y

maxMagnitude :: VectorBounds2d space units -> Quantity units
maxMagnitude (VectorBounds2d (Bounds minX maxX) (Bounds minY maxY)) = do
  let xMagnitude = max (Quantity.abs minX) (Quantity.abs maxX)
  let yMagnitude = max (Quantity.abs minY) (Quantity.abs maxY)
  Quantity.hypot2 xMagnitude yMagnitude

maxSquaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorBounds2d space units1 ->
  Quantity units2
maxSquaredMagnitude = Units.specialize . maxSquaredMagnitude_

maxSquaredMagnitude_ :: VectorBounds2d space units -> Quantity (units ?*? units)
maxSquaredMagnitude_ (VectorBounds2d (Bounds minX maxX) (Bounds minY maxY)) = do
  let xMagnitude = max (Quantity.abs minX) (Quantity.abs maxX)
  let yMagnitude = max (Quantity.abs minY) (Quantity.abs maxY)
  Quantity.squared_ xMagnitude .+. Quantity.squared_ yMagnitude

direction :: VectorBounds2d space units -> DirectionBounds2d space
direction vectorBounds = DirectionBounds2d.unsafe (normalize vectorBounds)

normalize :: VectorBounds2d space units -> VectorBounds2d space Unitless
normalize vectorBounds = do
  let VectorBounds2d x y = vectorBounds ./. magnitude vectorBounds
  let nx = clampNormalized x
  let ny = clampNormalized y
  VectorBounds2d nx ny

normalizedBounds :: Bounds Unitless
normalizedBounds = Bounds -1 1

clampNormalized :: Bounds Unitless -> Bounds Unitless
clampNormalized (Bounds low high) =
  Bounds (Quantity.clampTo normalizedBounds low) (Quantity.clampTo normalizedBounds high)

exclusion :: Vector2d space units -> VectorBounds2d space units -> Quantity units
exclusion vector bounds = Quantity## (exclusion## vector bounds)

{-# INLINEABLE exclusion## #-}
exclusion## :: Vector2d space units -> VectorBounds2d space units -> Double#
exclusion## (Vector2d (Quantity## vx##) (Quantity## vy##)) (VectorBounds2d bx by) = do
  let exclusionX## = Bounds.exclusion## vx## bx
  let exclusionY## = Bounds.exclusion## vy## by
  let positiveX## = exclusionX## >=## 0.0##
  let positiveY## = exclusionY## >=## 0.0##
  case (# positiveX##, positiveY## #) of
    (# 1#, 1# #) -> hypot2## exclusionX## exclusionY##
    (# 1#, _ #) -> exclusionX##
    (# _, 1# #) -> exclusionY##
    (# _, _ #) -> max## exclusionX## exclusionY##

inclusion :: Vector2d space units -> VectorBounds2d space units -> Quantity units
inclusion vector box = Quantity## (inclusion## vector box)

{-# INLINE inclusion## #-}
inclusion## :: Vector2d space units -> VectorBounds2d space units -> Double#
inclusion## vector box = negate## (exclusion## vector box)

includes :: Vector2d space units -> VectorBounds2d space units -> Bool
includes (Vector2d vx vy) (VectorBounds2d x y) = Bounds.includes vx x && Bounds.includes vy y

contains :: VectorBounds2d space units -> VectorBounds2d space units -> Bool
contains (VectorBounds2d x2 y2) (VectorBounds2d x1 y1) =
  Bounds.contains x2 x1 && Bounds.contains y2 y1

isContainedIn :: VectorBounds2d space units -> VectorBounds2d space units -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

separation :: VectorBounds2d space units -> VectorBounds2d space units -> Quantity units
separation bounds1 bounds2 = Quantity## (separation## bounds1 bounds2)

{-# INLINEABLE separation## #-}
separation## :: VectorBounds2d space units -> VectorBounds2d space units -> Double#
separation## (VectorBounds2d x1 y1) (VectorBounds2d x2 y2) = do
  let separationX## = Bounds.separation## x1 x2
  let separationY## = Bounds.separation## y1 y2
  let positiveX## = separationX## >=## 0.0##
  let positiveY## = separationY## >=## 0.0##
  case (# positiveX##, positiveY## #) of
    (# 1#, 1# #) -> hypot2## separationX## separationY##
    (# 1#, _ #) -> separationX##
    (# _, 1# #) -> separationY##
    (# _, _ #) -> max## separationX## separationY##

overlap :: VectorBounds2d space units -> VectorBounds2d space units -> Quantity units
overlap first second = Quantity## (overlap## first second)

{-# INLINE overlap## #-}
overlap## :: VectorBounds2d space units -> VectorBounds2d space units -> Double#
overlap## first second = negate## (separation## first second)

intersection ::
  VectorBounds2d space units ->
  VectorBounds2d space units ->
  Maybe (VectorBounds2d space units)
intersection (VectorBounds2d x1 y1) (VectorBounds2d x2 y2) =
  Maybe.map2 VectorBounds2d (Bounds.intersection x1 x2) (Bounds.intersection y1 y2)

interpolate :: VectorBounds2d space units -> Number -> Number -> Vector2d space units
interpolate (VectorBounds2d x y) u v = Vector2d (Bounds.interpolate x u) (Bounds.interpolate y v)

placeIn ::
  Frame2d global frameUnits (Defines local) ->
  VectorBounds2d local units ->
  VectorBounds2d global units
placeIn frame = placeInOrientation frame.orientation

placeInOrientation ::
  Orientation2d global ->
  VectorBounds2d local units ->
  VectorBounds2d global units
placeInOrientation orientation (VectorBounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Vector2d x0 y0 = Vector2d.placeInOrientation orientation (Vector2d xMid yMid)
  let Orientation2d i j = orientation
  let Direction2d ix iy = i
  let Direction2d jx jy = j
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs jx
  let ry = 0.5 *. xWidth .*. Number.abs iy .+. 0.5 *. yWidth .*. Number.abs jy
  VectorBounds2d (Bounds (x0 .-. rx) (x0 .+. rx)) (Bounds (y0 .-. ry) (y0 .+. ry))

relativeTo ::
  Frame2d global frameUnits (Defines local) ->
  VectorBounds2d global units ->
  VectorBounds2d local units
relativeTo frame = relativeToOrientation frame.orientation

relativeToOrientation ::
  Orientation2d global ->
  VectorBounds2d global units ->
  VectorBounds2d local units
relativeToOrientation orientation (VectorBounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Vector2d x0 y0 = Vector2d.relativeToOrientation orientation (Vector2d xMid yMid)
  let Orientation2d i j = orientation
  let Direction2d ix iy = i
  let Direction2d jx jy = j
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs iy
  let ry = 0.5 *. xWidth .*. Number.abs jx .+. 0.5 *. yWidth .*. Number.abs jy
  VectorBounds2d (Bounds (x0 .-. rx) (x0 .+. rx)) (Bounds (y0 .-. ry) (y0 .+. ry))

placeOn ::
  Plane3d global frameUnits (Defines local) ->
  VectorBounds2d local units ->
  VectorBounds3d global units
placeOn plane = placeOnOrientation plane.orientation

placeOnOrientation ::
  PlaneOrientation3d global ->
  VectorBounds2d local units ->
  VectorBounds3d global units
placeOnOrientation orientation (VectorBounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Vector3d x0 y0 z0 = Vector2d.placeOnOrientation orientation (Vector2d xMid yMid)
  let PlaneOrientation3d i j = orientation
  let Direction3d ix iy iz = i
  let Direction3d jx jy jz = j
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs jx
  let ry = 0.5 *. xWidth .*. Number.abs iy .+. 0.5 *. yWidth .*. Number.abs jy
  let rz = 0.5 *. xWidth .*. Number.abs iz .+. 0.5 *. yWidth .*. Number.abs jz
  VectorBounds3d
    (Bounds (x0 .-. rx) (x0 .+. rx))
    (Bounds (y0 .-. ry) (y0 .+. ry))
    (Bounds (z0 .-. rz) (z0 .+. rz))

convert ::
  Quantity (units2 ?/? units1) ->
  VectorBounds2d space units1 ->
  VectorBounds2d space units2
convert factor vectorBounds = Units.simplify (vectorBounds ?*? factor)

unconvert ::
  Quantity (units2 ?/? units1) ->
  VectorBounds2d space units2 ->
  VectorBounds2d space units1
unconvert factor vectorBounds = Units.simplify (vectorBounds ?/? factor)

transformBy ::
  Transform2d tag space units1 ->
  VectorBounds2d space units2 ->
  VectorBounds2d space units2
transformBy transform (VectorBounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Vector2d x0 y0 = Vector2d.transformBy transform (Vector2d xMid yMid)
  let Transform2d _ i j = transform
  let Vector2d ix iy = i
  let Vector2d jx jy = j
  let rx = 0.5 *. Number.abs ix .*. xWidth .+. 0.5 *. Number.abs jx .*. yWidth
  let ry = 0.5 *. Number.abs iy .*. xWidth .+. 0.5 *. Number.abs jy .*. yWidth
  VectorBounds2d (Bounds (x0 .-. rx) (x0 .+. rx)) (Bounds (y0 .-. ry) (y0 .+. ry))
