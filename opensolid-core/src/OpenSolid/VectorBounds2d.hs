module OpenSolid.VectorBounds2d
  ( VectorBounds2d (VectorBounds2d)
  , constant
  , coerce
  , aggregate2
  , aggregate3
  , hull2
  , hull3
  , hull4
  , hullN
  , polar
  , xComponent
  , yComponent
  , components
  , squaredMagnitude
  , squaredMagnitude'
  , magnitude
  , maxMagnitude
  , maxSquaredMagnitude
  , maxSquaredMagnitude'
  , normalize
  , includes
  , contains
  , isContainedIn
  , interpolate
  , relativeTo
  , placeIn
  , on
  , convert
  , unconvert
  , transformBy
  )
where

import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Float qualified as Float
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction2d (Direction2d)
  , Orientation2d (Orientation2d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , VectorBounds2d (VectorBounds2d)
  , VectorBounds3d
  )
import OpenSolid.Qty qualified as Qty
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d

constant :: Vector2d (space @ units) -> VectorBounds2d (space @ units)
constant (Vector2d x y) = VectorBounds2d (Bounds.constant x) (Bounds.constant y)

{-# INLINE coerce #-}
coerce :: VectorBounds2d (space1 @ units1) -> VectorBounds2d (space2 @ units2)
coerce (VectorBounds2d x y) = VectorBounds2d (Bounds.coerce x) (Bounds.coerce y)

hull2 :: Vector2d (space @ units) -> Vector2d (space @ units) -> VectorBounds2d (space @ units)
hull2 (Vector2d x1 y1) (Vector2d x2 y2) =
  VectorBounds2d (Bounds x1 x2) (Bounds y1 y2)

hull3 ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorBounds2d (space @ units)
hull3 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) = do
  let minX = Qty.min (Qty.min x1 x2) x3
  let maxX = Qty.max (Qty.max x1 x2) x3
  let minY = Qty.min (Qty.min y1 y2) y3
  let maxY = Qty.max (Qty.max y1 y2) y3
  VectorBounds2d (Bounds minX maxX) (Bounds minY maxY)

hull4 ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorBounds2d (space @ units)
hull4 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) (Vector2d x4 y4) = do
  let minX = Qty.min (Qty.min (Qty.min x1 x2) x3) x4
  let maxX = Qty.max (Qty.max (Qty.max x1 x2) x3) x4
  let minY = Qty.min (Qty.min (Qty.min y1 y2) y3) y4
  let maxY = Qty.max (Qty.max (Qty.max y1 y2) y3) y4
  VectorBounds2d (Bounds minX maxX) (Bounds minY maxY)

hullN :: NonEmpty (Vector2d (space @ units)) -> VectorBounds2d (space @ units)
hullN (Vector2d x0 y0 :| rest) = go x0 x0 y0 y0 rest
 where
  go :: Qty units -> Qty units -> Qty units -> Qty units -> List (Vector2d (space @ units)) -> VectorBounds2d (space @ units)
  go xLow xHigh yLow yHigh [] = VectorBounds2d (Bounds xLow xHigh) (Bounds yLow yHigh)
  go xLow xHigh yLow yHigh (Vector2d x y : remaining) =
    go (Qty.min xLow x) (Qty.max xHigh x) (Qty.min yLow y) (Qty.max yHigh y) remaining

aggregate2 ::
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units)
aggregate2 (VectorBounds2d x1 y1) (VectorBounds2d x2 y2) =
  VectorBounds2d (Bounds.aggregate2 x1 x2) (Bounds.aggregate2 y1 y2)

aggregate3 ::
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units)
aggregate3 (VectorBounds2d x1 y1) (VectorBounds2d x2 y2) (VectorBounds2d x3 y3) =
  VectorBounds2d (Bounds.aggregate3 x1 x2 x3) (Bounds.aggregate3 y1 y2 y3)

polar :: Bounds units -> Bounds Radians -> VectorBounds2d (space @ units)
polar r theta = VectorBounds2d (r * Bounds.cos theta) (r * Bounds.sin theta)

xComponent :: VectorBounds2d (space @ units) -> Bounds units
xComponent (VectorBounds2d vx _) = vx

yComponent :: VectorBounds2d (space @ units) -> Bounds units
yComponent (VectorBounds2d _ vy) = vy

components :: VectorBounds2d (space @ units) -> (Bounds units, Bounds units)
components (VectorBounds2d vx vy) = (vx, vy)

squaredMagnitude :: Units.Squared units1 units2 => VectorBounds2d (space @ units1) -> Bounds units2
squaredMagnitude = Units.specialize . squaredMagnitude'

squaredMagnitude' :: VectorBounds2d (space @ units) -> Bounds (units :*: units)
squaredMagnitude' (VectorBounds2d x y) = Bounds.squared' x + Bounds.squared' y

magnitude :: VectorBounds2d (space @ units) -> Bounds units
magnitude (VectorBounds2d x y) = Bounds.hypot2 x y

maxMagnitude :: VectorBounds2d (space @ units) -> Qty units
maxMagnitude (VectorBounds2d (Bounds minX maxX) (Bounds minY maxY)) = do
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
  let yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
  Qty.hypot2 xMagnitude yMagnitude

maxSquaredMagnitude :: Units.Squared units1 units2 => VectorBounds2d (space @ units1) -> Qty units2
maxSquaredMagnitude = Units.specialize . maxSquaredMagnitude'

maxSquaredMagnitude' :: VectorBounds2d (space @ units) -> Qty (units :*: units)
maxSquaredMagnitude' (VectorBounds2d (Bounds minX maxX) (Bounds minY maxY)) = do
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
  let yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
  Qty.squared' xMagnitude + Qty.squared' yMagnitude

normalize :: VectorBounds2d (space @ units) -> VectorBounds2d (space @ Unitless)
normalize vectorBounds = do
  let (VectorBounds2d x y) = vectorBounds / magnitude vectorBounds
  let nx = clampNormalized x
  let ny = clampNormalized y
  VectorBounds2d nx ny

normalizedBounds :: Bounds Unitless
normalizedBounds = Bounds -1.0 1.0

clampNormalized :: Bounds Unitless -> Bounds Unitless
clampNormalized (Bounds low high) =
  Bounds (Qty.clampTo normalizedBounds low) (Qty.clampTo normalizedBounds high)

includes :: Vector2d (space @ units) -> VectorBounds2d (space @ units) -> Bool
includes (Vector2d vx vy) (VectorBounds2d x y) = Bounds.includes vx x && Bounds.includes vy y

contains :: VectorBounds2d (space @ units) -> VectorBounds2d (space @ units) -> Bool
contains (VectorBounds2d x2 y2) (VectorBounds2d x1 y1) =
  Bounds.contains x2 x1 && Bounds.contains y2 y1

isContainedIn :: VectorBounds2d (space @ units) -> VectorBounds2d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

interpolate :: VectorBounds2d (space @ units) -> Float -> Float -> Vector2d (space @ units)
interpolate (VectorBounds2d x y) u v =
  Vector2d.xy (Bounds.interpolate x u) (Bounds.interpolate y v)

placeIn ::
  Orientation2d global (Defines local) ->
  VectorBounds2d (local @ units) ->
  VectorBounds2d (global @ units)
placeIn orientation (VectorBounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Vector2d x0 y0 = Vector2d.placeIn orientation (Vector2d xMid yMid)
  let Orientation2d i j = orientation
  let Direction2d ix iy = i
  let Direction2d jx jy = j
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs jx
  let ry = 0.5 * xWidth * Float.abs iy + 0.5 * yWidth * Float.abs jy
  VectorBounds2d (Bounds (x0 - rx) (x0 + rx)) (Bounds (y0 - ry) (y0 + ry))

relativeTo ::
  Orientation2d global (Defines local) ->
  VectorBounds2d (global @ units) ->
  VectorBounds2d (local @ units)
relativeTo orientation (VectorBounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Vector2d x0 y0 = Vector2d.relativeTo orientation (Vector2d xMid yMid)
  let Orientation2d i j = orientation
  let Direction2d ix iy = i
  let Direction2d jx jy = j
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs iy
  let ry = 0.5 * xWidth * Float.abs jx + 0.5 * yWidth * Float.abs jy
  VectorBounds2d (Bounds (x0 - rx) (x0 + rx)) (Bounds (y0 - ry) (y0 + ry))

on ::
  PlaneOrientation3d space (Defines local) ->
  VectorBounds2d (local @ units) ->
  VectorBounds3d (space @ units)
on (PlaneOrientation3d i j) (VectorBounds2d x y) = x * i + y * j

convert ::
  Qty (units2 :/: units1) ->
  VectorBounds2d (space @ units1) ->
  VectorBounds2d (space @ units2)
convert factor vectorBounds = vectorBounds !* factor

unconvert ::
  Qty (units2 :/: units1) ->
  VectorBounds2d (space @ units2) ->
  VectorBounds2d (space @ units1)
unconvert factor vectorBounds = vectorBounds !/ factor

transformBy ::
  Transform2d tag (space @ units1) ->
  VectorBounds2d (space @ units2) ->
  VectorBounds2d (space @ units2)
transformBy transform (VectorBounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let (x0, y0) = Vector2d.components (Vector2d.transformBy transform (Vector2d.xy xMid yMid))
  let (Transform2d _ i j) = transform
  let (ix, iy) = Vector2d.components i
  let (jx, jy) = Vector2d.components j
  let rx = 0.5 * Float.abs ix * xWidth + 0.5 * Float.abs jx * yWidth
  let ry = 0.5 * Float.abs iy * xWidth + 0.5 * Float.abs jy * yWidth
  VectorBounds2d (Bounds (x0 - rx) (x0 + rx)) (Bounds (y0 - ry) (y0 + ry))
