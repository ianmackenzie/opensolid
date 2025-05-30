module OpenSolid.VectorBounds3d
  ( VectorBounds3d
  , constant
  , rightwardForwardUpward
  , coerce
  , aggregate2
  , aggregate3
  , hull2
  , hull3
  , hull4
  , hullN
  , xComponent
  , yComponent
  , zComponent
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
  , transformBy
  , rotateIn
  , rotateAround
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Float qualified as Float
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3d (Axis3d)
  , Direction3d (Direction3d)
  , Vector3d (Vector3d)
  , VectorBounds3d (VectorBounds3d)
  )
import OpenSolid.Qty qualified as Qty
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d qualified as Vector3d

constant :: Vector3d (space @ units) -> VectorBounds3d (space @ units)
constant (Vector3d x y z) =
  VectorBounds3d (Bounds.constant x) (Bounds.constant y) (Bounds.constant z)

{-# INLINE coerce #-}
coerce :: VectorBounds3d (space1 @ units1) -> VectorBounds3d (space2 @ units2)
coerce (VectorBounds3d x y z) = VectorBounds3d (Bounds.coerce x) (Bounds.coerce y) (Bounds.coerce z)

{-# INLINE rightwardForwardUpward #-}
rightwardForwardUpward ::
  Bounds units ->
  Bounds units ->
  Bounds units ->
  VectorBounds3d (space @ units)
rightwardForwardUpward = VectorBounds3d

hull2 :: Vector3d (space @ units) -> Vector3d (space @ units) -> VectorBounds3d (space @ units)
hull2 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) =
  VectorBounds3d (Bounds x1 x2) (Bounds y1 y2) (Bounds z1 z2)

hull3 ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  VectorBounds3d (space @ units)
hull3 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) = do
  let minX = Qty.min (Qty.min x1 x2) x3
  let maxX = Qty.max (Qty.max x1 x2) x3
  let minY = Qty.min (Qty.min y1 y2) y3
  let maxY = Qty.max (Qty.max y1 y2) y3
  let minZ = Qty.min (Qty.min z1 z2) z3
  let maxZ = Qty.max (Qty.max z1 z2) z3
  VectorBounds3d (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)

hull4 ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  VectorBounds3d (space @ units)
hull4 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) (Vector3d x4 y4 z4) = do
  let minX = Qty.min (Qty.min (Qty.min x1 x2) x3) x4
  let maxX = Qty.max (Qty.max (Qty.max x1 x2) x3) x4
  let minY = Qty.min (Qty.min (Qty.min y1 y2) y3) y4
  let maxY = Qty.max (Qty.max (Qty.max y1 y2) y3) y4
  let minZ = Qty.min (Qty.min (Qty.min z1 z2) z3) z4
  let maxZ = Qty.max (Qty.max (Qty.max z1 z2) z3) z4
  VectorBounds3d (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)

hullN :: NonEmpty (Vector3d (space @ units)) -> VectorBounds3d (space @ units)
hullN (Vector3d x0 y0 z0 :| rest) = go x0 x0 y0 y0 z0 z0 rest
 where
  go :: Qty units -> Qty units -> Qty units -> Qty units -> Qty units -> Qty units -> List (Vector3d (space @ units)) -> VectorBounds3d (space @ units)
  go xLow xHigh yLow yHigh zLow zHigh [] = VectorBounds3d (Bounds xLow xHigh) (Bounds yLow yHigh) (Bounds zLow zHigh)
  go xLow xHigh yLow yHigh zLow zHigh (Vector3d x y z : remaining) =
    go (Qty.min xLow x) (Qty.max xHigh x) (Qty.min yLow y) (Qty.max yHigh y) (Qty.min zLow z) (Qty.max zHigh z) remaining

aggregate2 ::
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units)
aggregate2 (VectorBounds3d x1 y1 z1) (VectorBounds3d x2 y2 z2) =
  VectorBounds3d (Bounds.aggregate2 x1 x2) (Bounds.aggregate2 y1 y2) (Bounds.aggregate2 z1 z2)

aggregate3 ::
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units)
aggregate3 (VectorBounds3d x1 y1 z1) (VectorBounds3d x2 y2 z2) (VectorBounds3d x3 y3 z3) =
  VectorBounds3d (Bounds.aggregate3 x1 x2 x3) (Bounds.aggregate3 y1 y2 y3) (Bounds.aggregate3 z1 z2 z3)

xComponent :: VectorBounds3d (space @ units) -> Bounds units
xComponent (VectorBounds3d vx _ _) = vx

yComponent :: VectorBounds3d (space @ units) -> Bounds units
yComponent (VectorBounds3d _ vy _) = vy

zComponent :: VectorBounds3d (space @ units) -> Bounds units
zComponent (VectorBounds3d _ _ vz) = vz

components :: VectorBounds3d (space @ units) -> (Bounds units, Bounds units, Bounds units)
components (VectorBounds3d vx vy vz) = (vx, vy, vz)

squaredMagnitude :: Units.Squared units1 units2 => VectorBounds3d (space @ units1) -> Bounds units2
squaredMagnitude = Units.specialize . squaredMagnitude'

squaredMagnitude' :: VectorBounds3d (space @ units) -> Bounds (units :*: units)
squaredMagnitude' (VectorBounds3d x y z) = Bounds.squared' x + Bounds.squared' y + Bounds.squared' z

magnitude :: VectorBounds3d (space @ units) -> Bounds units
magnitude (VectorBounds3d x y z) = Bounds.hypot3 x y z

maxMagnitude :: VectorBounds3d (space @ units) -> Qty units
maxMagnitude (VectorBounds3d (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)) = do
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
  let yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
  let zMagnitude = Qty.max (Qty.abs minZ) (Qty.abs maxZ)
  Qty.hypot3 xMagnitude yMagnitude zMagnitude

maxSquaredMagnitude :: Units.Squared units1 units2 => VectorBounds3d (space @ units1) -> Qty units2
maxSquaredMagnitude = Units.specialize . maxSquaredMagnitude'

maxSquaredMagnitude' :: VectorBounds3d (space @ units) -> Qty (units :*: units)
maxSquaredMagnitude' (VectorBounds3d (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)) = do
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
  let yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
  let zMagnitude = Qty.max (Qty.abs minZ) (Qty.abs maxZ)
  Qty.squared' xMagnitude + Qty.squared' yMagnitude + Qty.squared' zMagnitude

normalize :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ Unitless)
normalize vectorBounds = do
  let (VectorBounds3d x y z) = vectorBounds / magnitude vectorBounds
  let nx = clampNormalized x
  let ny = clampNormalized y
  let nz = clampNormalized z
  VectorBounds3d nx ny nz

normalizedBounds :: Bounds Unitless
normalizedBounds = Bounds -1.0 1.0

clampNormalized :: Bounds Unitless -> Bounds Unitless
clampNormalized (Bounds low high) =
  Bounds (Qty.clampTo normalizedBounds low) (Qty.clampTo normalizedBounds high)

includes :: Vector3d (space @ units) -> VectorBounds3d (space @ units) -> Bool
includes (Vector3d vx vy vz) (VectorBounds3d x y z) =
  Bounds.includes vx x && Bounds.includes vy y && Bounds.includes vz z

contains :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ units) -> Bool
contains (VectorBounds3d x2 y2 z2) (VectorBounds3d x1 y1 z1) =
  Bounds.contains x2 x1 && Bounds.contains y2 y1 && Bounds.contains z2 z1

isContainedIn :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

interpolate :: VectorBounds3d (space @ units) -> Float -> Float -> Float -> Vector3d (space @ units)
interpolate (VectorBounds3d x y z) u v w =
  Vector3d (Bounds.interpolate x u) (Bounds.interpolate y v) (Bounds.interpolate z w)

placeIn ::
  Basis3d global (Defines local) ->
  VectorBounds3d (local @ units) ->
  VectorBounds3d (global @ units)
placeIn basis (VectorBounds3d vR vF vU) = do
  let cR = Bounds.midpoint vR
  let cF = Bounds.midpoint vF
  let cU = Bounds.midpoint vU
  let rR = 0.5 * Bounds.width vR
  let rF = 0.5 * Bounds.width vF
  let rU = 0.5 * Bounds.width vU
  let Vector3d cR' cF' cU' = Vector3d.placeIn basis (Vector3d cR cF cU)
  let Direction3d iR iF iU = Basis3d.rightwardDirection basis
  let Direction3d jR jF jU = Basis3d.forwardDirection basis
  let Direction3d kR kF kU = Basis3d.upwardDirection basis
  let rR' = rR * Float.abs iR + rF * Float.abs jR + rU * Float.abs kR
  let rF' = rR * Float.abs iF + rF * Float.abs jF + rU * Float.abs kF
  let rU' = rR * Float.abs iU + rF * Float.abs jU + rU * Float.abs kU
  VectorBounds3d
    # Bounds (cR' - rR') (cR' + rR')
    # Bounds (cF' - rF') (cF' + rF')
    # Bounds (cU' - rU') (cU' + rU')

relativeTo ::
  Basis3d global (Defines local) ->
  VectorBounds3d (global @ units) ->
  VectorBounds3d (local @ units)
relativeTo basis (VectorBounds3d vR vF vU) = do
  let cR = Bounds.midpoint vR
  let cF = Bounds.midpoint vF
  let cU = Bounds.midpoint vU
  let rR = 0.5 * Bounds.width vR
  let rF = 0.5 * Bounds.width vF
  let rU = 0.5 * Bounds.width vU
  let Vector3d cR' cF' cU' = Vector3d.relativeTo basis (Vector3d cR cF cU)
  let Direction3d iR iF iU = Basis3d.rightwardDirection basis
  let Direction3d jR jF jU = Basis3d.forwardDirection basis
  let Direction3d kR kF kU = Basis3d.upwardDirection basis
  let rR' = rR * Float.abs iR + rF * Float.abs iF + rU * Float.abs iU
  let rF' = rR * Float.abs jR + rF * Float.abs jF + rU * Float.abs jU
  let rU' = rR * Float.abs kR + rF * Float.abs kF + rU * Float.abs kU
  VectorBounds3d
    # Bounds (cR' - rR') (cR' + rR')
    # Bounds (cF' - rF') (cF' + rF')
    # Bounds (cU' - rU') (cU' + rU')

transformBy ::
  Transform3d tag (space @ units1) ->
  VectorBounds3d (space @ units2) ->
  VectorBounds3d (space @ units2)
transformBy transform (VectorBounds3d vR vF vU) = do
  let cR = Bounds.midpoint vR
  let cF = Bounds.midpoint vF
  let cU = Bounds.midpoint vU
  let rR = 0.5 * Bounds.width vR
  let rF = 0.5 * Bounds.width vF
  let rU = 0.5 * Bounds.width vU
  let Vector3d cR' cF' cU' = Vector3d.transformBy transform (Vector3d cR cF cU)
  let (Transform3d _ i j k) = transform
  let Vector3d iR iF iU = i
  let Vector3d jR jF jU = j
  let Vector3d kR kF kU = k
  let rR' = Float.abs iR * rR + Float.abs jR * rF + Float.abs kR * rU
  let rF' = Float.abs iF * rR + Float.abs jF * rF + Float.abs kF * rU
  let rU' = Float.abs iU * rR + Float.abs jU * rF + Float.abs kU * rU
  VectorBounds3d
    # Bounds (cR' - rR') (cR' + rR')
    # Bounds (cF' - rF') (cF' + rF')
    # Bounds (cU' - rU') (cU' + rU')

rotateIn ::
  Direction3d space ->
  Angle ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units)
rotateIn axisDirection = rotateAround (Axis3d Point3d.origin axisDirection)

rotateAround ::
  Axis3d (space @ axisUnits) ->
  Angle ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units)
rotateAround = Transform3d.rotateAroundImpl transformBy
