{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.VectorBounds3D
  ( VectorBounds3D
  , constant
  , coerce
  , aggregate2
  , aggregate3
  , aggregateN
  , hull2
  , hull3
  , hull4
  , hullN
  , xComponent
  , yComponent
  , zComponent
  , components
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  , maxMagnitude
  , maxMagnitude#
  , maxSquaredMagnitude
  , maxSquaredMagnitude_
  , direction
  , normalize
  , exclusion
  , inclusion
  , includes
  , contains
  , isContainedIn
  , separation
  , overlap
  , intersection
  , interpolate
  , on
  , relativeTo
  , placeIn
  , transformBy
  , rotateIn
  , rotateAround
  , tripleProduct
  )
where

import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Bounds (Bounds (Bounds, Bounds#))
import OpenSolid.Bounds qualified as Bounds
import {-# SOURCE #-} OpenSolid.DirectionBounds3D (DirectionBounds3D)
import {-# SOURCE #-} OpenSolid.DirectionBounds3D qualified as DirectionBounds3D
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3D (Axis3D)
  , Direction3D (Direction3D)
  , Frame3D
  , Plane3D (Plane3D)
  , PlaneOrientation3D (PlaneOrientation3D)
  , Vector2D (Vector2D)
  , Vector3D (Vector3D)
  , VectorBounds2D (VectorBounds2D)
  , VectorBounds3D (VectorBounds3D, VectorBounds3D#)
  )
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Transform3D (Transform3D (Transform3D))
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.World3D qualified as World3D

constant :: Vector3D units space -> VectorBounds3D units space
constant (Vector3D x y z) =
  VectorBounds3D (Bounds.constant x) (Bounds.constant y) (Bounds.constant z)

{-# INLINE coerce #-}
coerce :: VectorBounds3D units1 space1 -> VectorBounds3D units2 space2
coerce = Data.Coerce.coerce

hull2 :: Vector3D units space -> Vector3D units space -> VectorBounds3D units space
hull2 (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) =
  VectorBounds3D (Bounds x1 x2) (Bounds y1 y2) (Bounds z1 z2)

hull3 ::
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  VectorBounds3D units space
hull3 (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) (Vector3D x3 y3 z3) = do
  let minX = min (min x1 x2) x3
  let maxX = max (max x1 x2) x3
  let minY = min (min y1 y2) y3
  let maxY = max (max y1 y2) y3
  let minZ = min (min z1 z2) z3
  let maxZ = max (max z1 z2) z3
  VectorBounds3D (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)

hull4 ::
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  VectorBounds3D units space
hull4 (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) (Vector3D x3 y3 z3) (Vector3D x4 y4 z4) = do
  let minX = min (min (min x1 x2) x3) x4
  let maxX = max (max (max x1 x2) x3) x4
  let minY = min (min (min y1 y2) y3) y4
  let maxY = max (max (max y1 y2) y3) y4
  let minZ = min (min (min z1 z2) z3) z4
  let maxZ = max (max (max z1 z2) z3) z4
  VectorBounds3D (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)

hullN :: NonEmpty (Vector3D units space) -> VectorBounds3D units space
hullN (Vector3D x0 y0 z0 :| rest) = go x0 x0 y0 y0 z0 z0 rest
 where
  go ::
    Quantity units ->
    Quantity units ->
    Quantity units ->
    Quantity units ->
    Quantity units ->
    Quantity units ->
    List (Vector3D units space) ->
    VectorBounds3D units space
  go xLow xHigh yLow yHigh zLow zHigh [] =
    VectorBounds3D (Bounds xLow xHigh) (Bounds yLow yHigh) (Bounds zLow zHigh)
  go xLow xHigh yLow yHigh zLow zHigh (Vector3D x y z : remaining) =
    go (min xLow x) (max xHigh x) (min yLow y) (max yHigh y) (min zLow z) (max zHigh z) remaining

aggregate2 ::
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  VectorBounds3D units space
aggregate2 (VectorBounds3D x1 y1 z1) (VectorBounds3D x2 y2 z2) =
  VectorBounds3D (Bounds.aggregate2 x1 x2) (Bounds.aggregate2 y1 y2) (Bounds.aggregate2 z1 z2)

aggregate3 ::
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  VectorBounds3D units space
aggregate3 (VectorBounds3D x1 y1 z1) (VectorBounds3D x2 y2 z2) (VectorBounds3D x3 y3 z3) =
  VectorBounds3D
    (Bounds.aggregate3 x1 x2 x3)
    (Bounds.aggregate3 y1 y2 y3)
    (Bounds.aggregate3 z1 z2 z3)

-- | Construct a vector bounding box containing all vector bounding boxes in the given list.
aggregateN :: NonEmpty (VectorBounds3D units space) -> VectorBounds3D units space
aggregateN (first :| rest) = do
  let VectorBounds3D (Bounds xLow0 xHigh0) (Bounds yLow0 yHigh0) (Bounds zLow0 zHigh0) = first
  aggregateImpl xLow0 xHigh0 yLow0 yHigh0 zLow0 zHigh0 rest

aggregateImpl ::
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Quantity units ->
  List (VectorBounds3D units space) ->
  VectorBounds3D units space
aggregateImpl xLow xHigh yLow yHigh zLow zHigh rest = case rest of
  [] -> VectorBounds3D (Bounds xLow xHigh) (Bounds yLow yHigh) (Bounds zLow zHigh)
  next : remaining -> do
    let VectorBounds3D xNext yNext zNext = next
    let Bounds xLowNext xHighNext = xNext
    let Bounds yLowNext yHighNext = yNext
    let Bounds zLowNext zHighNext = zNext
    aggregateImpl
      (min xLow xLowNext)
      (max xHigh xHighNext)
      (min yLow yLowNext)
      (max yHigh yHighNext)
      (min zLow zLowNext)
      (max zHigh zHighNext)
      remaining

xComponent :: VectorBounds3D units space -> Bounds units
xComponent (VectorBounds3D vx _ _) = vx

yComponent :: VectorBounds3D units space -> Bounds units
yComponent (VectorBounds3D _ vy _) = vy

zComponent :: VectorBounds3D units space -> Bounds units
zComponent (VectorBounds3D _ _ vz) = vz

components :: VectorBounds3D units space -> (Bounds units, Bounds units, Bounds units)
components (VectorBounds3D vx vy vz) = (vx, vy, vz)

squaredMagnitude :: Units.Squared units1 units2 => VectorBounds3D units1 space -> Bounds units2
squaredMagnitude = Units.specialize . squaredMagnitude_

squaredMagnitude_ :: VectorBounds3D units space -> Bounds (units ?*? units)
squaredMagnitude_ (VectorBounds3D x y z) =
  Bounds.squared_ x .+. Bounds.squared_ y .+. Bounds.squared_ z

magnitude :: VectorBounds3D units space -> Bounds units
magnitude bounds = do
  let !(VectorBounds3D# xMin# xMax# yMin# yMax# zMin# zMax#) = bounds
  let positiveX# = xMin# >=# 0.0##
  let negativeX# = xMax# <=# 0.0##
  let positiveY# = yMin# >=# 0.0##
  let negativeY# = yMax# <=# 0.0##
  let positiveZ# = zMin# >=# 0.0##
  let negativeZ# = zMax# <=# 0.0##
  let minMagnitude# =
        case (# positiveX#, negativeX#, positiveY#, negativeY#, positiveZ#, negativeZ# #) of
          (# 1#, _, 1#, _, 1#, _ #) -> hypot3# xMin# yMin# zMin#
          (# 1#, _, 1#, _, _, 1# #) -> hypot3# xMin# yMin# zMax#
          (# 1#, _, _, 1#, 1#, _ #) -> hypot3# xMin# yMax# zMin#
          (# 1#, _, _, 1#, _, 1# #) -> hypot3# xMin# yMax# zMax#
          (# _, 1#, 1#, _, 1#, _ #) -> hypot3# xMax# yMin# zMin#
          (# _, 1#, 1#, _, _, 1# #) -> hypot3# xMax# yMin# zMax#
          (# _, 1#, _, 1#, 1#, _ #) -> hypot3# xMax# yMax# zMin#
          (# _, 1#, _, 1#, _, 1# #) -> hypot3# xMax# yMax# zMax#
          (# _, _, 1#, _, 1#, _ #) -> hypot2# yMin# zMin#
          (# _, _, 1#, _, _, 1# #) -> hypot2# yMin# zMax#
          (# _, _, _, 1#, 1#, _ #) -> hypot2# yMax# zMin#
          (# _, _, _, 1#, _, 1# #) -> hypot2# yMax# zMax#
          (# 1#, _, _, _, 1#, _ #) -> hypot2# xMin# zMin#
          (# 1#, _, _, _, _, 1# #) -> hypot2# xMin# zMax#
          (# _, 1#, _, _, 1#, _ #) -> hypot2# xMax# zMin#
          (# _, 1#, _, _, _, 1# #) -> hypot2# xMax# zMax#
          (# 1#, _, 1#, _, _, _ #) -> hypot2# xMin# yMin#
          (# 1#, _, _, 1#, _, _ #) -> hypot2# xMin# yMax#
          (# _, 1#, 1#, _, _, _ #) -> hypot2# xMax# yMin#
          (# _, 1#, _, 1#, _, _ #) -> hypot2# xMax# yMax#
          (# 1#, _, _, _, _, _ #) -> xMin#
          (# _, 1#, _, _, _, _ #) -> negate# xMax#
          (# _, _, 1#, _, _, _ #) -> yMin#
          (# _, _, _, 1#, _, _ #) -> negate# yMax#
          (# _, _, _, _, 1#, _ #) -> zMin#
          (# _, _, _, _, _, 1# #) -> negate# zMax#
          (# _, _, _, _, _, _ #) -> 0.0##
  Bounds# minMagnitude# (maxMagnitude# bounds)

maxMagnitude :: VectorBounds3D units space -> Quantity units
maxMagnitude bounds = Quantity# (maxMagnitude# bounds)

maxMagnitude# :: VectorBounds3D units space -> Double#
maxMagnitude# (VectorBounds3D# minX# maxX# minY# maxY# minZ# maxZ#) = do
  let xMagnitude# = max# (abs# minX#) (abs# maxX#)
  let yMagnitude# = max# (abs# minY#) (abs# maxY#)
  let zMagnitude# = max# (abs# minZ#) (abs# maxZ#)
  hypot3# xMagnitude# yMagnitude# zMagnitude#

maxSquaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorBounds3D units1 space ->
  Quantity units2
maxSquaredMagnitude = Units.specialize . maxSquaredMagnitude_

maxSquaredMagnitude_ :: VectorBounds3D units space -> Quantity (units ?*? units)
maxSquaredMagnitude_ (VectorBounds3D (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)) = do
  let xMagnitude = max (Quantity.abs minX) (Quantity.abs maxX)
  let yMagnitude = max (Quantity.abs minY) (Quantity.abs maxY)
  let zMagnitude = max (Quantity.abs minZ) (Quantity.abs maxZ)
  Quantity.squared_ xMagnitude .+. Quantity.squared_ yMagnitude .+. Quantity.squared_ zMagnitude

direction :: VectorBounds3D units space -> DirectionBounds3D space
direction vectorBounds = DirectionBounds3D.unsafe (normalize vectorBounds)

normalize :: VectorBounds3D units space -> VectorBounds3D Unitless space
normalize vectorBounds = do
  let VectorBounds3D x y z = vectorBounds ./. magnitude vectorBounds
  let nx = clampNormalized x
  let ny = clampNormalized y
  let nz = clampNormalized z
  VectorBounds3D nx ny nz

normalizedBounds :: Bounds Unitless
normalizedBounds = Bounds -1 1

clampNormalized :: Bounds Unitless -> Bounds Unitless
clampNormalized (Bounds low high) =
  Bounds (Quantity.clampTo normalizedBounds low) (Quantity.clampTo normalizedBounds high)

exclusion :: Vector3D units space -> VectorBounds3D units space -> Quantity units
exclusion (Vector3D x y z) (VectorBounds3D bx by bz) = do
  let exclusionX = Bounds.exclusion x bx
  let exclusionY = Bounds.exclusion y by
  let exclusionZ = Bounds.exclusion z bz
  let positiveX = exclusionX >= Quantity.zero
  let positiveY = exclusionY >= Quantity.zero
  let positiveZ = exclusionZ >= Quantity.zero
  if
    | positiveX && positiveY && positiveZ -> Quantity.hypot3 exclusionX exclusionY exclusionZ
    | positiveX && positiveY -> Quantity.hypot2 exclusionX exclusionY
    | positiveX && positiveZ -> Quantity.hypot2 exclusionX exclusionZ
    | positiveY && positiveZ -> Quantity.hypot2 exclusionY exclusionZ
    | positiveX -> exclusionX
    | positiveY -> exclusionY
    | positiveZ -> exclusionZ
    | otherwise -> max (max exclusionX exclusionY) exclusionZ

inclusion :: Vector3D units space -> VectorBounds3D units space -> Quantity units
inclusion point box = negative (exclusion point box)

includes :: Vector3D units space -> VectorBounds3D units space -> Bool
includes (Vector3D vx vy vz) (VectorBounds3D x y z) =
  Bounds.includes vx x && Bounds.includes vy y && Bounds.includes vz z

contains :: VectorBounds3D units space -> VectorBounds3D units space -> Bool
contains (VectorBounds3D x2 y2 z2) (VectorBounds3D x1 y1 z1) =
  Bounds.contains x2 x1 && Bounds.contains y2 y1 && Bounds.contains z2 z1

isContainedIn :: VectorBounds3D units space -> VectorBounds3D units space -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

separation :: VectorBounds3D units space -> VectorBounds3D units space -> Quantity units
separation (VectorBounds3D x1 y1 z1) (VectorBounds3D x2 y2 z2) = do
  let separationX = Bounds.separation x1 x2
  let separationY = Bounds.separation y1 y2
  let separationZ = Bounds.separation z1 z2
  let positiveX = separationX >= Quantity.zero
  let positiveY = separationY >= Quantity.zero
  let positiveZ = separationZ >= Quantity.zero
  if
    | positiveX && positiveY && positiveZ -> Quantity.hypot3 separationX separationY separationZ
    | positiveX && positiveY -> Quantity.hypot2 separationX separationY
    | positiveX && positiveZ -> Quantity.hypot2 separationX separationZ
    | positiveY && positiveZ -> Quantity.hypot2 separationY separationZ
    | positiveX -> separationX
    | positiveY -> separationY
    | positiveZ -> separationZ
    | otherwise -> max (max separationX separationY) separationZ

overlap :: VectorBounds3D units space -> VectorBounds3D units space -> Quantity units
overlap first second = negative (separation first second)

intersection ::
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  Maybe (VectorBounds3D units space)
intersection (VectorBounds3D x1 y1 z1) (VectorBounds3D x2 y2 z2) = do
  x <- Bounds.intersection x1 x2
  y <- Bounds.intersection y1 y2
  z <- Bounds.intersection z1 z2
  Just (VectorBounds3D x y z)

interpolate ::
  VectorBounds3D units space ->
  Number ->
  Number ->
  Number ->
  Vector3D units space
interpolate (VectorBounds3D x y z) u v w =
  Vector3D (Bounds.interpolate x u) (Bounds.interpolate y v) (Bounds.interpolate z w)

on :: Plane3D global local -> VectorBounds2D units local -> VectorBounds3D units global
on plane bounds2D = do
  let VectorBounds2D bX bY = bounds2D
  let cX = Bounds.midpoint bX
  let cY = Bounds.midpoint bY
  let rX = 0.5 *. Bounds.width bX
  let rY = 0.5 *. Bounds.width bY
  let Plane3D _ (PlaneOrientation3D i j) = plane
  let Direction3D iR iF iU = i
  let Direction3D jR jF jU = j
  let Vector3D cR cF cU = Vector3D.on plane (Vector2D cX cY)
  let rR = rX .*. Number.abs iR .+. rY .*. Number.abs jR
  let rF = rX .*. Number.abs iF .+. rY .*. Number.abs jF
  let rU = rX .*. Number.abs iU .+. rY .*. Number.abs jU
  let bR = Bounds (cR .-. rR) (cR .+. rR)
  let bF = Bounds (cF .-. rF) (cF .+. rF)
  let bU = Bounds (cU .-. rU) (cU .+. rU)
  VectorBounds3D bR bF bU

placeIn :: Frame3D global local -> VectorBounds3D units local -> VectorBounds3D units global
placeIn frame (VectorBounds3D vR vF vU) = do
  let cR = Bounds.midpoint vR
  let cF = Bounds.midpoint vF
  let cU = Bounds.midpoint vU
  let rR = 0.5 *. Bounds.width vR
  let rF = 0.5 *. Bounds.width vF
  let rU = 0.5 *. Bounds.width vU
  let Vector3D cR' cF' cU' = Vector3D.placeIn frame (Vector3D cR cF cU)
  let Direction3D iR iF iU = frame.rightwardDirection
  let Direction3D jR jF jU = frame.forwardDirection
  let Direction3D kR kF kU = frame.upwardDirection
  let rR' = rR .*. Number.abs iR .+. rF .*. Number.abs jR .+. rU .*. Number.abs kR
  let rF' = rR .*. Number.abs iF .+. rF .*. Number.abs jF .+. rU .*. Number.abs kF
  let rU' = rR .*. Number.abs iU .+. rF .*. Number.abs jU .+. rU .*. Number.abs kU
  VectorBounds3D
    (Bounds (cR' .-. rR') (cR' .+. rR'))
    (Bounds (cF' .-. rF') (cF' .+. rF'))
    (Bounds (cU' .-. rU') (cU' .+. rU'))

relativeTo :: Frame3D global local -> VectorBounds3D units global -> VectorBounds3D units local
relativeTo frame (VectorBounds3D vR vF vU) = do
  let cR = Bounds.midpoint vR
  let cF = Bounds.midpoint vF
  let cU = Bounds.midpoint vU
  let rR = 0.5 *. Bounds.width vR
  let rF = 0.5 *. Bounds.width vF
  let rU = 0.5 *. Bounds.width vU
  let Vector3D cR' cF' cU' = Vector3D.relativeTo frame (Vector3D cR cF cU)
  let Direction3D iR iF iU = frame.rightwardDirection
  let Direction3D jR jF jU = frame.forwardDirection
  let Direction3D kR kF kU = frame.upwardDirection
  let rR' = rR .*. Number.abs iR .+. rF .*. Number.abs iF .+. rU .*. Number.abs iU
  let rF' = rR .*. Number.abs jR .+. rF .*. Number.abs jF .+. rU .*. Number.abs jU
  let rU' = rR .*. Number.abs kR .+. rF .*. Number.abs kF .+. rU .*. Number.abs kU
  VectorBounds3D
    (Bounds (cR' .-. rR') (cR' .+. rR'))
    (Bounds (cF' .-. rF') (cF' .+. rF'))
    (Bounds (cU' .-. rU') (cU' .+. rU'))

transformBy :: Transform3D tag space -> VectorBounds3D units space -> VectorBounds3D units space
transformBy transform (VectorBounds3D vR vF vU) = do
  let cR = Bounds.midpoint vR
  let cF = Bounds.midpoint vF
  let cU = Bounds.midpoint vU
  let rR = 0.5 *. Bounds.width vR
  let rF = 0.5 *. Bounds.width vF
  let rU = 0.5 *. Bounds.width vU
  let Vector3D cR' cF' cU' = Vector3D.transformBy transform (Vector3D cR cF cU)
  let Transform3D _ i j k = transform
  let Vector3D iR iF iU = i
  let Vector3D jR jF jU = j
  let Vector3D kR kF kU = k
  let rR' = Number.abs iR .*. rR .+. Number.abs jR .*. rF .+. Number.abs kR .*. rU
  let rF' = Number.abs iF .*. rR .+. Number.abs jF .*. rF .+. Number.abs kF .*. rU
  let rU' = Number.abs iU .*. rR .+. Number.abs jU .*. rF .+. Number.abs kU .*. rU
  VectorBounds3D
    (Bounds (cR' .-. rR') (cR' .+. rR'))
    (Bounds (cF' .-. rF') (cF' .+. rF'))
    (Bounds (cU' .-. rU') (cU' .+. rU'))

rotateIn ::
  Direction3D space ->
  Angle ->
  VectorBounds3D units space ->
  VectorBounds3D units space
rotateIn axisDirection = rotateAround (Axis3D World3D.originPoint axisDirection)

rotateAround :: Axis3D space -> Angle -> VectorBounds3D units space -> VectorBounds3D units space
rotateAround = Transform3D.rotateAroundImpl transformBy

tripleProduct ::
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  Bounds ((units ?*? units) ?*? units)
tripleProduct bounds1 bounds2 bounds3 = do
  let !(VectorBounds3D# xMin1# xMax1# yMin1# yMax1# zMin1# zMax1#) = bounds1
  let !(VectorBounds3D# xMin2# xMax2# yMin2# yMax2# zMin2# zMax2#) = bounds2
  let !(VectorBounds3D# xMin3# xMax3# yMin3# yMax3# zMin3# zMax3#) = bounds3
  let !(# low#, high# #) =
        determinantBounds3D#
          xMin1#
          xMax1#
          yMin1#
          yMax1#
          zMin1#
          zMax1#
          xMin2#
          xMax2#
          yMin2#
          yMax2#
          zMin2#
          zMax2#
          xMin3#
          xMax3#
          yMin3#
          yMax3#
          zMin3#
          zMax3#
  Bounds# low# high#
