{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.VectorBounds3D
  ( VectorBounds3D
  , constant
  , coerce
  , erase
  , unerase
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
  , center
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  , maxMagnitude
  , maxMagnitude#
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
  , placeIn
  , transformBy
  , rotateIn
  , rotateAround
  , tripleProduct
  , tripleProduct_
  )
where

import Data.Coerce qualified
import OpenSolid.Angle (Angle)
import {-# SOURCE #-} OpenSolid.DirectionBounds3D (DirectionBounds3D)
import {-# SOURCE #-} OpenSolid.DirectionBounds3D qualified as DirectionBounds3D
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval (I#, Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3D
  , Direction3D (Direction3D)
  , Frame3D
  , Vector3D (V3D#, Vector3D)
  , VectorBounds3D (VB3D#, VectorBounds3D)
  , VectorTransform3D (VectorTransform3D)
  )
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorTransform3D qualified as VectorTransform3D

constant :: Vector3D units space -> VectorBounds3D units space
constant (Vector3D x y z) =
  VectorBounds3D (Interval.constant x) (Interval.constant y) (Interval.constant z)

{-# INLINE coerce #-}
coerce :: VectorBounds3D units1 space1 -> VectorBounds3D units2 space2
coerce = Data.Coerce.coerce

{-# INLINE erase #-}
erase :: VectorBounds3D units space -> VectorBounds3D Unitless Void
erase = coerce

{-# INLINE unerase #-}
unerase :: VectorBounds3D Unitless Void -> VectorBounds3D units space
unerase = coerce

hull2 :: Vector3D units space -> Vector3D units space -> VectorBounds3D units space
hull2 (Vector3D x1 y1 z1) (Vector3D x2 y2 z2) =
  VectorBounds3D (Interval x1 x2) (Interval y1 y2) (Interval z1 z2)

hull3 ::
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  VectorBounds3D units space
hull3 (V3D# x1# y1# z1#) (V3D# x2# y2# z2#) (V3D# x3# y3# z3#) =
  VB3D# (VectorBounds3D# (hull3# x1# x2# x3#) (hull3# y1# y2# y3#) (hull3# z1# z2# z3#))

hull4 ::
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  Vector3D units space ->
  VectorBounds3D units space
hull4 (V3D# x1# y1# z1#) (V3D# x2# y2# z2#) (V3D# x3# y3# z3#) (V3D# x4# y4# z4#) =
  VB3D# $
    VectorBounds3D#
      (hull4# x1# x2# x3# x4#)
      (hull4# y1# y2# y3# y4#)
      (hull4# z1# z2# z3# z4#)

hullN :: NonEmpty (Vector3D units space) -> VectorBounds3D units space
hullN (V3D# x# y# z# :| rest) = hullImpl x# x# y# y# z# z# rest

hullImpl ::
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  List (Vector3D units space) ->
  VectorBounds3D units space
hullImpl xLow# xHigh# yLow# yHigh# zLow# zHigh# vectors = case vectors of
  [] ->
    VB3D# $
      VectorBounds3D#
        (Interval# xLow# xHigh#)
        (Interval# yLow# yHigh#)
        (Interval# zLow# zHigh#)
  V3D# x# y# z# : remaining ->
    hullImpl
      (min# xLow# x#)
      (max# xHigh# x#)
      (min# yLow# y#)
      (max# yHigh# y#)
      (min# zLow# z#)
      (max# zHigh# z#)
      remaining

aggregate2 ::
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  VectorBounds3D units space
aggregate2 (VectorBounds3D x1 y1 z1) (VectorBounds3D x2 y2 z2) =
  VectorBounds3D (Interval.aggregate2 x1 x2) (Interval.aggregate2 y1 y2) (Interval.aggregate2 z1 z2)

aggregate3 ::
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  VectorBounds3D units space
aggregate3 (VectorBounds3D x1 y1 z1) (VectorBounds3D x2 y2 z2) (VectorBounds3D x3 y3 z3) =
  VectorBounds3D
    (Interval.aggregate3 x1 x2 x3)
    (Interval.aggregate3 y1 y2 y3)
    (Interval.aggregate3 z1 z2 z3)

-- | Construct a vector bounding box containing all vector bounding boxes in the given list.
aggregateN :: NonEmpty (VectorBounds3D units space) -> VectorBounds3D units space
aggregateN (first :| rest) = do
  let !(VB3D# (VectorBounds3D# x# y# z#)) = first
  let !(Interval# xLow# xHigh#) = x#
  let !(Interval# yLow# yHigh#) = y#
  let !(Interval# zLow# zHigh#) = z#
  aggregateImpl xLow# xHigh# yLow# yHigh# zLow# zHigh# rest

aggregateImpl ::
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  List (VectorBounds3D units space) ->
  VectorBounds3D units space
aggregateImpl xLow# xHigh# yLow# yHigh# zLow# zHigh# rest = case rest of
  [] ->
    VB3D# $
      VectorBounds3D#
        (Interval# xLow# xHigh#)
        (Interval# yLow# yHigh#)
        (Interval# zLow# zHigh#)
  next : remaining -> do
    let !(VB3D# (VectorBounds3D# xNext# yNext# zNext#)) = next
    let !(Interval# xLowNext# xHighNext#) = xNext#
    let !(Interval# yLowNext# yHighNext#) = yNext#
    let !(Interval# zLowNext# zHighNext#) = zNext#
    aggregateImpl
      (min# xLow# xLowNext#)
      (max# xHigh# xHighNext#)
      (min# yLow# yLowNext#)
      (max# yHigh# yHighNext#)
      (min# zLow# zLowNext#)
      (max# zHigh# zHighNext#)
      remaining

xComponent :: VectorBounds3D units space -> Interval units
xComponent (VectorBounds3D vx _ _) = vx

yComponent :: VectorBounds3D units space -> Interval units
yComponent (VectorBounds3D _ vy _) = vy

zComponent :: VectorBounds3D units space -> Interval units
zComponent (VectorBounds3D _ _ vz) = vz

components :: VectorBounds3D units space -> (Interval units, Interval units, Interval units)
components (VectorBounds3D vx vy vz) = (vx, vy, vz)

center :: VectorBounds3D units space -> Vector3D units space
center (VectorBounds3D vx vy vz) =
  Vector3D (Interval.midpoint vx) (Interval.midpoint vy) (Interval.midpoint vz)

squaredMagnitude :: Units.Squared units1 units2 => VectorBounds3D units1 space -> Interval units2
squaredMagnitude = Units.specialize . squaredMagnitude_

squaredMagnitude_ :: VectorBounds3D units space -> Interval (units ?*? units)
squaredMagnitude_ (VectorBounds3D x y z) =
  Interval.squared_ x + Interval.squared_ y + Interval.squared_ z

magnitude :: VectorBounds3D units space -> Interval units
magnitude bounds = do
  let !(VB3D# bounds#) = bounds
  let !(VectorBounds3D# r# f# u#) = bounds#
  let !(Interval# rMin# rMax#) = r#
  let !(Interval# fMin# fMax#) = f#
  let !(Interval# uMin# uMax#) = u#
  let positiveR# = rMin# >=# 0.0##
  let negativeR# = rMax# <=# 0.0##
  let positiveF# = fMin# >=# 0.0##
  let negativeF# = fMax# <=# 0.0##
  let positiveU# = uMin# >=# 0.0##
  let negativeU# = uMax# <=# 0.0##
  let minMagnitude# =
        case (# positiveR#, negativeR#, positiveF#, negativeF#, positiveU#, negativeU# #) of
          (# True#, _, True#, _, True#, _ #) -> hypot3# rMin# fMin# uMin#
          (# True#, _, True#, _, _, True# #) -> hypot3# rMin# fMin# uMax#
          (# True#, _, _, True#, True#, _ #) -> hypot3# rMin# fMax# uMin#
          (# True#, _, _, True#, _, True# #) -> hypot3# rMin# fMax# uMax#
          (# _, True#, True#, _, True#, _ #) -> hypot3# rMax# fMin# uMin#
          (# _, True#, True#, _, _, True# #) -> hypot3# rMax# fMin# uMax#
          (# _, True#, _, True#, True#, _ #) -> hypot3# rMax# fMax# uMin#
          (# _, True#, _, True#, _, True# #) -> hypot3# rMax# fMax# uMax#
          (# _, _, True#, _, True#, _ #) -> hypot2# fMin# uMin#
          (# _, _, True#, _, _, True# #) -> hypot2# fMin# uMax#
          (# _, _, _, True#, True#, _ #) -> hypot2# fMax# uMin#
          (# _, _, _, True#, _, True# #) -> hypot2# fMax# uMax#
          (# True#, _, _, _, True#, _ #) -> hypot2# rMin# uMin#
          (# True#, _, _, _, _, True# #) -> hypot2# rMin# uMax#
          (# _, True#, _, _, True#, _ #) -> hypot2# rMax# uMin#
          (# _, True#, _, _, _, True# #) -> hypot2# rMax# uMax#
          (# True#, _, True#, _, _, _ #) -> hypot2# rMin# fMin#
          (# True#, _, _, True#, _, _ #) -> hypot2# rMin# fMax#
          (# _, True#, True#, _, _, _ #) -> hypot2# rMax# fMin#
          (# _, True#, _, True#, _, _ #) -> hypot2# rMax# fMax#
          (# True#, _, _, _, _, _ #) -> rMin#
          (# _, True#, _, _, _, _ #) -> negate# rMax#
          (# _, _, True#, _, _, _ #) -> fMin#
          (# _, _, _, True#, _, _ #) -> negate# fMax#
          (# _, _, _, _, True#, _ #) -> uMin#
          (# _, _, _, _, _, True# #) -> negate# uMax#
          (# _, _, _, _, _, _ #) -> 0.0##
  I# (Interval# minMagnitude# (maxMagnitude# bounds))

maxMagnitude :: VectorBounds3D units space -> Quantity units
maxMagnitude bounds = Q# (maxMagnitude# bounds)

maxMagnitude# :: VectorBounds3D units space -> Double#
maxMagnitude# (VB3D# bounds#) = do
  let !(VectorBounds3D# r# f# u#) = bounds#
  let !(Interval# rMin# rMax#) = r#
  let !(Interval# fMin# fMax#) = f#
  let !(Interval# uMin# uMax#) = u#
  let rMagnitude# = max# (abs# rMin#) (abs# rMax#)
  let fMagnitude# = max# (abs# fMin#) (abs# fMax#)
  let uMagnitude# = max# (abs# uMin#) (abs# uMax#)
  hypot3# rMagnitude# fMagnitude# uMagnitude#

maxSquaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorBounds3D units1 space ->
  Quantity units2
maxSquaredMagnitude = Units.specialize . maxSquaredMagnitude_

maxSquaredMagnitude_ :: VectorBounds3D units space -> Quantity (units ?*? units)
maxSquaredMagnitude_ (VectorBounds3D x y z) = do
  let Interval minX maxX = x
  let Interval minY maxY = y
  let Interval minZ maxZ = z
  let xMagnitude = max (Quantity.abs minX) (Quantity.abs maxX)
  let yMagnitude = max (Quantity.abs minY) (Quantity.abs maxY)
  let zMagnitude = max (Quantity.abs minZ) (Quantity.abs maxZ)
  Quantity.squared_ xMagnitude + Quantity.squared_ yMagnitude + Quantity.squared_ zMagnitude

diameter :: VectorBounds3D units space -> Quantity units
diameter (VectorBounds3D x y z) =
  Quantity.hypot3 (Interval.width x) (Interval.width y) (Interval.width z)

isResolved :: VectorBounds3D units space -> Bool
isResolved (VectorBounds3D x y z) =
  Interval.isResolved x || Interval.isResolved y || Interval.isResolved z

areDistinct :: VectorBounds3D units space -> VectorBounds3D units space -> Bool
areDistinct (VectorBounds3D x1 y1 z1) (VectorBounds3D x2 y2 z2) =
  Interval.areDistinct x1 x2 || Interval.areDistinct y1 y2 || Interval.areDistinct z1 z2

areIndependent :: VectorBounds3D units space -> VectorBounds3D units space -> Bool
areIndependent bounds1 bounds2 = isResolved (bounds1 `cross_` bounds2)

direction :: VectorBounds3D units space -> DirectionBounds3D space
direction vectorBounds = DirectionBounds3D.unsafe (normalize vectorBounds)

normalize :: VectorBounds3D units space -> VectorBounds3D Unitless space
normalize vectorBounds = do
  let VectorBounds3D x y z = vectorBounds / magnitude vectorBounds
  let nx = clampNormalized x
  let ny = clampNormalized y
  let nz = clampNormalized z
  VectorBounds3D nx ny nz

normalizedBounds :: Interval Unitless
normalizedBounds = Interval -1.0 1.0

clampNormalized :: Interval Unitless -> Interval Unitless
clampNormalized (Interval low high) =
  Interval (Number.clampTo normalizedBounds low) (Number.clampTo normalizedBounds high)

exclusion :: Vector3D units space -> VectorBounds3D units space -> Quantity units
exclusion (Vector3D x y z) (VectorBounds3D bx by bz) = do
  let exclusionX = Interval.exclusion x bx
  let exclusionY = Interval.exclusion y by
  let exclusionZ = Interval.exclusion z bz
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
inclusion point box = negate (exclusion point box)

member :: Vector3D units space -> VectorBounds3D units space -> Bool
member (Vector3D vx vy vz) (VectorBounds3D x y z) =
  Interval.member vx x && Interval.member vy y && Interval.member vz z

contains :: VectorBounds3D units space -> VectorBounds3D units space -> Bool
contains (VectorBounds3D x2 y2 z2) (VectorBounds3D x1 y1 z1) =
  Interval.contains x2 x1 && Interval.contains y2 y1 && Interval.contains z2 z1

isContainedIn :: VectorBounds3D units space -> VectorBounds3D units space -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

separation :: VectorBounds3D units space -> VectorBounds3D units space -> Quantity units
separation (VectorBounds3D x1 y1 z1) (VectorBounds3D x2 y2 z2) = do
  let separationX = Interval.separation x1 x2
  let separationY = Interval.separation y1 y2
  let separationZ = Interval.separation z1 z2
  let positiveX = separationX > Quantity.zero
  let positiveY = separationY > Quantity.zero
  let positiveZ = separationZ > Quantity.zero
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
overlap first second = negate (separation first second)

intersection ::
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  Maybe (VectorBounds3D units space)
intersection (VectorBounds3D x1 y1 z1) (VectorBounds3D x2 y2 z2) = do
  x <- Interval.intersection x1 x2
  y <- Interval.intersection y1 y2
  z <- Interval.intersection z1 z2
  Just (VectorBounds3D x y z)

interpolate ::
  VectorBounds3D units space ->
  Number ->
  Number ->
  Number ->
  Vector3D units space
interpolate (VectorBounds3D x y z) u v w = Vector3D (x @ u) (y @ v) (z @ w)

placeIn :: Frame3D global local -> VectorBounds3D units local -> VectorBounds3D units global
placeIn frame (VectorBounds3D vR vF vU) = do
  let cR = Interval.midpoint vR
  let cF = Interval.midpoint vF
  let cU = Interval.midpoint vU
  let rR = 0.5 * Interval.width vR
  let rF = 0.5 * Interval.width vF
  let rU = 0.5 * Interval.width vU
  let Vector3D cR' cF' cU' = Vector3D.placeIn frame (Vector3D cR cF cU)
  let Direction3D iR iF iU = Frame3D.rightwardDirection frame
  let Direction3D jR jF jU = Frame3D.forwardDirection frame
  let Direction3D kR kF kU = Frame3D.upwardDirection frame
  let rR' = rR * Number.abs iR + rF * Number.abs jR + rU * Number.abs kR
  let rF' = rR * Number.abs iF + rF * Number.abs jF + rU * Number.abs kF
  let rU' = rR * Number.abs iU + rF * Number.abs jU + rU * Number.abs kU
  VectorBounds3D
    (Interval (cR' - rR') (cR' + rR'))
    (Interval (cF' - rF') (cF' + rF'))
    (Interval (cU' - rU') (cU' + rU'))

relativeTo :: Frame3D global local -> VectorBounds3D units global -> VectorBounds3D units local
relativeTo frame (VectorBounds3D vR vF vU) = do
  let cR = Interval.midpoint vR
  let cF = Interval.midpoint vF
  let cU = Interval.midpoint vU
  let rR = 0.5 * Interval.width vR
  let rF = 0.5 * Interval.width vF
  let rU = 0.5 * Interval.width vU
  let Vector3D cR' cF' cU' = Vector3D.relativeTo frame (Vector3D cR cF cU)
  let Direction3D iR iF iU = Frame3D.rightwardDirection frame
  let Direction3D jR jF jU = Frame3D.forwardDirection frame
  let Direction3D kR kF kU = Frame3D.upwardDirection frame
  let rR' = rR * Number.abs iR + rF * Number.abs iF + rU * Number.abs iU
  let rF' = rR * Number.abs jR + rF * Number.abs jF + rU * Number.abs jU
  let rU' = rR * Number.abs kR + rF * Number.abs kF + rU * Number.abs kU
  VectorBounds3D
    (Interval (cR' - rR') (cR' + rR'))
    (Interval (cF' - rF') (cF' + rF'))
    (Interval (cU' - rU') (cU' + rU'))

transformBy ::
  VectorTransform3D tag space ->
  VectorBounds3D units space ->
  VectorBounds3D units space
transformBy transform (VectorBounds3D vR vF vU) = do
  let cR = Interval.midpoint vR
  let cF = Interval.midpoint vF
  let cU = Interval.midpoint vU
  let rR = 0.5 * Interval.width vR
  let rF = 0.5 * Interval.width vF
  let rU = 0.5 * Interval.width vU
  let Vector3D cR' cF' cU' = Vector3D.transformBy transform (Vector3D cR cF cU)
  let VectorTransform3D i j k = transform
  let Vector3D iR iF iU = i
  let Vector3D jR jF jU = j
  let Vector3D kR kF kU = k
  let rR' = Number.abs iR * rR + Number.abs jR * rF + Number.abs kR * rU
  let rF' = Number.abs iF * rR + Number.abs jF * rF + Number.abs kF * rU
  let rU' = Number.abs iU * rR + Number.abs jU * rF + Number.abs kU * rU
  VectorBounds3D
    (Interval (cR' - rR') (cR' + rR'))
    (Interval (cF' - rF') (cF' + rF'))
    (Interval (cU' - rU') (cU' + rU'))

rotateIn ::
  Direction3D space ->
  Angle ->
  VectorBounds3D units space ->
  VectorBounds3D units space
rotateIn = VectorTransform3D.rotateInImpl transformBy

rotateAround :: Axis3D space -> Angle -> VectorBounds3D units space -> VectorBounds3D units space
rotateAround = VectorTransform3D.rotateAroundImpl transformBy

tripleProduct ::
  ( Units.Product units units squaredUnits
  , Units.Product squaredUnits units cubedUnits
  ) =>
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  Interval cubedUnits
tripleProduct bounds1 bounds2 bounds3 =
  Units.coerce (tripleProduct_ bounds1 bounds2 bounds3)

tripleProduct_ ::
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  VectorBounds3D units space ->
  Interval ((units ?*? units) ?*? units)
tripleProduct_ bounds1 bounds2 bounds3 = do
  let !(VB3D# bounds1#) = bounds1
  let !(VB3D# bounds2#) = bounds2
  let !(VB3D# bounds3#) = bounds3
  let !(VectorBounds3D# r1# f1# u1#) = bounds1#
  let !(VectorBounds3D# r2# f2# u2#) = bounds2#
  let !(VectorBounds3D# r3# f3# u3#) = bounds3#
  let matrix# =
        MatrixBounds3D#
          (# r1#, r2#, r3# #)
          (# f1#, f2#, f3# #)
          (# u1#, u2#, u3# #)
  I# (determinantBounds3D# matrix#)
