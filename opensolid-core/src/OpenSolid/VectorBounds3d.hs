{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.VectorBounds3d
  ( VectorBounds3d
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
  , squaredMagnitude'
  , magnitude
  , maxMagnitude
  , maxMagnitude##
  , maxSquaredMagnitude
  , maxSquaredMagnitude'
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
import OpenSolid.Bounds (Bounds (Bounds, Bounds##))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Number qualified as Number
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3d (Axis3d)
  , Direction3d (Direction3d)
  , Frame3d
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , Vector2d (Vector2d)
  , Vector3d (Vector3d)
  , VectorBounds2d (VectorBounds2d)
  , VectorBounds3d (VectorBounds3d, VectorBounds3d##)
  )
import OpenSolid.Quantity (Quantity (Quantity##))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d qualified as Vector3d

constant :: Vector3d (space @ units) -> VectorBounds3d (space @ units)
constant (Vector3d x y z) =
  VectorBounds3d (Bounds.constant x) (Bounds.constant y) (Bounds.constant z)

{-# INLINE coerce #-}
coerce :: VectorBounds3d (space1 @ units1) -> VectorBounds3d (space2 @ units2)
coerce = Data.Coerce.coerce

hull2 :: Vector3d (space @ units) -> Vector3d (space @ units) -> VectorBounds3d (space @ units)
hull2 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) =
  VectorBounds3d (Bounds x1 x2) (Bounds y1 y2) (Bounds z1 z2)

hull3 ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  VectorBounds3d (space @ units)
hull3 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) = do
  let minX = Quantity.min (Quantity.min x1 x2) x3
  let maxX = Quantity.max (Quantity.max x1 x2) x3
  let minY = Quantity.min (Quantity.min y1 y2) y3
  let maxY = Quantity.max (Quantity.max y1 y2) y3
  let minZ = Quantity.min (Quantity.min z1 z2) z3
  let maxZ = Quantity.max (Quantity.max z1 z2) z3
  VectorBounds3d (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)

hull4 ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  VectorBounds3d (space @ units)
hull4 (Vector3d x1 y1 z1) (Vector3d x2 y2 z2) (Vector3d x3 y3 z3) (Vector3d x4 y4 z4) = do
  let minX = Quantity.min (Quantity.min (Quantity.min x1 x2) x3) x4
  let maxX = Quantity.max (Quantity.max (Quantity.max x1 x2) x3) x4
  let minY = Quantity.min (Quantity.min (Quantity.min y1 y2) y3) y4
  let maxY = Quantity.max (Quantity.max (Quantity.max y1 y2) y3) y4
  let minZ = Quantity.min (Quantity.min (Quantity.min z1 z2) z3) z4
  let maxZ = Quantity.max (Quantity.max (Quantity.max z1 z2) z3) z4
  VectorBounds3d (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)

hullN :: NonEmpty (Vector3d (space @ units)) -> VectorBounds3d (space @ units)
hullN (Vector3d x0 y0 z0 :| rest) = go x0 x0 y0 y0 z0 z0 rest
 where
  go ::
    Quantity units ->
    Quantity units ->
    Quantity units ->
    Quantity units ->
    Quantity units ->
    Quantity units ->
    List (Vector3d (space @ units)) ->
    VectorBounds3d (space @ units)
  go xLow xHigh yLow yHigh zLow zHigh [] =
    VectorBounds3d (Bounds xLow xHigh) (Bounds yLow yHigh) (Bounds zLow zHigh)
  go xLow xHigh yLow yHigh zLow zHigh (Vector3d x y z : remaining) =
    go
      (Quantity.min xLow x)
      (Quantity.max xHigh x)
      (Quantity.min yLow y)
      (Quantity.max yHigh y)
      (Quantity.min zLow z)
      (Quantity.max zHigh z)
      remaining

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
  VectorBounds3d
    (Bounds.aggregate3 x1 x2 x3)
    (Bounds.aggregate3 y1 y2 y3)
    (Bounds.aggregate3 z1 z2 z3)

-- | Construct a vector bounding box containing all vector bounding boxes in the given list.
aggregateN :: NonEmpty (VectorBounds3d (space @ units)) -> VectorBounds3d (space @ units)
aggregateN (first :| rest) = do
  let VectorBounds3d (Bounds xLow0 xHigh0) (Bounds yLow0 yHigh0) (Bounds zLow0 zHigh0) = first
  aggregateImpl xLow0 xHigh0 yLow0 yHigh0 zLow0 zHigh0 rest

aggregateImpl ::
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Quantity units ->
  List (VectorBounds3d (space @ units)) ->
  VectorBounds3d (space @ units)
aggregateImpl xLow xHigh yLow yHigh zLow zHigh rest = case rest of
  [] -> VectorBounds3d (Bounds xLow xHigh) (Bounds yLow yHigh) (Bounds zLow zHigh)
  next : remaining -> do
    let VectorBounds3d xNext yNext zNext = next
    let Bounds xLowNext xHighNext = xNext
    let Bounds yLowNext yHighNext = yNext
    let Bounds zLowNext zHighNext = zNext
    aggregateImpl
      (Quantity.min xLow xLowNext)
      (Quantity.max xHigh xHighNext)
      (Quantity.min yLow yLowNext)
      (Quantity.max yHigh yHighNext)
      (Quantity.min zLow zLowNext)
      (Quantity.max zHigh zHighNext)
      remaining

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

squaredMagnitude' :: VectorBounds3d (space @ units) -> Bounds (units *# units)
squaredMagnitude' (VectorBounds3d x y z) =
  Bounds.squared' x + Bounds.squared' y + Bounds.squared' z

magnitude :: VectorBounds3d (space @ units) -> Bounds units
magnitude bounds = do
  let !(VectorBounds3d## xMin## xMax## yMin## yMax## zMin## zMax##) = bounds
  let positiveX## = xMin## >=## 0.0##
  let negativeX## = xMax## <=## 0.0##
  let positiveY## = yMin## >=## 0.0##
  let negativeY## = yMax## <=## 0.0##
  let positiveZ## = zMin## >=## 0.0##
  let negativeZ## = zMax## <=## 0.0##
  let minMagnitude## =
        case (# positiveX##, negativeX##, positiveY##, negativeY##, positiveZ##, negativeZ## #) of
          (# 1#, _, 1#, _, 1#, _ #) -> hypot3## xMin## yMin## zMin##
          (# 1#, _, 1#, _, _, 1# #) -> hypot3## xMin## yMin## zMax##
          (# 1#, _, _, 1#, 1#, _ #) -> hypot3## xMin## yMax## zMin##
          (# 1#, _, _, 1#, _, 1# #) -> hypot3## xMin## yMax## zMax##
          (# _, 1#, 1#, _, 1#, _ #) -> hypot3## xMax## yMin## zMin##
          (# _, 1#, 1#, _, _, 1# #) -> hypot3## xMax## yMin## zMax##
          (# _, 1#, _, 1#, 1#, _ #) -> hypot3## xMax## yMax## zMin##
          (# _, 1#, _, 1#, _, 1# #) -> hypot3## xMax## yMax## zMax##
          (# _, _, 1#, _, 1#, _ #) -> hypot2## yMin## zMin##
          (# _, _, 1#, _, _, 1# #) -> hypot2## yMin## zMax##
          (# _, _, _, 1#, 1#, _ #) -> hypot2## yMax## zMin##
          (# _, _, _, 1#, _, 1# #) -> hypot2## yMax## zMax##
          (# 1#, _, _, _, 1#, _ #) -> hypot2## xMin## zMin##
          (# 1#, _, _, _, _, 1# #) -> hypot2## xMin## zMax##
          (# _, 1#, _, _, 1#, _ #) -> hypot2## xMax## zMin##
          (# _, 1#, _, _, _, 1# #) -> hypot2## xMax## zMax##
          (# 1#, _, 1#, _, _, _ #) -> hypot2## xMin## yMin##
          (# 1#, _, _, 1#, _, _ #) -> hypot2## xMin## yMax##
          (# _, 1#, 1#, _, _, _ #) -> hypot2## xMax## yMin##
          (# _, 1#, _, 1#, _, _ #) -> hypot2## xMax## yMax##
          (# 1#, _, _, _, _, _ #) -> xMin##
          (# _, 1#, _, _, _, _ #) -> negate## xMax##
          (# _, _, 1#, _, _, _ #) -> yMin##
          (# _, _, _, 1#, _, _ #) -> negate## yMax##
          (# _, _, _, _, 1#, _ #) -> zMin##
          (# _, _, _, _, _, 1# #) -> negate## zMax##
          (# _, _, _, _, _, _ #) -> 0.0##
  Bounds## minMagnitude## (maxMagnitude## bounds)

maxMagnitude :: VectorBounds3d (space @ units) -> Quantity units
maxMagnitude bounds = Quantity## (maxMagnitude## bounds)

maxMagnitude## :: VectorBounds3d (space @ units) -> Double#
maxMagnitude## (VectorBounds3d## minX## maxX## minY## maxY## minZ## maxZ##) = do
  let xMagnitude## = max## (abs## minX##) (abs## maxX##)
  let yMagnitude## = max## (abs## minY##) (abs## maxY##)
  let zMagnitude## = max## (abs## minZ##) (abs## maxZ##)
  hypot3## xMagnitude## yMagnitude## zMagnitude##

maxSquaredMagnitude ::
  Units.Squared units1 units2 =>
  VectorBounds3d (space @ units1) ->
  Quantity units2
maxSquaredMagnitude = Units.specialize . maxSquaredMagnitude'

maxSquaredMagnitude' :: VectorBounds3d (space @ units) -> Quantity (units *# units)
maxSquaredMagnitude' (VectorBounds3d (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)) = do
  let xMagnitude = Quantity.max (Quantity.abs minX) (Quantity.abs maxX)
  let yMagnitude = Quantity.max (Quantity.abs minY) (Quantity.abs maxY)
  let zMagnitude = Quantity.max (Quantity.abs minZ) (Quantity.abs maxZ)
  Quantity.squared' xMagnitude + Quantity.squared' yMagnitude + Quantity.squared' zMagnitude

normalize :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ Unitless)
normalize vectorBounds = do
  let VectorBounds3d x y z = vectorBounds / magnitude vectorBounds
  let nx = clampNormalized x
  let ny = clampNormalized y
  let nz = clampNormalized z
  VectorBounds3d nx ny nz

normalizedBounds :: Bounds Unitless
normalizedBounds = Bounds -1.0 1.0

clampNormalized :: Bounds Unitless -> Bounds Unitless
clampNormalized (Bounds low high) =
  Bounds (Quantity.clampTo normalizedBounds low) (Quantity.clampTo normalizedBounds high)

exclusion :: Vector3d (space @ units) -> VectorBounds3d (space @ units) -> Quantity units
exclusion (Vector3d x y z) (VectorBounds3d bx by bz) = do
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
    | otherwise -> Quantity.max (Quantity.max exclusionX exclusionY) exclusionZ

inclusion :: Vector3d (space @ units) -> VectorBounds3d (space @ units) -> Quantity units
inclusion point box = -(exclusion point box)

includes :: Vector3d (space @ units) -> VectorBounds3d (space @ units) -> Bool
includes (Vector3d vx vy vz) (VectorBounds3d x y z) =
  Bounds.includes vx x && Bounds.includes vy y && Bounds.includes vz z

contains :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ units) -> Bool
contains (VectorBounds3d x2 y2 z2) (VectorBounds3d x1 y1 z1) =
  Bounds.contains x2 x1 && Bounds.contains y2 y1 && Bounds.contains z2 z1

isContainedIn :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

separation :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ units) -> Quantity units
separation (VectorBounds3d x1 y1 z1) (VectorBounds3d x2 y2 z2) = do
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
    | otherwise -> Quantity.max (Quantity.max separationX separationY) separationZ

overlap :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ units) -> Quantity units
overlap first second = -(separation first second)

intersection ::
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units) ->
  Maybe (VectorBounds3d (space @ units))
intersection (VectorBounds3d x1 y1 z1) (VectorBounds3d x2 y2 z2) = do
  x <- Bounds.intersection x1 x2
  y <- Bounds.intersection y1 y2
  z <- Bounds.intersection z1 z2
  Just (VectorBounds3d x y z)

interpolate ::
  VectorBounds3d (space @ units) ->
  Number ->
  Number ->
  Number ->
  Vector3d (space @ units)
interpolate (VectorBounds3d x y z) u v w =
  Vector3d (Bounds.interpolate x u) (Bounds.interpolate y v) (Bounds.interpolate z w)

on ::
  Plane3d (space @ planeUnits) (Defines local) ->
  VectorBounds2d (local @ units) ->
  VectorBounds3d (space @ units)
on plane bounds2d = do
  let VectorBounds2d bX bY = bounds2d
  let cX = Bounds.midpoint bX
  let cY = Bounds.midpoint bY
  let rX = 0.5 * Bounds.width bX
  let rY = 0.5 * Bounds.width bY
  let Plane3d _ (PlaneOrientation3d i j) = plane
  let Direction3d iR iF iU = i
  let Direction3d jR jF jU = j
  let Vector3d cR cF cU = Vector3d.on plane (Vector2d cX cY)
  let rR = rX * Number.abs iR + rY * Number.abs jR
  let rF = rX * Number.abs iF + rY * Number.abs jF
  let rU = rX * Number.abs iU + rY * Number.abs jU
  let bR = Bounds (cR - rR) (cR + rR)
  let bF = Bounds (cF - rF) (cF + rF)
  let bU = Bounds (cU - rU) (cU + rU)
  VectorBounds3d bR bF bU

placeIn ::
  Frame3d (global @ frameUnits) (Defines local) ->
  VectorBounds3d (local @ units) ->
  VectorBounds3d (global @ units)
placeIn frame (VectorBounds3d vR vF vU) = do
  let cR = Bounds.midpoint vR
  let cF = Bounds.midpoint vF
  let cU = Bounds.midpoint vU
  let rR = 0.5 * Bounds.width vR
  let rF = 0.5 * Bounds.width vF
  let rU = 0.5 * Bounds.width vU
  let Vector3d cR' cF' cU' = Vector3d.placeIn frame (Vector3d cR cF cU)
  let Direction3d iR iF iU = frame.rightwardDirection
  let Direction3d jR jF jU = frame.forwardDirection
  let Direction3d kR kF kU = frame.upwardDirection
  let rR' = rR * Number.abs iR + rF * Number.abs jR + rU * Number.abs kR
  let rF' = rR * Number.abs iF + rF * Number.abs jF + rU * Number.abs kF
  let rU' = rR * Number.abs iU + rF * Number.abs jU + rU * Number.abs kU
  VectorBounds3d
    @ Bounds (cR' - rR') (cR' + rR')
    @ Bounds (cF' - rF') (cF' + rF')
    @ Bounds (cU' - rU') (cU' + rU')

relativeTo ::
  Frame3d (global @ frameUnits) (Defines local) ->
  VectorBounds3d (global @ units) ->
  VectorBounds3d (local @ units)
relativeTo frame (VectorBounds3d vR vF vU) = do
  let cR = Bounds.midpoint vR
  let cF = Bounds.midpoint vF
  let cU = Bounds.midpoint vU
  let rR = 0.5 * Bounds.width vR
  let rF = 0.5 * Bounds.width vF
  let rU = 0.5 * Bounds.width vU
  let Vector3d cR' cF' cU' = Vector3d.relativeTo frame (Vector3d cR cF cU)
  let Direction3d iR iF iU = frame.rightwardDirection
  let Direction3d jR jF jU = frame.forwardDirection
  let Direction3d kR kF kU = frame.upwardDirection
  let rR' = rR * Number.abs iR + rF * Number.abs iF + rU * Number.abs iU
  let rF' = rR * Number.abs jR + rF * Number.abs jF + rU * Number.abs jU
  let rU' = rR * Number.abs kR + rF * Number.abs kF + rU * Number.abs kU
  VectorBounds3d
    @ Bounds (cR' - rR') (cR' + rR')
    @ Bounds (cF' - rF') (cF' + rF')
    @ Bounds (cU' - rU') (cU' + rU')

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
  let Transform3d _ i j k = transform
  let Vector3d iR iF iU = i
  let Vector3d jR jF jU = j
  let Vector3d kR kF kU = k
  let rR' = Number.abs iR * rR + Number.abs jR * rF + Number.abs kR * rU
  let rF' = Number.abs iF * rR + Number.abs jF * rF + Number.abs kF * rU
  let rU' = Number.abs iU * rR + Number.abs jU * rF + Number.abs kU * rU
  VectorBounds3d
    @ Bounds (cR' - rR') (cR' + rR')
    @ Bounds (cF' - rF') (cF' + rF')
    @ Bounds (cU' - rU') (cU' + rU')

rotateIn ::
  Direction3d space ->
  Angle ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units)
rotateIn axisDirection = rotateAround (Axis3d Point3d.dummy axisDirection)

rotateAround ::
  Axis3d (space @ axisUnits) ->
  Angle ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units)
rotateAround = Transform3d.rotateAroundImpl transformBy

tripleProduct ::
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units) ->
  Bounds ((units *# units) *# units)
tripleProduct bounds1 bounds2 bounds3 = do
  let !(VectorBounds3d## xMin1## xMax1## yMin1## yMax1## zMin1## zMax1##) = bounds1
  let !(VectorBounds3d## xMin2## xMax2## yMin2## yMax2## zMin2## zMax2##) = bounds2
  let !(VectorBounds3d## xMin3## xMax3## yMin3## yMax3## zMin3## zMax3##) = bounds3
  let !(# low##, high## #) =
        determinantBounds3d##
          xMin1##
          xMax1##
          yMin1##
          yMax1##
          zMin1##
          zMax1##
          xMin2##
          xMax2##
          yMin2##
          yMax2##
          zMin2##
          zMax2##
          xMin3##
          xMax3##
          yMin3##
          yMax3##
          zMin3##
          zMax3##
  Bounds## low## high##
