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
  , relativeTo
  , placeIn
  , transformBy
  , rotateIn
  , rotateAround
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Float qualified as Float
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3d (Axis3d)
  , Direction3d (Direction3d)
  , Frame3d
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

-- | Construct a vector bounding box containing all vector bounding boxes in the given list.
aggregateN :: NonEmpty (VectorBounds3d (space @ units)) -> VectorBounds3d (space @ units)
aggregateN (first :| rest) = do
  let VectorBounds3d (Bounds xLow0 xHigh0) (Bounds yLow0 yHigh0) (Bounds zLow0 zHigh0) = first
  aggregateImpl xLow0 xHigh0 yLow0 yHigh0 zLow0 zHigh0 rest

aggregateImpl ::
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
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
      (Qty.min xLow xLowNext)
      (Qty.max xHigh xHighNext)
      (Qty.min yLow yLowNext)
      (Qty.max yHigh yHighNext)
      (Qty.min zLow zLowNext)
      (Qty.max zHigh zHighNext)
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
  let VectorBounds3d x y z = vectorBounds / magnitude vectorBounds
  let nx = clampNormalized x
  let ny = clampNormalized y
  let nz = clampNormalized z
  VectorBounds3d nx ny nz

normalizedBounds :: Bounds Unitless
normalizedBounds = Bounds -1.0 1.0

clampNormalized :: Bounds Unitless -> Bounds Unitless
clampNormalized (Bounds low high) =
  Bounds (Qty.clampTo normalizedBounds low) (Qty.clampTo normalizedBounds high)

exclusion :: Vector3d (space @ units) -> VectorBounds3d (space @ units) -> Qty units
exclusion (Vector3d x y z) (VectorBounds3d bx by bz) = do
  let exclusionX = Bounds.exclusion x bx
  let exclusionY = Bounds.exclusion y by
  let exclusionZ = Bounds.exclusion z bz
  let positiveX = exclusionX >= Qty.zero
  let positiveY = exclusionY >= Qty.zero
  let positiveZ = exclusionZ >= Qty.zero
  if
    | positiveX && positiveY && positiveZ -> Qty.hypot3 exclusionX exclusionY exclusionZ
    | positiveX && positiveY -> Qty.hypot2 exclusionX exclusionY
    | positiveX && positiveZ -> Qty.hypot2 exclusionX exclusionZ
    | positiveY && positiveZ -> Qty.hypot2 exclusionY exclusionZ
    | positiveX -> exclusionX
    | positiveY -> exclusionY
    | positiveZ -> exclusionZ
    | otherwise -> Qty.max (Qty.max exclusionX exclusionY) exclusionZ

inclusion :: Vector3d (space @ units) -> VectorBounds3d (space @ units) -> Qty units
inclusion point box = -(exclusion point box)

includes :: Vector3d (space @ units) -> VectorBounds3d (space @ units) -> Bool
includes (Vector3d vx vy vz) (VectorBounds3d x y z) =
  Bounds.includes vx x && Bounds.includes vy y && Bounds.includes vz z

contains :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ units) -> Bool
contains (VectorBounds3d x2 y2 z2) (VectorBounds3d x1 y1 z1) =
  Bounds.contains x2 x1 && Bounds.contains y2 y1 && Bounds.contains z2 z1

isContainedIn :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

separation :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ units) -> Qty units
separation (VectorBounds3d x1 y1 z1) (VectorBounds3d x2 y2 z2) = do
  let separationX = Bounds.separation x1 x2
  let separationY = Bounds.separation y1 y2
  let separationZ = Bounds.separation z1 z2
  let positiveX = separationX >= Qty.zero
  let positiveY = separationY >= Qty.zero
  let positiveZ = separationZ >= Qty.zero
  if
    | positiveX && positiveY && positiveZ -> Qty.hypot3 separationX separationY separationZ
    | positiveX && positiveY -> Qty.hypot2 separationX separationY
    | positiveX && positiveZ -> Qty.hypot2 separationX separationZ
    | positiveY && positiveZ -> Qty.hypot2 separationY separationZ
    | positiveX -> separationX
    | positiveY -> separationY
    | positiveZ -> separationZ
    | otherwise -> Qty.max (Qty.max separationX separationY) separationZ

overlap :: VectorBounds3d (space @ units) -> VectorBounds3d (space @ units) -> Qty units
overlap first second = -(separation first second)

intersection ::
  VectorBounds3d (space @ units) ->
  VectorBounds3d (space @ units) ->
  Maybe (VectorBounds3d (space @ units))
intersection (VectorBounds3d x1 y1 z1) (VectorBounds3d x2 y2 z2) = Maybe.do
  x <- Bounds.intersection x1 x2
  y <- Bounds.intersection y1 y2
  z <- Bounds.intersection z1 z2
  Just (VectorBounds3d x y z)

interpolate :: VectorBounds3d (space @ units) -> Float -> Float -> Float -> Vector3d (space @ units)
interpolate (VectorBounds3d x y z) u v w =
  Vector3d (Bounds.interpolate x u) (Bounds.interpolate y v) (Bounds.interpolate z w)

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
  let rR' = rR * Float.abs iR + rF * Float.abs jR + rU * Float.abs kR
  let rF' = rR * Float.abs iF + rF * Float.abs jF + rU * Float.abs kF
  let rU' = rR * Float.abs iU + rF * Float.abs jU + rU * Float.abs kU
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
  let rR' = rR * Float.abs iR + rF * Float.abs iF + rU * Float.abs iU
  let rF' = rR * Float.abs jR + rF * Float.abs jF + rU * Float.abs jU
  let rU' = rR * Float.abs kR + rF * Float.abs kF + rU * Float.abs kU
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
  let rR' = Float.abs iR * rR + Float.abs jR * rF + Float.abs kR * rU
  let rF' = Float.abs iF * rR + Float.abs jF * rF + Float.abs kF * rU
  let rU' = Float.abs iU * rR + Float.abs jU * rF + Float.abs kU * rU
  VectorBounds3d
    @ Bounds (cR' - rR') (cR' + rR')
    @ Bounds (cF' - rF') (cF' + rF')
    @ Bounds (cU' - rU') (cU' + rU')

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
