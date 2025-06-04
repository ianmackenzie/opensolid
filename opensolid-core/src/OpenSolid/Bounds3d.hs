module OpenSolid.Bounds3d
  ( Bounds3d
  , rightwardForwardUpward
  , rightwardCoordinate
  , forwardCoordinate
  , upwardCoordinate
  , coordinates
  , constant
  , hull2
  , hull3
  , hull4
  , hullN
  , aggregate2
  , aggregateN
  , centerPoint
  , length
  , width
  , height
  , exclusion
  , inclusion
  , contains
  , isContainedIn
  , separation
  , overlap
  , intersection
  , diameter
  , interpolate
  , placeIn
  , relativeTo
  , projectInto
  , distanceAlong
  , transformBy
  )
where

import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Convention3d (Convention3d)
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Float qualified as Float
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3d (Axis3d)
  , Bounds3d (Bounds3d)
  , Direction3d (Direction3d)
  , Frame3d (Frame3d)
  , Orientation3d (Orientation3d)
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , Point3d (Point3d)
  , Vector3d (Vector3d)
  )
import OpenSolid.Qty qualified as Qty
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Vertex3d (Vertex3d)
import OpenSolid.Vertex3d qualified as Vertex3d

{-# INLINE rightwardForwardUpward #-}
rightwardForwardUpward :: Bounds units -> Bounds units -> Bounds units -> Bounds3d (space @ units)
rightwardForwardUpward = Bounds3d

-- | Get the bounds on the rightward coordinate of a bounding box.
rightwardCoordinate :: Bounds3d (space @ units) -> Bounds units
rightwardCoordinate (Bounds3d r _ _) = r

-- | Get the bounds on the forward coordinate of a bounding box.
forwardCoordinate :: Bounds3d (space @ units) -> Bounds units
forwardCoordinate (Bounds3d _ f _) = f

-- | Get the bounds on the upward coordinate of a bounding box.
upwardCoordinate :: Bounds3d (space @ units) -> Bounds units
upwardCoordinate (Bounds3d _ _ u) = u

-- | Get the XYZ coordinate ranges of a bounding box, given an XYZ coordinate convention to use.
coordinates ::
  Convention3d ->
  Bounds3d (space @ units) ->
  (Bounds units, Bounds units, Bounds units)
coordinates convention bounds =
  ( distanceAlong (Convention3d.xAxis Frame3d.world convention) bounds
  , distanceAlong (Convention3d.yAxis Frame3d.world convention) bounds
  , distanceAlong (Convention3d.zAxis Frame3d.world convention) bounds
  )

-- | Construct a zero-size bounding box containing a single point.
constant :: Point3d (space @ units) -> Bounds3d (space @ units)
constant (Point3d x y z) =
  Bounds3d (Bounds.constant x) (Bounds.constant y) (Bounds.constant z)

aggregate2 :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Bounds3d (space @ units)
aggregate2 (Bounds3d x1 y1 z1) (Bounds3d x2 y2 z2) =
  Bounds3d (Bounds.aggregate2 x1 x2) (Bounds.aggregate2 y1 y2) (Bounds.aggregate2 z1 z2)

-- | Construct a bounding box containing all bounding boxes in the given non-empty list.
aggregateN :: NonEmpty (Bounds3d (space @ units)) -> Bounds3d (space @ units)
aggregateN (Bounds3d (Bounds xLow0 xHigh0) (Bounds yLow0 yHigh0) (Bounds zLow0 zHigh0) :| rest) =
  aggregateImpl xLow0 xHigh0 yLow0 yHigh0 zLow0 zHigh0 rest

aggregateImpl ::
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  List (Bounds3d (space @ units)) ->
  Bounds3d (space @ units)
aggregateImpl xLow xHigh yLow yHigh zLow zHigh rest = case rest of
  [] -> Bounds3d (Bounds xLow xHigh) (Bounds yLow yHigh) (Bounds zLow zHigh)
  next : remaining -> do
    let Bounds3d xNext yNext zNext = next
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

centerPoint :: Bounds3d (space @ units) -> Point3d (space @ units)
centerPoint (Bounds3d r f u) = Point3d (Bounds.midpoint r) (Bounds.midpoint f) (Bounds.midpoint u)

length :: Bounds3d (space @ units) -> Qty units
length (Bounds3d _ f _) = Bounds.width f

width :: Bounds3d (space @ units) -> Qty units
width (Bounds3d r _ _) = Bounds.width r

height :: Bounds3d (space @ units) -> Qty units
height (Bounds3d _ _ u) = Bounds.width u

exclusion :: Point3d (space @ units) -> Bounds3d (space @ units) -> Qty units
exclusion (Point3d x y z) (Bounds3d bx by bz) = do
  let dx = Bounds.exclusion x bx
  let dy = Bounds.exclusion y by
  let dz = Bounds.exclusion z bz
  let px = dx >= Qty.zero
  let py = dy >= Qty.zero
  let pz = dz >= Qty.zero
  if
    | px && py && pz -> Qty.hypot3 dx dy dz
    | px && py -> Qty.hypot2 dx dy
    | px && pz -> Qty.hypot2 dx dz
    | py && pz -> Qty.hypot2 dy dz
    | px -> dx
    | py -> dy
    | pz -> dz
    | otherwise -> Qty.max (Qty.max dx dy) dz

inclusion :: Point3d (space @ units) -> Bounds3d (space @ units) -> Qty units
inclusion point box = -(exclusion point box)

contains :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Bool
contains (Bounds3d x2 y2 z2) (Bounds3d x1 y1 z1) =
  Bounds.contains x2 x1 && Bounds.contains y2 y1 && Bounds.contains z2 z1

isContainedIn :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

separation :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Qty units
separation (Bounds3d x1 y1 z1) (Bounds3d x2 y2 z2) = do
  let dx = Bounds.separation x1 x2
  let dy = Bounds.separation y1 y2
  let dz = Bounds.separation z1 z2
  let px = dx >= Qty.zero
  let py = dy >= Qty.zero
  let pz = dz >= Qty.zero
  if
    | px && py && pz -> Qty.hypot3 dx dy dz
    | px && py -> Qty.hypot2 dx dy
    | px && pz -> Qty.hypot2 dx dz
    | py && pz -> Qty.hypot2 dy dz
    | px -> dx
    | py -> dy
    | pz -> dz
    | otherwise -> Qty.max (Qty.max dx dy) dz

overlap :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Qty units
overlap first second = -(separation first second)

intersection :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Maybe (Bounds3d (space @ units))
intersection (Bounds3d x1 y1 z1) (Bounds3d x2 y2 z2) = Maybe.do
  x <- Bounds.intersection x1 x2
  y <- Bounds.intersection y1 y2
  z <- Bounds.intersection z1 z2
  Just (Bounds3d x y z)

-- | Construct a bounding box from two corner points.
hull2 ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Bounds3d (space @ units)
hull2 (Point3d x1 y1 z1) (Point3d x2 y2 z2) =
  Bounds3d (Bounds x1 x2) (Bounds y1 y2) (Bounds z1 z2)

hull3 ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Bounds3d (space @ units)
hull3 (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3) = do
  let minX = Qty.min (Qty.min x1 x2) x3
  let maxX = Qty.max (Qty.max x1 x2) x3
  let minY = Qty.min (Qty.min y1 y2) y3
  let maxY = Qty.max (Qty.max y1 y2) y3
  let minZ = Qty.min (Qty.min z1 z2) z3
  let maxZ = Qty.max (Qty.max z1 z2) z3
  Bounds3d (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)

hull4 ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Bounds3d (space @ units)
hull4 (Point3d x1 y1 z1) (Point3d x2 y2 z2) (Point3d x3 y3 z3) (Point3d x4 y4 z4) = do
  let minX = Qty.min (Qty.min (Qty.min x1 x2) x3) x4
  let maxX = Qty.max (Qty.max (Qty.max x1 x2) x3) x4
  let minY = Qty.min (Qty.min (Qty.min y1 y2) y3) y4
  let maxY = Qty.max (Qty.max (Qty.max y1 y2) y3) y4
  let minZ = Qty.min (Qty.min (Qty.min z1 z2) z3) z4
  let maxZ = Qty.max (Qty.max (Qty.max z1 z2) z3) z4
  Bounds3d (Bounds minX maxX) (Bounds minY maxY) (Bounds minZ maxZ)

-- | Construct a bounding box containing all vertices in the given non-empty list.
hullN :: Vertex3d vertex (space @ units) => NonEmpty vertex -> Bounds3d (space @ units)
hullN (v0 :| rest) = do
  let Point3d x0 y0 z0 = Vertex3d.position v0
  accumulateHull x0 x0 y0 y0 z0 z0 rest

accumulateHull ::
  Vertex3d vertex (space @ units) =>
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  List vertex ->
  Bounds3d (space @ units)
accumulateHull xLow xHigh yLow yHigh zLow zHigh remaining = case remaining of
  [] -> Bounds3d (Bounds xLow xHigh) (Bounds yLow yHigh) (Bounds zLow zHigh)
  vertex : following -> do
    let Point3d x y z = Vertex3d.position vertex
    accumulateHull
      (Qty.min xLow x)
      (Qty.max xHigh x)
      (Qty.min yLow y)
      (Qty.max yHigh y)
      (Qty.min zLow z)
      (Qty.max zHigh z)
      following

diameter :: Bounds3d (space @ units) -> Qty units
diameter (Bounds3d x y z) = Qty.hypot3 (Bounds.width x) (Bounds.width y) (Bounds.width z)

interpolate :: Bounds3d (space @ units) -> Float -> Float -> Float -> Point3d (space @ units)
interpolate (Bounds3d x y z) u v w =
  Point3d (Bounds.interpolate x u) (Bounds.interpolate y v) (Bounds.interpolate z w)

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Bounds3d (local @ units) ->
  Bounds3d (global @ units)
placeIn frame (Bounds3d pR pF pU) = do
  let Frame3d _ (Orientation3d i j k) = frame
  let Direction3d iR iF iU = i
  let Direction3d jR jF jU = j
  let Direction3d kR kF kU = k
  let cR = Bounds.midpoint pR
  let cF = Bounds.midpoint pF
  let cU = Bounds.midpoint pU
  let rR = 0.5 * Bounds.width pR
  let rF = 0.5 * Bounds.width pF
  let rU = 0.5 * Bounds.width pU
  let Point3d cR' cF' cU' = Point3d.placeIn frame (Point3d cR cF cU)
  let rR' = rR * Float.abs iR + rF * Float.abs jR + rU * Float.abs kR
  let rF' = rR * Float.abs iF + rF * Float.abs jF + rU * Float.abs kF
  let rU' = rR * Float.abs iU + rF * Float.abs jU + rU * Float.abs kU
  Bounds3d
    @ Bounds (cR' - rR') (cR' + rR')
    @ Bounds (cF' - rF') (cF' + rF')
    @ Bounds (cU' - rU') (cU' + rU')

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Bounds3d (global @ units) ->
  Bounds3d (local @ units)
relativeTo frame (Bounds3d pR pF pU) = do
  let Frame3d _ (Orientation3d i j k) = frame
  let Direction3d iR iF iU = i
  let Direction3d jR jF jU = j
  let Direction3d kR kF kU = k
  let cR = Bounds.midpoint pR
  let cF = Bounds.midpoint pF
  let cU = Bounds.midpoint pU
  let rR = 0.5 * Bounds.width pR
  let rF = 0.5 * Bounds.width pF
  let rU = 0.5 * Bounds.width pU
  let Point3d cR' cF' cU' = Point3d.relativeTo frame (Point3d cR cF cU)
  let rR' = rR * Float.abs iR + rF * Float.abs iF + rU * Float.abs iU
  let rF' = rR * Float.abs jR + rF * Float.abs jF + rU * Float.abs jU
  let rU' = rR * Float.abs kR + rF * Float.abs kF + rU * Float.abs kU
  Bounds3d
    @ Bounds (cR' - rR') (cR' + rR')
    @ Bounds (cF' - rF') (cF' + rF')
    @ Bounds (cU' - rU') (cU' + rU')

projectInto ::
  Plane3d (global @ units) (Defines local) ->
  Bounds3d (global @ units) ->
  Bounds2d (local @ units)
projectInto plane (Bounds3d pR pF pU) = do
  let Plane3d _ (PlaneOrientation3d i j) = plane
  let Direction3d iR iF iU = i
  let Direction3d jR jF jU = j
  let cR = Bounds.midpoint pR
  let cF = Bounds.midpoint pF
  let cU = Bounds.midpoint pU
  let rR = 0.5 * Bounds.width pR
  let rF = 0.5 * Bounds.width pF
  let rU = 0.5 * Bounds.width pU
  let Point2d cX cY = Point3d.projectInto plane (Point3d cR cF cU)
  let rX = rR * Float.abs iR + rF * Float.abs iF + rU * Float.abs iU
  let rY = rR * Float.abs jR + rF * Float.abs jF + rU * Float.abs jU
  Bounds2d
    @ Bounds (cX - rX) (cX + rX)
    @ Bounds (cY - rY) (cY + rY)

distanceAlong :: Axis3d (space @ units) -> Bounds3d (space @ units) -> Bounds units
distanceAlong axis bounds = do
  let Axis3d _ (Direction3d dR dF dU) = axis
  let mid = Point3d.distanceAlong axis (centerPoint bounds)
  let rR = 0.5 * width bounds
  let rF = 0.5 * length bounds
  let rU = 0.5 * height bounds
  let radius = rR * Float.abs dR + rF * Float.abs dF + rU * Float.abs dU
  Bounds (mid - radius) (mid + radius)

transformBy ::
  Transform3d tag (space @ units) ->
  Bounds3d (space @ units) ->
  Bounds3d (space @ units)
transformBy transform (Bounds3d pR pF pU) = do
  let cR = Bounds.midpoint pR
  let cF = Bounds.midpoint pF
  let cU = Bounds.midpoint pU
  let rR = 0.5 * Bounds.width pR
  let rF = 0.5 * Bounds.width pF
  let rU = 0.5 * Bounds.width pU
  let Point3d cR' cF' cU' = Point3d.transformBy transform (Point3d cR cF cU)
  let Transform3d _ i j k = transform
  let Vector3d iR iF iU = i
  let Vector3d jR jF jU = j
  let Vector3d kR kF kU = k
  let rR' = rR * Float.abs iR + rF * Float.abs jR + rU * Float.abs kR
  let rF' = rR * Float.abs iF + rF * Float.abs jF + rU * Float.abs kF
  let rU' = rR * Float.abs iU + rF * Float.abs jU + rU * Float.abs kU
  Bounds3d
    @ Bounds (cR' - rR') (cR' + rR')
    @ Bounds (cF' - rF') (cF' + rF')
    @ Bounds (cU' - rU') (cU' + rU')
