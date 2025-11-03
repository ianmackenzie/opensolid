module OpenSolid.Bounds3d
  ( Bounds3d
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
  , on
  , placeIn
  , relativeTo
  , projectInto
  , distanceAlong
  , transformBy
  )
where

import Data.Coerce qualified
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Convention3d (Convention3d)
import OpenSolid.Convention3d qualified as Convention3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3d (Axis3d)
  , Bounds3d (Bounds3d, PositionBounds3d)
  , Direction3d (Direction3d)
  , Frame3d (Frame3d)
  , Orientation3d (Orientation3d)
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , Point3d (Point3d, Position3d)
  , Vector3d (Vector3d)
  )
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import OpenSolid.Vertex3d (Vertex3d)
import OpenSolid.Vertex3d qualified as Vertex3d
import OpenSolid.World3d qualified as World3d

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
  ( distanceAlong (Convention3d.xAxis World3d.frame convention) bounds
  , distanceAlong (Convention3d.yAxis World3d.frame convention) bounds
  , distanceAlong (Convention3d.zAxis World3d.frame convention) bounds
  )

-- | Construct a zero-size bounding box containing a single point.
constant :: Point3d (space @ units) -> Bounds3d (space @ units)
constant (Position3d p) = PositionBounds3d (VectorBounds3d.constant p)

aggregate2 :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Bounds3d (space @ units)
aggregate2 (PositionBounds3d pb1) (PositionBounds3d pb2) =
  PositionBounds3d (VectorBounds3d.aggregate2 pb1 pb2)

-- | Construct a bounding box containing all bounding boxes in the given non-empty list.
aggregateN :: NonEmpty (Bounds3d (space @ units)) -> Bounds3d (space @ units)
aggregateN list = PositionBounds3d (VectorBounds3d.aggregateN (Data.Coerce.coerce list))

centerPoint :: Bounds3d (space @ units) -> Point3d (space @ units)
centerPoint (Bounds3d r f u) = Point3d (Bounds.midpoint r) (Bounds.midpoint f) (Bounds.midpoint u)

length :: Bounds3d (space @ units) -> Quantity units
length (Bounds3d _ f _) = Bounds.width f

width :: Bounds3d (space @ units) -> Quantity units
width (Bounds3d r _ _) = Bounds.width r

height :: Bounds3d (space @ units) -> Quantity units
height (Bounds3d _ _ u) = Bounds.width u

exclusion :: Point3d (space @ units) -> Bounds3d (space @ units) -> Quantity units
exclusion (Position3d p) (PositionBounds3d pb) = VectorBounds3d.exclusion p pb

inclusion :: Point3d (space @ units) -> Bounds3d (space @ units) -> Quantity units
inclusion (Position3d p) (PositionBounds3d pb) = VectorBounds3d.inclusion p pb

contains :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Bool
contains (PositionBounds3d pb2) (PositionBounds3d pb1) = VectorBounds3d.contains pb2 pb1

isContainedIn :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Bool
isContainedIn (PositionBounds3d pb1) (PositionBounds3d pb2) = VectorBounds3d.isContainedIn pb1 pb2

separation :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Quantity units
separation (PositionBounds3d pb1) (PositionBounds3d pb2) = VectorBounds3d.separation pb1 pb2

overlap :: Bounds3d (space @ units) -> Bounds3d (space @ units) -> Quantity units
overlap (PositionBounds3d pb1) (PositionBounds3d pb2) = VectorBounds3d.overlap pb1 pb2

intersection ::
  Bounds3d (space @ units) ->
  Bounds3d (space @ units) ->
  Maybe (Bounds3d (space @ units))
intersection (PositionBounds3d pb1) (PositionBounds3d pb2) =
  Maybe.map PositionBounds3d (VectorBounds3d.intersection pb1 pb2)

-- | Construct a bounding box from two corner points.
hull2 ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Bounds3d (space @ units)
hull2 (Position3d p1) (Position3d p2) = PositionBounds3d (VectorBounds3d.hull2 p1 p2)

hull3 ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Bounds3d (space @ units)
hull3 (Position3d p1) (Position3d p2) (Position3d p3) =
  PositionBounds3d (VectorBounds3d.hull3 p1 p2 p3)

hull4 ::
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Point3d (space @ units) ->
  Bounds3d (space @ units)
hull4 (Position3d p1) (Position3d p2) (Position3d p3) (Position3d p4) =
  PositionBounds3d (VectorBounds3d.hull4 p1 p2 p3 p4)

-- | Construct a bounding box containing all vertices in the given non-empty list.
hullN :: Vertex3d vertex (space @ units) => NonEmpty vertex -> Bounds3d (space @ units)
hullN vertices = do
  let positionVectors = Data.Coerce.coerce (NonEmpty.map Vertex3d.position vertices)
  PositionBounds3d (VectorBounds3d.hullN positionVectors)

diameter :: Bounds3d (space @ units) -> Quantity units
diameter (Bounds3d x y z) = Quantity.hypot3 (Bounds.width x) (Bounds.width y) (Bounds.width z)

interpolate :: Bounds3d (space @ units) -> Number -> Number -> Number -> Point3d (space @ units)
interpolate (PositionBounds3d pb) u v w = Position3d (VectorBounds3d.interpolate pb u v w)

on ::
  Plane3d (space @ units) (Defines local) ->
  Bounds2d (local @ units) ->
  Bounds3d (space @ units)
on plane bounds2d = do
  let Bounds2d bX bY = bounds2d
  let rX = 0.5 * Bounds.width bX
  let rY = 0.5 * Bounds.width bY
  let Plane3d _ (PlaneOrientation3d i j) = plane
  let Direction3d iR iF iU = i
  let Direction3d jR jF jU = j
  let Point3d cR cF cU = Point3d.on plane (Bounds2d.centerPoint bounds2d)
  let rR = rX * Number.abs iR + rY * Number.abs jR
  let rF = rX * Number.abs iF + rY * Number.abs jF
  let rU = rX * Number.abs iU + rY * Number.abs jU
  let bR = Bounds (cR - rR) (cR + rR)
  let bF = Bounds (cF - rF) (cF + rF)
  let bU = Bounds (cU - rU) (cU + rU)
  Bounds3d bR bF bU

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
  let rR' = rR * Number.abs iR + rF * Number.abs jR + rU * Number.abs kR
  let rF' = rR * Number.abs iF + rF * Number.abs jF + rU * Number.abs kF
  let rU' = rR * Number.abs iU + rF * Number.abs jU + rU * Number.abs kU
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
  let rR' = rR * Number.abs iR + rF * Number.abs iF + rU * Number.abs iU
  let rF' = rR * Number.abs jR + rF * Number.abs jF + rU * Number.abs jU
  let rU' = rR * Number.abs kR + rF * Number.abs kF + rU * Number.abs kU
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
  let rX = rR * Number.abs iR + rF * Number.abs iF + rU * Number.abs iU
  let rY = rR * Number.abs jR + rF * Number.abs jF + rU * Number.abs jU
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
  let radius = rR * Number.abs dR + rF * Number.abs dF + rU * Number.abs dU
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
  let rR' = rR * Number.abs iR + rF * Number.abs jR + rU * Number.abs kR
  let rF' = rR * Number.abs iF + rF * Number.abs jF + rU * Number.abs kF
  let rU' = rR * Number.abs iU + rF * Number.abs jU + rU * Number.abs kU
  Bounds3d
    @ Bounds (cR' - rR') (cR' + rR')
    @ Bounds (cF' - rF') (cF' + rF')
    @ Bounds (cU' - rU') (cU' + rU')
