module OpenSolid.Bounds3D
  ( Bounds3D
  , rightwardCoordinate
  , forwardCoordinate
  , upwardCoordinate
  , coordinates
  , constant
  , hull2
  , hull3
  , hull4
  , hullN
  , hullOfN
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
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Convention3D (Convention3D)
import OpenSolid.Convention3D qualified as Convention3D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Length (Length)
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (pattern Point2D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3D (Axis3D)
  , Bounds3D (Bounds3D, PositionBounds3D)
  , Direction3D (Direction3D)
  , Frame3D (Frame3D)
  , Orientation3D (Orientation3D)
  , Plane3D (Plane3D)
  , PlaneOrientation3D (PlaneOrientation3D)
  , Point3D (Point3D, Position3D)
  , Vector3D (Vector3D)
  )
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Transform3D (Transform3D (Transform3D))
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import OpenSolid.World3D qualified as World3D

-- | Get the bounds on the rightward coordinate of a bounding box.
rightwardCoordinate :: Bounds3D space -> Bounds Meters
rightwardCoordinate (Bounds3D r _ _) = r

-- | Get the bounds on the forward coordinate of a bounding box.
forwardCoordinate :: Bounds3D space -> Bounds Meters
forwardCoordinate (Bounds3D _ f _) = f

-- | Get the bounds on the upward coordinate of a bounding box.
upwardCoordinate :: Bounds3D space -> Bounds Meters
upwardCoordinate (Bounds3D _ _ u) = u

-- | Get the XYZ coordinate ranges of a bounding box, given an XYZ coordinate convention to use.
coordinates :: Convention3D -> Bounds3D space -> (Bounds Meters, Bounds Meters, Bounds Meters)
coordinates convention bounds =
  ( distanceAlong (Convention3D.xAxis World3D.frame convention) bounds
  , distanceAlong (Convention3D.yAxis World3D.frame convention) bounds
  , distanceAlong (Convention3D.zAxis World3D.frame convention) bounds
  )

-- | Construct a zero-size bounding box containing a single point.
constant :: Point3D space -> Bounds3D space
constant (Position3D p) = PositionBounds3D (VectorBounds3D.constant p)

aggregate2 :: Bounds3D space -> Bounds3D space -> Bounds3D space
aggregate2 (PositionBounds3D pb1) (PositionBounds3D pb2) =
  PositionBounds3D (VectorBounds3D.aggregate2 pb1 pb2)

-- | Construct a bounding box containing all bounding boxes in the given non-empty list.
aggregateN :: NonEmpty (Bounds3D space) -> Bounds3D space
aggregateN list = PositionBounds3D (VectorBounds3D.aggregateN (Data.Coerce.coerce list))

centerPoint :: Bounds3D space -> Point3D space
centerPoint (Bounds3D r f u) = Point3D (Bounds.midpoint r) (Bounds.midpoint f) (Bounds.midpoint u)

length :: Bounds3D space -> Length
length (Bounds3D _ f _) = Bounds.width f

width :: Bounds3D space -> Length
width (Bounds3D r _ _) = Bounds.width r

height :: Bounds3D space -> Length
height (Bounds3D _ _ u) = Bounds.width u

exclusion :: Point3D space -> Bounds3D space -> Length
exclusion (Position3D p) (PositionBounds3D pb) = VectorBounds3D.exclusion p pb

inclusion :: Point3D space -> Bounds3D space -> Length
inclusion (Position3D p) (PositionBounds3D pb) = VectorBounds3D.inclusion p pb

contains :: Bounds3D space -> Bounds3D space -> Bool
contains (PositionBounds3D pb2) (PositionBounds3D pb1) = VectorBounds3D.contains pb2 pb1

isContainedIn :: Bounds3D space -> Bounds3D space -> Bool
isContainedIn (PositionBounds3D pb1) (PositionBounds3D pb2) = VectorBounds3D.isContainedIn pb1 pb2

separation :: Bounds3D space -> Bounds3D space -> Length
separation (PositionBounds3D pb1) (PositionBounds3D pb2) = VectorBounds3D.separation pb1 pb2

overlap :: Bounds3D space -> Bounds3D space -> Length
overlap (PositionBounds3D pb1) (PositionBounds3D pb2) = VectorBounds3D.overlap pb1 pb2

intersection :: Bounds3D space -> Bounds3D space -> Maybe (Bounds3D space)
intersection (PositionBounds3D pb1) (PositionBounds3D pb2) =
  Maybe.map PositionBounds3D (VectorBounds3D.intersection pb1 pb2)

-- | Construct a bounding box from two corner points.
hull2 :: Point3D space -> Point3D space -> Bounds3D space
hull2 (Position3D p1) (Position3D p2) = PositionBounds3D (VectorBounds3D.hull2 p1 p2)

hull3 :: Point3D space -> Point3D space -> Point3D space -> Bounds3D space
hull3 (Position3D p1) (Position3D p2) (Position3D p3) =
  PositionBounds3D (VectorBounds3D.hull3 p1 p2 p3)

hull4 :: Point3D space -> Point3D space -> Point3D space -> Point3D space -> Bounds3D space
hull4 (Position3D p1) (Position3D p2) (Position3D p3) (Position3D p4) =
  PositionBounds3D (VectorBounds3D.hull4 p1 p2 p3 p4)

-- | Construct a bounding box containing all points in the given non-empty list.
hullN :: NonEmpty (Point3D space) -> Bounds3D space
hullN vertices = PositionBounds3D (VectorBounds3D.hullN (Data.Coerce.coerce vertices))

hullOfN :: (a -> Point3D space) -> NonEmpty a -> Bounds3D space
hullOfN function values = hullN (NonEmpty.map function values)

diameter :: Bounds3D space -> Length
diameter (Bounds3D x y z) = Quantity.hypot3 (Bounds.width x) (Bounds.width y) (Bounds.width z)

interpolate :: Bounds3D space -> Number -> Number -> Number -> Point3D space
interpolate (PositionBounds3D pb) u v w = Position3D (VectorBounds3D.interpolate pb u v w)

on :: Plane3D global local -> Bounds2D Meters local -> Bounds3D global
on plane bounds2D = do
  let Bounds2D bX bY = bounds2D
  let rX = 0.5 *. Bounds.width bX
  let rY = 0.5 *. Bounds.width bY
  let Plane3D _ (PlaneOrientation3D i j) = plane
  let Direction3D iR iF iU = i
  let Direction3D jR jF jU = j
  let Point3D cR cF cU = Point3D.on plane (Bounds2D.centerPoint bounds2D)
  let rR = rX .*. Number.abs iR .+. rY .*. Number.abs jR
  let rF = rX .*. Number.abs iF .+. rY .*. Number.abs jF
  let rU = rX .*. Number.abs iU .+. rY .*. Number.abs jU
  let bR = Bounds (cR .-. rR) (cR .+. rR)
  let bF = Bounds (cF .-. rF) (cF .+. rF)
  let bU = Bounds (cU .-. rU) (cU .+. rU)
  Bounds3D bR bF bU

placeIn :: Frame3D global local -> Bounds3D local -> Bounds3D global
placeIn frame (Bounds3D pR pF pU) = do
  let Frame3D _ (Orientation3D i j k) = frame
  let Direction3D iR iF iU = i
  let Direction3D jR jF jU = j
  let Direction3D kR kF kU = k
  let cR = Bounds.midpoint pR
  let cF = Bounds.midpoint pF
  let cU = Bounds.midpoint pU
  let rR = 0.5 *. Bounds.width pR
  let rF = 0.5 *. Bounds.width pF
  let rU = 0.5 *. Bounds.width pU
  let Point3D cR' cF' cU' = Point3D.placeIn frame (Point3D cR cF cU)
  let rR' = rR .*. Number.abs iR .+. rF .*. Number.abs jR .+. rU .*. Number.abs kR
  let rF' = rR .*. Number.abs iF .+. rF .*. Number.abs jF .+. rU .*. Number.abs kF
  let rU' = rR .*. Number.abs iU .+. rF .*. Number.abs jU .+. rU .*. Number.abs kU
  Bounds3D
    (Bounds (cR' .-. rR') (cR' .+. rR'))
    (Bounds (cF' .-. rF') (cF' .+. rF'))
    (Bounds (cU' .-. rU') (cU' .+. rU'))

relativeTo :: Frame3D global local -> Bounds3D global -> Bounds3D local
relativeTo frame (Bounds3D pR pF pU) = do
  let Frame3D _ (Orientation3D i j k) = frame
  let Direction3D iR iF iU = i
  let Direction3D jR jF jU = j
  let Direction3D kR kF kU = k
  let cR = Bounds.midpoint pR
  let cF = Bounds.midpoint pF
  let cU = Bounds.midpoint pU
  let rR = 0.5 *. Bounds.width pR
  let rF = 0.5 *. Bounds.width pF
  let rU = 0.5 *. Bounds.width pU
  let Point3D cR' cF' cU' = Point3D.relativeTo frame (Point3D cR cF cU)
  let rR' = rR .*. Number.abs iR .+. rF .*. Number.abs iF .+. rU .*. Number.abs iU
  let rF' = rR .*. Number.abs jR .+. rF .*. Number.abs jF .+. rU .*. Number.abs jU
  let rU' = rR .*. Number.abs kR .+. rF .*. Number.abs kF .+. rU .*. Number.abs kU
  Bounds3D
    (Bounds (cR' .-. rR') (cR' .+. rR'))
    (Bounds (cF' .-. rF') (cF' .+. rF'))
    (Bounds (cU' .-. rU') (cU' .+. rU'))

projectInto :: Plane3D global local -> Bounds3D global -> Bounds2D Meters local
projectInto plane (Bounds3D pR pF pU) = do
  let Plane3D _ (PlaneOrientation3D i j) = plane
  let Direction3D iR iF iU = i
  let Direction3D jR jF jU = j
  let cR = Bounds.midpoint pR
  let cF = Bounds.midpoint pF
  let cU = Bounds.midpoint pU
  let rR = 0.5 *. Bounds.width pR
  let rF = 0.5 *. Bounds.width pF
  let rU = 0.5 *. Bounds.width pU
  let Point2D cX cY = Point3D.projectInto plane (Point3D cR cF cU)
  let rX = rR .*. Number.abs iR .+. rF .*. Number.abs iF .+. rU .*. Number.abs iU
  let rY = rR .*. Number.abs jR .+. rF .*. Number.abs jF .+. rU .*. Number.abs jU
  Bounds2D
    (Bounds (cX .-. rX) (cX .+. rX))
    (Bounds (cY .-. rY) (cY .+. rY))

distanceAlong :: Axis3D space -> Bounds3D space -> Bounds Meters
distanceAlong axis bounds = do
  let Axis3D _ (Direction3D dR dF dU) = axis
  let mid = Point3D.distanceAlong axis (centerPoint bounds)
  let rR = 0.5 *. width bounds
  let rF = 0.5 *. length bounds
  let rU = 0.5 *. height bounds
  let radius = rR .*. Number.abs dR .+. rF .*. Number.abs dF .+. rU .*. Number.abs dU
  Bounds (mid .-. radius) (mid .+. radius)

transformBy :: Transform3D tag space -> Bounds3D space -> Bounds3D space
transformBy transform (Bounds3D pR pF pU) = do
  let cR = Bounds.midpoint pR
  let cF = Bounds.midpoint pF
  let cU = Bounds.midpoint pU
  let rR = 0.5 *. Bounds.width pR
  let rF = 0.5 *. Bounds.width pF
  let rU = 0.5 *. Bounds.width pU
  let Point3D cR' cF' cU' = Point3D.transformBy transform (Point3D cR cF cU)
  let Transform3D _ i j k = transform
  let Vector3D iR iF iU = i
  let Vector3D jR jF jU = j
  let Vector3D kR kF kU = k
  let rR' = rR .*. Number.abs iR .+. rF .*. Number.abs jR .+. rU .*. Number.abs kR
  let rF' = rR .*. Number.abs iF .+. rF .*. Number.abs jF .+. rU .*. Number.abs kF
  let rU' = rR .*. Number.abs iU .+. rF .*. Number.abs jU .+. rU .*. Number.abs kU
  Bounds3D
    (Bounds (cR' .-. rR') (cR' .+. rR'))
    (Bounds (cF' .-. rF') (cF' .+. rF'))
    (Bounds (cU' .-. rU') (cU' .+. rU'))
