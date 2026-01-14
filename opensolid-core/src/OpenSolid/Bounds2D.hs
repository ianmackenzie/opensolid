module OpenSolid.Bounds2D
  ( Bounds2D (Bounds2D)
  , coerce
  , xCoordinate
  , yCoordinate
  , coordinates
  , dimensions
  , centerPoint
  , constant
  , hull2
  , hull3
  , hull4
  , hullN
  , aggregate2
  , aggregateN
  , exclusion
  , exclusion#
  , inclusion
  , inclusion#
  , includes
  , contains
  , isContainedIn
  , overlap
  , overlap#
  , separation
  , separation#
  , intersection
  , lowerLeftCorner
  , lowerRightCorner
  , upperLeftCorner
  , upperRightCorner
  , corners
  , diameter
  , diameter#
  , area_
  , area
  , interpolate
  , placeIn
  , relativeTo
  , placeOn
  , transformBy
  , distanceAlong
  , convert
  , unconvert
  )
where

import Data.Coerce qualified
import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Direction2D (Direction2D (Direction2D))
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Bounds2D (Bounds2D, PositionBounds2D)
  , Bounds3D (Bounds3D)
  , Direction3D (Direction3D)
  , Plane3D (Plane3D)
  , PlaneOrientation3D (PlaneOrientation3D)
  , Point2D (Position2D)
  , Point3D (Point3D)
  )
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Transform2D (Transform2D (Transform2D))
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.VectorBounds2D qualified as VectorBounds2D

coerce :: Bounds2D units1 space1 -> Bounds2D units2 space2
coerce (Bounds2D x y) = Bounds2D (Interval.coerce x) (Interval.coerce y)

-- | Get the X coordinate bounds of a bounding box.
xCoordinate :: Bounds2D units space -> Interval units
xCoordinate (PositionBounds2D pb) = VectorBounds2D.xComponent pb

-- | Get the Y coordinate bounds of a bounding box.
yCoordinate :: Bounds2D units space -> Interval units
yCoordinate (PositionBounds2D pb) = VectorBounds2D.yComponent pb

-- | Get the X and Y coordinate bounds of a bounding box.
{-# INLINE coordinates #-}
coordinates :: Bounds2D units space -> (Interval units, Interval units)
coordinates (PositionBounds2D pb) = VectorBounds2D.components pb

dimensions :: Bounds2D units space -> (Quantity units, Quantity units)
dimensions (Bounds2D x y) = (Interval.width x, Interval.width y)

centerPoint :: Bounds2D units space -> Point2D units space
centerPoint (Bounds2D x y) = Point2D (Interval.midpoint x) (Interval.midpoint y)

-- | Construct a zero-size bounding box containing a single point.
constant :: Point2D units space -> Bounds2D units space
constant (Point2D x y) = Bounds2D (Interval.constant x) (Interval.constant y)

aggregate2 :: Bounds2D units space -> Bounds2D units space -> Bounds2D units space
aggregate2 (PositionBounds2D pb1) (PositionBounds2D pb2) =
  PositionBounds2D (VectorBounds2D.aggregate2 pb1 pb2)

-- | Construct a bounding box containing all bounding boxes in the given non-empty list.
aggregateN :: NonEmpty (Bounds2D units space) -> Bounds2D units space
aggregateN list = PositionBounds2D (VectorBounds2D.aggregateN (Data.Coerce.coerce list))

exclusion :: Point2D units space -> Bounds2D units space -> Quantity units
exclusion (Position2D p) (PositionBounds2D pb) = VectorBounds2D.exclusion p pb

{-# INLINE exclusion# #-}
exclusion# :: Point2D units space -> Bounds2D units space -> Double#
exclusion# (Position2D p) (PositionBounds2D pb) = VectorBounds2D.exclusion# p pb

inclusion :: Point2D units space -> Bounds2D units space -> Quantity units
inclusion (Position2D p) (PositionBounds2D pb) = VectorBounds2D.inclusion p pb

{-# INLINE inclusion# #-}
inclusion# :: Point2D units space -> Bounds2D units space -> Double#
inclusion# (Position2D p) (PositionBounds2D pb) = VectorBounds2D.inclusion# p pb

includes :: Point2D units space -> Bounds2D units space -> Bool
includes (Position2D p) (PositionBounds2D pb) = VectorBounds2D.includes p pb

contains :: Bounds2D units space -> Bounds2D units space -> Bool
contains (PositionBounds2D pb2) (PositionBounds2D pb1) = VectorBounds2D.contains pb2 pb1

isContainedIn :: Bounds2D units space -> Bounds2D units space -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

{-# INLINE separation #-}
separation :: Bounds2D units space -> Bounds2D units space -> Quantity units
separation (PositionBounds2D pb1) (PositionBounds2D pb2) = VectorBounds2D.separation pb1 pb2

{-# INLINE separation# #-}
separation# :: Bounds2D units space -> Bounds2D units space -> Double#
separation# (PositionBounds2D pb1) (PositionBounds2D pb2) = VectorBounds2D.separation# pb1 pb2

{-# INLINE overlap #-}
overlap :: Bounds2D units space -> Bounds2D units space -> Quantity units
overlap (PositionBounds2D pb1) (PositionBounds2D pb2) = VectorBounds2D.overlap pb1 pb2

{-# INLINE overlap# #-}
overlap# :: Bounds2D units space -> Bounds2D units space -> Double#
overlap# (PositionBounds2D pb1) (PositionBounds2D pb2) = VectorBounds2D.overlap# pb1 pb2

intersection ::
  Bounds2D units space ->
  Bounds2D units space ->
  Maybe (Bounds2D units space)
intersection (PositionBounds2D pb1) (PositionBounds2D pb2) =
  Maybe.map PositionBounds2D (VectorBounds2D.intersection pb1 pb2)

-- | Construct a bounding box from two corner points.
hull2 ::
  Point2D units space ->
  Point2D units space ->
  Bounds2D units space
hull2 (Point2D x1 y1) (Point2D x2 y2) = Bounds2D (Interval x1 x2) (Interval y1 y2)

hull3 ::
  Point2D units space ->
  Point2D units space ->
  Point2D units space ->
  Bounds2D units space
hull3 (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3) = do
  let minX = min (min x1 x2) x3
  let maxX = max (max x1 x2) x3
  let minY = min (min y1 y2) y3
  let maxY = max (max y1 y2) y3
  Bounds2D (Interval minX maxX) (Interval minY maxY)

hull4 ::
  Point2D units space ->
  Point2D units space ->
  Point2D units space ->
  Point2D units space ->
  Bounds2D units space
hull4 (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3) (Point2D x4 y4) = do
  let minX = min (min (min x1 x2) x3) x4
  let maxX = max (max (max x1 x2) x3) x4
  let minY = min (min (min y1 y2) y3) y4
  let maxY = max (max (max y1 y2) y3) y4
  Bounds2D (Interval minX maxX) (Interval minY maxY)

-- | Construct a bounding box containing all vertices in the given non-empty list.
hullN :: NonEmpty (Point2D units space) -> Bounds2D units space
hullN (Point2D x0 y0 :| rest) = do
  let go xLow xHigh yLow yHigh [] = Bounds2D (Interval xLow xHigh) (Interval yLow yHigh)
      go xLow xHigh yLow yHigh (Point2D x y : remaining) =
        go (min xLow x) (max xHigh x) (min yLow y) (max yHigh y) remaining
  go x0 x0 y0 y0 rest

lowerLeftCorner :: Bounds2D units space -> Point2D units space
lowerLeftCorner (Bounds2D x y) = Point2D (Interval.lower x) (Interval.lower y)

lowerRightCorner :: Bounds2D units space -> Point2D units space
lowerRightCorner (Bounds2D x y) = Point2D (Interval.upper x) (Interval.lower y)

upperLeftCorner :: Bounds2D units space -> Point2D units space
upperLeftCorner (Bounds2D x y) = Point2D (Interval.lower x) (Interval.upper y)

upperRightCorner :: Bounds2D units space -> Point2D units space
upperRightCorner (Bounds2D x y) = Point2D (Interval.upper x) (Interval.upper y)

corners :: Bounds2D units space -> List (Point2D units space)
corners box =
  [ lowerLeftCorner box
  , lowerRightCorner box
  , upperRightCorner box
  , upperLeftCorner box
  ]

{-# INLINE diameter #-}
diameter :: Bounds2D units space -> Quantity units
diameter bounds = Quantity# (diameter# bounds)

{-# INLINE diameter# #-}
diameter# :: Bounds2D units space -> Double#
diameter# (Bounds2D x y) = hypot2# (Interval.width# x) (Interval.width# y)

area_ :: Bounds2D units space -> Quantity (units ?*? units)
area_ (Bounds2D x y) = Interval.width x ?*? Interval.width y

area :: Units.Squared units1 units2 => Bounds2D units1 space -> Quantity units2
area (Bounds2D x y) = Interval.width x .*. Interval.width y

interpolate :: Bounds2D units space -> Number -> Number -> Point2D units space
interpolate (PositionBounds2D pb) u v = Position2D (VectorBounds2D.interpolate pb u v)

placeIn :: Frame2D units global local -> Bounds2D units local -> Bounds2D units global
placeIn frame (Bounds2D x y) = do
  let xMid = Interval.midpoint x
  let yMid = Interval.midpoint y
  let xWidth = Interval.width x
  let yWidth = Interval.width y
  let Point2D x0 y0 = Point2D.placeIn frame (Point2D xMid yMid)
  let Direction2D ix iy = frame.xDirection
  let Direction2D jx jy = frame.yDirection
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs jx
  let ry = 0.5 *. xWidth .*. Number.abs iy .+. 0.5 *. yWidth .*. Number.abs jy
  Bounds2D (Interval (x0 .-. rx) (x0 .+. rx)) (Interval (y0 .-. ry) (y0 .+. ry))

relativeTo :: Frame2D units global local -> Bounds2D units global -> Bounds2D units local
relativeTo frame (Bounds2D x y) = do
  let xMid = Interval.midpoint x
  let yMid = Interval.midpoint y
  let xWidth = Interval.width x
  let yWidth = Interval.width y
  let Point2D x0 y0 = Point2D.relativeTo frame (Point2D xMid yMid)
  let Direction2D ix iy = frame.xDirection
  let Direction2D jx jy = frame.yDirection
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs iy
  let ry = 0.5 *. xWidth .*. Number.abs jx .+. 0.5 *. yWidth .*. Number.abs jy
  Bounds2D (Interval (x0 .-. rx) (x0 .+. rx)) (Interval (y0 .-. ry) (y0 .+. ry))

placeOn :: Plane3D global local -> Bounds2D Meters local -> Bounds3D global
placeOn plane (Bounds2D x y) = do
  let Plane3D _ (PlaneOrientation3D i j) = plane
  let Direction3D ix iy iz = i
  let Direction3D jx jy jz = j
  let xMid = Interval.midpoint x
  let yMid = Interval.midpoint y
  let xWidth = Interval.width x
  let yWidth = Interval.width y
  let Point3D x0 y0 z0 = Point3D.on plane (Point2D xMid yMid)
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs jx
  let ry = 0.5 *. xWidth .*. Number.abs iy .+. 0.5 *. yWidth .*. Number.abs jy
  let rz = 0.5 *. xWidth .*. Number.abs iz .+. 0.5 *. yWidth .*. Number.abs jz
  let bx = Interval (x0 .-. rx) (x0 .+. rx)
  let by = Interval (y0 .-. ry) (y0 .+. ry)
  let bz = Interval (z0 .-. rz) (z0 .+. rz)
  Bounds3D bx by bz

transformBy ::
  Transform2D tag units space ->
  Bounds2D units space ->
  Bounds2D units space
transformBy transform (Bounds2D x y) = do
  let xMid = Interval.midpoint x
  let yMid = Interval.midpoint y
  let xWidth = Interval.width x
  let yWidth = Interval.width y
  let Point2D x0 y0 = Point2D.transformBy transform (Point2D xMid yMid)
  let Transform2D _ i j = transform
  let Vector2D ix iy = i
  let Vector2D jx jy = j
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs jx
  let ry = 0.5 *. xWidth .*. Number.abs iy .+. 0.5 *. yWidth .*. Number.abs jy
  Bounds2D (Interval (x0 .-. rx) (x0 .+. rx)) (Interval (y0 .-. ry) (y0 .+. ry))

distanceAlong :: Axis2D units space -> Bounds2D units space -> Interval units
distanceAlong axis (Bounds2D x y) = do
  let xMid = Interval.midpoint x
  let yMid = Interval.midpoint y
  let xWidth = Interval.width x
  let yWidth = Interval.width y
  let d0 = Point2D.distanceAlong axis (Point2D xMid yMid)
  let Direction2D ax ay = Axis2D.direction axis
  let r = 0.5 *. xWidth .*. Number.abs ax .+. 0.5 *. yWidth .*. Number.abs ay
  Interval (d0 .-. r) (d0 .+. r)

convert :: Quantity (units2 ?/? units1) -> Bounds2D units1 space -> Bounds2D units2 space
convert factor (PositionBounds2D pb) = PositionBounds2D (VectorBounds2D.convert factor pb)

unconvert :: Quantity (units2 ?/? units1) -> Bounds2D units2 space -> Bounds2D units1 space
unconvert factor (PositionBounds2D pb) = PositionBounds2D (VectorBounds2D.unconvert factor pb)
