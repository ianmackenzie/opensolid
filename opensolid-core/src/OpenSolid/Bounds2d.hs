module OpenSolid.Bounds2d
  ( Bounds2d (Bounds2d)
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
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Direction2d (Direction2d (Direction2d))
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Bounds2d (Bounds2d, PositionBounds2d)
  , Bounds3d (Bounds3d)
  , Direction3d (Direction3d)
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , Point2D (Position2D)
  , Point3d (Point3d)
  )
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.VectorBounds2d qualified as VectorBounds2d

coerce :: Bounds2d units1 space1 -> Bounds2d units2 space2
coerce (Bounds2d x y) = Bounds2d (Bounds.coerce x) (Bounds.coerce y)

-- | Get the X coordinate bounds of a bounding box.
xCoordinate :: Bounds2d units space -> Bounds units
xCoordinate (PositionBounds2d pb) = VectorBounds2d.xComponent pb

-- | Get the Y coordinate bounds of a bounding box.
yCoordinate :: Bounds2d units space -> Bounds units
yCoordinate (PositionBounds2d pb) = VectorBounds2d.yComponent pb

-- | Get the X and Y coordinate bounds of a bounding box.
{-# INLINE coordinates #-}
coordinates :: Bounds2d units space -> (Bounds units, Bounds units)
coordinates (PositionBounds2d pb) = VectorBounds2d.components pb

dimensions :: Bounds2d units space -> (Quantity units, Quantity units)
dimensions (Bounds2d x y) = (Bounds.width x, Bounds.width y)

centerPoint :: Bounds2d units space -> Point2D units space
centerPoint (Bounds2d x y) = Point2D (Bounds.midpoint x) (Bounds.midpoint y)

-- | Construct a zero-size bounding box containing a single point.
constant :: Point2D units space -> Bounds2d units space
constant (Point2D x y) = Bounds2d (Bounds.constant x) (Bounds.constant y)

aggregate2 :: Bounds2d units space -> Bounds2d units space -> Bounds2d units space
aggregate2 (PositionBounds2d pb1) (PositionBounds2d pb2) =
  PositionBounds2d (VectorBounds2d.aggregate2 pb1 pb2)

-- | Construct a bounding box containing all bounding boxes in the given non-empty list.
aggregateN :: NonEmpty (Bounds2d units space) -> Bounds2d units space
aggregateN list = PositionBounds2d (VectorBounds2d.aggregateN (Data.Coerce.coerce list))

exclusion :: Point2D units space -> Bounds2d units space -> Quantity units
exclusion (Position2D p) (PositionBounds2d pb) = VectorBounds2d.exclusion p pb

{-# INLINE exclusion# #-}
exclusion# :: Point2D units space -> Bounds2d units space -> Double#
exclusion# (Position2D p) (PositionBounds2d pb) = VectorBounds2d.exclusion# p pb

inclusion :: Point2D units space -> Bounds2d units space -> Quantity units
inclusion (Position2D p) (PositionBounds2d pb) = VectorBounds2d.inclusion p pb

{-# INLINE inclusion# #-}
inclusion# :: Point2D units space -> Bounds2d units space -> Double#
inclusion# (Position2D p) (PositionBounds2d pb) = VectorBounds2d.inclusion# p pb

includes :: Point2D units space -> Bounds2d units space -> Bool
includes (Position2D p) (PositionBounds2d pb) = VectorBounds2d.includes p pb

contains :: Bounds2d units space -> Bounds2d units space -> Bool
contains (PositionBounds2d pb2) (PositionBounds2d pb1) = VectorBounds2d.contains pb2 pb1

isContainedIn :: Bounds2d units space -> Bounds2d units space -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

{-# INLINE separation #-}
separation :: Bounds2d units space -> Bounds2d units space -> Quantity units
separation (PositionBounds2d pb1) (PositionBounds2d pb2) = VectorBounds2d.separation pb1 pb2

{-# INLINE separation# #-}
separation# :: Bounds2d units space -> Bounds2d units space -> Double#
separation# (PositionBounds2d pb1) (PositionBounds2d pb2) = VectorBounds2d.separation# pb1 pb2

{-# INLINE overlap #-}
overlap :: Bounds2d units space -> Bounds2d units space -> Quantity units
overlap (PositionBounds2d pb1) (PositionBounds2d pb2) = VectorBounds2d.overlap pb1 pb2

{-# INLINE overlap# #-}
overlap# :: Bounds2d units space -> Bounds2d units space -> Double#
overlap# (PositionBounds2d pb1) (PositionBounds2d pb2) = VectorBounds2d.overlap# pb1 pb2

intersection ::
  Bounds2d units space ->
  Bounds2d units space ->
  Maybe (Bounds2d units space)
intersection (PositionBounds2d pb1) (PositionBounds2d pb2) =
  Maybe.map PositionBounds2d (VectorBounds2d.intersection pb1 pb2)

-- | Construct a bounding box from two corner points.
hull2 ::
  Point2D units space ->
  Point2D units space ->
  Bounds2d units space
hull2 (Point2D x1 y1) (Point2D x2 y2) = Bounds2d (Bounds x1 x2) (Bounds y1 y2)

hull3 ::
  Point2D units space ->
  Point2D units space ->
  Point2D units space ->
  Bounds2d units space
hull3 (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3) = do
  let minX = min (min x1 x2) x3
  let maxX = max (max x1 x2) x3
  let minY = min (min y1 y2) y3
  let maxY = max (max y1 y2) y3
  Bounds2d (Bounds minX maxX) (Bounds minY maxY)

hull4 ::
  Point2D units space ->
  Point2D units space ->
  Point2D units space ->
  Point2D units space ->
  Bounds2d units space
hull4 (Point2D x1 y1) (Point2D x2 y2) (Point2D x3 y3) (Point2D x4 y4) = do
  let minX = min (min (min x1 x2) x3) x4
  let maxX = max (max (max x1 x2) x3) x4
  let minY = min (min (min y1 y2) y3) y4
  let maxY = max (max (max y1 y2) y3) y4
  Bounds2d (Bounds minX maxX) (Bounds minY maxY)

-- | Construct a bounding box containing all vertices in the given non-empty list.
hullN :: NonEmpty (Point2D units space) -> Bounds2d units space
hullN (Point2D x0 y0 :| rest) = do
  let go xLow xHigh yLow yHigh [] = Bounds2d (Bounds xLow xHigh) (Bounds yLow yHigh)
      go xLow xHigh yLow yHigh (Point2D x y : remaining) =
        go (min xLow x) (max xHigh x) (min yLow y) (max yHigh y) remaining
  go x0 x0 y0 y0 rest

lowerLeftCorner :: Bounds2d units space -> Point2D units space
lowerLeftCorner (Bounds2d x y) = Point2D (Bounds.lower x) (Bounds.lower y)

lowerRightCorner :: Bounds2d units space -> Point2D units space
lowerRightCorner (Bounds2d x y) = Point2D (Bounds.upper x) (Bounds.lower y)

upperLeftCorner :: Bounds2d units space -> Point2D units space
upperLeftCorner (Bounds2d x y) = Point2D (Bounds.lower x) (Bounds.upper y)

upperRightCorner :: Bounds2d units space -> Point2D units space
upperRightCorner (Bounds2d x y) = Point2D (Bounds.upper x) (Bounds.upper y)

corners :: Bounds2d units space -> List (Point2D units space)
corners box =
  [ lowerLeftCorner box
  , lowerRightCorner box
  , upperRightCorner box
  , upperLeftCorner box
  ]

{-# INLINE diameter #-}
diameter :: Bounds2d units space -> Quantity units
diameter bounds = Quantity# (diameter# bounds)

{-# INLINE diameter# #-}
diameter# :: Bounds2d units space -> Double#
diameter# (Bounds2d x y) = hypot2# (Bounds.width# x) (Bounds.width# y)

area_ :: Bounds2d units space -> Quantity (units ?*? units)
area_ (Bounds2d x y) = Bounds.width x ?*? Bounds.width y

area :: Units.Squared units1 units2 => Bounds2d units1 space -> Quantity units2
area (Bounds2d x y) = Bounds.width x .*. Bounds.width y

interpolate :: Bounds2d units space -> Number -> Number -> Point2D units space
interpolate (PositionBounds2d pb) u v = Position2D (VectorBounds2d.interpolate pb u v)

placeIn :: Frame2d units global local -> Bounds2d units local -> Bounds2d units global
placeIn frame (Bounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Point2D x0 y0 = Point2D.placeIn frame (Point2D xMid yMid)
  let Direction2d ix iy = frame.xDirection
  let Direction2d jx jy = frame.yDirection
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs jx
  let ry = 0.5 *. xWidth .*. Number.abs iy .+. 0.5 *. yWidth .*. Number.abs jy
  Bounds2d (Bounds (x0 .-. rx) (x0 .+. rx)) (Bounds (y0 .-. ry) (y0 .+. ry))

relativeTo :: Frame2d units global local -> Bounds2d units global -> Bounds2d units local
relativeTo frame (Bounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Point2D x0 y0 = Point2D.relativeTo frame (Point2D xMid yMid)
  let Direction2d ix iy = frame.xDirection
  let Direction2d jx jy = frame.yDirection
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs iy
  let ry = 0.5 *. xWidth .*. Number.abs jx .+. 0.5 *. yWidth .*. Number.abs jy
  Bounds2d (Bounds (x0 .-. rx) (x0 .+. rx)) (Bounds (y0 .-. ry) (y0 .+. ry))

placeOn :: Plane3d global local -> Bounds2d Meters local -> Bounds3d global
placeOn plane (Bounds2d x y) = do
  let Plane3d _ (PlaneOrientation3d i j) = plane
  let Direction3d ix iy iz = i
  let Direction3d jx jy jz = j
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Point3d x0 y0 z0 = Point3d.on plane (Point2D xMid yMid)
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs jx
  let ry = 0.5 *. xWidth .*. Number.abs iy .+. 0.5 *. yWidth .*. Number.abs jy
  let rz = 0.5 *. xWidth .*. Number.abs iz .+. 0.5 *. yWidth .*. Number.abs jz
  let bx = Bounds (x0 .-. rx) (x0 .+. rx)
  let by = Bounds (y0 .-. ry) (y0 .+. ry)
  let bz = Bounds (z0 .-. rz) (z0 .+. rz)
  Bounds3d bx by bz

transformBy ::
  Transform2d tag units space ->
  Bounds2d units space ->
  Bounds2d units space
transformBy transform (Bounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Point2D x0 y0 = Point2D.transformBy transform (Point2D xMid yMid)
  let Transform2d _ i j = transform
  let Vector2D ix iy = i
  let Vector2D jx jy = j
  let rx = 0.5 *. xWidth .*. Number.abs ix .+. 0.5 *. yWidth .*. Number.abs jx
  let ry = 0.5 *. xWidth .*. Number.abs iy .+. 0.5 *. yWidth .*. Number.abs jy
  Bounds2d (Bounds (x0 .-. rx) (x0 .+. rx)) (Bounds (y0 .-. ry) (y0 .+. ry))

distanceAlong :: Axis2d units space -> Bounds2d units space -> Bounds units
distanceAlong axis (Bounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let d0 = Point2D.distanceAlong axis (Point2D xMid yMid)
  let Direction2d ax ay = Axis2d.direction axis
  let r = 0.5 *. xWidth .*. Number.abs ax .+. 0.5 *. yWidth .*. Number.abs ay
  Bounds (d0 .-. r) (d0 .+. r)

convert :: Quantity (units2 ?/? units1) -> Bounds2d units1 space -> Bounds2d units2 space
convert factor (PositionBounds2d pb) = PositionBounds2d (VectorBounds2d.convert factor pb)

unconvert :: Quantity (units2 ?/? units1) -> Bounds2d units2 space -> Bounds2d units1 space
unconvert factor (PositionBounds2d pb) = PositionBounds2d (VectorBounds2d.unconvert factor pb)
