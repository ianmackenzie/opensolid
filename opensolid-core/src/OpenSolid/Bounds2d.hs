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
  , exclusion##
  , inclusion
  , inclusion##
  , includes
  , contains
  , isContainedIn
  , overlap
  , overlap##
  , separation
  , separation##
  , intersection
  , lowerLeftCorner
  , lowerRightCorner
  , upperLeftCorner
  , upperRightCorner
  , corners
  , diameter
  , diameter##
  , area'
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
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Bounds2d (Bounds2d, PositionBounds2d)
  , Bounds3d (Bounds3d)
  , Direction3d (Direction3d)
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , Point2d (Position2d)
  , Point3d (Point3d)
  )
import OpenSolid.Quantity (Quantity (Quantity##))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.Vertex2d (Vertex2d)
import OpenSolid.Vertex2d qualified as Vertex2d

coerce :: Bounds2d (space1 @ units1) -> Bounds2d (space2 @ units2)
coerce (Bounds2d x y) = Bounds2d (Bounds.coerce x) (Bounds.coerce y)

-- | Get the X coordinate bounds of a bounding box.
xCoordinate :: Bounds2d (space @ units) -> Bounds units
xCoordinate (PositionBounds2d pb) = VectorBounds2d.xComponent pb

-- | Get the Y coordinate bounds of a bounding box.
yCoordinate :: Bounds2d (space @ units) -> Bounds units
yCoordinate (PositionBounds2d pb) = VectorBounds2d.yComponent pb

-- | Get the X and Y coordinate bounds of a bounding box.
{-# INLINE coordinates #-}
coordinates :: Bounds2d (space @ units) -> (Bounds units, Bounds units)
coordinates (PositionBounds2d pb) = VectorBounds2d.components pb

dimensions :: Bounds2d (space @ units) -> (Quantity units, Quantity units)
dimensions (Bounds2d x y) = (Bounds.width x, Bounds.width y)

centerPoint :: Bounds2d (space @ units) -> Point2d (space @ units)
centerPoint (Bounds2d x y) = Point2d (Bounds.midpoint x) (Bounds.midpoint y)

-- | Construct a zero-size bounding box containing a single point.
constant :: Point2d (space @ units) -> Bounds2d (space @ units)
constant (Point2d x y) = Bounds2d (Bounds.constant x) (Bounds.constant y)

aggregate2 :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bounds2d (space @ units)
aggregate2 (PositionBounds2d pb1) (PositionBounds2d pb2) =
  PositionBounds2d (VectorBounds2d.aggregate2 pb1 pb2)

-- | Construct a bounding box containing all bounding boxes in the given non-empty list.
aggregateN :: NonEmpty (Bounds2d (space @ units)) -> Bounds2d (space @ units)
aggregateN list = PositionBounds2d (VectorBounds2d.aggregateN (Data.Coerce.coerce list))

exclusion :: Point2d (space @ units) -> Bounds2d (space @ units) -> Quantity units
exclusion (Position2d p) (PositionBounds2d pb) = VectorBounds2d.exclusion p pb

{-# INLINE exclusion## #-}
exclusion## :: Point2d (space @ units) -> Bounds2d (space @ units) -> Double#
exclusion## (Position2d p) (PositionBounds2d pb) = VectorBounds2d.exclusion## p pb

inclusion :: Point2d (space @ units) -> Bounds2d (space @ units) -> Quantity units
inclusion (Position2d p) (PositionBounds2d pb) = VectorBounds2d.inclusion p pb

{-# INLINE inclusion## #-}
inclusion## :: Point2d (space @ units) -> Bounds2d (space @ units) -> Double#
inclusion## (Position2d p) (PositionBounds2d pb) = VectorBounds2d.inclusion## p pb

includes :: Point2d (space @ units) -> Bounds2d (space @ units) -> Bool
includes (Position2d p) (PositionBounds2d pb) = VectorBounds2d.includes p pb

contains :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bool
contains (PositionBounds2d pb2) (PositionBounds2d pb1) = VectorBounds2d.contains pb2 pb1

isContainedIn :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

{-# INLINE separation #-}
separation :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Quantity units
separation (PositionBounds2d pb1) (PositionBounds2d pb2) = VectorBounds2d.separation pb1 pb2

{-# INLINE separation## #-}
separation## :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Double#
separation## (PositionBounds2d pb1) (PositionBounds2d pb2) = VectorBounds2d.separation## pb1 pb2

{-# INLINE overlap #-}
overlap :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Quantity units
overlap (PositionBounds2d pb1) (PositionBounds2d pb2) = VectorBounds2d.overlap pb1 pb2

{-# INLINE overlap## #-}
overlap## :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Double#
overlap## (PositionBounds2d pb1) (PositionBounds2d pb2) = VectorBounds2d.overlap## pb1 pb2

intersection ::
  Bounds2d (space @ units) ->
  Bounds2d (space @ units) ->
  Maybe (Bounds2d (space @ units))
intersection (PositionBounds2d pb1) (PositionBounds2d pb2) =
  Maybe.map PositionBounds2d (VectorBounds2d.intersection pb1 pb2)

-- | Construct a bounding box from two corner points.
hull2 ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Bounds2d (space @ units)
hull2 (Point2d x1 y1) (Point2d x2 y2) = Bounds2d (Bounds x1 x2) (Bounds y1 y2)

hull3 ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Bounds2d (space @ units)
hull3 (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3) = do
  let minX = Quantity.min (Quantity.min x1 x2) x3
  let maxX = Quantity.max (Quantity.max x1 x2) x3
  let minY = Quantity.min (Quantity.min y1 y2) y3
  let maxY = Quantity.max (Quantity.max y1 y2) y3
  Bounds2d (Bounds minX maxX) (Bounds minY maxY)

hull4 ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Bounds2d (space @ units)
hull4 (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3) (Point2d x4 y4) = do
  let minX = Quantity.min (Quantity.min (Quantity.min x1 x2) x3) x4
  let maxX = Quantity.max (Quantity.max (Quantity.max x1 x2) x3) x4
  let minY = Quantity.min (Quantity.min (Quantity.min y1 y2) y3) y4
  let maxY = Quantity.max (Quantity.max (Quantity.max y1 y2) y3) y4
  Bounds2d (Bounds minX maxX) (Bounds minY maxY)

-- | Construct a bounding box containing all vertices in the given non-empty list.
hullN :: Vertex2d vertex (space @ units) => NonEmpty vertex -> Bounds2d (space @ units)
hullN (v0 :| rest) = do
  let Point2d x0 y0 = Vertex2d.position v0
  let go xLow xHigh yLow yHigh [] = Bounds2d (Bounds xLow xHigh) (Bounds yLow yHigh)
      go xLow xHigh yLow yHigh (vertex : remaining) = do
        let (x, y) = Point2d.coordinates (Vertex2d.position vertex)
        go
          (Quantity.min xLow x)
          (Quantity.max xHigh x)
          (Quantity.min yLow y)
          (Quantity.max yHigh y)
          remaining
  go x0 x0 y0 y0 rest

lowerLeftCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
lowerLeftCorner (Bounds2d x y) = Point2d (Bounds.lower x) (Bounds.lower y)

lowerRightCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
lowerRightCorner (Bounds2d x y) = Point2d (Bounds.upper x) (Bounds.lower y)

upperLeftCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
upperLeftCorner (Bounds2d x y) = Point2d (Bounds.lower x) (Bounds.upper y)

upperRightCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
upperRightCorner (Bounds2d x y) = Point2d (Bounds.upper x) (Bounds.upper y)

corners :: Bounds2d (space @ units) -> List (Point2d (space @ units))
corners box =
  [ lowerLeftCorner box
  , lowerRightCorner box
  , upperRightCorner box
  , upperLeftCorner box
  ]

{-# INLINE diameter #-}
diameter :: Bounds2d (space @ units) -> Quantity units
diameter bounds = Quantity## (diameter## bounds)

{-# INLINE diameter## #-}
diameter## :: Bounds2d (space @ units) -> Double#
diameter## (Bounds2d x y) = hypot2## (Bounds.width## x) (Bounds.width## y)

area' :: Bounds2d (space @ units) -> Quantity (units :*: units)
area' (Bounds2d x y) = Bounds.width x *# Bounds.width y

area :: Units.Squared units1 units2 => Bounds2d (space @ units1) -> Quantity units2
area (Bounds2d x y) = Bounds.width x * Bounds.width y

interpolate :: Bounds2d (space @ units) -> Number -> Number -> Point2d (space @ units)
interpolate (PositionBounds2d pb) u v = Position2d (VectorBounds2d.interpolate pb u v)

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Bounds2d (local @ units) ->
  Bounds2d (global @ units)
placeIn frame (Bounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Point2d x0 y0 = Point2d.placeIn frame (Point2d xMid yMid)
  let Direction2d ix iy = frame.xDirection
  let Direction2d jx jy = frame.yDirection
  let rx = 0.5 * xWidth * Number.abs ix + 0.5 * yWidth * Number.abs jx
  let ry = 0.5 * xWidth * Number.abs iy + 0.5 * yWidth * Number.abs jy
  Bounds2d (Bounds (x0 - rx) (x0 + rx)) (Bounds (y0 - ry) (y0 + ry))

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Bounds2d (global @ units) ->
  Bounds2d (local @ units)
relativeTo frame (Bounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Point2d x0 y0 = Point2d.relativeTo frame (Point2d xMid yMid)
  let Direction2d ix iy = frame.xDirection
  let Direction2d jx jy = frame.yDirection
  let rx = 0.5 * xWidth * Number.abs ix + 0.5 * yWidth * Number.abs iy
  let ry = 0.5 * xWidth * Number.abs jx + 0.5 * yWidth * Number.abs jy
  Bounds2d (Bounds (x0 - rx) (x0 + rx)) (Bounds (y0 - ry) (y0 + ry))

placeOn ::
  Plane3d (space @ units) (Defines local) ->
  Bounds2d (local @ units) ->
  Bounds3d (space @ units)
placeOn plane (Bounds2d x y) = do
  let Plane3d _ (PlaneOrientation3d i j) = plane
  let Direction3d ix iy iz = i
  let Direction3d jx jy jz = j
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Point3d x0 y0 z0 = Point3d.on plane (Point2d xMid yMid)
  let rx = 0.5 * xWidth * Number.abs ix + 0.5 * yWidth * Number.abs jx
  let ry = 0.5 * xWidth * Number.abs iy + 0.5 * yWidth * Number.abs jy
  let rz = 0.5 * xWidth * Number.abs iz + 0.5 * yWidth * Number.abs jz
  let bx = Bounds (x0 - rx) (x0 + rx)
  let by = Bounds (y0 - ry) (y0 + ry)
  let bz = Bounds (z0 - rz) (z0 + rz)
  Bounds3d bx by bz

transformBy ::
  Transform2d tag (space @ units) ->
  Bounds2d (space @ units) ->
  Bounds2d (space @ units)
transformBy transform (Bounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Point2d x0 y0 = Point2d.transformBy transform (Point2d xMid yMid)
  let Transform2d _ i j = transform
  let Vector2d ix iy = i
  let Vector2d jx jy = j
  let rx = 0.5 * xWidth * Number.abs ix + 0.5 * yWidth * Number.abs jx
  let ry = 0.5 * xWidth * Number.abs iy + 0.5 * yWidth * Number.abs jy
  Bounds2d (Bounds (x0 - rx) (x0 + rx)) (Bounds (y0 - ry) (y0 + ry))

distanceAlong :: Axis2d (space @ units) -> Bounds2d (space @ units) -> Bounds units
distanceAlong axis (Bounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let d0 = Point2d.distanceAlong axis (Point2d xMid yMid)
  let Direction2d ax ay = Axis2d.direction axis
  let r = 0.5 * xWidth * Number.abs ax + 0.5 * yWidth * Number.abs ay
  Bounds (d0 - r) (d0 + r)

convert :: Quantity (units2 :/: units1) -> Bounds2d (space @ units1) -> Bounds2d (space @ units2)
convert factor (PositionBounds2d pb) = PositionBounds2d (VectorBounds2d.convert factor pb)

unconvert :: Quantity (units2 :/: units1) -> Bounds2d (space @ units2) -> Bounds2d (space @ units1)
unconvert factor (PositionBounds2d pb) = PositionBounds2d (VectorBounds2d.unconvert factor pb)
