module OpenSolid.Point2d
  ( Point2d (Point2d)
  , origin
  , x
  , y
  , along
  , xy
  , xyIn
  , fromCoordinates
  , meters
  , centimeters
  , millimeters
  , inches
  , xCoordinate
  , yCoordinate
  , coordinates
  , midpoint
  , interpolateFrom
  , distanceFrom
  , angleFrom
  , signedDistanceAlong
  , signedDistanceFrom
  , placeIn
  , relativeTo
  , convert
  , unconvert
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , mirrorAcross
  , scaleAbout
  , scaleAlong
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis2d (Axis2d)
  , Basis2d (Basis2d)
  , Frame2d (Frame2d)
  , Point2d (Point2d)
  , Transform2d (Transform2d)
  )
import OpenSolid.Qty qualified as Qty
import {-# SOURCE #-} OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units (Meters)
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d

-- | The point with coordinates (0,0).
origin :: Point2d (space @ units)
origin = Point2d Qty.zero Qty.zero

-- | Construct a point along the X axis, with the given X coordinate.
x :: forall space units. Qty units -> Point2d (space @ units)
x px = Point2d px Qty.zero

-- | Construct a point along the Y axis, with the given Y coordinate.
y :: forall space units. Qty units -> Point2d (space @ units)
y py = Point2d Qty.zero py

along :: Axis2d (space @ units) -> Qty units -> Point2d (space @ units)
along (Axis2d originPoint direction) distance = originPoint + distance * direction

-- | Construct a point from its X and Y coordinates.
xy :: forall space units. Qty units -> Qty units -> Point2d (space @ units)
xy = Point2d

xyIn :: Frame2d (space @ units) defines -> Qty units -> Qty units -> Point2d (space @ units)
xyIn (Frame2d p0 (Basis2d i j)) px py = p0 + px * i + py * j

-- | Construct a point from a pair of X and Y coordinates.
fromCoordinates :: forall space units. (Qty units, Qty units) -> Point2d (space @ units)
fromCoordinates (px, py) = Point2d px py

apply :: (Float -> Qty units) -> Float -> Float -> Point2d (space @ units)
apply units fx fy = Point2d (units fx) (units fy)

-- | Construct a point from its X and Y coordinates given in meters.
meters :: Float -> Float -> Point2d (space @ Meters)
meters = apply Length.meters

-- | Construct a point from its X and Y coordinates given in centimeters.
centimeters :: Float -> Float -> Point2d (space @ Meters)
centimeters = apply Length.centimeters

-- | Construct a point from its X and Y coordinates given in millimeters.
millimeters :: Float -> Float -> Point2d (space @ Meters)
millimeters = apply Length.millimeters

-- | Construct a point from its X and Y coordinates given in inches.
inches :: Float -> Float -> Point2d (space @ Meters)
inches = apply Length.inches

-- | Get the X coordinate of a point.
xCoordinate :: Point2d (space @ units) -> Qty units
xCoordinate (Point2d px _) = px

-- | Get the Y coordinate of a point.
yCoordinate :: Point2d (space @ units) -> Qty units
yCoordinate (Point2d _ py) = py

-- | Get the X and Y coordinates of a point.
{-# INLINE coordinates #-}
coordinates :: Point2d (space @ units) -> (Qty units, Qty units)
coordinates (Point2d px py) = (px, py)

interpolateFrom ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Float ->
  Point2d (space @ units)
interpolateFrom (Point2d x1 y1) (Point2d x2 y2) t =
  Point2d (x1 + t * (x2 - x1)) (y1 + t * (y2 - y1))

-- | Find the midpoint between two points.
midpoint :: Point2d (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
midpoint (Point2d x1 y1) (Point2d x2 y2) =
  Point2d (0.5 * (x1 + x2)) (0.5 * (y1 + y2))

-- | Compute the distance from one point to another.
distanceFrom :: Point2d (space @ units) -> Point2d (space @ units) -> Qty units
distanceFrom p1 p2 = Vector2d.magnitude (p2 - p1)

angleFrom :: Point2d (space @ units) -> Point2d (space @ units) -> Angle
angleFrom p1 p2 = Vector2d.angle (p2 - p1)

signedDistanceAlong :: Axis2d (space @ units) -> Point2d (space @ units) -> Qty units
signedDistanceAlong (Axis2d originPoint direction) point = direction <> (point - originPoint)

signedDistanceFrom :: Axis2d (space @ units) -> Point2d (space @ units) -> Qty units
signedDistanceFrom (Axis2d originPoint direction) point = direction >< (point - originPoint)

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Point2d (local @ units) ->
  Point2d (global @ units)
placeIn (Frame2d p0 (Basis2d i j)) (Point2d px py) = p0 + px * i + py * j

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Point2d (global @ units) ->
  Point2d (local @ units)
relativeTo (Frame2d p0 (Basis2d i j)) point = let d = point - p0 in Point2d (d <> i) (d <> j)

convert :: Qty (units2 :/: units1) -> Point2d (space @ units1) -> Point2d (space @ units2)
convert factor (Point2d px py) = Point2d (factor *! px) (factor *! py)

unconvert :: Qty (units2 :/: units1) -> Point2d (space @ units2) -> Point2d (space @ units1)
unconvert factor (Point2d px py) = Point2d (px !/ factor) (py !/ factor)

transformBy :: Transform2d tag (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
transformBy transform point = do
  let (Transform2d p0 vx vy) = transform
  let (px, py) = coordinates point
  p0 + px * vx + py * vy

translateBy :: Vector2d (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
translateBy = Transform2d.translateByImpl transformBy

translateIn :: Direction2d space -> Qty units -> Point2d (space @ units) -> Point2d (space @ units)
translateIn = Transform2d.translateInImpl transformBy

translateAlong :: Axis2d (space @ units) -> Qty units -> Point2d (space @ units) -> Point2d (space @ units)
translateAlong = Transform2d.translateAlongImpl transformBy

rotateAround :: Point2d (space @ units) -> Angle -> Point2d (space @ units) -> Point2d (space @ units)
rotateAround = Transform2d.rotateAroundImpl transformBy

mirrorAcross :: Axis2d (space @ units) -> Point2d (space @ units) -> Point2d (space @ units)
mirrorAcross = Transform2d.mirrorAcrossImpl transformBy

scaleAbout :: Point2d (space @ units) -> Float -> Point2d (space @ units) -> Point2d (space @ units)
scaleAbout = Transform2d.scaleAboutImpl transformBy

scaleAlong :: Axis2d (space @ units) -> Float -> Point2d (space @ units) -> Point2d (space @ units)
scaleAlong = Transform2d.scaleAlongImpl transformBy
