module OpenSolid.Bounds2d
  ( Bounds2d (Bounds2d)
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
  , inclusion
  , includes
  , contains
  , isContainedIn
  , overlap
  , separation
  , intersection
  , lowerLeftCorner
  , lowerRightCorner
  , upperLeftCorner
  , upperRightCorner
  , corners
  , diameter
  , area'
  , area
  , interpolate
  , any
  , all
  , resolve
  , placeIn
  , relativeTo
  , placeOn
  , transformBy
  , distanceAlong
  , convert
  , unconvert
  )
where

import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Direction2d (Direction2d (Direction2d))
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Float qualified as Float
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Bounds2d (Bounds2d)
  , Bounds3d (Bounds3d)
  , Direction3d (Direction3d)
  , PlanarBasis3d (PlanarBasis3d)
  , Plane3d (Plane3d)
  , Point3d (Point3d)
  )
import OpenSolid.Qty qualified as Qty
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d))

-- | Get the X coordinate bounds of a bounding box.
xCoordinate :: Bounds2d (space @ units) -> Bounds units
xCoordinate (Bounds2d x _) = x

-- | Get the Y coordinate bounds of a bounding box.
yCoordinate :: Bounds2d (space @ units) -> Bounds units
yCoordinate (Bounds2d _ y) = y

-- | Get the X and Y coordinate bounds of a bounding box.
{-# INLINE coordinates #-}
coordinates :: Bounds2d (space @ units) -> (Bounds units, Bounds units)
coordinates (Bounds2d x y) = (x, y)

dimensions :: Bounds2d (space @ units) -> (Qty units, Qty units)
dimensions (Bounds2d x y) = (Bounds.width x, Bounds.width y)

centerPoint :: Bounds2d (space @ units) -> Point2d (space @ units)
centerPoint (Bounds2d x y) = Point2d (Bounds.midpoint x) (Bounds.midpoint y)

-- | Construct a zero-size bounding box containing a single point.
constant :: Point2d (space @ units) -> Bounds2d (space @ units)
constant point = do
  let (x, y) = Point2d.coordinates point
  Bounds2d (Bounds.constant x) (Bounds.constant y)

aggregate2 :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bounds2d (space @ units)
aggregate2 (Bounds2d x1 y1) (Bounds2d x2 y2) =
  Bounds2d (Bounds.aggregate2 x1 x2) (Bounds.aggregate2 y1 y2)

-- | Construct a bounding box containing all bounding boxes in the given non-empty list.
aggregateN :: NonEmpty (Bounds2d (space @ units)) -> Bounds2d (space @ units)
aggregateN (Bounds2d (Bounds xLow0 xHigh0) (Bounds yLow0 yHigh0) :| rest) =
  aggregateImpl xLow0 xHigh0 yLow0 yHigh0 rest

aggregateImpl :: Qty units -> Qty units -> Qty units -> Qty units -> List (Bounds2d (space @ units)) -> Bounds2d (space @ units)
aggregateImpl xLow xHigh yLow yHigh [] = Bounds2d (Bounds xLow xHigh) (Bounds yLow yHigh)
aggregateImpl xLow xHigh yLow yHigh (next : remaining) = do
  let Bounds2d (Bounds xLowNext xHighNext) (Bounds yLowNext yHighNext) = next
  aggregateImpl
    (Qty.min xLow xLowNext)
    (Qty.max xHigh xHighNext)
    (Qty.min yLow yLowNext)
    (Qty.max yHigh yHighNext)
    remaining

exclusion :: Point2d (space @ units) -> Bounds2d (space @ units) -> Qty units
exclusion point box = do
  let (x, y) = Point2d.coordinates point
  let (bx, by) = coordinates box
  let dx = Bounds.exclusion x bx
  let dy = Bounds.exclusion y by
  let px = dx >= Qty.zero
  let py = dy >= Qty.zero
  if
    | px && py -> Qty.hypot2 dx dy
    | px -> dx
    | py -> dy
    | otherwise -> Qty.max dx dy

inclusion :: Point2d (space @ units) -> Bounds2d (space @ units) -> Qty units
inclusion point box = -(exclusion point box)

includes :: Point2d (space @ units) -> Bounds2d (space @ units) -> Bool
includes point box = do
  let (px, py) = Point2d.coordinates point
  let (bx, by) = coordinates box
  Bounds.includes px bx && Bounds.includes py by

contains :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bool
contains (Bounds2d x2 y2) (Bounds2d x1 y1) =
  Bounds.contains x2 x1 && Bounds.contains y2 y1

isContainedIn :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

separation :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Qty units
separation (Bounds2d x1 y1) (Bounds2d x2 y2) = do
  let dx = Bounds.separation x1 x2
  let dy = Bounds.separation y1 y2
  let px = dx >= Qty.zero
  let py = dy >= Qty.zero
  if
    | px && py -> Qty.hypot2 dx dy
    | px -> dx
    | py -> dy
    | otherwise -> Qty.max dx dy

overlap :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Qty units
overlap first second = -(separation first second)

intersection :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Maybe (Bounds2d (space @ units))
intersection (Bounds2d x1 y1) (Bounds2d x2 y2) = Maybe.do
  x <- Bounds.intersection x1 x2
  y <- Bounds.intersection y1 y2
  Just (Bounds2d x y)

-- | Construct a bounding box from two corner points.
hull2 ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Bounds2d (space @ units)
hull2 p1 p2 = do
  let (x1, y1) = Point2d.coordinates p1
  let (x2, y2) = Point2d.coordinates p2
  Bounds2d (Bounds x1 x2) (Bounds y1 y2)

hull3 ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Bounds2d (space @ units)
hull3 p1 p2 p3 = do
  let (x1, y1) = Point2d.coordinates p1
  let (x2, y2) = Point2d.coordinates p2
  let (x3, y3) = Point2d.coordinates p3
  let minX = Qty.min (Qty.min x1 x2) x3
  let maxX = Qty.max (Qty.max x1 x2) x3
  let minY = Qty.min (Qty.min y1 y2) y3
  let maxY = Qty.max (Qty.max y1 y2) y3
  Bounds2d (Bounds minX maxX) (Bounds minY maxY)

hull4 ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Bounds2d (space @ units)
hull4 p1 p2 p3 p4 = do
  let (x1, y1) = Point2d.coordinates p1
  let (x2, y2) = Point2d.coordinates p2
  let (x3, y3) = Point2d.coordinates p3
  let (x4, y4) = Point2d.coordinates p4
  let minX = Qty.min (Qty.min (Qty.min x1 x2) x3) x4
  let maxX = Qty.max (Qty.max (Qty.max x1 x2) x3) x4
  let minY = Qty.min (Qty.min (Qty.min y1 y2) y3) y4
  let maxY = Qty.max (Qty.max (Qty.max y1 y2) y3) y4
  Bounds2d (Bounds minX maxX) (Bounds minY maxY)

-- | Construct a bounding box containing all points in the given non-empty list.
hullN :: NonEmpty (Point2d (space @ units)) -> Bounds2d (space @ units)
hullN (p0 :| rest) = do
  let (x0, y0) = Point2d.coordinates p0
  let go xLow xHigh yLow yHigh [] = Bounds2d (Bounds xLow xHigh) (Bounds yLow yHigh)
      go xLow xHigh yLow yHigh (point : remaining) = do
        let (x, y) = Point2d.coordinates point
        go (Qty.min xLow x) (Qty.max xHigh x) (Qty.min yLow y) (Qty.max yHigh y) remaining
  go x0 x0 y0 y0 rest

lowerLeftCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
lowerLeftCorner (Bounds2d x y) = Point2d.xy (Bounds.lower x) (Bounds.lower y)

lowerRightCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
lowerRightCorner (Bounds2d x y) = Point2d.xy (Bounds.upper x) (Bounds.lower y)

upperLeftCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
upperLeftCorner (Bounds2d x y) = Point2d.xy (Bounds.lower x) (Bounds.upper y)

upperRightCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
upperRightCorner (Bounds2d x y) = Point2d.xy (Bounds.upper x) (Bounds.upper y)

corners :: Bounds2d (space @ units) -> List (Point2d (space @ units))
corners box =
  [ lowerLeftCorner box
  , lowerRightCorner box
  , upperRightCorner box
  , upperLeftCorner box
  ]

diameter :: Bounds2d (space @ units) -> Qty units
diameter (Bounds2d x y) = Qty.hypot2 (Bounds.width x) (Bounds.width y)

area' :: Bounds2d (space @ units) -> Qty (units :*: units)
area' (Bounds2d x y) = Bounds.width x .*. Bounds.width y

area :: Units.Squared units1 units2 => Bounds2d (space @ units1) -> Qty units2
area (Bounds2d x y) = Bounds.width x * Bounds.width y

interpolate :: Bounds2d (space @ units) -> Float -> Float -> Point2d (space @ units)
interpolate (Bounds2d x y) u v =
  Point2d.xy (Bounds.interpolate x u) (Bounds.interpolate y v)

any :: (Bounds2d (space @ units) -> Fuzzy Bool) -> Bounds2d (space @ units) -> Bool
any assess box@(Bounds2d x y) =
  case assess box of
    Resolved assessment -> assessment
    Unresolved
      | Bounds.isAtomic x && Bounds.isAtomic y -> False
      | Bounds.isAtomic x -> do
          let (y1, y2) = Bounds.bisect y
          any assess (Bounds2d x y1) || any assess (Bounds2d x y2)
      | Bounds.isAtomic y -> do
          let (x1, x2) = Bounds.bisect x
          any assess (Bounds2d x1 y) || any assess (Bounds2d x2 y)
      | otherwise -> do
          let (x1, x2) = Bounds.bisect x
          let (y1, y2) = Bounds.bisect y
          any assess (Bounds2d x1 y1)
            || any assess (Bounds2d x1 y2)
            || any assess (Bounds2d x2 y1)
            || any assess (Bounds2d x2 y2)

all :: (Bounds2d (space @ units) -> Fuzzy Bool) -> Bounds2d (space @ units) -> Bool
all assess box@(Bounds2d x y) =
  case assess box of
    Resolved assessment -> assessment
    Unresolved
      | Bounds.isAtomic x && Bounds.isAtomic y -> True
      | Bounds.isAtomic x -> do
          let (y1, y2) = Bounds.bisect y
          all assess (Bounds2d x y1) && all assess (Bounds2d x y2)
      | Bounds.isAtomic y -> do
          let (x1, x2) = Bounds.bisect x
          all assess (Bounds2d x1 y) && all assess (Bounds2d x2 y)
      | otherwise -> do
          let (x1, x2) = Bounds.bisect x
          let (y1, y2) = Bounds.bisect y
          all assess (Bounds2d x1 y1)
            && all assess (Bounds2d x1 y2)
            && all assess (Bounds2d x2 y1)
            && all assess (Bounds2d x2 y2)

resolve :: Eq a => (Bounds2d (space @ units) -> Fuzzy a) -> Bounds2d (space @ units) -> Fuzzy a
resolve assess box@(Bounds2d x y) =
  case assess box of
    Resolved value -> Resolved value
    Unresolved
      | Bounds.isAtomic x && Bounds.isAtomic y -> Unresolved
      | Bounds.isAtomic x -> Fuzzy.do
          let (y1, y2) = Bounds.bisect y
          value1 <- resolve assess (Bounds2d x y1)
          value2 <- resolve assess (Bounds2d x y2)
          if value1 == value2 then Resolved value1 else Unresolved
      | Bounds.isAtomic y -> Fuzzy.do
          let (x1, x2) = Bounds.bisect x
          value1 <- resolve assess (Bounds2d x1 y)
          value2 <- resolve assess (Bounds2d x2 y)
          if value1 == value2 then Resolved value1 else Unresolved
      | otherwise -> Fuzzy.do
          let (x1, x2) = Bounds.bisect x
          let (y1, y2) = Bounds.bisect y
          value11 <- resolve assess (Bounds2d x1 y1)
          value12 <- resolve assess (Bounds2d x1 y2)
          value21 <- resolve assess (Bounds2d x2 y1)
          value22 <- resolve assess (Bounds2d x2 y2)
          if value11 == value12 && value11 == value21 && value11 == value22
            then Resolved value11
            else Unresolved

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
  let (ix, iy) = Direction2d.components (Frame2d.xDirection frame)
  let (jx, jy) = Direction2d.components (Frame2d.yDirection frame)
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs jx
  let ry = 0.5 * xWidth * Float.abs iy + 0.5 * yWidth * Float.abs jy
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
  let (x0, y0) = Point2d.coordinates (Point2d.relativeTo frame (Point2d.xy xMid yMid))
  let (ix, iy) = Direction2d.components (Frame2d.xDirection frame)
  let (jx, jy) = Direction2d.components (Frame2d.yDirection frame)
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs iy
  let ry = 0.5 * xWidth * Float.abs jx + 0.5 * yWidth * Float.abs jy
  Bounds2d (Bounds (x0 - rx) (x0 + rx)) (Bounds (y0 - ry) (y0 + ry))

placeOn ::
  Plane3d (space @ units) (Defines local) ->
  Bounds2d (local @ units) ->
  Bounds3d (space @ units)
placeOn plane (Bounds2d x y) = do
  let Plane3d _ (PlanarBasis3d i j) = plane
  let Direction3d ix iy iz = i
  let Direction3d jx jy jz = j
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Point3d x0 y0 z0 = Point3d.on plane (Point2d xMid yMid)
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs jx
  let ry = 0.5 * xWidth * Float.abs iy + 0.5 * yWidth * Float.abs jy
  let rz = 0.5 * xWidth * Float.abs iz + 0.5 * yWidth * Float.abs jz
  Bounds3d (Bounds (x0 - rx) (x0 + rx)) (Bounds (y0 - ry) (y0 + ry)) (Bounds (z0 - rz) (z0 + rz))

transformBy ::
  Transform2d tag (space @ units) ->
  Bounds2d (space @ units) ->
  Bounds2d (space @ units)
transformBy transform (Bounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let Point2d x0 y0 = Point2d.transformBy transform (Point2d.xy xMid yMid)
  let Transform2d _ i j = transform
  let Vector2d ix iy = i
  let Vector2d jx jy = j
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs jx
  let ry = 0.5 * xWidth * Float.abs iy + 0.5 * yWidth * Float.abs jy
  Bounds2d (Bounds (x0 - rx) (x0 + rx)) (Bounds (y0 - ry) (y0 + ry))

distanceAlong :: Axis2d (space @ units) -> Bounds2d (space @ units) -> Bounds units
distanceAlong axis (Bounds2d x y) = do
  let xMid = Bounds.midpoint x
  let yMid = Bounds.midpoint y
  let xWidth = Bounds.width x
  let yWidth = Bounds.width y
  let d0 = Point2d.distanceAlong axis (Point2d.xy xMid yMid)
  let Direction2d ax ay = Axis2d.direction axis
  let r = 0.5 * xWidth * Float.abs ax + 0.5 * yWidth * Float.abs ay
  Bounds (d0 - r) (d0 + r)

convert :: Qty (units2 :/: units1) -> Bounds2d (space @ units1) -> Bounds2d (space @ units2)
convert factor (Bounds2d x y) = Bounds2d (x !* factor) (y !* factor)

unconvert :: Qty (units2 :/: units1) -> Bounds2d (space @ units2) -> Bounds2d (space @ units1)
unconvert factor (Bounds2d x y) = Bounds2d (x !/ factor) (y !/ factor)
