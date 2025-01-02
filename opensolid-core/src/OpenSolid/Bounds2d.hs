module OpenSolid.Bounds2d
  ( Bounds2d (Bounds2d)
  , xCoordinate
  , yCoordinate
  , coordinates
  , xy
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
  , interpolate
  , any
  , all
  , resolve
  , placeIn
  , relativeTo
  , transformBy
  , signedDistanceAlong
  , convert
  , unconvert
  )
where

import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Float qualified as Float
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range (Range))
import OpenSolid.Range qualified as Range
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Units (Meters)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))

type role Bounds2d nominal

data Bounds2d (coordinateSystem :: CoordinateSystem) where
  Bounds2d ::
    Range units ->
    Range units ->
    Bounds2d (space @ units)

deriving instance Show (Bounds2d (space @ units))

instance HasUnits (Bounds2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (Bounds2d (space1 @ unitsA)) (Bounds2d (space2 @ unitsB))
  where
  coerce (Bounds2d x y) = Bounds2d (Units.coerce x) (Units.coerce y)

instance Bounds.Interface (Bounds2d (space @ units)) where
  aggregate2 = aggregate2
  intersection = intersection

instance FFI (Bounds2d (space @ Meters)) where
  representation = FFI.classRepresentation "Bounds2d"

instance FFI (Bounds2d (space @ Unitless)) where
  representation = FFI.classRepresentation "UvBounds"

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Point2d (space1 @ units1))
    (Bounds2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  Point2d px py - Bounds2d bx by = VectorBounds2d (px - bx) (py - by)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (Point2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  Bounds2d bx by - Point2d px py = VectorBounds2d (bx - px) (by - py)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (Bounds2d (space2 @ units2))
    (VectorBounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 - Bounds2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Bounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 + Vector2d x2 y2 = Bounds2d (x1 + x2) (y1 + y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (Bounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 + VectorBounds2d x2 y2 = Bounds2d (x1 + x2) (y1 + y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 - Vector2d x2 y2 = Bounds2d (x1 - x2) (y1 - y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (Bounds2d (space1 @ units1))
    (VectorBounds2d (space2 @ units2))
    (Bounds2d (space1 @ units1))
  where
  Bounds2d x1 y1 - VectorBounds2d x2 y2 = Bounds2d (x1 - x2) (y1 - y2)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  ApproximateEquality (Point2d (space1 @ units1)) (Bounds2d (space2 @ units2)) units1
  where
  Point2d px py ~= Bounds2d bx by = px ~= bx && py ~= by

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  ApproximateEquality (Bounds2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  bounds ~= point = point ~= bounds

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Point2d (space1 @ units1)) (Bounds2d (space2 @ units2)) units1
  where
  Point2d px py ^ Bounds2d bx by = px ^ bx && py ^ by

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Bounds2d (space1 @ units1)) (Point2d (space2 @ units2)) units1
  where
  bounds ^ point = point ^ bounds

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Intersects (Bounds2d (space1 @ units1)) (Bounds2d (space2 @ units2)) units1
  where
  Bounds2d x1 y1 ^ Bounds2d x2 y2 = x1 ^ x2 && y1 ^ y2

-- | Get the X coordinate range of a bounding box.
xCoordinate :: Bounds2d (space @ units) -> Range units
xCoordinate (Bounds2d x _) = x

-- | Get the Y coordinate range of a bounding box.
yCoordinate :: Bounds2d (space @ units) -> Range units
yCoordinate (Bounds2d _ y) = y

-- | Get the X and Y coordinate ranges of a bounding box.
{-# INLINE coordinates #-}
coordinates :: Bounds2d (space @ units) -> (Range units, Range units)
coordinates (Bounds2d x y) = (x, y)

-- | Construct a bounding box from its X and Y coordinate ranges.
xy :: forall space units. Range units -> Range units -> Bounds2d (space @ units)
xy = Bounds2d

-- | Construct a zero-size bounding box containing a single point.
constant :: Point2d (space @ units) -> Bounds2d (space @ units)
constant point = do
  let (x, y) = Point2d.coordinates point
  Bounds2d (Range.constant x) (Range.constant y)

aggregate2 :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bounds2d (space @ units)
aggregate2 (Bounds2d x1 y1) (Bounds2d x2 y2) =
  Bounds2d (Range.aggregate2 x1 x2) (Range.aggregate2 y1 y2)

-- | Construct a bounding box containing all bounding boxes in the given non-empty list.
aggregateN :: NonEmpty (Bounds2d (space @ units)) -> Bounds2d (space @ units)
aggregateN (Bounds2d (Range xLow0 xHigh0) (Range yLow0 yHigh0) :| rest) =
  aggregateImpl xLow0 xHigh0 yLow0 yHigh0 rest

aggregateImpl :: Qty units -> Qty units -> Qty units -> Qty units -> List (Bounds2d (space @ units)) -> Bounds2d (space @ units)
aggregateImpl xLow xHigh yLow yHigh [] = Bounds2d (Range.unsafe xLow xHigh) (Range.unsafe yLow yHigh)
aggregateImpl xLow xHigh yLow yHigh (next : remaining) = do
  let Bounds2d (Range xLowNext xHighNext) (Range yLowNext yHighNext) = next
  aggregateImpl
    (Qty.min xLow xLowNext)
    (Qty.max xHigh xHighNext)
    (Qty.min yLow yLowNext)
    (Qty.max yHigh yHighNext)
    remaining

exclusion :: Point2d (space @ units) -> Bounds2d (space @ units) -> Qty units
exclusion point bounds = do
  let (x, y) = Point2d.coordinates point
  let (bx, by) = coordinates bounds
  let dx = Range.exclusion x bx
  let dy = Range.exclusion y by
  let px = dx >= Qty.zero
  let py = dy >= Qty.zero
  if
    | px && py -> Qty.hypot2 dx dy
    | px -> dx
    | py -> dy
    | otherwise -> Qty.max dx dy

inclusion :: Point2d (space @ units) -> Bounds2d (space @ units) -> Qty units
inclusion point bounds = -(exclusion point bounds)

includes :: Point2d (space @ units) -> Bounds2d (space @ units) -> Bool
includes point bounds = do
  let (px, py) = Point2d.coordinates point
  let (bx, by) = coordinates bounds
  Range.includes px bx && Range.includes py by

contains :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bool
contains (Bounds2d x2 y2) (Bounds2d x1 y1) =
  Range.contains x2 x1 && Range.contains y2 y1

isContainedIn :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

separation :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Qty units
separation (Bounds2d x1 y1) (Bounds2d x2 y2) = do
  let dx = Range.separation x1 x2
  let dy = Range.separation y1 y2
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
  x <- Range.intersection x1 x2
  y <- Range.intersection y1 y2
  Just (Bounds2d x y)

-- | Construct a bounding box from two corner points.
hull2 ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Bounds2d (space @ units)
hull2 p1 p2 = do
  let (x1, y1) = Point2d.coordinates p1
  let (x2, y2) = Point2d.coordinates p2
  Bounds2d (Range.from x1 x2) (Range.from y1 y2)

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
  Bounds2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

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
  Bounds2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

-- | Construct a bounding box containing all points in the given non-empty list.
hullN :: NonEmpty (Point2d (space @ units)) -> Bounds2d (space @ units)
hullN (p0 :| rest) = do
  let (x0, y0) = Point2d.coordinates p0
  let go xLow xHigh yLow yHigh [] = Bounds2d (Range.unsafe xLow xHigh) (Range.unsafe yLow yHigh)
      go xLow xHigh yLow yHigh (point : remaining) = do
        let (x, y) = Point2d.coordinates point
        go (Qty.min xLow x) (Qty.max xHigh x) (Qty.min yLow y) (Qty.max yHigh y) remaining
  go x0 x0 y0 y0 rest

lowerLeftCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
lowerLeftCorner (Bounds2d x y) = Point2d.xy (Range.lowerBound x) (Range.lowerBound y)

lowerRightCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
lowerRightCorner (Bounds2d x y) = Point2d.xy (Range.upperBound x) (Range.lowerBound y)

upperLeftCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
upperLeftCorner (Bounds2d x y) = Point2d.xy (Range.lowerBound x) (Range.upperBound y)

upperRightCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
upperRightCorner (Bounds2d x y) = Point2d.xy (Range.upperBound x) (Range.upperBound y)

corners :: Bounds2d (space @ units) -> List (Point2d (space @ units))
corners bounds =
  [ lowerLeftCorner bounds
  , lowerRightCorner bounds
  , upperRightCorner bounds
  , upperLeftCorner bounds
  ]

diameter :: Bounds2d (space @ units) -> Qty units
diameter (Bounds2d x y) = Qty.hypot2 (Range.width x) (Range.width y)

interpolate :: Bounds2d (space @ units) -> Float -> Float -> Point2d (space @ units)
interpolate (Bounds2d x y) u v =
  Point2d.xy (Range.interpolate x u) (Range.interpolate y v)

any :: (Bounds2d (space @ units) -> Fuzzy Bool) -> Bounds2d (space @ units) -> Bool
any assess bounds@(Bounds2d x y) =
  case assess bounds of
    Resolved assessment -> assessment
    Unresolved
      | Range.isAtomic x && Range.isAtomic y -> False
      | Range.isAtomic x -> do
          let (y1, y2) = Range.bisect y
          any assess (Bounds2d x y1) || any assess (Bounds2d x y2)
      | Range.isAtomic y -> do
          let (x1, x2) = Range.bisect x
          any assess (Bounds2d x1 y) || any assess (Bounds2d x2 y)
      | otherwise -> do
          let (x1, x2) = Range.bisect x
          let (y1, y2) = Range.bisect y
          any assess (Bounds2d x1 y1)
            || any assess (Bounds2d x1 y2)
            || any assess (Bounds2d x2 y1)
            || any assess (Bounds2d x2 y2)

all :: (Bounds2d (space @ units) -> Fuzzy Bool) -> Bounds2d (space @ units) -> Bool
all assess bounds@(Bounds2d x y) =
  case assess bounds of
    Resolved assessment -> assessment
    Unresolved
      | Range.isAtomic x && Range.isAtomic y -> True
      | Range.isAtomic x -> do
          let (y1, y2) = Range.bisect y
          all assess (Bounds2d x y1) && all assess (Bounds2d x y2)
      | Range.isAtomic y -> do
          let (x1, x2) = Range.bisect x
          all assess (Bounds2d x1 y) && all assess (Bounds2d x2 y)
      | otherwise -> do
          let (x1, x2) = Range.bisect x
          let (y1, y2) = Range.bisect y
          all assess (Bounds2d x1 y1)
            && all assess (Bounds2d x1 y2)
            && all assess (Bounds2d x2 y1)
            && all assess (Bounds2d x2 y2)

resolve :: Eq a => (Bounds2d (space @ units) -> Fuzzy a) -> Bounds2d (space @ units) -> Fuzzy a
resolve assess bounds@(Bounds2d x y) =
  case assess bounds of
    Resolved value -> Resolved value
    Unresolved
      | Range.isAtomic x && Range.isAtomic y -> Unresolved
      | Range.isAtomic x -> Fuzzy.do
          let (y1, y2) = Range.bisect y
          value1 <- resolve assess (Bounds2d x y1)
          value2 <- resolve assess (Bounds2d x y2)
          if value1 == value2 then Resolved value1 else Unresolved
      | Range.isAtomic y -> Fuzzy.do
          let (x1, x2) = Range.bisect x
          value1 <- resolve assess (Bounds2d x1 y)
          value2 <- resolve assess (Bounds2d x2 y)
          if value1 == value2 then Resolved value1 else Unresolved
      | otherwise -> Fuzzy.do
          let (x1, x2) = Range.bisect x
          let (y1, y2) = Range.bisect y
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
  let xMid = Range.midpoint x
  let yMid = Range.midpoint y
  let xWidth = Range.width x
  let yWidth = Range.width y
  let (x0, y0) = Point2d.coordinates (Point2d.xyIn frame xMid yMid)
  let (ix, iy) = Direction2d.components (Frame2d.xDirection frame)
  let (jx, jy) = Direction2d.components (Frame2d.yDirection frame)
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs jx
  let ry = 0.5 * xWidth * Float.abs iy + 0.5 * yWidth * Float.abs jy
  Bounds2d (Range.from (x0 - rx) (x0 + rx)) (Range.from (y0 - ry) (y0 + ry))

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Bounds2d (global @ units) ->
  Bounds2d (local @ units)
relativeTo frame (Bounds2d x y) = do
  let xMid = Range.midpoint x
  let yMid = Range.midpoint y
  let xWidth = Range.width x
  let yWidth = Range.width y
  let (x0, y0) = Point2d.coordinates (Point2d.relativeTo frame (Point2d.xy xMid yMid))
  let (ix, iy) = Direction2d.components (Frame2d.xDirection frame)
  let (jx, jy) = Direction2d.components (Frame2d.yDirection frame)
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs iy
  let ry = 0.5 * xWidth * Float.abs jx + 0.5 * yWidth * Float.abs jy
  Bounds2d (Range.from (x0 - rx) (x0 + rx)) (Range.from (y0 - ry) (y0 + ry))

transformBy ::
  Transform2d tag (space @ units) ->
  Bounds2d (space @ units) ->
  Bounds2d (space @ units)
transformBy transform (Bounds2d x y) = do
  let xMid = Range.midpoint x
  let yMid = Range.midpoint y
  let xWidth = Range.width x
  let yWidth = Range.width y
  let (x0, y0) = Point2d.coordinates (Point2d.transformBy transform (Point2d.xy xMid yMid))
  let (Transform2d _ i j) = transform
  let (ix, iy) = Vector2d.components i
  let (jx, jy) = Vector2d.components j
  let rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs jx
  let ry = 0.5 * xWidth * Float.abs iy + 0.5 * yWidth * Float.abs jy
  Bounds2d (Range.from (x0 - rx) (x0 + rx)) (Range.from (y0 - ry) (y0 + ry))

signedDistanceAlong :: Axis2d (space @ units) -> Bounds2d (space @ units) -> Range units
signedDistanceAlong axis (Bounds2d x y) = do
  let xMid = Range.midpoint x
  let yMid = Range.midpoint y
  let xWidth = Range.width x
  let yWidth = Range.width y
  let d0 = Point2d.signedDistanceAlong axis (Point2d.xy xMid yMid)
  let (ax, ay) = Direction2d.components (Axis2d.direction axis)
  let r = 0.5 * xWidth * Float.abs ax + 0.5 * yWidth * Float.abs ay
  Range.from (d0 - r) (d0 + r)

convert :: Qty (units2 :/: units1) -> Bounds2d (space @ units1) -> Bounds2d (space @ units2)
convert conversion (Bounds2d x y) =
  Bounds2d (Range.convert conversion x) (Range.convert conversion y)

unconvert :: Qty (units2 :/: units1) -> Bounds2d (space @ units2) -> Bounds2d (space @ units1)
unconvert conversion (Bounds2d x y) =
  Bounds2d (Range.unconvert conversion x) (Range.unconvert conversion y)
