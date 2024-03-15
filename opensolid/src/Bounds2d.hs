module Bounds2d
  ( Bounds2d (Bounds2d)
  , xCoordinate
  , yCoordinate
  , xy
  , constant
  , hull2
  , hull3
  , hull4
  , hullN
  , aggregate2
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
  , sample
  , any
  , all
  , resolve
  , find
  , placeIn
  , relativeTo
  , signedDistanceAlong
  , convert
  , unconvert
  )
where

import Axis2d (Axis2d)
import Axis2d qualified
import Bounds qualified
import CoordinateSystem qualified
import Data.Coerce qualified
import Direction2d (Direction2d (Direction2d))
import Float qualified
import Frame2d (Frame2d)
import Frame2d qualified
import Fuzzy qualified
import Maybe qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Point2d qualified
import Qty qualified
import Quadrature qualified
import Range (Range)
import Range qualified
import Units qualified
import Vector2d (Vector2d (Vector2d))
import VectorBounds2d (VectorBounds2d (VectorBounds2d))

type role Bounds2d phantom

data Bounds2d (coordinateSystem :: CoordinateSystem) where
  Bounds2d ::
    Range (CoordinateSystem.Units coordinateSystem) ->
    Range (CoordinateSystem.Units coordinateSystem) ->
    Bounds2d coordinateSystem

deriving instance Show (Bounds2d (space @ units))

instance
  (units1 ~ units1', units2 ~ units2', space ~ space') =>
  Units.Coercion
    units1
    units2
    (Bounds2d (space @ units1'))
    (Bounds2d (space' @ units2'))
  where
  coerce = Data.Coerce.coerce

instance Bounds.Interface (Bounds2d (space @ units)) where
  aggregate2 = aggregate2
  intersection = intersection

instance (units ~ units', space ~ space') => Subtraction (Point2d (space @ units)) (Bounds2d (space' @ units')) (VectorBounds2d (space @ units)) where
  Point2d px py - Bounds2d bx by = VectorBounds2d (px - bx) (py - by)

instance (units ~ units', space ~ space') => Subtraction (Bounds2d (space @ units)) (Point2d (space' @ units')) (VectorBounds2d (space @ units)) where
  Bounds2d bx by - Point2d px py = VectorBounds2d (bx - px) (by - py)

instance (units ~ units', space ~ space') => Subtraction (Bounds2d (space @ units)) (Bounds2d (space' @ units')) (VectorBounds2d (space @ units)) where
  Bounds2d x1 y1 - Bounds2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

instance (space ~ space', units ~ units') => ApproximateEquality (Point2d (space @ units)) (Bounds2d (space' @ units')) units where
  Point2d px py ~= Bounds2d bx by = px ~= bx && py ~= by

instance (space ~ space', units ~ units') => ApproximateEquality (Bounds2d (space @ units)) (Point2d (space' @ units')) units where
  bounds ~= point = point ~= bounds

instance (space ~ space', units ~ units') => Intersects (Point2d (space @ units)) (Bounds2d (space' @ units')) units where
  Point2d px py ^ Bounds2d bx by = px ^ bx && py ^ by

instance (space ~ space', units ~ units') => Intersects (Bounds2d (space @ units)) (Point2d (space' @ units')) units where
  bounds ^ point = point ^ bounds

instance (space ~ space', units ~ units') => Intersects (Bounds2d (space @ units)) (Bounds2d (space' @ units')) units where
  Bounds2d x1 y1 ^ Bounds2d x2 y2 = x1 ^ x2 && y1 ^ y2

xCoordinate :: Bounds2d (space @ units) -> Range units
xCoordinate (Bounds2d x _) = x

yCoordinate :: Bounds2d (space @ units) -> Range units
yCoordinate (Bounds2d _ y) = y

xy :: Range units -> Range units -> Bounds2d (space @ units)
xy = Bounds2d

constant :: Point2d (space @ units) -> Bounds2d (space @ units)
constant (Point2d x y) =
  Bounds2d (Range.constant x) (Range.constant y)

aggregate2 :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bounds2d (space @ units)
aggregate2 (Bounds2d x1 y1) (Bounds2d x2 y2) =
  Bounds2d (Range.aggregate2 x1 x2) (Range.aggregate2 y1 y2)

exclusion :: Point2d (space @ units) -> Bounds2d (space @ units) -> Qty units
exclusion (Point2d x y) (Bounds2d bx by)
  | px && py = Qty.hypot2 dx dy
  | px = dx
  | py = dy
  | otherwise = Qty.max dx dy
 where
  dx = Range.exclusion x bx
  dy = Range.exclusion y by
  px = dx >= Qty.zero
  py = dy >= Qty.zero

inclusion :: Point2d (space @ units) -> Bounds2d (space @ units) -> Qty units
inclusion point bounds = -(exclusion point bounds)

includes :: Point2d (space @ units) -> Bounds2d (space @ units) -> Bool
includes (Point2d px py) (Bounds2d x y) = Range.includes px x && Range.includes py y

contains :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bool
contains (Bounds2d x2 y2) (Bounds2d x1 y1) =
  Range.contains x2 x1 && Range.contains y2 y1

isContainedIn :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

separation :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Qty units
separation (Bounds2d x1 y1) (Bounds2d x2 y2)
  | px && py = Qty.hypot2 dx dy
  | px = dx
  | py = dy
  | otherwise = Qty.max dx dy
 where
  dx = Range.separation x1 x2
  dy = Range.separation y1 y2
  px = dx >= Qty.zero
  py = dy >= Qty.zero

overlap :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Qty units
overlap first second = -(separation first second)

intersection :: Bounds2d (space @ units) -> Bounds2d (space @ units) -> Maybe (Bounds2d (space @ units))
intersection (Bounds2d x1 y1) (Bounds2d x2 y2) = Maybe.do
  x <- Range.intersection x1 x2
  y <- Range.intersection y1 y2
  return (Bounds2d x y)

hull2 ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Bounds2d (space @ units)
hull2 (Point2d x1 y1) (Point2d x2 y2) =
  Bounds2d (Range.from x1 x2) (Range.from y1 y2)

hull3 ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Bounds2d (space @ units)
hull3 (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3) =
  let minX = Qty.min (Qty.min x1 x2) x3
      maxX = Qty.max (Qty.max x1 x2) x3
      minY = Qty.min (Qty.min y1 y2) y3
      maxY = Qty.max (Qty.max y1 y2) y3
   in Bounds2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

hull4 ::
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Point2d (space @ units) ->
  Bounds2d (space @ units)
hull4 (Point2d x1 y1) (Point2d x2 y2) (Point2d x3 y3) (Point2d x4 y4) =
  let minX = Qty.min (Qty.min (Qty.min x1 x2) x3) x4
      maxX = Qty.max (Qty.max (Qty.max x1 x2) x3) x4
      minY = Qty.min (Qty.min (Qty.min y1 y2) y3) y4
      maxY = Qty.max (Qty.max (Qty.max y1 y2) y3) y4
   in Bounds2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

hullN :: NonEmpty (Point2d (space @ units)) -> Bounds2d (space @ units)
hullN (Point2d x0 y0 :| rest) = go x0 x0 y0 y0 rest
 where
  go :: Qty units -> Qty units -> Qty units -> Qty units -> List (Point2d (space @ units)) -> Bounds2d (space @ units)
  go xLow xHigh yLow yHigh [] = Bounds2d (Range.unsafe xLow xHigh) (Range.unsafe yLow yHigh)
  go xLow xHigh yLow yHigh (Point2d x y : remaining) =
    go (Qty.min xLow x) (Qty.max xHigh x) (Qty.min yLow y) (Qty.max yHigh y) remaining

lowerLeftCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
lowerLeftCorner (Bounds2d x y) = Point2d (Range.minValue x) (Range.minValue y)

lowerRightCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
lowerRightCorner (Bounds2d x y) = Point2d (Range.maxValue x) (Range.minValue y)

upperLeftCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
upperLeftCorner (Bounds2d x y) = Point2d (Range.minValue x) (Range.maxValue y)

upperRightCorner :: Bounds2d (space @ units) -> Point2d (space @ units)
upperRightCorner (Bounds2d x y) = Point2d (Range.maxValue x) (Range.maxValue y)

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
  Point2d (Range.interpolate x u) (Range.interpolate y v)

sample :: (Point2d (space @ units) -> a) -> Bounds2d (space @ units) -> List a
sample function (Bounds2d x y) =
  [ function (Point2d (Range.interpolate x Quadrature.t1) (Range.interpolate y Quadrature.t1))
  , function (Point2d (Range.interpolate x Quadrature.t2) (Range.interpolate y Quadrature.t5))
  , function (Point2d (Range.interpolate x Quadrature.t3) (Range.interpolate y Quadrature.t3))
  , function (Point2d (Range.interpolate x Quadrature.t4) (Range.interpolate y Quadrature.t4))
  , function (Point2d (Range.interpolate x Quadrature.t5) (Range.interpolate y Quadrature.t2))
  ]

any :: (Bounds2d (space @ units) -> Fuzzy Bool) -> Bounds2d (space @ units) -> Bool
any assess bounds@(Bounds2d x y) =
  case assess bounds of
    Resolved assessment -> assessment
    Unresolved
      | Range.isAtomic x && Range.isAtomic y -> False
      | Range.isAtomic x ->
          let (y1, y2) = Range.bisect y
           in any assess (Bounds2d x y1) || any assess (Bounds2d x y2)
      | Range.isAtomic y ->
          let (x1, x2) = Range.bisect x
           in any assess (Bounds2d x1 y) || any assess (Bounds2d x2 y)
      | otherwise ->
          let (x1, x2) = Range.bisect x
              (y1, y2) = Range.bisect y
           in any assess (Bounds2d x1 y1)
                || any assess (Bounds2d x1 y2)
                || any assess (Bounds2d x2 y1)
                || any assess (Bounds2d x2 y2)

all :: (Bounds2d (space @ units) -> Fuzzy Bool) -> Bounds2d (space @ units) -> Bool
all assess bounds@(Bounds2d x y) =
  case assess bounds of
    Resolved assessment -> assessment
    Unresolved
      | Range.isAtomic x && Range.isAtomic y -> True
      | Range.isAtomic x ->
          let (y1, y2) = Range.bisect y
           in all assess (Bounds2d x y1) && all assess (Bounds2d x y2)
      | Range.isAtomic y ->
          let (x1, x2) = Range.bisect x
           in all assess (Bounds2d x1 y) && all assess (Bounds2d x2 y)
      | otherwise ->
          let (x1, x2) = Range.bisect x
              (y1, y2) = Range.bisect y
           in all assess (Bounds2d x1 y1)
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

find :: (Bounds2d (space @ units) -> Bool) -> Bounds2d (space @ units) -> Maybe (Point2d (space @ units))
find isCandidate (Bounds2d xRange yRange) = Maybe.do
  (x0, y0) <- Range.find2 (\x y -> isCandidate (Bounds2d x y)) xRange yRange
  return (Point2d x0 y0)

placeIn ::
  Frame2d (global @ units) (Defines local) ->
  Bounds2d (local @ units) ->
  Bounds2d (global @ units)
placeIn frame (Bounds2d x y) =
  let xMid = Range.midpoint x
      yMid = Range.midpoint y
      xWidth = Range.width x
      yWidth = Range.width y
      Point2d x0 y0 = Point2d.xyIn frame xMid yMid
      Direction2d (Vector2d ix iy) = Frame2d.xDirection frame
      Direction2d (Vector2d jx jy) = Frame2d.yDirection frame
      rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs jx
      ry = 0.5 * xWidth * Float.abs iy + 0.5 * yWidth * Float.abs jy
   in Bounds2d (Range.from (x0 - rx) (x0 + rx)) (Range.from (y0 - ry) (y0 + ry))

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  Bounds2d (global @ units) ->
  Bounds2d (local @ units)
relativeTo frame (Bounds2d x y) =
  let xMid = Range.midpoint x
      yMid = Range.midpoint y
      xWidth = Range.width x
      yWidth = Range.width y
      Point2d x0 y0 = Point2d.relativeTo frame (Point2d xMid yMid)
      Direction2d (Vector2d ix iy) = Frame2d.xDirection frame
      Direction2d (Vector2d jx jy) = Frame2d.yDirection frame
      rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs iy
      ry = 0.5 * xWidth * Float.abs jx + 0.5 * yWidth * Float.abs jy
   in Bounds2d (Range.from (x0 - rx) (x0 + rx)) (Range.from (y0 - ry) (y0 + ry))

signedDistanceAlong :: Axis2d (space @ units) -> Bounds2d (space @ units) -> Range units
signedDistanceAlong axis (Bounds2d x y) =
  let xMid = Range.midpoint x
      yMid = Range.midpoint y
      xWidth = Range.width x
      yWidth = Range.width y
      d0 = Point2d.signedDistanceAlong axis (Point2d xMid yMid)
      Direction2d (Vector2d ax ay) = Axis2d.direction axis
      r = 0.5 * xWidth * Float.abs ax + 0.5 * yWidth * Float.abs ay
   in Range.from (d0 - r) (d0 + r)

convert :: Units.Conversion units1 units2 -> Bounds2d (space @ units1) -> Bounds2d (space @ units2)
convert conversion (Bounds2d x y) =
  Bounds2d (Range.convert conversion x) (Range.convert conversion y)

unconvert :: Units.Conversion units1 units2 -> Bounds2d (space @ units2) -> Bounds2d (space @ units1)
unconvert conversion (Bounds2d x y) =
  Bounds2d (Range.unconvert conversion x) (Range.unconvert conversion y)
