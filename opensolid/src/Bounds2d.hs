module Bounds2d
  ( Bounds2d (Bounds2d)
  , xCoordinate
  , yCoordinate
  , xy
  , constant
  , hull2
  , hull3
  , hull4
  , aggregate2
  , overlap
  , separation
  , intersection
  , interpolate
  , sample
  , any
  , all
  )
where

import Bounds qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Qty qualified
import Quadrature qualified
import Range (Range)
import Range qualified
import VectorBounds2d (VectorBounds2d (VectorBounds2d))

type role Bounds2d nominal

data Bounds2d (coordinateSystem :: CoordinateSystem) where
  Bounds2d :: Range units -> Range units -> Bounds2d (space @ units)

deriving instance Show (Bounds2d (space @ units))

instance Bounds.Interface (Bounds2d (space @ units)) where
  aggregate2Impl = aggregate2
  intersectionImpl = intersection

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
intersection (Bounds2d x1 y1) (Bounds2d x2 y2) = do
  x <- Range.intersection x1 x2
  y <- Range.intersection y1 y2
  Just (Bounds2d x y)

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
