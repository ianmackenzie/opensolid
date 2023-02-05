module Point2d
  ( Point2d (..)
  , origin
  , x
  , y
  , xy
  , meters
  , midpoint
  , interpolateFrom
  , distanceFrom
  , angleFrom
  , translateBy
  , signedDistanceAlong
  , signedDistanceFrom
  )
where

import {-# SOURCE #-} Axis2d (Axis2d)
import {-# SOURCE #-} Axis2d qualified
import {-# SOURCE #-} BoundingBox2d (BoundingBox2d (..))
import Direction2d qualified
import Length qualified
import OpenSolid
import Qty qualified
import Vector2d (Vector2d (..))
import Vector2d qualified
import VectorBox2d (VectorBox2d (..))

type role Point2d nominal nominal

type Point2d :: Type -> Type -> Type
data Point2d coordinates units = Point2d (Qty units) (Qty units)
  deriving (Eq, Show)

instance (units ~ units', coordinates ~ coordinates') => Addition (Point2d coordinates units) (Vector2d coordinates' units') (Point2d coordinates units) where
  Point2d px py + Vector2d vx vy = Point2d (px + vx) (py + vy)

instance (units ~ units', coordinates ~ coordinates') => Subtraction (Point2d coordinates units) (Vector2d coordinates' units') (Point2d coordinates units) where
  Point2d px py - Vector2d vx vy = Point2d (px - vx) (py - vy)

instance (units ~ units', coordinates ~ coordinates') => Subtraction (Point2d coordinates units) (Point2d coordinates' units') (Vector2d coordinates units) where
  Point2d x1 y1 - Point2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance (units ~ units', coordinates ~ coordinates') => Addition (Point2d coordinates units) (VectorBox2d coordinates' units') (BoundingBox2d coordinates units) where
  Point2d px py + VectorBox2d vx vy = BoundingBox2d (px + vx) (py + vy)

instance (units ~ units', coordinates ~ coordinates') => Subtraction (Point2d coordinates units) (VectorBox2d coordinates' units') (BoundingBox2d coordinates units) where
  Point2d px py - VectorBox2d vx vy = BoundingBox2d (px - vx) (py - vy)

instance ApproximateEquality (Point2d coordinates units) units where
  p1 ~= p2 = distanceFrom p1 p2 ~= Qty.zero

origin :: Point2d coordinates units
origin = Point2d Qty.zero Qty.zero

x :: Qty units -> Point2d coordinates units
x px = Point2d px Qty.zero

y :: Qty units -> Point2d coordinates units
y py = Point2d Qty.zero py

xy :: Qty units -> Qty units -> Point2d coordinates units
xy = Point2d

meters :: Float -> Float -> Point2d coordinates Meters
meters px py = Point2d (Length.meters px) (Length.meters py)

interpolateFrom :: Point2d coordinates units -> Point2d coordinates units -> Float -> Point2d coordinates units
interpolateFrom (Point2d x1 y1) (Point2d x2 y2) t =
  Point2d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t)

midpoint :: Point2d coordinates units -> Point2d coordinates units -> Point2d coordinates units
midpoint (Point2d x1 y1) (Point2d x2 y2) =
  Point2d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2)

distanceFrom :: Point2d coordinates units -> Point2d coordinates units -> Qty units
distanceFrom p1 p2 = Vector2d.magnitude (p2 - p1)

angleFrom :: Point2d coordinates units -> Point2d coordinates units -> Angle
angleFrom p1 p2 = Vector2d.angle (p2 - p1)

translateBy :: Vector2d coordinates units -> Point2d coordinates units -> Point2d coordinates units
translateBy (Vector2d vx vy) (Point2d px py) = Point2d (px + vx) (py + vy)

signedDistanceAlong :: Axis2d coordinates units -> Point2d coordinates units -> Qty units
signedDistanceAlong axis point =
  (point - Axis2d.originPoint axis) <> Axis2d.direction axis

signedDistanceFrom :: Axis2d coordinates units -> Point2d coordinates units -> Qty units
signedDistanceFrom axis point =
  (point - Axis2d.originPoint axis) <> Direction2d.rotateLeft (Axis2d.direction axis)
