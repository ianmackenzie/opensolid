module Vector2d
  ( Vector2d (..)
  , zero
  , x
  , y
  , xy
  , meters
  , squareMeters
  , polar
  , midpoint
  , interpolateFrom
  , determinant
  , magnitude
  , squaredMagnitude
  , angle
  , IsZero (..)
  , direction
  , magnitudeAndDirection
  , normalize
  )
where

import Angle qualified
import Area qualified
import {-# SOURCE #-} Direction2d (Direction2d)
import {-# SOURCE #-} Direction2d qualified
import Generic qualified
import Length qualified
import OpenSolid
import Qty qualified

type role Vector2d nominal nominal

type Vector2d :: Type -> Type -> Type
data Vector2d units coordinates = Vector2d (Qty units) (Qty units)
  deriving (Eq, Show)

instance Generic.Zero (Vector2d units coordinates) where
  zero = zero

instance Negation (Vector2d units coordinates) where
  negate (Vector2d vx vy) = Vector2d (negate vx) (negate vy)

instance (units ~ units', coordinates ~ coordinates') => Addition (Vector2d units coordinates) (Vector2d units' coordinates') (Vector2d units coordinates) where
  Vector2d x1 y1 + Vector2d x2 y2 = Vector2d (x1 + x2) (y1 + y2)

instance (units ~ units', coordinates ~ coordinates') => Subtraction (Vector2d units coordinates) (Vector2d units' coordinates') (Vector2d units coordinates) where
  Vector2d x1 y1 - Vector2d x2 y2 = Vector2d (x1 - x2) (y1 - y2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (Vector2d units2 coordinates) (Vector2d units3 coordinates) where
  scale * Vector2d vx vy = Vector2d (scale * vx) (scale * vy)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Vector2d units1 coordinates) (Qty units2) (Vector2d units3 coordinates) where
  Vector2d vx vy * scale = Vector2d (vx * scale) (vy * scale)

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Vector2d units1 coordinates) (Qty units2) (Vector2d units3 coordinates) where
  Vector2d vx vy / scale = Vector2d (vx / scale) (vy / scale)

instance (Multiplication (Qty units1) (Qty units2) (Qty units3), coordinates ~ coordinates') => DotProduct (Vector2d units1 coordinates) (Vector2d units2 coordinates') (Qty units3) where
  Vector2d x1 y1 <> Vector2d x2 y2 = x1 * x2 + y1 * y2

instance (Multiplication (Qty units1) (Qty units2) (Qty units3), coordinates ~ coordinates') => CrossProduct (Vector2d units1 coordinates) (Vector2d units2 coordinates') (Qty units3) where
  Vector2d x1 y1 >< Vector2d x2 y2 = x1 * y2 - y1 * x2

zero :: Vector2d units coordinates
zero = Vector2d Qty.zero Qty.zero

x :: Qty units -> Vector2d units coordinates
x vx = Vector2d vx Qty.zero

y :: Qty units -> Vector2d units coordinates
y vy = Vector2d Qty.zero vy

xy :: Qty units -> Qty units -> Vector2d units coordinates
xy = Vector2d

meters :: Float -> Float -> Vector2d Meters coordinates
meters vx vy = Vector2d (Length.meters vx) (Length.meters vy)

squareMeters :: Float -> Float -> Vector2d SquareMeters coordinates
squareMeters vx vy = Vector2d (Area.squareMeters vx) (Area.squareMeters vy)

polar :: Qty units -> Angle -> Vector2d units coordinates
polar r theta = Vector2d (r * Angle.cos theta) (r * Angle.sin theta)

interpolateFrom
  :: Vector2d units coordinates
  -> Vector2d units coordinates
  -> Float
  -> Vector2d units coordinates
interpolateFrom (Vector2d x1 y1) (Vector2d x2 y2) t =
  Vector2d (Qty.interpolateFrom x1 x2 t) (Qty.interpolateFrom y1 y2 t)

midpoint :: Vector2d units coordinates -> Vector2d units coordinates -> Vector2d units coordinates
midpoint (Vector2d x1 y1) (Vector2d x2 y2) =
  Vector2d (Qty.midpoint x1 x2) (Qty.midpoint y1 y2)

determinant
  :: Multiplication (Qty units1) (Qty units2) (Qty units3)
  => Vector2d units1 coordinates
  -> Vector2d units2 coordinates
  -> Qty units3
determinant (Vector2d x1 y1) (Vector2d x2 y2) = x1 * y2 - y1 * x2

magnitude :: Vector2d units coordinates -> Qty units
magnitude (Vector2d vx vy) = Qty.hypot2 vx vy

squaredMagnitude :: Squared (Qty units1) (Qty units2) => Vector2d units1 coordinates -> Qty units2
squaredMagnitude (Vector2d vx vy) = Qty.squared vx + Qty.squared vy

angle :: Vector2d units coordinates -> Angle
angle (Vector2d vx vy) = Angle.atan2 vy vx

data IsZero = IsZero

direction :: Vector2d units coordinates -> Result IsZero (Direction2d coordinates)
direction vector =
  let m = magnitude vector
   in if m == Qty.zero
        then Err IsZero
        else
          let (Vector2d vx vy) = vector
           in Ok (Direction2d.unsafe (vx / m) (vy / m))

magnitudeAndDirection :: Vector2d units coordinates -> Result IsZero (Qty units, Direction2d coordinates)
magnitudeAndDirection vector =
  let m = magnitude vector
   in if m == Qty.zero
        then Err IsZero
        else
          let (Vector2d vx vy) = vector
           in Ok (m, Direction2d.unsafe (vx / m) (vy / m))

normalize :: Vector2d units coordinates -> Vector2d Unitless coordinates
normalize vector =
  let m = magnitude vector
   in if m == Qty.zero
        then zero
        else
          let (Vector2d vx vy) = vector
           in Vector2d (vx / m) (vy / m)
