module VectorBox2d
  ( VectorBox2d (VectorBox2d)
  , constant
  , aggregate2
  , aggregate3
  , hull2
  , hull3
  , hull4
  , polar
  , xComponent
  , yComponent
  , squaredMagnitude
  , magnitude
  , maxMagnitude
  , maxSquaredMagnitude
  , normalize
  , interpolate
  )
where

import CoordinateSystem (Units)
import Direction2d (Direction2d (Direction2d))
import Generic qualified
import OpenSolid
import Qty qualified
import Range (Range (Range))
import Range qualified
import Units qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified

type role VectorBox2d nominal

data VectorBox2d (coordinateSystem :: CoordinateSystem)
  = VectorBox2d (Range (Units coordinateSystem)) (Range (Units coordinateSystem))
  deriving (Show)

instance
  (units1 ~ units1', units2 ~ units2', space ~ space') =>
  Units.Coercion
    units1
    units2
    (VectorBox2d (space @ units1'))
    (VectorBox2d (space' @ units2'))

instance Generic.HasZero (VectorBox2d (space @ units)) where
  zeroImpl = constant Vector2d.zero

instance Negation (VectorBox2d (space @ units)) where
  negate (VectorBox2d x y) = VectorBox2d (negate x) (negate y)

instance
  Multiplication
    Sign
    (VectorBox2d (space @ units))
    (VectorBox2d (space @ units))
  where
  Positive * vectorBox = vectorBox
  Negative * vectorBox = -vectorBox

instance
  Multiplication
    (VectorBox2d (space @ units))
    Sign
    (VectorBox2d (space @ units))
  where
  vectorBox * Positive = vectorBox
  vectorBox * Negative = -vectorBox

instance
  (space ~ space', units ~ units') =>
  Addition
    (VectorBox2d (space @ units))
    (VectorBox2d (space' @ units'))
    (VectorBox2d (space @ units))
  where
  VectorBox2d x1 y1 + VectorBox2d x2 y2 = VectorBox2d (x1 + x2) (y1 + y2)

instance
  (space ~ space', units ~ units') =>
  Addition
    (VectorBox2d (space @ units))
    (Vector2d (space' @ units'))
    (VectorBox2d (space @ units))
  where
  VectorBox2d x1 y1 + Vector2d x2 y2 = VectorBox2d (x1 + x2) (y1 + y2)

instance
  (space ~ space', units ~ units') =>
  Addition
    (Vector2d (space @ units))
    (VectorBox2d (space' @ units'))
    (VectorBox2d (space @ units))
  where
  Vector2d x1 y1 + VectorBox2d x2 y2 = VectorBox2d (x1 + x2) (y1 + y2)

instance
  (space ~ space', units ~ units') =>
  Subtraction
    (VectorBox2d (space @ units))
    (VectorBox2d (space' @ units'))
    (VectorBox2d (space @ units))
  where
  VectorBox2d x1 y1 - VectorBox2d x2 y2 = VectorBox2d (x1 - x2) (y1 - y2)

instance
  (space ~ space', units ~ units') =>
  Subtraction
    (VectorBox2d (space @ units))
    (Vector2d (space' @ units'))
    (VectorBox2d (space @ units))
  where
  VectorBox2d x1 y1 - Vector2d x2 y2 = VectorBox2d (x1 - x2) (y1 - y2)

instance
  (space ~ space', units ~ units') =>
  Subtraction
    (Vector2d (space @ units))
    (VectorBox2d (space' @ units'))
    (VectorBox2d (space @ units))
  where
  Vector2d x1 y1 - VectorBox2d x2 y2 = VectorBox2d (x1 - x2) (y1 - y2)

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (Qty units1)
    (VectorBox2d (space @ units2))
    (VectorBox2d (space @ units3))
  where
  value * VectorBox2d x y = VectorBox2d (value * x) (value * y)

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (VectorBox2d (space @ units1))
    (Qty units2)
    (VectorBox2d (space @ units3))
  where
  VectorBox2d x y * value = VectorBox2d (x * value) (y * value)

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (Range units1)
    (VectorBox2d (space @ units2))
    (VectorBox2d (space @ units3))
  where
  range * VectorBox2d x y = VectorBox2d (range * x) (range * y)

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (VectorBox2d (space @ units1))
    (Range units2)
    (VectorBox2d (space @ units3))
  where
  VectorBox2d x y * range = VectorBox2d (x * range) (y * range)

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (VectorBox2d (space @ units1))
    (Qty units2)
    (VectorBox2d (space @ units3))
  where
  VectorBox2d x y / value = VectorBox2d (x / value) (y / value)

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (VectorBox2d (space @ units1))
    (Range units2)
    (VectorBox2d (space @ units3))
  where
  VectorBox2d x y / range = VectorBox2d (x / range) (y / range)

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct
    (Vector2d (space @ units1))
    (VectorBox2d (space' @ units2))
    (Range units3)
  where
  Vector2d x1 y1 <> VectorBox2d x2 y2 = x1 * x2 + y1 * y2

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct
    (VectorBox2d (space @ units1))
    (Vector2d (space' @ units2))
    (Range units3)
  where
  VectorBox2d x1 y1 <> Vector2d x2 y2 = x1 * x2 + y1 * y2

instance
  (space ~ space') =>
  DotProduct
    (Direction2d space)
    (VectorBox2d (space' @ units))
    (Range units)
  where
  Direction2d vector <> vectorBox = vector <> vectorBox

instance
  (space ~ space') =>
  DotProduct
    (VectorBox2d (space @ units))
    (Direction2d space')
    (Range units)
  where
  vectorBox <> Direction2d vector = vectorBox <> vector

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct
    (VectorBox2d (space @ units1))
    (VectorBox2d (space' @ units2))
    (Range units3)
  where
  VectorBox2d x1 y1 <> VectorBox2d x2 y2 = x1 * x2 + y1 * y2

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  CrossProduct
    (Vector2d (space @ units1))
    (VectorBox2d (space' @ units2))
    (Range units3)
  where
  Vector2d x1 y1 >< VectorBox2d x2 y2 = x1 * y2 - y1 * x2

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  CrossProduct
    (VectorBox2d (space @ units1))
    (Vector2d (space' @ units2))
    (Range units3)
  where
  VectorBox2d x1 y1 >< Vector2d x2 y2 = x1 * y2 - y1 * x2

instance
  (space ~ space') =>
  CrossProduct
    (Direction2d space)
    (VectorBox2d (space' @ units))
    (Range units)
  where
  Direction2d vector >< vectorBox = vector >< vectorBox

instance
  (space ~ space') =>
  CrossProduct
    (VectorBox2d (space @ units))
    (Direction2d space')
    (Range units)
  where
  vectorBox >< Direction2d vector = vectorBox >< vector

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  CrossProduct
    (VectorBox2d (space @ units1))
    (VectorBox2d (space' @ units2))
    (Range units3)
  where
  VectorBox2d x1 y1 >< VectorBox2d x2 y2 = x1 * y2 - y1 * x2

constant :: Vector2d (space @ units) -> VectorBox2d (space @ units)
constant (Vector2d x y) = VectorBox2d (Range.constant x) (Range.constant y)

hull2 :: Vector2d (space @ units) -> Vector2d (space @ units) -> VectorBox2d (space @ units)
hull2 (Vector2d x1 y1) (Vector2d x2 y2) = VectorBox2d (Range.from x1 x2) (Range.from y1 y2)

hull3 ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorBox2d (space @ units)
hull3 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) =
  let minX = Qty.min (Qty.min x1 x2) x3
      maxX = Qty.max (Qty.max x1 x2) x3
      minY = Qty.min (Qty.min y1 y2) y3
      maxY = Qty.max (Qty.max y1 y2) y3
   in VectorBox2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

hull4 ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorBox2d (space @ units)
hull4 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) (Vector2d x4 y4) =
  let minX = Qty.min (Qty.min (Qty.min x1 x2) x3) x4
      maxX = Qty.max (Qty.max (Qty.max x1 x2) x3) x4
      minY = Qty.min (Qty.min (Qty.min y1 y2) y3) y4
      maxY = Qty.max (Qty.max (Qty.max y1 y2) y3) y4
   in VectorBox2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

aggregate2 ::
  VectorBox2d (space @ units) ->
  VectorBox2d (space @ units) ->
  VectorBox2d (space @ units)
aggregate2 (VectorBox2d x1 y1) (VectorBox2d x2 y2) =
  VectorBox2d (Range.aggregate2 x1 x2) (Range.aggregate2 y1 y2)

aggregate3 ::
  VectorBox2d (space @ units) ->
  VectorBox2d (space @ units) ->
  VectorBox2d (space @ units) ->
  VectorBox2d (space @ units)
aggregate3 (VectorBox2d x1 y1) (VectorBox2d x2 y2) (VectorBox2d x3 y3) =
  VectorBox2d (Range.aggregate3 x1 x2 x3) (Range.aggregate3 y1 y2 y3)

polar :: Range units -> Range Radians -> VectorBox2d (space @ units)
polar r theta = VectorBox2d (r * Range.cos theta) (r * Range.sin theta)

xComponent :: VectorBox2d (space @ units) -> Range units
xComponent (VectorBox2d vx _) = vx

yComponent :: VectorBox2d (space @ units) -> Range units
yComponent (VectorBox2d _ vy) = vy

squaredMagnitude :: (Units.Squared units1 units2) => VectorBox2d (space @ units1) -> Range units2
squaredMagnitude (VectorBox2d x y) = Range.squared x + Range.squared y

magnitude :: VectorBox2d (space @ units) -> Range units
magnitude (VectorBox2d x y) = Range.hypot2 x y

maxMagnitude :: VectorBox2d (space @ units) -> Qty units
maxMagnitude (VectorBox2d (Range minX maxX) (Range minY maxY)) =
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
      yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
   in Qty.hypot2 xMagnitude yMagnitude

maxSquaredMagnitude :: (Units.Squared units1 units2) => VectorBox2d (space @ units1) -> Qty units2
maxSquaredMagnitude (VectorBox2d (Range minX maxX) (Range minY maxY)) =
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
      yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
   in Qty.squared xMagnitude + Qty.squared yMagnitude

normalize :: VectorBox2d (space @ units) -> VectorBox2d (space @ Unitless)
normalize vectorBox =
  let (VectorBox2d x y) = vectorBox / magnitude vectorBox
      nx = clampNormalized x
      ny = clampNormalized y
   in VectorBox2d nx ny

clampNormalized :: Range Unitless -> Range Unitless
clampNormalized (Range low high) =
  Range.unsafe
    (Qty.clamp -1.0 1.0 low)
    (Qty.clamp -1.0 1.0 high)

interpolate :: VectorBox2d (space @ units) -> Float -> Float -> Vector2d (space @ units)
interpolate (VectorBox2d x y) u v =
  Vector2d (Range.interpolate x u) (Range.interpolate y v)
