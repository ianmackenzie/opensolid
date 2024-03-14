module VectorBounds2d
  ( VectorBounds2d (VectorBounds2d)
  , constant
  , xy
  , aggregate2
  , aggregate3
  , hull2
  , hull3
  , hull4
  , hullN
  , polar
  , xComponent
  , yComponent
  , components
  , squaredMagnitude
  , squaredMagnitude_
  , magnitude
  , maxMagnitude
  , maxSquaredMagnitude
  , maxSquaredMagnitude_
  , normalize
  , includes
  , contains
  , isContainedIn
  , interpolate
  , relativeTo
  , placeIn
  , placeInBasis
  , relativeToBasis
  )
where

import Basis2d (Basis2d)
import Basis2d qualified
import Direction2d (Direction2d (Direction2d))
import Float qualified
import {-# SOURCE #-} Frame2d (Frame2d)
import {-# SOURCE #-} Frame2d qualified
import OpenSolid
import Qty qualified
import Range (Range (Range))
import Range qualified
import Units qualified
import Vector2d (Vector2d (Vector2d))
import Vector2d qualified

type role VectorBounds2d nominal

data VectorBounds2d (coordinateSystem :: CoordinateSystem) where
  VectorBounds2d :: Range units -> Range units -> VectorBounds2d (space @ units)

deriving instance Show (VectorBounds2d (space @ units))

instance
  (units1 ~ units1', units2 ~ units2', space ~ space') =>
  Units.Coercion
    units1
    units2
    (VectorBounds2d (space @ units1'))
    (VectorBounds2d (space' @ units2'))

instance Negation (VectorBounds2d (space @ units)) where
  negate (VectorBounds2d x y) = VectorBounds2d (negate x) (negate y)

instance
  Multiplication
    Sign
    (VectorBounds2d (space @ units))
    (VectorBounds2d (space @ units))
  where
  Positive * vectorBounds = vectorBounds
  Negative * vectorBounds = -vectorBounds

instance
  Multiplication
    (VectorBounds2d (space @ units))
    Sign
    (VectorBounds2d (space @ units))
  where
  vectorBounds * Positive = vectorBounds
  vectorBounds * Negative = -vectorBounds

instance
  (space ~ space', units ~ units') =>
  Addition
    (VectorBounds2d (space @ units))
    (VectorBounds2d (space' @ units'))
    (VectorBounds2d (space @ units))
  where
  VectorBounds2d x1 y1 + VectorBounds2d x2 y2 = VectorBounds2d (x1 + x2) (y1 + y2)

instance
  (space ~ space', units ~ units') =>
  Addition
    (VectorBounds2d (space @ units))
    (Vector2d (space' @ units'))
    (VectorBounds2d (space @ units))
  where
  VectorBounds2d x1 y1 + Vector2d x2 y2 = VectorBounds2d (x1 + x2) (y1 + y2)

instance
  (space ~ space', units ~ units') =>
  Addition
    (Vector2d (space @ units))
    (VectorBounds2d (space' @ units'))
    (VectorBounds2d (space @ units))
  where
  Vector2d x1 y1 + VectorBounds2d x2 y2 = VectorBounds2d (x1 + x2) (y1 + y2)

instance
  (space ~ space', units ~ units') =>
  Subtraction
    (VectorBounds2d (space @ units))
    (VectorBounds2d (space' @ units'))
    (VectorBounds2d (space @ units))
  where
  VectorBounds2d x1 y1 - VectorBounds2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

instance
  (space ~ space', units ~ units') =>
  Subtraction
    (VectorBounds2d (space @ units))
    (Vector2d (space' @ units'))
    (VectorBounds2d (space @ units))
  where
  VectorBounds2d x1 y1 - Vector2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

instance
  (space ~ space', units ~ units') =>
  Subtraction
    (Vector2d (space @ units))
    (VectorBounds2d (space' @ units'))
    (VectorBounds2d (space @ units))
  where
  Vector2d x1 y1 - VectorBounds2d x2 y2 = VectorBounds2d (x1 - x2) (y1 - y2)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Qty units1)
    (VectorBounds2d (space @ units2))
    (VectorBounds2d (space @ units3))
  where
  value * VectorBounds2d x y = VectorBounds2d (value * x) (value * y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorBounds2d (space @ units1))
    (Qty units2)
    (VectorBounds2d (space @ units3))
  where
  VectorBounds2d x y * value = VectorBounds2d (x * value) (y * value)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Range units1)
    (VectorBounds2d (space @ units2))
    (VectorBounds2d (space @ units3))
  where
  range * VectorBounds2d x y = VectorBounds2d (range * x) (range * y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorBounds2d (space @ units1))
    (Range units2)
    (VectorBounds2d (space @ units3))
  where
  VectorBounds2d x y * range = VectorBounds2d (x * range) (y * range)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorBounds2d (space @ units1))
    (Qty units2)
    (VectorBounds2d (space @ units3))
  where
  VectorBounds2d x y / value = VectorBounds2d (x / value) (y / value)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorBounds2d (space @ units1))
    (Range units2)
    (VectorBounds2d (space @ units3))
  where
  VectorBounds2d x y / range = VectorBounds2d (x / range) (y / range)

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct
    (Vector2d (space @ units1))
    (VectorBounds2d (space' @ units2))
    (Range units3)
  where
  Vector2d x1 y1 <> VectorBounds2d x2 y2 = x1 * x2 + y1 * y2

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct
    (VectorBounds2d (space @ units1))
    (Vector2d (space' @ units2))
    (Range units3)
  where
  VectorBounds2d x1 y1 <> Vector2d x2 y2 = x1 * x2 + y1 * y2

instance
  space ~ space' =>
  DotProduct
    (Direction2d space)
    (VectorBounds2d (space' @ units))
    (Range units)
  where
  Direction2d vector <> vectorBounds = vector <> vectorBounds

instance
  space ~ space' =>
  DotProduct
    (VectorBounds2d (space @ units))
    (Direction2d space')
    (Range units)
  where
  vectorBounds <> Direction2d vector = vectorBounds <> vector

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  DotProduct
    (VectorBounds2d (space @ units1))
    (VectorBounds2d (space' @ units2))
    (Range units3)
  where
  VectorBounds2d x1 y1 <> VectorBounds2d x2 y2 = x1 * x2 + y1 * y2

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  CrossProduct
    (Vector2d (space @ units1))
    (VectorBounds2d (space' @ units2))
    (Range units3)
  where
  Vector2d x1 y1 >< VectorBounds2d x2 y2 = x1 * y2 - y1 * x2

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  CrossProduct
    (VectorBounds2d (space @ units1))
    (Vector2d (space' @ units2))
    (Range units3)
  where
  VectorBounds2d x1 y1 >< Vector2d x2 y2 = x1 * y2 - y1 * x2

instance
  space ~ space' =>
  CrossProduct
    (Direction2d space)
    (VectorBounds2d (space' @ units))
    (Range units)
  where
  Direction2d vector >< vectorBounds = vector >< vectorBounds

instance
  space ~ space' =>
  CrossProduct
    (VectorBounds2d (space @ units))
    (Direction2d space')
    (Range units)
  where
  vectorBounds >< Direction2d vector = vectorBounds >< vector

instance
  (Units.Product units1 units2 units3, space ~ space') =>
  CrossProduct
    (VectorBounds2d (space @ units1))
    (VectorBounds2d (space' @ units2))
    (Range units3)
  where
  VectorBounds2d x1 y1 >< VectorBounds2d x2 y2 = x1 * y2 - y1 * x2

constant :: Vector2d (space @ units) -> VectorBounds2d (space @ units)
constant (Vector2d x y) = VectorBounds2d (Range.constant x) (Range.constant y)

xy :: Range units -> Range units -> VectorBounds2d (space @ units)
xy = VectorBounds2d

hull2 :: Vector2d (space @ units) -> Vector2d (space @ units) -> VectorBounds2d (space @ units)
hull2 (Vector2d x1 y1) (Vector2d x2 y2) = VectorBounds2d (Range.from x1 x2) (Range.from y1 y2)

hull3 ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorBounds2d (space @ units)
hull3 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) =
  let minX = Qty.min (Qty.min x1 x2) x3
      maxX = Qty.max (Qty.max x1 x2) x3
      minY = Qty.min (Qty.min y1 y2) y3
      maxY = Qty.max (Qty.max y1 y2) y3
   in VectorBounds2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

hull4 ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorBounds2d (space @ units)
hull4 (Vector2d x1 y1) (Vector2d x2 y2) (Vector2d x3 y3) (Vector2d x4 y4) =
  let minX = Qty.min (Qty.min (Qty.min x1 x2) x3) x4
      maxX = Qty.max (Qty.max (Qty.max x1 x2) x3) x4
      minY = Qty.min (Qty.min (Qty.min y1 y2) y3) y4
      maxY = Qty.max (Qty.max (Qty.max y1 y2) y3) y4
   in VectorBounds2d (Range.unsafe minX maxX) (Range.unsafe minY maxY)

hullN :: NonEmpty (Vector2d (space @ units)) -> VectorBounds2d (space @ units)
hullN (Vector2d x0 y0 :| rest) = go x0 x0 y0 y0 rest
 where
  go :: Qty units -> Qty units -> Qty units -> Qty units -> List (Vector2d (space @ units)) -> VectorBounds2d (space @ units)
  go xLow xHigh yLow yHigh [] = VectorBounds2d (Range.unsafe xLow xHigh) (Range.unsafe yLow yHigh)
  go xLow xHigh yLow yHigh (Vector2d x y : remaining) =
    go (Qty.min xLow x) (Qty.max xHigh x) (Qty.min yLow y) (Qty.max yHigh y) remaining

aggregate2 ::
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units)
aggregate2 (VectorBounds2d x1 y1) (VectorBounds2d x2 y2) =
  VectorBounds2d (Range.aggregate2 x1 x2) (Range.aggregate2 y1 y2)

aggregate3 ::
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units) ->
  VectorBounds2d (space @ units)
aggregate3 (VectorBounds2d x1 y1) (VectorBounds2d x2 y2) (VectorBounds2d x3 y3) =
  VectorBounds2d (Range.aggregate3 x1 x2 x3) (Range.aggregate3 y1 y2 y3)

polar :: Range units -> Range Radians -> VectorBounds2d (space @ units)
polar r theta = VectorBounds2d (r * Range.cos theta) (r * Range.sin theta)

xComponent :: VectorBounds2d (space @ units) -> Range units
xComponent (VectorBounds2d vx _) = vx

yComponent :: VectorBounds2d (space @ units) -> Range units
yComponent (VectorBounds2d _ vy) = vy

components :: VectorBounds2d (space @ units) -> (Range units, Range units)
components (VectorBounds2d vx vy) = (vx, vy)

squaredMagnitude :: Units.Squared units1 units2 => VectorBounds2d (space @ units1) -> Range units2
squaredMagnitude = Units.specialize << squaredMagnitude_

squaredMagnitude_ :: VectorBounds2d (space @ units) -> Range (Units.GenericProduct units units)
squaredMagnitude_ (VectorBounds2d x y) = Range.squared_ x + Range.squared_ y

magnitude :: VectorBounds2d (space @ units) -> Range units
magnitude (VectorBounds2d x y) = Range.hypot2 x y

maxMagnitude :: VectorBounds2d (space @ units) -> Qty units
maxMagnitude (VectorBounds2d (Range minX maxX) (Range minY maxY)) =
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
      yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
   in Qty.hypot2 xMagnitude yMagnitude

maxSquaredMagnitude :: Units.Squared units1 units2 => VectorBounds2d (space @ units1) -> Qty units2
maxSquaredMagnitude = Units.specialize << maxSquaredMagnitude_

maxSquaredMagnitude_ :: VectorBounds2d (space @ units) -> Qty (Units.GenericProduct units units)
maxSquaredMagnitude_ (VectorBounds2d (Range minX maxX) (Range minY maxY)) =
  let xMagnitude = Qty.max (Qty.abs minX) (Qty.abs maxX)
      yMagnitude = Qty.max (Qty.abs minY) (Qty.abs maxY)
   in Qty.squared_ xMagnitude + Qty.squared_ yMagnitude

normalize :: VectorBounds2d (space @ units) -> VectorBounds2d (space @ Unitless)
normalize vectorBounds =
  let (VectorBounds2d x y) = vectorBounds / magnitude vectorBounds
      nx = clampNormalized x
      ny = clampNormalized y
   in VectorBounds2d nx ny

clampNormalized :: Range Unitless -> Range Unitless
clampNormalized (Range low high) =
  Range.unsafe
    (Qty.clamp -1.0 1.0 low)
    (Qty.clamp -1.0 1.0 high)

includes :: Vector2d (space @ units) -> VectorBounds2d (space @ units) -> Bool
includes (Vector2d vx vy) (VectorBounds2d x y) = Range.includes vx x && Range.includes vy y

contains :: VectorBounds2d (space @ units) -> VectorBounds2d (space @ units) -> Bool
contains (VectorBounds2d x2 y2) (VectorBounds2d x1 y1) =
  Range.contains x2 x1 && Range.contains y2 y1

isContainedIn :: VectorBounds2d (space @ units) -> VectorBounds2d (space @ units) -> Bool
isContainedIn bounds1 bounds2 = contains bounds2 bounds1

interpolate :: VectorBounds2d (space @ units) -> Float -> Float -> Vector2d (space @ units)
interpolate (VectorBounds2d x y) u v =
  Vector2d (Range.interpolate x u) (Range.interpolate y v)

placeIn ::
  Frame2d (global @ frameUnits) (Defines local) ->
  VectorBounds2d (local @ units) ->
  VectorBounds2d (global @ units)
placeIn frame = placeInBasis (Frame2d.basis frame)

relativeTo ::
  Frame2d (global @ units) (Defines local) ->
  VectorBounds2d (global @ units) ->
  VectorBounds2d (local @ units)
relativeTo frame = relativeToBasis (Frame2d.basis frame)

placeInBasis ::
  Basis2d global (Defines local) ->
  VectorBounds2d (local @ units) ->
  VectorBounds2d (global @ units)
placeInBasis basis (VectorBounds2d x y) =
  let xMid = Range.midpoint x
      yMid = Range.midpoint y
      xWidth = Range.width x
      yWidth = Range.width y
      Vector2d x0 y0 = Vector2d.xyInBasis basis xMid yMid
      Direction2d (Vector2d ix iy) = Basis2d.xDirection basis
      Direction2d (Vector2d jx jy) = Basis2d.yDirection basis
      rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs jx
      ry = 0.5 * xWidth * Float.abs iy + 0.5 * yWidth * Float.abs jy
   in VectorBounds2d (Range.from (x0 - rx) (x0 + rx)) (Range.from (y0 - ry) (y0 + ry))

relativeToBasis ::
  Basis2d global (Defines local) ->
  VectorBounds2d (global @ units) ->
  VectorBounds2d (local @ units)
relativeToBasis basis (VectorBounds2d x y) =
  let xMid = Range.midpoint x
      yMid = Range.midpoint y
      xWidth = Range.width x
      yWidth = Range.width y
      Vector2d x0 y0 = Vector2d.relativeToBasis basis (Vector2d xMid yMid)
      Direction2d (Vector2d ix iy) = Basis2d.xDirection basis
      Direction2d (Vector2d jx jy) = Basis2d.yDirection basis
      rx = 0.5 * xWidth * Float.abs ix + 0.5 * yWidth * Float.abs iy
      ry = 0.5 * xWidth * Float.abs jx + 0.5 * yWidth * Float.abs jy
   in VectorBounds2d (Range.from (x0 - rx) (x0 + rx)) (Range.from (y0 - ry) (y0 + ry))
