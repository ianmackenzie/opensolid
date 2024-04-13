module Surface1d.Function.Boundary
  ( Boundary (..)
  , left
  , right
  , bottom
  , top
  , adjacent
  )
where

import Bounds2d qualified
import OpenSolid
import Qty qualified
import Range (Range)
import Range qualified
import Uv qualified

data Boundary
  = Left Float (Range Unitless)
  | Right Float (Range Unitless)
  | Bottom (Range Unitless) Float
  | Top (Range Unitless) Float
  deriving (Show)

left :: Uv.Bounds -> Boundary
left uvBounds = do
  let (uRange, vRange) = Bounds2d.xyRanges uvBounds
  Left (Range.minValue uRange) vRange

right :: Uv.Bounds -> Boundary
right uvBounds = do
  let (uRange, vRange) = Bounds2d.xyRanges uvBounds
  Right (Range.maxValue uRange) vRange

bottom :: Uv.Bounds -> Boundary
bottom uvBounds = do
  let (uRange, vRange) = Bounds2d.xyRanges uvBounds
  Bottom uRange (Range.minValue vRange)

top :: Uv.Bounds -> Boundary
top uvBounds = do
  let (uRange, vRange) = Bounds2d.xyRanges uvBounds
  Top uRange (Range.maxValue vRange)

adjacent :: Boundary -> Boundary -> Bool
adjacent (Left u1 v1) (Right u2 v2) = u1 == u2 && Range.overlap v1 v2 > Qty.zero
adjacent (Right u1 v1) (Left u2 v2) = u1 == u2 && Range.overlap v1 v2 > Qty.zero
adjacent (Bottom u1 v1) (Top u2 v2) = v1 == v2 && Range.overlap u1 u2 > Qty.zero
adjacent (Top u1 v1) (Bottom u2 v2) = v1 == v2 && Range.overlap u1 u2 > Qty.zero
adjacent _ _ = False
