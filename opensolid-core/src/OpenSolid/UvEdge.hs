module OpenSolid.UvEdge (UvEdge (..)) where

import OpenSolid.Prelude hiding (Left, Right)
import OpenSolid.UvPoint (UvPoint, data UvPoint)

data UvEdge
  = Left
  | Right
  | Bottom
  | Top
  deriving (Eq, Show)

instance Intersects UvPoint UvEdge Unitless where
  UvPoint uValue vValue ^ edge = case edge of
    Left -> uValue ~= 0.0
    Right -> uValue ~= 1.0
    Bottom -> vValue ~= 0.0
    Top -> vValue ~= 1.0

instance Intersects UvEdge UvPoint Unitless where
  edge ^ point = point ^ edge
