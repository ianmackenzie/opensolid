module Surface1d.SaddleRegion
  ( SaddleRegion (..)
  , contains
  , corners
  , bounds
  )
where

import Bounds2d (Bounds2d (Bounds2d))
import Bounds2d qualified
import Direction2d (Direction2d (Direction2d))
import Float qualified
import Frame2d (Frame2d)
import Frame2d qualified
import List qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Point2d qualified
import Range qualified
import Uv qualified
import Vector2d (Vector2d (Vector2d))

data Space deriving (Show)

data SaddleRegion = SaddleRegion
  { frame :: Frame2d Uv.Coordinates (Defines Space)
  , halfWidth :: Float
  , halfHeight :: Float
  }
  deriving (Show)

isInsideRegion :: SaddleRegion -> Uv.Point -> Bool
isInsideRegion (SaddleRegion {frame, halfWidth, halfHeight}) point =
  let Point2d localX localY = Point2d.relativeTo frame point
   in Float.abs localX < halfWidth && Float.abs localY < halfHeight

corners :: SaddleRegion -> List Uv.Point
corners (SaddleRegion {frame, halfWidth, halfHeight}) =
  [ Point2d.xyIn frame -halfWidth -halfHeight
  , Point2d.xyIn frame halfWidth -halfHeight
  , Point2d.xyIn frame halfWidth halfHeight
  , Point2d.xyIn frame -halfWidth halfHeight
  ]

bounds :: SaddleRegion -> Uv.Bounds
bounds (SaddleRegion {frame, halfWidth, halfHeight}) =
  let Point2d x0 y0 = Frame2d.originPoint frame
      Direction2d (Vector2d ix iy) = Frame2d.xDirection frame
      Direction2d (Vector2d jx jy) = Frame2d.yDirection frame
      dx = Float.abs ix * halfWidth + Float.abs jx * halfHeight
      dy = Float.abs iy * halfWidth + Float.abs jy * halfHeight
   in Bounds2d (Range.from (x0 - dx) (x0 + dx)) (Range.from (y0 - dy) (y0 + dy))

contains :: Uv.Bounds -> SaddleRegion -> Bool
contains givenBounds region =
  List.all (isInsideRegion region) (Bounds2d.corners givenBounds)
