module Surface1d.Function.SaddleRegion
  ( SaddleRegion (..)
  , Solution (..)
  , Frame
  , point
  , includes
  , contains
  , corners
  , bounds
  )
where

import Bounds2d qualified
import Direction2d qualified
import Float qualified
import Frame2d (Frame2d)
import Frame2d qualified
import List qualified
import OpenSolid
import Point2d qualified
import Range qualified
import Uv qualified

type Frame = Frame2d Uv.Coordinates (Defines Uv.Space)

data SaddleRegion = SaddleRegion
  { frame :: Frame
  , halfWidth :: Float
  , halfHeight :: Float
  , positiveSolution :: Solution
  , negativeSolution :: Solution
  }
  deriving (Show)

data Solution = Solution
  { dydx :: Float
  , d2ydx2 :: Float
  }
  deriving (Show)

point :: SaddleRegion -> Uv.Point
point (SaddleRegion{frame}) = Frame2d.originPoint frame

isInsideRegion :: SaddleRegion -> Uv.Point -> Bool
isInsideRegion (SaddleRegion{frame, halfWidth, halfHeight}) givenPoint = do
  let (localX, localY) = Point2d.coordinates (Point2d.relativeTo frame givenPoint)
  Float.abs localX < halfWidth && Float.abs localY < halfHeight

corners :: SaddleRegion -> List Uv.Point
corners (SaddleRegion{frame, halfWidth, halfHeight}) =
  [ Point2d.xyIn frame -halfWidth -halfHeight
  , Point2d.xyIn frame halfWidth -halfHeight
  , Point2d.xyIn frame halfWidth halfHeight
  , Point2d.xyIn frame -halfWidth halfHeight
  ]

bounds :: SaddleRegion -> Uv.Bounds
bounds (SaddleRegion{frame, halfWidth, halfHeight}) = do
  let (x0, y0) = Point2d.coordinates (Frame2d.originPoint frame)
  let (ix, iy) = Direction2d.components (Frame2d.xDirection frame)
  let (jx, jy) = Direction2d.components (Frame2d.yDirection frame)
  let dx = Float.abs ix * halfWidth + Float.abs jx * halfHeight
  let dy = Float.abs iy * halfWidth + Float.abs jy * halfHeight
  Bounds2d.xy (Range.from (x0 - dx) (x0 + dx)) (Range.from (y0 - dy) (y0 + dy))

includes :: Uv.Point -> SaddleRegion -> Bool
includes givenPoint region = isInsideRegion region givenPoint

contains :: Uv.Bounds -> SaddleRegion -> Bool
contains givenBounds region =
  List.all (isInsideRegion region) (Bounds2d.corners givenBounds)
