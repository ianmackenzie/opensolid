module Surface1d.Function.SaddleRegion
  ( SaddleRegion (..)
  , Solution (..)
  , Frame
  , point
  , includes
  )
where

import Bounds2d qualified
import Frame2d (Frame2d)
import Frame2d qualified
import OpenSolid
import Uv qualified

type Frame = Frame2d Uv.Coordinates (Defines Uv.Space)

data SaddleRegion = SaddleRegion
  { frame :: Frame
  , halfWidth :: Float
  , halfHeight :: Float
  , positiveSolution :: Solution
  , negativeSolution :: Solution
  , exclusion :: Uv.Bounds
  , fxxSign :: Sign
  }
  deriving (Show)

data Solution = Solution
  { dydx :: Float
  , d2ydx2 :: Float
  }
  deriving (Show)

point :: SaddleRegion -> Uv.Point
point (SaddleRegion{frame}) = Frame2d.originPoint frame

includes :: Uv.Point -> SaddleRegion -> Bool
includes givenPoint (SaddleRegion{exclusion}) = Bounds2d.includes givenPoint exclusion
