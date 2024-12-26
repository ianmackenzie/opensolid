module OpenSolid.Surface1d.Function.Zeros
  ( Zeros (..)
  , empty
  , Error (..)
  )
where

import OpenSolid.Error qualified as Error
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (UvBounds, UvCoordinates, UvPoint)

data Zeros = Zeros
  { crossingCurves :: List (NonEmpty (Curve2d UvCoordinates, UvBounds))
  , crossingLoops :: List (NonEmpty (Curve2d UvCoordinates, UvBounds))
  , tangentPoints :: List (UvPoint, Sign, UvBounds)
  , saddlePoints :: List (UvPoint, UvBounds)
  }

empty :: Zeros
empty =
  Zeros
    { crossingCurves = []
    , crossingLoops = []
    , tangentPoints = []
    , saddlePoints = []
    }

data Error
  = HigherOrderZero
  | ZeroEverywhere
  deriving (Eq, Show, Error.Message)
