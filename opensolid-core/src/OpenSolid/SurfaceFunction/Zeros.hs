module OpenSolid.SurfaceFunction.Zeros
  ( Zeros (..)
  , empty
  , Error (..)
  )
where

import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Error qualified as Error
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (UvCoordinates, UvPoint)

data Zeros = Zeros
  { crossingCurves :: ~(List (Curve2d UvCoordinates))
  , crossingLoops :: ~(List (Curve2d UvCoordinates))
  , tangentPoints :: List (UvPoint, Sign)
  , saddlePoints :: List UvPoint
  }

empty :: Zeros
empty =
  Zeros
    { crossingCurves = []
    , crossingLoops = []
    , tangentPoints = []
    , saddlePoints = []
    }

data Error = ZeroEverywhere deriving (Eq, Show, Error.Message)
