module OpenSolid.SurfaceFunction.Zeros
  ( Zeros (Zeros, crossingCurves, crossingLoops, tangentPoints, saddlePoints)
  , empty
  )
where

import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
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
