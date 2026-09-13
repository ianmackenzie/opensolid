module OpenSolid.SurfaceFunction1D.Zeros
  ( Zeros (Zeros, crossingCurves, crossingLoops, tangentPoints, saddlePoints)
  , empty
  )
where

import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvPoint (UvPoint)

data Zeros = Zeros
  { crossingCurves :: ~(List UvCurve)
  , crossingLoops :: ~(List UvCurve)
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
