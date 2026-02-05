module OpenSolid.SurfaceFunction1D.Zeros
  ( Zeros (Zeros, crossingCurves, crossingLoops, tangentPoints, saddlePoints)
  , empty
  )
where

import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvSpace (UvSpace)

data Zeros = Zeros
  { crossingCurves :: ~(List (Curve2D Unitless UvSpace))
  , crossingLoops :: ~(List (Curve2D Unitless UvSpace))
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
