module OpenSolid.SurfaceFunction.Zeros
  ( Zeros (Zeros, crossingCurves, crossingLoops, tangentPoints, saddlePoints)
  , empty
  )
where

import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)

data Zeros = Zeros
  { crossingCurves :: ~(List (Curve2d UvSpace Unitless))
  , crossingLoops :: ~(List (Curve2d UvSpace Unitless))
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
