module Surface1d.Function.Zeros (Zeros (..), empty) where

import Curve2d (Curve2d)
import OpenSolid
import Uv qualified

data Zeros = Zeros
  { crossingCurves :: List (NonEmpty (Curve2d Uv.Coordinates))
  , crossingLoops :: List (NonEmpty (Curve2d Uv.Coordinates))
  , tangentPoints :: List (Uv.Point, Sign)
  , saddlePoints :: List (Uv.Point, Uv.Bounds)
  }

empty :: Zeros
empty =
  Zeros
    { crossingCurves = []
    , crossingLoops = []
    , tangentPoints = []
    , saddlePoints = []
    }
