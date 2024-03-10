module Surface1d.Function.Zeros (Zeros (..), empty) where

import Curve2d (Curve2d)
import OpenSolid
import Uv qualified

data Zeros = Zeros
  { crossingCurves :: List (NonEmpty (Curve2d Uv.Coordinates))
  , crossingLoops :: List (NonEmpty (Curve2d Uv.Coordinates))
  , tangentCurves :: List (NonEmpty (Curve2d Uv.Coordinates), Sign)
  , tangentLoops :: List (NonEmpty (Curve2d Uv.Coordinates), Sign)
  , saddlePoints :: List Uv.Point
  , tangentPoints :: List (Uv.Point, Sign)
  , crossingPoints :: List Uv.Point
  }

empty :: Zeros
empty =
  Zeros
    { crossingCurves = []
    , crossingLoops = []
    , tangentCurves = []
    , tangentLoops = []
    , saddlePoints = []
    , tangentPoints = []
    , crossingPoints = []
    }
