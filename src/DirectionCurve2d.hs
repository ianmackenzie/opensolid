module DirectionCurve2d
  ( DirectionCurve2d
  , evaluateAt
  , constant
  )
where

import Direction2d (Direction2d (Direction2d))
import Direction2d qualified
import OpenSolid
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

newtype DirectionCurve2d space = DirectionCurve2d (VectorCurve2d (space @ Unitless))

evaluateAt :: Float -> DirectionCurve2d space -> Direction2d space
evaluateAt t (DirectionCurve2d vectorCurve) =
  Direction2d.unsafe (VectorCurve2d.evaluateAt t vectorCurve)

constant :: Direction2d space -> DirectionCurve2d space
constant (Direction2d vector) = DirectionCurve2d (VectorCurve2d.constant vector)
