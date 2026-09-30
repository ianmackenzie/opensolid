module OpenSolid.Parameter
  ( isEndpoint
  , hasEndpoint
  , steps
  , random
  , leading
  , trailing
  , inBetween
  , midpoints
  , intervals
  , samples
  )
where

import OpenSolid.Int qualified as Int
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Quadrature qualified as Quadrature
import OpenSolid.Random qualified as Random

{-# INLINEABLE isEndpoint #-}
isEndpoint :: Number -> Bool
isEndpoint tValue = tValue == 0.0 || tValue == 1.0

{-# INLINEABLE hasEndpoint #-}
hasEndpoint :: Interval Unitless -> Bool
hasEndpoint (Interval tLow tHigh) = tLow == 0.0 || tHigh == 1.0

{-| For a given N, return N+1 evenly spaced parameter values from 0 to 1.

For example, N=4 will result in 0.0, 0.25, 0.5, 0.75 and 1.0
(five parameter values corresponding to four steps from 0 to 1).

Returns an empty list for N < 1.
-}
steps :: Int -> List Number
steps n = if n > 0 then List.map (divideBy n) [0 .. n] else []

{-| For a given N, return N evenly spaced parameter values from 0 to 1, omitting 1.

For example, N=4 will result in 0.0, 0.25, 0.5 and 0.75.

Returns an empty list for N < 1.
-}
leading :: Int -> List Number
leading n = List.map (divideBy n) [0 .. n - 1]

{-| For a given N, return N evenly spaced parameter values from 0 to 1, omitting 0.

For example, N=4 will result in  0.25, 0.5, 0.75 and 1.0.

Returns an empty list for N < 1.
-}
trailing :: Int -> List Number
trailing n = List.map (divideBy n) [1 .. n]

{-| For a given N, return N-1 evenly spaced parameter values in between 0 to 1, omitting 0 and 1.

For example, N=4 will result in 0.25, 0.5 and 0.75.

Returns an empty list for N < 2.
-}
inBetween :: Int -> List Number
inBetween n = List.map (divideBy n) [1 .. n - 1]

{-| For a given N, subdivide the range [0,1] into N intervals and then return the midpoint of each one.

For example, N=4 will result in 0.125, 0.375, 0.625 and 0.875
(the midpoints of the four intervals [0,0.25], [0.25, 0.5], [0.5, 0.75] and [0.75, 1.0]).

Returns an empty list for N < 1.
-}
midpoints :: Int -> List Number
midpoints n = List.map (midpointOf n) [0 .. n - 1]

{-| For a given N, return the result of subdividing the range [0,1] into N intervals.

For example, N=4 will result in the intervals [0,0.25], [0.25, 0.5], [0.5, 0.75] and [0.75, 1.0]).

Returns an empty list for N < 1.
-}
intervals :: Int -> List (Interval Unitless)
intervals n = if n > 0 then List.map (intervalOf n) [0 .. n - 1] else []

samples :: NonEmpty Number
samples = do
  let (p1, p2, p3, p4, p5) = Quadrature.abscissae5
  NonEmpty.five p1 p2 p3 p4 p5

random :: Random.Generator Number
random = Random.number 0.0 1.0

{-# INLINE divideBy #-}
divideBy :: Int -> Int -> Number
divideBy n i = Int.ratio i n

{-# INLINE midpointOf #-}
midpointOf :: Int -> Int -> Number
midpointOf n i = Int.ratio (2 * i + 1) (2 * n)

{-# INLINE intervalOf #-}
intervalOf :: Int -> Int -> Interval Unitless
intervalOf n i = Interval (Int.ratio i n) (Int.ratio (i + 1) n)
