module OpenSolid.Parameter
  ( steps
  , random
  , leading
  , trailing
  , inBetween
  , midpoints
  , intervals
  , samples
  )
where

import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Int qualified as Int
import OpenSolid.List qualified as List
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude
import OpenSolid.Quadrature qualified as Quadrature
import OpenSolid.Random qualified as Random

steps :: Int -> List Number
steps n = if n > 0 then List.map (divideBy n) [0 .. n] else []

leading :: Int -> List Number
leading n = List.map (divideBy n) [0 .. n - 1]

trailing :: Int -> List Number
trailing n = List.map (divideBy n) [1 .. n]

inBetween :: Int -> List Number
inBetween n = List.map (divideBy n) [1 .. n - 1]

midpoints :: Int -> List Number
midpoints n = List.map (midpointOf n) [0 .. n - 1]

intervals :: Int -> List (Bounds Unitless)
intervals n = if n > 0 then List.map (intervalOf n) [0 .. n - 1] else []

samples :: List Number
samples = do
  let (p1, p2, p3, p4, p5) = Quadrature.abscissae5
  [p1, p2, p3, p4, p5]

random :: Random.Generator Number
random = Number.random 0 1

{-# INLINE divideBy #-}
divideBy :: Int -> Int -> Number
divideBy n i = Int.ratio i n

{-# INLINE midpointOf #-}
midpointOf :: Int -> Int -> Number
midpointOf n i = Int.ratio (2 * i + 1) (2 * n)

{-# INLINE intervalOf #-}
intervalOf :: Int -> Int -> Bounds Unitless
intervalOf n i = Bounds (Int.ratio i n) (Int.ratio (i + 1) n)
