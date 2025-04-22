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
import OpenSolid.Float qualified as Float
import OpenSolid.List qualified as List
import OpenSolid.Prelude
import OpenSolid.Quadrature qualified as Quadrature
import OpenSolid.Random qualified as Random

steps :: Int -> List Float
steps n = if n > 0 then List.map (/ n) [0 .. n] else []

leading :: Int -> List Float
leading n = List.map (/ n) [0 .. n - 1]

trailing :: Int -> List Float
trailing n = List.map (/ n) [1 .. n]

inBetween :: Int -> List Float
inBetween n = List.map (/ n) [1 .. n - 1]

midpoints :: Int -> List Float
midpoints n = List.map (\i -> (2 * i + 1) / (2 * n)) [0 .. n - 1]

intervalOf :: Int -> Int -> Bounds Unitless
intervalOf n i = Bounds (i / n) ((i + 1) / n)

intervals :: Int -> List (Bounds Unitless)
intervals n = if n > 0 then List.map (intervalOf n) [0 .. n - 1] else []

samples :: List Float
samples = do
  let (p1, p2, p3, p4) = Quadrature.abscissae4
  [p1, p2, p3, p4]

random :: Random.Generator Float
random = Float.random 0.0 1.0
