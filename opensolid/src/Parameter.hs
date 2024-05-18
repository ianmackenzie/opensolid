module Parameter
  ( steps
  , random
  , leading
  , trailing
  , inBetween
  , midpoints
  , intervals
  )
where

import Float qualified
import List qualified
import OpenSolid
import Random qualified
import Range (Range)
import Range qualified

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

intervalOf :: Int -> Int -> Range Unitless
intervalOf n i = Range.from (i / n) ((i + 1) / n)

intervals :: Int -> List (Range Unitless)
intervals n = if n > 0 then List.map (intervalOf n) [0 .. n - 1] else []

random :: Random.Generator Float
random = Float.random 0.0 1.0
