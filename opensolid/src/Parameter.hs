module Parameter
  ( steps
  , random
  , leading
  , trailing
  , inBetween
  , midpoints
  )
where

import Float qualified
import List qualified
import OpenSolid
import Random qualified

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

random :: Random.Generator Float
random = Float.random 0.0 1.0
