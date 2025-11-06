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
import OpenSolid.List qualified as List
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude hiding ((*), (+), (-))
import OpenSolid.Quadrature qualified as Quadrature
import OpenSolid.Random qualified as Random
import Prelude ((*), (+), (-))

steps :: Int -> List Number
steps n = if n > 0 then List.map (/ n) [0 .. n] else []

leading :: Int -> List Number
leading n = List.map (/ n) [0 .. n - 1]

trailing :: Int -> List Number
trailing n = List.map (/ n) [1 .. n]

inBetween :: Int -> List Number
inBetween n = List.map (/ n) [1 .. n - 1]

midpoints :: Int -> List Number
midpoints n = List.map (\i -> (2 * i + 1) / (2 * n)) [0 .. n - 1]

intervalOf :: Int -> Int -> Bounds Unitless
intervalOf n i = Bounds (i / n) ((i + 1) / n)

intervals :: Int -> List (Bounds Unitless)
intervals n = if n > 0 then List.map (intervalOf n) [0 .. n - 1] else []

samples :: List Number
samples = do
  let (p1, p2, p3, p4, p5) = Quadrature.abscissae5
  [p1, p2, p3, p4, p5]

random :: Random.Generator Number
random = Number.random 0.0 1.0
