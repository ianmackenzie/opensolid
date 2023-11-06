module U
  ( Bounds
  , domain
  , steps
  , generator
  , leading
  , trailing
  , inBetween
  , midpoints
  , samples
  )
where

import List qualified
import OpenSolid
import Random qualified
import Range (Range)
import Range qualified

type Bounds = Range Unitless

domain :: Bounds
domain = Range.from 0.0 1.0

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

samples :: List Float
samples = Range.samples domain

generator :: Random.Generator Float
generator = Random.float 0.0 1.0
