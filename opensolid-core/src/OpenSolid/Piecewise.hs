module OpenSolid.Piecewise
  ( interpolate
  , aggregate
  )
where

import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Float qualified as Float
import OpenSolid.Int qualified as Int
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range

{-# INLINEABLE position #-}
position :: Int -> Float -> (# Int, Float #)
position n t = do
  let n' = Float.int n
  let i = Int.clampTo (0, n - 1) (Float.floor (t * n'))
  let t0 = Float.int i / n'
  (# i, (t - t0) * n' #)

{-# INLINEABLE interpolate #-}
interpolate :: (a -> Float -> b) -> Array a -> Float -> b
interpolate f array t = do
  let n = Array.length array
  let (# i, ti #) = position n t
  f (Array.get i array) ti

aggregate :: (b -> b -> b) -> (a -> Range Unitless -> b) -> Array a -> Range Unitless -> b
aggregate combine f array t = do
  let n = Array.length array
  let (# i, ti #) = position n (Range.lowerBound t)
  let (# j, tj #) = position n (Range.upperBound t)
  if i == j
    then f (Array.get i array) (Range.from ti tj)
    else do
      let bi = f (Array.get i array) (Range.from ti 1.0)
      let bj = f (Array.get j array) (Range.from 0.0 tj)
      aggregateImpl combine f array (i + 1) (j - 1) (combine bi bj)

aggregateImpl :: (b -> b -> b) -> (a -> Range Unitless -> b) -> Array a -> Int -> Int -> b -> b
aggregateImpl combine f array i j acc
  | i <= j = do
      let bi = f (Array.get i array) Range.unit
      aggregateImpl combine f array (i + 1) j (combine acc bi)
  | otherwise = acc
