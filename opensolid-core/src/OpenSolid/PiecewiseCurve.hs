module OpenSolid.PiecewiseCurve
  ( interpolate
  , aggregate
  , all
  , segments
  , map2
  )
where

import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Curve1d.Function (Function)
import OpenSolid.Curve1d.Function qualified as Function
import OpenSolid.Float qualified as Float
import OpenSolid.Int qualified as Int
import OpenSolid.NonEmpty qualified as NonEmpty
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

all :: (a -> Range Unitless -> Bool) -> Array a -> Range Unitless -> Bool
all = aggregate (&&)

segments :: Array a -> NonEmpty (Range Unitless, a)
segments array = do
  let n = Float.int (Array.length array)
  let segment index item = (Range.from (Float.int index / n) (Float.int (index + 1) / n), item)
  NonEmpty.mapWithIndex segment (Array.items array)

map2 ::
  (Composition (Function Unitless) f1 f1, Composition (Function Unitless) f2 f2) =>
  (f1 -> f2 -> a) ->
  Array f1 ->
  Array f2 ->
  Array a
map2 f array1 array2 = do
  let n1 = Array.length array1
  let n2 = Array.length array2
  let n = Int.lcm n1 n2
  Array.map2 f (split (n // n1) array1) (split (n // n2) array2)

split :: Composition (Function Unitless) f f => Int -> Array f -> Array f
split 1 array = array
split n array = Array.new (NonEmpty.collect (splitFunction n) (Array.items array))

splitFunction :: Composition (Function Unitless) f f => Int -> f -> NonEmpty f
splitFunction n function = do
  let step = 1.0 / Float.int n
  let functionSegment i = function . (Float.int i * step + Function.t * step)
  NonEmpty.map functionSegment (0 :| [1 .. n - 1])
