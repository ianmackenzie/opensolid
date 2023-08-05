module Parameter1d
  ( steps
  , leading
  , trailing
  , inBetween
  , midpoints
  , generator
  )
where

import Float qualified
import OpenSolid
import Random qualified

steps :: Int -> (Float -> a) -> List a
steps n function
  | n < 1 = []
  | otherwise = range 0 n (Float.fromInt n) function []

leading :: Int -> (Float -> a) -> List a
leading n function
  | n < 1 = []
  | otherwise = range 0 (n - 1) (Float.fromInt n) function []

trailing :: Int -> (Float -> a) -> List a
trailing n function
  | n < 1 = []
  | otherwise = range 1 n (Float.fromInt n) function []

inBetween :: Int -> (Float -> a) -> List a
inBetween n function
  | n < 2 = []
  | otherwise = range 1 (n - 1) (Float.fromInt n) function []

range :: Int -> Int -> Float -> (Float -> a) -> List a -> List a
range startIndex index divisor function accumulated
  | index > startIndex = range startIndex (index - 1) divisor function updated
  | otherwise = updated
 where
  updated = function (Float.fromInt index / divisor) : accumulated

midpoints :: Int -> (Float -> a) -> List a
midpoints n function
  | n < 1 = []
  | otherwise = midpointsImpl (2 * n - 1) (2 * Float.fromInt n) function []

midpointsImpl :: Int -> Float -> (Float -> a) -> List a -> List a
midpointsImpl index divisor function accumulated
  | index > 1 = midpointsImpl (index - 2) divisor function updated
  | otherwise = updated
 where
  updated = function (Float.fromInt index / divisor) : accumulated

generator :: Random.Generator Float
generator = Random.float 0.0 1.0
