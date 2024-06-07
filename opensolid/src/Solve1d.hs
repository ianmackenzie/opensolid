{-# LANGUAGE NoFieldSelectors #-}

module Solve1d
  ( domain
  , isAtomic
  , bisect
  , half
  , offset
  , interior
  , bounds
  , overlaps
  , contains
  , isResolved
  , resolvedSign
  , run
  )
where

import Debug qualified
import Float qualified
import OpenSolid
import Qty qualified
import Queue (Queue)
import Queue qualified
import Range (Range)
import Range qualified

data Subdomain = Subdomain
  { n :: Float
  , i :: Float
  , j :: Float
  }
  deriving (Eq, Show)

domain :: Subdomain
domain = Subdomain{n = 1.0, i = 0.0, j = 1.0}

isAtomic :: Subdomain -> Bool
isAtomic (Subdomain{n, i, j}) = (j - i) / n <= Float.epsilon

bisect :: Subdomain -> (Subdomain, Subdomain)
bisect (Subdomain{n, i, j}) = do
  let n2 = 2 * n
  let i2 = 2 * i
  let j2 = 2 * j
  let mid = i2 + (j - i)
  (Subdomain n2 i2 mid, Subdomain n2 mid j2)

half :: Subdomain -> Subdomain
half (Subdomain{n, i, j}) = do
  let n2 = 4 * n
  let delta = j - i
  Subdomain n2 (4 * i + delta) (4 * j - delta)

offset :: Subdomain -> Subdomain
offset (Subdomain{n, i, j}) = do
  let delta = j - i
  Subdomain (2 * n) (2 * i + delta) (2 * j + delta)

bounds :: Subdomain -> Range Unitless
bounds (Subdomain{n, i, j}) = Range.unsafe (i / n) (j / n)

interior :: Subdomain -> Range Unitless
interior (Subdomain{n, i, j}) = do
  let n8 = 8 * n
  let delta = j - i
  Range.unsafe
    (if i == 0.0 then 0.0 else (8 * i + delta) / n8)
    (if j == n then 1.0 else (8 * j - delta) / n8)

overlaps :: Subdomain -> Subdomain -> Bool
overlaps (Subdomain n2 i2 j2) (Subdomain n1 i1 j1) =
  i1 * n2 < j2 * n1 && j1 * n2 > i2 * n1

contains :: Subdomain -> Subdomain -> Bool
contains (Subdomain n2 i2 j2) (Subdomain n1 i1 j1) =
  i1 * n2 <= i2 * n1 && j1 * n2 >= j2 * n1

isResolved :: Range units -> Bool
isResolved range = resolvedSign range /= Nothing

resolvedSign :: Range units -> Maybe Sign
resolvedSign range = do
  let resolution = Range.resolution range
  if Qty.abs resolution >= 0.5 then Just (Qty.sign resolution) else Nothing

run :: a -> (Subdomain -> a -> (a, Bool)) -> a
run initialState processSubdomain = loop initialState processSubdomain (Queue.singleton domain)

loop :: a -> (Subdomain -> a -> (a, Bool)) -> Queue Subdomain -> a
loop currentState processSubdomain queue =
  case Queue.pop queue of
    Just (subdomain, remaining) -> do
      let (updatedState, resolved) = processSubdomain subdomain currentState
      let updatedQueue = if resolved then remaining else remaining |> enqueueChildren subdomain
      loop updatedState processSubdomain updatedQueue
    Nothing -> currentState

enqueueChildren :: Subdomain -> Queue Subdomain -> Queue Subdomain
enqueueChildren subdomain queue = do
  Debug.assert (not (isAtomic subdomain))
  let mid = half subdomain
  let (left, right) = bisect subdomain
  queue |> Queue.push mid |> Queue.push left |> Queue.push right
