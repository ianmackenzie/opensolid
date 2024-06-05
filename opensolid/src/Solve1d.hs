{-# LANGUAGE NoFieldSelectors #-}

module Solve1d
  ( domain
  , bisect
  , offset
  , narrow
  , bounds
  , overlap
  , isResolved
  , resolvedSign
  )
where

import OpenSolid
import Qty qualified
import Range (Range)
import Range qualified

data Subdomain = Subdomain
  { n :: Int
  , i :: Int
  , j :: Int
  }
  deriving (Eq, Show)

domain :: Subdomain
domain = Subdomain{n = 1, i = 0, j = 1}

bisect :: Subdomain -> (Subdomain, Subdomain)
bisect (Subdomain{n, i, j}) = do
  let n2 = 2 * n
  let i2 = 2 * i
  let j2 = 2 * j
  let mid = i2 + (j - i)
  (Subdomain n2 i2 mid, Subdomain n2 mid j2)

offset :: Subdomain -> Subdomain
offset (Subdomain{n, i, j}) = do
  let delta = j - i
  Subdomain (2 * n) (2 * i + delta) (2 * j + delta)

narrow :: Subdomain -> Subdomain
narrow (Subdomain{n, i, j}) = do
  let delta = j - i
  Subdomain (8 * n) (8 * i + delta) (8 * j - delta)

bounds :: Subdomain -> Range Unitless
bounds (Subdomain{n, i, j}) = Range.unsafe (i / n) (j / n)

overlap :: Subdomain -> Subdomain -> Bool
overlap (Subdomain n1 i1 j1) (Subdomain n2 i2 j2) =
  i1 * n2 < j2 * n1 && j1 * n2 > i2 * n1

isResolved :: Range units -> Bool
isResolved range = resolvedSign range /= Nothing

resolvedSign :: Range units -> Maybe Sign
resolvedSign range = do
  let resolution = Range.resolution range
  if Qty.abs resolution >= 0.5 then Just (Qty.sign resolution) else Nothing
