module Curve2d.Intersection
  ( Intersection (..)
  , Kind (..)
  , sign
  )
where

import OpenSolid

data Intersection = Intersection
  { u1 :: Float
  , u2 :: Float
  , kind :: Kind
  }
  deriving (Eq, Ord, Show)

data Kind = Crossing Sign | Tangent Sign deriving (Eq, Ord, Show)

sign :: Intersection -> Sign
sign (Intersection _ _ (Crossing s)) = s
sign (Intersection _ _ (Tangent s)) = s
