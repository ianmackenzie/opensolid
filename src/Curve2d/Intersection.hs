module Curve2d.Intersection
  ( Intersection (..)
  , Kind (..)
  )
where

import OpenSolid

data Intersection = Intersection
  { u1 :: Float
  , u2 :: Float
  , kind :: Kind
  , sign :: Sign
  }
  deriving (Eq, Ord, Show)

data Kind = Crossing | Tangent deriving (Eq, Ord, Show)
