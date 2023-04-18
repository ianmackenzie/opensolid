module Curve2d.Intersection (Intersection (..)) where

import OpenSolid

data Intersection = Intersection
  { u :: Float
  , v :: Float
  , sign :: Maybe Sign
  }
  deriving (Eq, Ord, Show)
