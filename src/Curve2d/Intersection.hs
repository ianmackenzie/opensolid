module Curve2d.Intersection (Intersection (..)) where

import OpenSolid

data Intersection coordinates = Intersection
    { parameterValue :: Float
    , order :: Int
    , sign :: Sign
    }
    deriving (Eq, Show)
