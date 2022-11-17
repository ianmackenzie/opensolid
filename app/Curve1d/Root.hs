module Curve1d.Root (
    Root (..),
    Sign (..),
) where

import OpenSolid

data Sign = Positive | Negative deriving (Eq, Show)

data Root = Root
    { value :: !Float
    , order :: !Int
    , sign :: !Sign
    }
    deriving (Eq, Show)
