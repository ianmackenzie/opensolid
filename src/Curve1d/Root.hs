module Curve1d.Root (
    Root (..),
    at,
) where

import OpenSolid
import Qty qualified

data Root = Root
    { value :: Float
    , order :: Int
    , sign :: Sign
    }
    deriving (Eq, Show)

at :: Float -> Int -> Qty units -> Root
at rootX rootOrder derivativeValue =
    Root rootX rootOrder (Qty.sign derivativeValue)
