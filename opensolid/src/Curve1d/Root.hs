module Curve1d.Root (Root (Root, value, order, sign)) where

import OpenSolid

data Root = Root
  { value :: Float
  , order :: Int
  , sign :: Sign
  }
  deriving (Eq, Show)

instance ApproximateEquality Root Root Unitless where
  Root value1 order1 sign1 ~= Root value2 order2 sign2 =
    value1 ~= value2 && order1 == order2 && sign1 == sign2
