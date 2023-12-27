module Curve1d.Root (Root (Root, value, order, sign)) where

import OpenSolid

data Root = Root
  { value :: Float
  , order :: Int
  , sign :: Sign
  }
  deriving (Eq, Show)

instance ApproximateEquality Root Root Unitless where
  root1 ~= root2 =
    value root1 ~= value root2
      && order root1 == order root2
      && sign root1 == sign root2
