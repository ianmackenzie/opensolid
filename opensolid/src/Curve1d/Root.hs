module Curve1d.Root (Root (..)) where

import OpenSolid

data Root = Root
  { value :: Float
  , order :: Int
  , sign :: Sign
  }
  deriving (Eq, Show)

instance ApproximateEquality Root Root Unitless where
  root1 ~= root2 =
    root1.value ~= root2.value
      && root1.order == root2.order
      && root1.sign == root2.sign
