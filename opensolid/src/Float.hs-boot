module Float
  ( Float
  , fromRational
  , fromDouble
  , toDouble
  , int
  )
where

import Basics
import {-# SOURCE #-} Qty (Qty)
import {-# SOURCE #-} Units (Units (..))
import Prelude qualified

type Float = Qty Unitless

fromDouble :: Prelude.Double -> Float
toDouble :: Float -> Prelude.Double
fromRational :: Prelude.Rational -> Qty Unitless
int :: Int -> Float
