module Float
  ( Float
  , fromRational
  , fromDouble
  , toDouble
  , fromInt
  )
where

import Basics
import {-# SOURCE #-} Qty (Qty)
import {-# SOURCE #-} Units (Unitless)
import Prelude qualified

type Float = Qty Unitless

fromDouble :: Prelude.Double -> Float
toDouble :: Float -> Prelude.Double
fromRational :: Prelude.Rational -> Qty Unitless
fromInt :: Int -> Float
