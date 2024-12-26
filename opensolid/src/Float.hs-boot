module Float
  ( Float
  , fromRational
  , fromDouble
  , toDouble
  , int
  )
where

import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Qty (Qty)
import Unitless (Unitless)
import Prelude qualified

type Float = Qty Unitless

fromDouble :: Prelude.Double -> Float
toDouble :: Float -> Prelude.Double
fromRational :: Prelude.Rational -> Qty Unitless
int :: Int -> Float
