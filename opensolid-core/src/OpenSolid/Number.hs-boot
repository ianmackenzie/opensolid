module OpenSolid.Number
  ( Number
  , fromRational
  , fromDouble
  , toDouble
  )
where

import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Quantity (Quantity)
import OpenSolid.Unitless (Unitless)
import Prelude qualified

type Number = Quantity Unitless

fromDouble :: Double -> Number
toDouble :: Number -> Double
fromRational :: Prelude.Rational -> Quantity Unitless
