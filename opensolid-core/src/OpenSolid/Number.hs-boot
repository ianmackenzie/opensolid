module OpenSolid.Number
  ( Number
  , fromDouble
  , toDouble
  , fromInt
  )
where

import {-# SOURCE #-} OpenSolid.Quantity (Quantity)
import OpenSolid.Unitless (Unitless)
import Prelude (Double, Int)

type Number = Quantity Unitless

fromDouble :: Double -> Number
toDouble :: Number -> Double
fromInt :: Int -> Number
