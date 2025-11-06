module OpenSolid.Number
  ( Number
  , fromDouble
  , toDouble
  , fromInt
  )
where

import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Quantity (Quantity)
import OpenSolid.Unitless (Unitless)

type Number = Quantity Unitless

fromDouble :: Double -> Number
toDouble :: Number -> Double
fromInt :: Int -> Number
