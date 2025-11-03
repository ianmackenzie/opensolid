module OpenSolid.Float
  ( Float
  , fromRational
  , fromDouble
  , toDouble
  , int
  )
where

import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Quantity (Quantity)
import OpenSolid.Unitless (Unitless)
import Prelude qualified

type Float = Quantity Unitless

fromDouble :: Double -> Float
toDouble :: Float -> Double
fromRational :: Prelude.Rational -> Quantity Unitless
int :: Int -> Float
