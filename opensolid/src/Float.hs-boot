module Float (Float, fromRational, fromInt) where

import Basics
import {-# SOURCE #-} Qty (Qty)
import {-# SOURCE #-} Units (Unitless)
import Prelude qualified

type Float = Qty Unitless

fromRational :: Prelude.Rational -> Qty Unitless
fromInt :: Int -> Float
