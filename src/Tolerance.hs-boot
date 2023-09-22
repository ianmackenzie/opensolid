module Tolerance
  ( Tolerance
  , ApproximateEquality ((~=))
  , (!=)
  )
where

import Basics
import {-# SOURCE #-} Qty (Qty)

type Tolerance units = ?tolerance :: Qty units

class ApproximateEquality a b units | a b -> units where
  (~=) :: (Tolerance units) => a -> b -> Bool

infix 4 ~=

instance (units ~ units') => ApproximateEquality (Qty units) (Qty units') units

(!=) :: (ApproximateEquality a b units, Tolerance units) => a -> b -> Bool
