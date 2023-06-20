module Tolerance
  ( Tolerance
  , ApproximateEquality ((~=))
  , (!=)
  )
where

import Arithmetic
import Basics
import Qty (Qty)
import Qty qualified

type Tolerance units = ?tolerance :: Qty units

class ApproximateEquality a b units | a -> units, b -> units where
  (~=) :: Tolerance units => a -> b -> Bool

infix 4 ~=

instance units ~ units' => ApproximateEquality (Qty units) (Qty units') units where
  x ~= y = Qty.abs (x - y) <= ?tolerance

(!=) :: (ApproximateEquality a b units, Tolerance units) => a -> b -> Bool
(!=) first second = not (first ~= second)
