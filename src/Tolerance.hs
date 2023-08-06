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

class ApproximateEquality a b units where
  (~=) :: Tolerance units => a -> b -> Bool

instance units ~ units' => ApproximateEquality (Qty units) (Qty units') units where
  x ~= y = Qty.abs (x - y) <= ?tolerance

instance ApproximateEquality a b units => ApproximateEquality (List a) (List b) units where
  (x : xs) ~= (y : ys)
    | x ~= y = xs ~= ys
    | otherwise = False
  [] ~= [] = True
  (_ : _) ~= [] = False
  [] ~= (_ : _) = False

instance ApproximateEquality a b units => ApproximateEquality (Maybe a) (Maybe b) units where
  Just a ~= Just b = a ~= b
  Nothing ~= Nothing = True
  Just _ ~= Nothing = False
  Nothing ~= Just _ = False

(!=) :: (ApproximateEquality a b units, Tolerance units) => a -> b -> Bool
(!=) first second = not (first ~= second)

infix 4 ~=, !=
