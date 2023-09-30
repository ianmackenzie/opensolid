module Tolerance
  ( Tolerance
  , ApproximateEquality ((~=))
  , (!=)
  )
where

import Arithmetic
import Basics
import NonEmpty (NonEmpty ((:|)))
import Qty (Qty)
import Qty qualified

type Tolerance units = ?tolerance :: Qty units

class ApproximateEquality a b units | a b -> units where
  (~=) :: (Tolerance units) => a -> b -> Bool

instance (units ~ units') => ApproximateEquality (Qty units) (Qty units') units where
  x ~= y = Qty.abs (x - y) <= ?tolerance

instance (ApproximateEquality a b units) => ApproximateEquality (List a) (List b) units where
  (x : xs) ~= (y : ys)
    | x ~= y = xs ~= ys
    | otherwise = False
  [] ~= [] = True
  (_ : _) ~= [] = False
  [] ~= (_ : _) = False

instance (ApproximateEquality a b units) => ApproximateEquality (NonEmpty a) (NonEmpty b) units where
  (x :| xs) ~= (y :| ys) =
    x ~= y && xs ~= ys

instance (ApproximateEquality a b units) => ApproximateEquality (Maybe a) (Maybe b) units where
  Just a ~= Just b = a ~= b
  Nothing ~= Nothing = True
  Just _ ~= Nothing = False
  Nothing ~= Just _ = False

instance
  ( ApproximateEquality a1 b1 units
  , ApproximateEquality a2 b2 units'
  , units ~ units'
  ) =>
  ApproximateEquality (a1, a2) (b1, b2) units
  where
  (a1, a2) ~= (b1, b2) = a1 ~= b1 && a2 ~= b2

instance
  ( ApproximateEquality a1 b1 units
  , ApproximateEquality a2 b2 units'
  , ApproximateEquality a3 b3 units''
  , units ~ units'
  , units ~ units''
  ) =>
  ApproximateEquality (a1, a2, a3) (b1, b2, b3) units
  where
  (a1, a2, a3) ~= (b1, b2, b3) = a1 ~= b1 && a2 ~= b2 && a3 ~= b3

instance
  ( ApproximateEquality a1 b1 units
  , ApproximateEquality a2 b2 units'
  , ApproximateEquality a3 b3 units''
  , ApproximateEquality a4 b4 units'''
  , units ~ units'
  , units ~ units''
  , units ~ units'''
  ) =>
  ApproximateEquality (a1, a2, a3, a4) (b1, b2, b3, b4) units
  where
  (a1, a2, a3, a4) ~= (b1, b2, b3, b4) = a1 ~= b1 && a2 ~= b2 && a3 ~= b3 && a4 ~= b4

(!=) :: (ApproximateEquality a b units, Tolerance units) => a -> b -> Bool
(!=) first second = not (first ~= second)

infix 4 ~=, !=
