module Tolerance
  ( Tolerance
  , ApproximateEquality ((~=))
  , (!=)
  , using
  , exactly
  , squared
  , squared'
  , ofSquared
  , ofSquared'
  , times
  , times'
  )
where

import Arithmetic
import Basics
import NonEmpty (NonEmpty ((:|)), pattern NonEmpty)
import Qty (Qty)
import Qty qualified
import Units ((:*:))
import Units qualified

type Tolerance units = ?tolerance :: Qty units

class ApproximateEquality a b units | a b -> units where
  (~=) :: Tolerance units => a -> b -> Bool

instance units ~ units_ => ApproximateEquality (Qty units) (Qty units_) units where
  x ~= y = Qty.abs (x - y) <= ?tolerance

instance ApproximateEquality a b units => ApproximateEquality (List a) (List b) units where
  x : xs ~= y : ys = x ~= y && xs ~= ys
  [] ~= [] = True
  NonEmpty _ ~= [] = False
  [] ~= NonEmpty _ = False

instance ApproximateEquality a b units => ApproximateEquality (NonEmpty a) (NonEmpty b) units where
  x :| xs ~= y :| ys = x ~= y && xs ~= ys

instance ApproximateEquality a b units => ApproximateEquality (Maybe a) (Maybe b) units where
  Just a ~= Just b = a ~= b
  Nothing ~= Nothing = True
  Just _ ~= Nothing = False
  Nothing ~= Just _ = False

instance
  ( ApproximateEquality a1 b1 units
  , ApproximateEquality a2 b2 units_
  , units ~ units_
  ) =>
  ApproximateEquality (a1, a2) (b1, b2) units
  where
  (a1, a2) ~= (b1, b2) = a1 ~= b1 && a2 ~= b2

instance
  ( ApproximateEquality a1 b1 units
  , ApproximateEquality a2 b2 units_
  , ApproximateEquality a3 b3 units__
  , units ~ units_
  , units ~ units__
  ) =>
  ApproximateEquality (a1, a2, a3) (b1, b2, b3) units
  where
  (a1, a2, a3) ~= (b1, b2, b3) = a1 ~= b1 && a2 ~= b2 && a3 ~= b3

instance
  ( ApproximateEquality a1 b1 units
  , ApproximateEquality a2 b2 units_
  , ApproximateEquality a3 b3 units__
  , ApproximateEquality a4 b4 units___
  , units ~ units_
  , units ~ units__
  , units ~ units___
  ) =>
  ApproximateEquality (a1, a2, a3, a4) (b1, b2, b3, b4) units
  where
  (a1, a2, a3, a4) ~= (b1, b2, b3, b4) = a1 ~= b1 && a2 ~= b2 && a3 ~= b3 && a4 ~= b4

(!=) :: (ApproximateEquality a b units, Tolerance units) => a -> b -> Bool
(!=) first second = not (first ~= second)

{- | Take an expression which would normally require a tolerance,
and evaluate it using a tolerance of zero. For example, the expression

  value ^ range

would normally require an implicit tolerance to be present;

  exactly (value ^ range)

will evaluate that expression with a tolerance of zero,
equivalent to

  let ?tolerance = Qty.zero in value ^ range
-}
exactly :: (Tolerance units => a) -> a
exactly expression = using Qty.zero expression

using :: Qty units -> (Tolerance units => a) -> a
using tolerance expression = let ?tolerance = tolerance in expression

infix 4 ~=, !=

squared :: (Tolerance units, Units.Squared units squaredUnits) => Qty squaredUnits
squared = Qty.squared ?tolerance

squared' :: Tolerance units => Qty (units :*: units)
squared' = Qty.squared' ?tolerance

ofSquared' :: Tolerance units => Qty units -> Qty (units :*: units)
ofSquared' value = ?tolerance .*. ?tolerance + 2 * Qty.abs value .*. ?tolerance

ofSquared :: (Tolerance units, Units.Squared units squaredUnits) => Qty units -> Qty squaredUnits
ofSquared = Units.specialize . ofSquared'

times' :: Tolerance units1 => Qty units2 -> Qty (units1 :*: units2)
times' value = ?tolerance .*. value

times :: (Tolerance units1, Units.Product units1 units2 units3) => Qty units2 -> Qty units3
times = Units.specialize . times'
