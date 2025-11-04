module OpenSolid.Tolerance
  ( Tolerance
  , ApproximateEquality ((~=))
  , (!=)
  , using
  , unitless
  , squared
  , squared'
  , forEndpointDerivative
  , (~=##)
  )
where

import OpenSolid.Arithmetic
import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Int qualified as Int
import OpenSolid.NonEmpty (NonEmpty ((:|)), pattern NonEmpty)
import OpenSolid.Number (Number, fromRational)
import {-# SOURCE #-} OpenSolid.Number qualified as Number
import OpenSolid.Quantity (Quantity (Quantity##))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Unboxed.Math
import OpenSolid.Units ((:*:))
import OpenSolid.Units qualified as Units

type Tolerance units = ?tolerance :: Quantity units

class ApproximateEquality a b units | a b -> units where
  (~=) :: Tolerance units => a -> b -> Bool

infix 4 ~=

instance units1 ~ units2 => ApproximateEquality (Quantity units1) (Quantity units2) units1 where
  x ~= y = Quantity.abs (x - y) <= ?tolerance

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
  ( ApproximateEquality a1 b1 units1
  , ApproximateEquality a2 b2 units2
  , units1 ~ units2
  ) =>
  ApproximateEquality (a1, a2) (b1, b2) units1
  where
  (a1, a2) ~= (b1, b2) = a1 ~= b1 && a2 ~= b2

instance
  ( ApproximateEquality a1 b1 units1
  , ApproximateEquality a2 b2 units2
  , ApproximateEquality a3 b3 units3
  , units1 ~ units2
  , units1 ~ units3
  ) =>
  ApproximateEquality (a1, a2, a3) (b1, b2, b3) units1
  where
  (a1, a2, a3) ~= (b1, b2, b3) = a1 ~= b1 && a2 ~= b2 && a3 ~= b3

instance
  ( ApproximateEquality a1 b1 units1
  , ApproximateEquality a2 b2 units2
  , ApproximateEquality a3 b3 units3
  , ApproximateEquality a4 b4 units4
  , units1 ~ units2
  , units1 ~ units3
  , units1 ~ units4
  ) =>
  ApproximateEquality (a1, a2, a3, a4) (b1, b2, b3, b4) units1
  where
  (a1, a2, a3, a4) ~= (b1, b2, b3, b4) = a1 ~= b1 && a2 ~= b2 && a3 ~= b3 && a4 ~= b4

(!=) :: (ApproximateEquality a b units, Tolerance units) => a -> b -> Bool
(!=) first second = not (first ~= second)

infix 4 !=

-- | A default tolerance (1e-9) for comparing unitless values with expected magnitude near 1.
unitless :: Number
unitless = 1e-9

using :: Quantity units -> (Tolerance units => a) -> a
using tolerance expression = let ?tolerance = tolerance in expression

squared :: (Tolerance units, Units.Squared units squaredUnits) => Quantity squaredUnits
squared = Quantity.squared ?tolerance

squared' :: Tolerance units => Quantity (units :*: units)
squared' = Quantity.squared' ?tolerance

forEndpointDerivative :: Tolerance units => Int -> Quantity units
forEndpointDerivative n = ?tolerance / Number.fromInt (Int.factorial n * 2 ** (2 * n))

{-# INLINE (~=##) #-}
(~=##) :: Tolerance units => Double# -> Double# -> Int#
(~=##) x## y## = let !(Quantity## tolerance##) = ?tolerance in abs## (x## -## y##) <=## tolerance##
