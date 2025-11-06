module OpenSolid.Tolerance
  ( Tolerance
  , ApproximateEquality ((~=))
  , (!=)
  , using
  , unitless
  , squared
  , squared#
  , (~=##)
  )
where

import OpenSolid.Arithmetic hiding ((+), (-))
import OpenSolid.Bootstrap
import OpenSolid.NonEmpty (NonEmpty ((:|)), pattern NonEmpty)
import OpenSolid.Number (Number)
import OpenSolid.Quantity (Quantity (Quantity##))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Unboxed.Math
import OpenSolid.Units (type (#*#))
import OpenSolid.Units qualified as Units

type Tolerance units = ?tolerance :: Quantity units

class ApproximateEquality a units | a -> units where
  (~=) :: Tolerance units => a -> a -> Bool

infix 4 ~=

instance ApproximateEquality (Quantity units) units where
  x ~= y = Quantity.abs (x .-. y) <= ?tolerance

instance ApproximateEquality a units => ApproximateEquality (List a) units where
  x : xs ~= y : ys = x ~= y && xs ~= ys
  [] ~= [] = True
  NonEmpty _ ~= [] = False
  [] ~= NonEmpty _ = False

instance ApproximateEquality a units => ApproximateEquality (NonEmpty a) units where
  x :| xs ~= y :| ys = x ~= y && xs ~= ys

instance ApproximateEquality a units => ApproximateEquality (Maybe a) units where
  Just a ~= Just b = a ~= b
  Nothing ~= Nothing = True
  Just _ ~= Nothing = False
  Nothing ~= Just _ = False

instance
  ( ApproximateEquality a1 units1
  , ApproximateEquality a2 units2
  , units1 ~ units2
  ) =>
  ApproximateEquality (a1, a2) units1
  where
  (a1, a2) ~= (b1, b2) = a1 ~= b1 && a2 ~= b2

instance
  ( ApproximateEquality a1 units1
  , ApproximateEquality a2 units2
  , ApproximateEquality a3 units3
  , units1 ~ units2
  , units1 ~ units3
  ) =>
  ApproximateEquality (a1, a2, a3) units1
  where
  (a1, a2, a3) ~= (b1, b2, b3) = a1 ~= b1 && a2 ~= b2 && a3 ~= b3

instance
  ( ApproximateEquality a1 units1
  , ApproximateEquality a2 units2
  , ApproximateEquality a3 units3
  , ApproximateEquality a4 units4
  , units1 ~ units2
  , units1 ~ units3
  , units1 ~ units4
  ) =>
  ApproximateEquality (a1, a2, a3, a4) units1
  where
  (a1, a2, a3, a4) ~= (b1, b2, b3, b4) = a1 ~= b1 && a2 ~= b2 && a3 ~= b3 && a4 ~= b4

(!=) :: (ApproximateEquality a units, Tolerance units) => a -> a -> Bool
(!=) first second = not (first ~= second)

infix 4 !=

-- | A default tolerance (1e-9) for comparing unitless values with expected magnitude near 1.
unitless :: Number
unitless = 1e-9

using :: Quantity units -> (Tolerance units => a) -> a
using tolerance expression = let ?tolerance = tolerance in expression

squared :: (Tolerance units, Units.Squared units squaredUnits) => Quantity squaredUnits
squared = Quantity.squared ?tolerance

squared# :: Tolerance units => Quantity (units #*# units)
squared# = Quantity.squared# ?tolerance

{-# INLINE (~=##) #-}
(~=##) :: Tolerance units => Double# -> Double# -> Int#
(~=##) x## y## = let !(Quantity## tolerance##) = ?tolerance in abs## (x## -## y##) <=## tolerance##
