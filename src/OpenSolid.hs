module OpenSolid
  ( module Basics
  , module Arithmetic
  , module Result
  , module Qty
  , module Float
  , module List
  , module Sign
  , Indeterminate (Indeterminate)
  , Invalid (Invalid)
  , validate
  , Tolerance
  , ApproximateEquality ((~=))
  , (.*)
  , (./)
  )
where

import Arithmetic
import Basics
import Float (Float, fromRational)
import List (List)
import Qty (Qty (..))
import Result (IsError (errorMessage), Result (Error, Ok), (??))
import Sign (Sign (Negative, Positive))
import Units (Unitless)
import Units qualified
import Prelude qualified

data Indeterminate = Indeterminate

instance IsError Indeterminate where
  errorMessage Indeterminate = "Result is indeterminate"

data Invalid = Invalid

instance IsError Invalid where
  errorMessage Invalid = "Validation failed"

validate :: (a -> Bool) -> a -> Result Invalid a
validate function value = if function value then Ok value else Error Invalid

type Tolerance units = ?tolerance :: Qty units

class ApproximateEquality a b units | a -> units, b -> units where
  (~=) :: Tolerance units => a -> b -> Bool

infix 4 ~=

instance units ~ units' => ApproximateEquality (Qty units) (Qty units') units where
  x ~= y = let (Qty delta) = x - y in Qty (Prelude.abs delta) <= ?tolerance

(.*)
  :: ( Units.Coercion a
     , Units.Coercion b
     , Units.Coercion c
     , Multiplication (a Unitless) (b Unitless) (c Unitless)
     )
  => a units1
  -> b units2
  -> c (Units.GenericProduct units1 units2)
(.*) lhs rhs = Units.add (Units.drop lhs * Units.drop rhs)

(./)
  :: ( Units.Coercion a
     , Units.Coercion b
     , Units.Coercion c
     , Division (a Unitless) (b Unitless) (c Unitless)
     )
  => a units1
  -> b units2
  -> c (Units.GenericQuotient units1 units2)
(./) lhs rhs = Units.add (Units.drop lhs / Units.drop rhs)
