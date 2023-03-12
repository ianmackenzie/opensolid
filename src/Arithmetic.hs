module Arithmetic
  ( Negation (negate)
  , Addition ((+))
  , Subtraction ((-))
  , subtract
  , Multiplication ((*))
  , Division ((/))
  , (//)
  , DotProduct ((<>))
  , CrossProduct ((><))
  , (.*)
  , (./)
  )
where

import Basics
import {-# SOURCE #-} Float (Float)
import {-# SOURCE #-} Qty (Qty (Qty))
import Units (Unitless)
import Units qualified
import Prelude qualified

class Negation a where
  negate :: a -> a

class Addition a b c | a b -> c where
  (+) :: a -> b -> c

class Subtraction a b c | a b -> c where
  (-) :: a -> b -> c

subtract :: Subtraction a b c => b -> a -> c
subtract b a = a - b

class Multiplication b a c => Multiplication a b c | a b -> c where
  (*) :: a -> b -> c

class Division a b c | a b -> c where
  (/) :: a -> b -> c

class DotProduct b a c => DotProduct a b c | a b -> c where
  (<>) :: a -> b -> c

class CrossProduct b a c => CrossProduct a b c | a b -> c where
  (><) :: a -> b -> c

(//) :: Int -> Int -> Int
(//) = Prelude.quot

instance Addition Int Int Int where
  (+) = (Prelude.+)

instance Subtraction Int Int Int where
  (-) = (Prelude.-)

instance Multiplication Int Int Int where
  (*) = (Prelude.*)

instance Division Int Int Float where
  n / m = Qty (Prelude.fromIntegral n Prelude./ Prelude.fromIntegral m)

infixl 6 +, -

infixl 7 *, /, //

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
