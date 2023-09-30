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
  , (.<>)
  , (.><)
  )
where

import Basics
import {-# SOURCE #-} Float (Float)
import Generic qualified
import {-# SOURCE #-} Qty (Qty (Qty))
import {-# SOURCE #-} Sign (Sign (Negative, Positive))
import Units (Unitless)
import Units qualified
import Prelude qualified

class (Multiplication Sign a a, Multiplication a Sign a) => Negation a where
  negate :: a -> a

class (Generic.HasZero b) => Addition a b c | a b -> c where
  (+) :: a -> b -> c

class Subtraction a b c | a b -> c where
  (-) :: a -> b -> c

subtract :: (Subtraction a b c) => b -> a -> c
subtract b a = a - b

class (Multiplication b a c) => Multiplication a b c | a b -> c where
  (*) :: a -> b -> c

class Division a b c | a b -> c where
  (/) :: a -> b -> c

class (DotProduct b a c) => DotProduct a b c | a b -> c where
  (<>) :: a -> b -> c

class (CrossProduct b a c) => CrossProduct a b c | a b -> c where
  (><) :: a -> b -> c

(//) :: Int -> Int -> Int
(//) = Prelude.quot

instance Negation Int where
  negate = Prelude.negate

instance Multiplication Sign Int Int where
  Positive * n = n
  Negative * n = -n

instance Multiplication Int Sign Int where
  n * Positive = n
  n * Negative = -n

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

(.*) ::
  ( Units.Coercion unitsA Unitless a aUnitless
  , Units.Coercion unitsB Unitless b bUnitless
  , Units.Coercion Unitless unitsC cUnitless c
  , Units.GenericProduct unitsA unitsB ~ unitsC
  , Multiplication aUnitless bUnitless cUnitless
  ) =>
  a ->
  b ->
  c
(.*) lhs rhs = Units.add (Units.drop lhs * Units.drop rhs)

(./) ::
  ( Units.Coercion unitsA Unitless a aUnitless
  , Units.Coercion unitsB Unitless b bUnitless
  , Units.Coercion Unitless unitsC cUnitless c
  , Units.GenericQuotient unitsA unitsB ~ unitsC
  , Division aUnitless bUnitless cUnitless
  ) =>
  a ->
  b ->
  c
(./) lhs rhs = Units.add (Units.drop lhs / Units.drop rhs)

(.<>) ::
  ( Units.Coercion unitsA Unitless a aUnitless
  , Units.Coercion unitsB Unitless b bUnitless
  , Units.Coercion Unitless unitsC cUnitless c
  , Units.GenericProduct unitsA unitsB ~ unitsC
  , DotProduct aUnitless bUnitless cUnitless
  ) =>
  a ->
  b ->
  c
(.<>) lhs rhs = Units.add (Units.drop lhs <> Units.drop rhs)

(.><) ::
  ( Units.Coercion unitsA Unitless a aUnitless
  , Units.Coercion unitsB Unitless b bUnitless
  , Units.Coercion Unitless unitsC cUnitless c
  , Units.GenericProduct unitsA unitsB ~ unitsC
  , CrossProduct aUnitless bUnitless cUnitless
  ) =>
  a ->
  b ->
  c
(.><) lhs rhs = Units.add (Units.drop lhs >< Units.drop rhs)