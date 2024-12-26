module OpenSolid.Arithmetic
  ( Negation (negate)
  , Addition ((+))
  , Sum (Sum)
  , Subtraction ((-))
  , subtract
  , Difference (Difference)
  , Multiplication' (type (.*.), (.*.))
  , Multiplication ((*))
  , Product (Product)
  , Division' (type (./.), (./.))
  , Division ((/))
  , Quotient (Quotient)
  , DivMod ((//), (%))
  , DotMultiplication' (type (.<>.), (.<>.))
  , DotMultiplication ((<>))
  , DotProduct (DotProduct)
  , CrossMultiplication' (type (.><.), (.><.))
  , CrossMultiplication ((><))
  , CrossProduct (CrossProduct)
  , Exponentiation ((**))
  , (*!)
  , (!*)
  , (^*.)
  , (.*^)
  , (!/)
  , (/%)
  , (.!/!)
  , (!./!)
  , (!/!.)
  , (!/.!)
  , (.!/.!)
  , (./^)
  , (!?/.!?)
  )
where

import Data.List.NonEmpty (NonEmpty ((:|)))
import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Float (Float)
import {-# SOURCE #-} OpenSolid.Qty (Qty (Qty))
import {-# SOURCE #-} OpenSolid.Sign (Sign (Negative, Positive))
import OpenSolid.Units (Unitless, UnitsOf, (:*:), (:/:))
import OpenSolid.Units qualified as Units
import Prelude qualified

class (Multiplication Sign a a, Multiplication a Sign a) => Negation a where
  negate :: a -> a

class Addition a b c | a b -> c where
  (+) :: a -> b -> c

infixl 6 +

data Sum a b = Sum a b deriving (Eq, Show)

class Subtraction a b c | a b -> c where
  (-) :: a -> b -> c

infixl 6 -

subtract :: Subtraction a b c => b -> a -> c
subtract b a = a - b

data Difference a b = Difference a b deriving (Eq, Show)

class Multiplication' b a => Multiplication' a b where
  type a .*. b
  (.*.) :: a -> b -> a .*. b

infixl 7 .*.

class
  ( Multiplication' a b
  , Units.Coercion (a .*. b) c
  , Units.Specialize (UnitsOf (a .*. b)) (UnitsOf c)
  , Multiplication b a c
  ) =>
  Multiplication a b c
    | a b -> c
  where
  {-# INLINEABLE (*) #-}
  (*) :: a -> b -> c
  a * b = Units.specialize (a .*. b)

infixl 7 *

data Product a b = Product a b deriving (Eq, Show)

class Division' a b where
  type a ./. b
  (./.) :: a -> b -> a ./. b

infixl 7 ./.

class
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , Units.Specialize (UnitsOf (a ./. b)) (UnitsOf c)
  ) =>
  Division a b c
    | a b -> c
  where
  {-# INLINEABLE (/) #-}
  (/) :: a -> b -> c
  a / b = Units.specialize (a ./. b)

infixl 7 /

data Quotient a b = Quotient a b deriving (Eq, Show)

class DotMultiplication' a b where
  type a .<>. b
  (.<>.) :: a -> b -> a .<>. b

infixl 7 .<>.

class
  ( DotMultiplication' a b
  , Units.Coercion (a .<>. b) c
  , Units.Specialize (UnitsOf (a .<>. b)) (UnitsOf c)
  , DotMultiplication b a c
  ) =>
  DotMultiplication a b c
    | a b -> c
  where
  {-# INLINEABLE (<>) #-}
  (<>) :: a -> b -> c
  a <> b = Units.specialize (a .<>. b)

infixl 7 <>

data DotProduct a b = DotProduct a b deriving (Show)

class CrossMultiplication' a b where
  type a .><. b
  (.><.) :: a -> b -> a .><. b

infixl 7 .><.

class
  ( CrossMultiplication' a b
  , Units.Coercion (a .><. b) c
  , Units.Specialize (UnitsOf (a .><. b)) (UnitsOf c)
  , CrossMultiplication b a c
  ) =>
  CrossMultiplication a b c
    | a b -> c
  where
  {-# INLINEABLE (><) #-}
  (><) :: a -> b -> c
  a >< b = Units.specialize (a .><. b)

infixl 7 ><

data CrossProduct a b = CrossProduct a b deriving (Show)

class DivMod a where
  (//) :: a -> a -> Int
  (%) :: a -> a -> a

infixl 7 //

infixl 7 %

instance DivMod Int where
  (//) = Prelude.div
  (%) = Prelude.mod

instance Negation Int where
  negate = Prelude.negate

instance Multiplication' Sign Int where
  type Sign .*. Int = Int
  {-# INLINEABLE (.*.) #-}
  Positive .*. n = n
  Negative .*. n = -n

instance Multiplication Sign Int Int

instance Multiplication' Int Sign where
  type Int .*. Sign = Int
  {-# INLINEABLE (.*.) #-}
  n .*. Positive = n
  n .*. Negative = -n

instance Multiplication Int Sign Int

instance Addition Int Int Int where
  {-# INLINEABLE (+) #-}
  (+) = (Prelude.+)

instance Addition Text Text Text where
  {-# INLINEABLE (+) #-}
  (+) = Prelude.mappend

instance Addition Text (Maybe Text) Text where
  text + Just suffix = text + suffix
  text + Nothing = text

instance Addition (Maybe Text) Text Text where
  Just prefix + text = prefix + text
  Nothing + text = text

instance a ~ a' => Addition (List a) (List a') (List a) where
  (+) = Prelude.mappend

instance a ~ a' => Addition (NonEmpty a) (NonEmpty a') (NonEmpty a) where
  (a :| as) + (b :| bs) = a :| (as + (b : bs))

instance a ~ a' => Addition (NonEmpty a) (List a') (NonEmpty a) where
  nonEmpty + [] = nonEmpty
  (a :| as) + bs = a :| (as + bs)

instance a ~ a' => Addition (List a) (NonEmpty a') (NonEmpty a) where
  [] + nonEmpty = nonEmpty
  (a : as) + (b :| bs) = a :| (as + (b : bs))

instance a ~ a' => Addition (Maybe a) (Maybe a') (List a) where
  Just first + Just second = [first, second]
  Just first + Nothing = [first]
  Nothing + Just second = [second]
  Nothing + Nothing = []

instance a ~ a' => Addition (Maybe a) (List a') (List a) where
  Just value + list = value : list
  Nothing + list = list

instance a ~ a' => Addition (List a) (Maybe a') (List a) where
  list + Nothing = list
  list + Just value = list + [value]

instance a ~ a' => Addition (Maybe a) (NonEmpty a') (NonEmpty a) where
  Just a + (b :| bs) = a :| b : bs
  Nothing + nonEmpty = nonEmpty

instance a ~ a' => Addition (NonEmpty a) (Maybe a') (NonEmpty a) where
  nonEmpty + Nothing = nonEmpty
  nonEmpty + Just value = nonEmpty + [value]

instance Subtraction Int Int Int where
  (-) = (Prelude.-)

instance Multiplication' Int Int where
  type Int .*. Int = Int
  (.*.) = (Prelude.*)

instance Multiplication Int Int Int

instance Division' Int Int where
  type Int ./. Int = Float
  n ./. m = Qty (fromIntegral n Prelude./ fromIntegral m)

instance Division Int Int Float

class Exponentiation a b where
  (**) :: a -> b -> a

infixr 8 **

instance Exponentiation Int Int where
  (**) = (Prelude.^)

instance units ~ Unitless => Exponentiation (Qty units) Int where
  x ** n = x ** (fromIntegral n :: Qty Unitless)

instance
  (baseUnits ~ Unitless, exponentUnits ~ Unitless) =>
  Exponentiation (Qty baseUnits) (Qty exponentUnits)
  where
  (**) = (Prelude.**)

-- GENERIC UNITS MANIPULATION

(*!) ::
  ( Multiplication' a b
  , Units.Coercion (a .*. b) c
  , UnitsOf a ~ units1 :/: units2
  , UnitsOf b ~ units2
  , UnitsOf c ~ units1
  ) =>
  a ->
  b ->
  c
a *! b = Units.coerce (a .*. b)

infixl 7 *!

(!*) ::
  ( Multiplication' a b
  , Units.Coercion (a .*. b) c
  , UnitsOf a ~ units2
  , UnitsOf b ~ units1 :/: units2
  , UnitsOf c ~ units1
  ) =>
  a ->
  b ->
  c
a !* b = Units.coerce (a .*. b)

infixl 7 !*

(^*.) ::
  ( Multiplication' a b
  , Units.Coercion (a .*. b) c
  , UnitsOf a ~ units1
  , UnitsOf b ~ Unitless :/: units2
  , UnitsOf c ~ units1 :/: units2
  ) =>
  a ->
  b ->
  c
a ^*. b = Units.coerce (a .*. b)

infixl 7 ^*.

(.*^) ::
  ( Multiplication' a b
  , Units.Coercion (a .*. b) c
  , UnitsOf a ~ Unitless :/: units2
  , UnitsOf b ~ units1
  , UnitsOf c ~ units1 :/: units2
  ) =>
  a ->
  b ->
  c
a .*^ b = Units.coerce (a .*. b)

infixl 7 .*^

(/%) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , UnitsOf a ~ Unitless
  , UnitsOf b ~ units1 :/: units2
  , UnitsOf c ~ units2 :/: units1
  ) =>
  a ->
  b ->
  c
a /% b = Units.coerce (a ./. b)

infixl 7 /%

(!/) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , UnitsOf a ~ units1
  , UnitsOf b ~ units1 :/: units2
  , UnitsOf c ~ units2
  ) =>
  a ->
  b ->
  c
a !/ b = Units.coerce (a ./. b)

infixl 7 !/

(.!/!) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , UnitsOf a ~ units1 :*: units2
  , UnitsOf b ~ units2
  , UnitsOf c ~ units1
  ) =>
  a ->
  b ->
  c
a .!/! b = Units.coerce (a ./. b)

infixl 7 .!/!

(!./!) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , UnitsOf a ~ units1 :*: units2
  , UnitsOf b ~ units1
  , UnitsOf c ~ units2
  ) =>
  a ->
  b ->
  c
a !./! b = Units.coerce (a ./. b)

infixl 7 !./!

(!/!.) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , UnitsOf a ~ units1
  , UnitsOf b ~ units1 :*: units2
  , UnitsOf c ~ Unitless :/: units2
  ) =>
  a ->
  b ->
  c
a !/!. b = Units.coerce (a ./. b)

infixl 7 !/!.

(!/.!) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , UnitsOf a ~ units2
  , UnitsOf b ~ units1 :*: units2
  , UnitsOf c ~ Unitless :/: units1
  ) =>
  a ->
  b ->
  c
a !/.! b = Units.coerce (a ./. b)

infixl 7 !/.!

(.!/.!) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , UnitsOf a ~ units1 :*: units3
  , UnitsOf b ~ units2 :*: units3
  , UnitsOf c ~ units1 :/: units2
  ) =>
  a ->
  b ->
  c
a .!/.! b = Units.coerce (a ./. b)

infixl 7 .!/.!

(./^) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , UnitsOf a ~ units1
  , UnitsOf b ~ Unitless :/: units2
  , UnitsOf c ~ units1 :*: units2
  ) =>
  a ->
  b ->
  c
a ./^ b = Units.coerce (a ./. b)

infixl 7 ./^

(!?/.!?) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , UnitsOf a ~ units1 :*: units2
  , UnitsOf b ~ units1 :*: units2 :*: units3
  , UnitsOf c ~ Unitless :/: units3
  ) =>
  a ->
  b ->
  c
a !?/.!? b = Units.coerce (a ./. b)

infixl 7 !?/.!?
