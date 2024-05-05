module Arithmetic
  ( Negation (negate)
  , Addition ((+))
  , Subtraction ((-))
  , subtract
  , Multiplication' (type (.*.), (.*.))
  , Multiplication ((*))
  , Division' (type (./.), (./.))
  , Division ((/))
  , DivMod ((//), (%))
  , DotMultiplication' (type (.<>.), (.<>.))
  , DotMultiplication ((<>))
  , CrossMultiplication' (type (.><.), (.><.))
  , CrossMultiplication ((><))
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

import Basics
import Data.List.NonEmpty (NonEmpty ((:|)))
import {-# SOURCE #-} Float (Float)
import {-# SOURCE #-} Qty (Qty (Qty_))
import {-# SOURCE #-} Sign (Sign (Negative, Positive))
import Units (Unitless, Units, (:*:), (:/:))
import Units qualified
import Prelude qualified

class (Multiplication Sign a a, Multiplication a Sign a) => Negation a where
  negate :: a -> a

class Addition a b c | a b -> c where
  (+) :: a -> b -> c

class Subtraction a b c | a b -> c where
  (-) :: a -> b -> c

subtract :: Subtraction a b c => b -> a -> c
subtract b a = a - b

class Multiplication' b a => Multiplication' a b where
  type a .*. b
  (.*.) :: a -> b -> a .*. b

class
  ( Multiplication' a b
  , Units.Coercion (a .*. b) c
  , Units.Specialize (Units (a .*. b)) (Units c)
  , Multiplication b a c
  ) =>
  Multiplication a b c
    | a b -> c
  where
  (*) :: a -> b -> c
  a * b = Units.specialize (a .*. b)

class Division' a b where
  type a ./. b
  (./.) :: a -> b -> a ./. b

class
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , Units.Specialize (Units (a ./. b)) (Units c)
  ) =>
  Division a b c
    | a b -> c
  where
  (/) :: a -> b -> c
  a / b = Units.specialize (a ./. b)

class DotMultiplication' a b where
  type a .<>. b
  (.<>.) :: a -> b -> a .<>. b

class
  ( DotMultiplication' a b
  , Units.Coercion (a .<>. b) c
  , Units.Specialize (Units (a .<>. b)) (Units c)
  , DotMultiplication b a c
  ) =>
  DotMultiplication a b c
    | a b -> c
  where
  (<>) :: a -> b -> c
  a <> b = Units.specialize (a .<>. b)

class CrossMultiplication' a b where
  type a .><. b
  (.><.) :: a -> b -> a .><. b

class
  ( CrossMultiplication' a b
  , Units.Coercion (a .><. b) c
  , Units.Specialize (Units (a .><. b)) (Units c)
  , CrossMultiplication b a c
  ) =>
  CrossMultiplication a b c
    | a b -> c
  where
  (><) :: a -> b -> c
  a >< b = Units.specialize (a .><. b)

class DivMod a where
  (//) :: a -> a -> Int
  (%) :: a -> a -> a

instance DivMod Int where
  (//) = Prelude.div
  (%) = Prelude.mod

instance Negation Int where
  negate = Prelude.negate

instance Multiplication' Sign Int where
  type Sign .*. Int = Int
  Positive .*. n = n
  Negative .*. n = -n

instance Multiplication Sign Int Int

instance Multiplication' Int Sign where
  type Int .*. Sign = Int
  n .*. Positive = n
  n .*. Negative = -n

instance Multiplication Int Sign Int

instance Addition Int Int Int where
  (+) = (Prelude.+)

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
  n ./. m = Qty_ (fromIntegral n Prelude./ fromIntegral m)

instance Division Int Int Float

infixl 6 +, -

infixl 7 *, /, <>, ><, //, %, .*., ./., .<>., .><.

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
  , Units a ~ units1 :/: units2
  , Units b ~ units2
  , Units c ~ units1
  ) =>
  a ->
  b ->
  c
a *! b = Units.coerce (a .*. b)

(!*) ::
  ( Multiplication' a b
  , Units.Coercion (a .*. b) c
  , Units a ~ units2
  , Units b ~ units1 :/: units2
  , Units c ~ units1
  ) =>
  a ->
  b ->
  c
a !* b = Units.coerce (a .*. b)

(^*.) ::
  ( Multiplication' a b
  , Units.Coercion (a .*. b) c
  , Units a ~ units1
  , Units b ~ Unitless :/: units2
  , Units c ~ units1 :/: units2
  ) =>
  a ->
  b ->
  c
a ^*. b = Units.coerce (a .*. b)

(.*^) ::
  ( Multiplication' a b
  , Units.Coercion (a .*. b) c
  , Units a ~ Unitless :/: units2
  , Units b ~ units1
  , Units c ~ units1 :/: units2
  ) =>
  a ->
  b ->
  c
a .*^ b = Units.coerce (a .*. b)

(/%) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , Units a ~ Unitless
  , Units b ~ units1 :/: units2
  , Units c ~ units2 :/: units1
  ) =>
  a ->
  b ->
  c
a /% b = Units.coerce (a ./. b)

(!/) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , Units a ~ units1
  , Units b ~ units1 :/: units2
  , Units c ~ units2
  ) =>
  a ->
  b ->
  c
a !/ b = Units.coerce (a ./. b)

(.!/!) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , Units a ~ units1 :*: units2
  , Units b ~ units2
  , Units c ~ units1
  ) =>
  a ->
  b ->
  c
a .!/! b = Units.coerce (a ./. b)

(!./!) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , Units a ~ units1 :*: units2
  , Units b ~ units1
  , Units c ~ units2
  ) =>
  a ->
  b ->
  c
a !./! b = Units.coerce (a ./. b)

(!/!.) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , Units a ~ units1
  , Units b ~ units1 :*: units2
  , Units c ~ Unitless :/: units2
  ) =>
  a ->
  b ->
  c
a !/!. b = Units.coerce (a ./. b)

(!/.!) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , Units a ~ units2
  , Units b ~ units1 :*: units2
  , Units c ~ Unitless :/: units1
  ) =>
  a ->
  b ->
  c
a !/.! b = Units.coerce (a ./. b)

(.!/.!) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , Units a ~ units1 :*: units3
  , Units b ~ units2 :*: units3
  , Units c ~ units1 :/: units2
  ) =>
  a ->
  b ->
  c
a .!/.! b = Units.coerce (a ./. b)

(./^) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , Units a ~ units1
  , Units b ~ Unitless :/: units2
  , Units c ~ units1 :*: units2
  ) =>
  a ->
  b ->
  c
a ./^ b = Units.coerce (a ./. b)

(!?/.!?) ::
  ( Division' a b
  , Units.Coercion (a ./. b) c
  , Units a ~ units1 :*: units2
  , Units b ~ units1 :*: units2 :*: units3
  , Units c ~ Unitless :/: units3
  ) =>
  a ->
  b ->
  c
a !?/.!? b = Units.coerce (a ./. b)

infixl 7 ^*., .*^, .!/!, !./!, !/!., !/.!, .!/.!, ./^, !?/.!?
