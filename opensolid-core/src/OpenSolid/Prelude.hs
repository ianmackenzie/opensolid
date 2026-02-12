module OpenSolid.Prelude
  ( module Prelude
  , Type
  , List
  , NonEmpty ((:|))
  , Text
  , Result (Ok, Error)
  , Exception
  , pattern NonEmpty
  , type (#) (Named)
  , (&)
  , Negation (negate)
  , Addition ((+))
  , Subtraction ((-))
  , subtract
  , Multiplication_ ((?*?))
  , type (?*?)
  , Multiplication ((*))
  , Division_ ((?/?))
  , type (?/?)
  , Division ((/))
  , DotMultiplication_ (dot_)
  , DotMultiplication (dot)
  , CrossMultiplication_ (cross_)
  , CrossMultiplication (cross)
  , DivMod ((//), (%))
  , Exponentiation ((**))
  , Composition ((.))
  , Tolerance
  , ApproximateEquality ((~=))
  , (!=)
  , Intersects (intersects)
  , Quantity (Quantity)
  , Unitless
  , Radians
  , Meters
  , Number
  , Sign (Sign, Positive, Negative)
  , fromInteger
  , fromRational
  , fromString
  , fromLabel
  , getField
  , ifThenElse
  , assert
  , throw
  , pattern TODO
  )
where

import Control.Exception (Exception, assert, throw)
import Data.Coerce
import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified
import Data.String (fromString)
import Data.Text (Text)
import Data.Text qualified
import Foreign.Storable (Storable)
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.Records (getField)
import GHC.Stack (HasCallStack)
import GHC.Stack qualified
import GHC.TypeLits (Symbol)
import OpenSolid.Units (HasUnits, Meters, Radians, Unitless, type (?*?), type (?/?))
import OpenSolid.Units qualified as Units
import System.Random.Stateful qualified
import Prelude
  ( Applicative
  , Bool (False, True)
  , Char
  , Double
  , Either (Left, Right)
  , Eq
  , Foldable
  , Functor
  , IO
  , Int
  , Maybe (Just, Nothing)
  , Monad ((>>), (>>=))
  , MonadFail
  , Ord
  , Ordering (EQ, GT, LT)
  , Show
  , Traversable
  , compare
  , const
  , fromIntegral
  , id
  , max
  , min
  , not
  , otherwise
  , ($)
  , (&&)
  , (/=)
  , (<)
  , (<$>)
  , (<*>)
  , (<=)
  , (<>)
  , (==)
  , (>)
  , (>=)
  , (||)
  , type (~)
  )
import Prelude qualified

----- Arithmetic -----

class (Multiplication Sign a a, Multiplication a Sign a) => Negation a where
  negate :: a -> a

instance Negation Int where
  {-# INLINE negate #-}
  negate = Prelude.negate

class Addition a b c | a b -> c where
  (+) :: a -> b -> c

infixl 6 +

instance Addition Int Int Int where
  {-# INLINE (+) #-}
  (+) = (Prelude.+)

class Subtraction a b c | a b -> c where
  (-) :: a -> b -> c

infixl 6 -

{-# INLINE subtract #-}
subtract :: Subtraction a b c => b -> a -> c
subtract value = (- value)

instance Subtraction Int Int Int where
  {-# INLINE (-) #-}
  (-) = (Prelude.-)

class Multiplication_ a b c | a b -> c where
  (?*?) :: a -> b -> c

infixl 7 ?*?

class Multiplication b a c => Multiplication a b c | a b -> c where
  (*) :: a -> b -> c

infixl 7 *

instance Multiplication Int Int Int where
  {-# INLINE (*) #-}
  (*) = (Prelude.*)

instance Multiplication Sign Int Int where
  Positive * value = value
  Negative * value = -value

instance Multiplication Int Sign Int where
  value * Positive = value
  value * Negative = -value

class Division_ a b c | a b -> c where
  (?/?) :: a -> b -> c

infixl 7 ?/?

class Division a b c | a b -> c where
  (/) :: a -> b -> c

infixl 7 /

instance Division Int Int Number where
  {-# INLINEABLE (/) #-}
  n / m = (fromIntegral n :: Number) / (fromIntegral m :: Number)

class DotMultiplication_ a b c | a b -> c where
  dot_ :: a -> b -> c

infixl 7 `dot_`

class DotMultiplication b a c => DotMultiplication a b c | a b -> c where
  dot :: DotMultiplication a b c => a -> b -> c

infixl 7 `dot`

class CrossMultiplication_ a b c | a b -> c where
  cross_ :: a -> b -> c

infixl 7 `cross_`

class CrossMultiplication b a c => CrossMultiplication a b c | a b -> c where
  cross :: a -> b -> c

infixl 7 `cross`

class DivMod a where
  (%) :: a -> a -> a
  (//) :: a -> a -> Int

infixl 7 //

infixl 7 %

instance DivMod Int where
  {-# INLINE (//) #-}
  (//) = Prelude.div
  {-# INLINE (%) #-}
  (%) = Prelude.mod

class Exponentiation a b c | a b -> c where
  (**) :: a -> b -> c

infixr 8 **

instance Exponentiation Int Int Int where
  {-# INLINE (**) #-}
  (**) = (Prelude.^)

instance Exponentiation Number Number Number where
  {-# INLINE (**) #-}
  (**) = (Prelude.**)

instance Exponentiation Number Int Number where
  {-# INLINE (**) #-}
  (**) = (Prelude.^^)

----- Tolerance -----

type Tolerance units = ?tolerance :: Quantity units

----- Approximate equality -----

class ApproximateEquality a units | a -> units where
  (~=) :: Tolerance units => a -> a -> Bool

infix 4 ~=

{-# INLINE (!=) #-}
(!=) :: (ApproximateEquality a units, Tolerance units) => a -> a -> Bool
(!=) first second = not (first ~= second)

infix 4 !=

instance ApproximateEquality (Quantity units) units where
  x ~= y = x >= y - ?tolerance && x <= y + ?tolerance

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

----- Named -----

newtype (name :: Symbol) # a = Named a

infix 0 #

instance (name1 ~ name2, a1 ~ a2) => IsLabel name1 (a1 -> name2 # a2) where
  fromLabel = Named

----- Composition -----

class Composition f g h | f g -> h where
  (.) :: f -> g -> h

instance b1 ~ b2 => Composition (b2 -> c) (a -> b1) (a -> c) where
  (.) = (Prelude..)

infixr 9 .

----- List -----

type List a = [a]

----- NonEmpty -----

{-# COMPLETE [], NonEmpty #-}

pattern NonEmpty :: NonEmpty a -> List a
pattern NonEmpty nonEmpty <- (Data.List.NonEmpty.nonEmpty -> Just nonEmpty)

----- Result -----

data Result x a where
  Ok :: a -> Result x a
  Error :: Show x => x -> Result x a

deriving instance (Eq x, Eq a) => Eq (Result x a)

deriving instance (Show x, Show a) => Show (Result x a)

instance Functor (Result x) where
  fmap function (Ok value) = Ok (function value)
  fmap _ (Error error) = Error error

instance Applicative (Result x) where
  pure = Ok

  Ok function <*> Ok value = Ok (function value)
  Error error <*> _ = Error error
  _ <*> Error error = Error error

instance Monad (Result x) where
  Ok value >>= function = function value
  Error error >>= _ = Error error

instance MonadFail (Result Text) where
  fail message = Error (Data.Text.pack message)

----- Quantity -----

type Quantity :: Type -> Type
newtype Quantity units = Quantity Prelude.Double deriving (Eq, Ord, Show)

deriving newtype instance Storable (Quantity units)

deriving newtype instance Hashable (Quantity units)

type Number = Quantity Unitless

deriving newtype instance Prelude.Num Number

deriving newtype instance Prelude.Real Number

deriving newtype instance Prelude.Fractional Number

deriving newtype instance Prelude.RealFrac Number

deriving newtype instance Prelude.Floating Number

deriving newtype instance Prelude.RealFloat Number

deriving newtype instance Prelude.Read Number

instance HasUnits (Quantity units) units

instance Units.Coercion (Quantity units1) (Quantity units2) where
  {-# INLINE coerce #-}
  coerce = Data.Coerce.coerce

instance Negation (Quantity units) where
  {-# INLINE negate #-}
  negate (Quantity x) = Quantity (Prelude.negate x)

instance Multiplication Sign (Quantity units) (Quantity units) where
  {-# INLINE (*) #-}
  Sign sign_ * value = sign_ * value

instance Multiplication (Quantity units) Sign (Quantity units) where
  {-# INLINEABLE (*) #-}
  value * Sign sign_ = value * sign_

instance units1 ~ units2 => Addition (Quantity units1) (Quantity units2) (Quantity units1) where
  {-# INLINE (+) #-}
  Quantity x + Quantity y = Quantity (x Prelude.+ y)

instance units1 ~ units2 => Subtraction (Quantity units1) (Quantity units2) (Quantity units1) where
  {-# INLINE (-) #-}
  Quantity x - Quantity y = Quantity (x Prelude.- y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Quantity units2) (Quantity units3)
  where
  {-# INLINE (*) #-}
  Quantity x * Quantity y = Quantity (x Prelude.* y)

instance Multiplication_ (Quantity units1) (Quantity units2) (Quantity (units1 ?*? units2)) where
  {-# INLINE (?*?) #-}
  Quantity x ?*? Quantity y = Quantity (x Prelude.* y)

instance DotMultiplication_ (Quantity units1) (Quantity units2) (Quantity (units1 ?*? units2)) where
  {-# INLINE dot_ #-}
  dot_ = (?*?)

instance
  Units.Product units1 units2 units3 =>
  DotMultiplication (Quantity units1) (Quantity units2) (Quantity units3)
  where
  {-# INLINE dot #-}
  dot = (*)

instance Division_ (Quantity units1) (Quantity units2) (Quantity (units1 ?/? units2)) where
  {-# INLINE (?/?) #-}
  Quantity x ?/? Quantity y = Quantity (x Prelude./ y)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Quantity units1) (Quantity units2) (Quantity units3)
  where
  {-# INLINE (/) #-}
  Quantity x / Quantity y = Quantity (x Prelude./ y)

instance DivMod (Quantity units) where
  x // y = Prelude.floor (x / y)
  x % y = x - y * (fromIntegral (x // y) :: Number)

----- Intersection -----

class Intersects b a units => Intersects a b units | a b -> units where
  intersects :: Tolerance units => a -> b -> Bool

infix 4 `intersects`

----- Sign -----

newtype Sign = Sign_ Number deriving (Eq, Ord, Show)

instance HasUnits Sign Unitless

{-# COMPLETE Sign #-}

{-# INLINEABLE Sign #-}
pattern Sign :: Number -> Sign
pattern Sign x <- Sign_ x

{-# COMPLETE Negative, Positive #-}

{-# INLINEABLE Negative #-}
pattern Negative :: Sign
pattern Negative = Sign_ -1.0

{-# INLINEABLE Positive #-}
pattern Positive :: Sign
pattern Positive = Sign_ 1.0

instance Prelude.Bounded Sign where
  minBound = Negative
  maxBound = Positive

instance Prelude.Enum Sign where
  toEnum 0 = Negative
  toEnum _ = Positive
  fromEnum Negative = 0
  fromEnum Positive = 1

instance System.Random.Stateful.Uniform Sign where
  uniformM = System.Random.Stateful.uniformEnumM

instance Negation Sign where
  {-# INLINE negate #-}
  negate (Sign_ value) = Sign_ (negate value)

instance Multiplication Sign Sign Sign where
  {-# INLINE (*) #-}
  Sign_ value1 * Sign_ value2 = Sign_ (value1 * value2)

{-# INLINE fromInteger #-}
fromInteger :: Prelude.Integer -> Int
fromInteger = Prelude.fromInteger

{-# INLINE fromRational #-}
fromRational :: Prelude.Rational -> Number
fromRational = Prelude.fromRational

{-# INLINE ifThenElse #-}
ifThenElse :: Bool -> a -> a -> a
ifThenElse True first _ = first
ifThenElse False _ second = second

{-# COMPLETE TODO #-}

pattern TODO :: HasCallStack => a
pattern TODO <- (GHC.Stack.withFrozenCallStack todoImpl -> ())
  where
    TODO = GHC.Stack.withFrozenCallStack todoImpl

todoImpl :: a
todoImpl = Prelude.error "Not implemented"
