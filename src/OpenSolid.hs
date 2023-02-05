-- Set in package.yaml, but hlint/HLS doesn't seem to notice it there
-- Can remove this if doing so no longer triggers warnings in ifThenElse
-- (e.g. an update to hlint or HLS notices the extension in package.yaml/opensolid.cabal)
{-# LANGUAGE Strict #-}

module OpenSolid
  ( module Prelude
  , module Control.Category
  , module Data.Void
  , module Data.Kind
  , Qty (..)
  , Float
  , Sign (..)
  , Indeterminate (..)
  , Text
  , List
  , Result (..)
  , Negation (..)
  , Addition (..)
  , Subtraction (..)
  , Multiplication (..)
  , Division (..)
  , Squared
  , (//)
  , DotProduct (..)
  , CrossProduct (..)
  , (++)
  , fromInteger
  , fromRational
  , fromString
  , float
  , ifThenElse
  , identity
  , always
  , infallible
  , internalError
  , notImplemented
  , subtract
  , (|>)
  , (<|)
  , (??)
  , (?=)
  , (?!)
  , Tolerance
  , ApproximateEquality (..)
  , Named (..)
  , fromLabel
  , Unitless
  , Angle
  , Radians
  , Length
  , Meters
  , Area
  , SquareMeters
  , Volume
  , CubicMeters
  , Duration
  , Seconds
  , Speed
  , MetersPerSecond
  , Acceleration
  , MetersPerSecondSquared
  )
where

import Control.Category ((<<<), (>>>))
import Data.Coerce (coerce)
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified
import Data.Void (Void)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Generic qualified
import Units qualified
import Prelude
  ( Applicative (..)
  , Bool (..)
  , Char
  , Enum
  , Eq (..)
  , Functor (..)
  , IO
  , Int
  , Maybe (..)
  , Monad (..)
  , Ord (..)
  , Show
  , not
  , otherwise
  , (&&)
  , (||)
  )
import Prelude qualified

type Text = Data.Text.Text

type List a = [a]

data Result x a
  = Ok a
  | Err x
  deriving (Show, Eq)

instance Prelude.Functor (Result x) where
  fmap function (Ok value) = Ok (function value)
  fmap _ (Err err) = Err err

instance Prelude.Applicative (Result x) where
  pure = Ok

  Ok function <*> Ok value = Ok (function value)
  Err err <*> _ = Err err
  Ok _ <*> Err err = Err err

instance Prelude.Monad (Result x) where
  Ok value >>= function = function value
  Err err >>= _ = Err err

type role Qty nominal

type Qty :: Type -> Type
newtype Qty units = Qty Prelude.Double deriving (Eq, Ord, Show)

data Unitless

type Float = Qty Unitless

deriving instance Prelude.Num Float

deriving instance Prelude.Real Float

deriving instance Prelude.Fractional Float

deriving instance Prelude.RealFrac Float

deriving instance Prelude.Floating Float

deriving instance Prelude.RealFloat Float

data Sign = Positive | Negative deriving (Eq, Show)

data Indeterminate = Indeterminate

class Negation a where
  negate :: a -> a

class Addition a b c | a b -> c where
  (+) :: a -> b -> c

class Subtraction a b c | a b -> c where
  (-) :: a -> b -> c

class Multiplication b a c => Multiplication a b c | a b -> c where
  (*) :: a -> b -> c

class Division a b c | a b -> c where
  (/) :: a -> b -> c

instance Negation (Qty units) where
  {-# INLINE negate #-}
  negate (Qty x) = Qty (Prelude.negate x)

instance Negation Sign where
  negate Positive = Negative
  negate Negative = Positive

instance Generic.Zero (Qty units) where
  zero = coerce 0.0

instance Addition Int Int Int where
  (+) = (Prelude.+)

instance units ~ units' => Addition (Qty units) (Qty units') (Qty units) where
  {-# INLINE (+) #-}
  Qty x + Qty y = Qty (x Prelude.+ y)

instance Subtraction Int Int Int where
  (-) = (Prelude.-)

instance units ~ units' => Subtraction (Qty units) (Qty units') (Qty units) where
  {-# INLINE (-) #-}
  Qty x - Qty y = Qty (x Prelude.- y)

(//) :: Int -> Int -> Int
(//) = Prelude.quot

class DotProduct p q r | p q -> r where
  (<>) :: p a -> q a -> r

class CrossProduct a b c | a b -> c where
  (><) :: a -> b -> c

(++) :: Prelude.Monoid a => a -> a -> a
(++) = Prelude.mappend

{-# INLINE fromInteger #-}
fromInteger :: Prelude.Integer -> Int
fromInteger = Prelude.fromInteger

{-# INLINE fromRational #-}
fromRational :: Prelude.Rational -> Float
fromRational x = Qty (Prelude.fromRational x)

fromString :: Prelude.String -> Text
fromString = Data.Text.pack

{-# INLINE float #-}
float :: Int -> Float
float = Prelude.fromIntegral

ifThenElse :: Bool -> a -> a -> a
ifThenElse True ifBranch ~_ = ifBranch
ifThenElse False ~_ elseBranch = elseBranch

identity :: a -> a
identity = Prelude.id

always :: a -> b -> a
always = Prelude.const

infallible :: Result Void a -> a
infallible (Ok value) = value

internalError :: Text -> a
internalError message = Prelude.error (Data.Text.unpack message)

notImplemented :: a
notImplemented = internalError "Not implemented"

subtract :: Subtraction a b c => b -> a -> c
subtract b a = a - b

{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
(|>) value function = function value

{-# INLINE (<|) #-}
(<|) :: (a -> b) -> a -> b
(<|) = identity

class Nullable nullable where
  (??) :: forall applicative a. Applicative applicative => nullable a -> applicative a -> applicative a

instance Nullable Maybe where
  Just value ?? ~_ = pure value
  Nothing ?? ~fallback = fallback

instance Nullable (Result x) where
  Ok value ?? ~_ = pure value
  Err _ ?? ~fallback = fallback

(?=) :: Nullable nullable => nullable a -> a -> a
(?=) nullable ~fallback = runIdentity (nullable ?? Identity fallback)

(?!) :: Nullable nullable => nullable a -> x -> Result x a
(?!) nullable ~err = nullable ?? Err err

infixr 0 <|

infixl 0 |>

infixr 2 ??, ?=, ?!

infixl 6 +, -

infixl 7 *, /, //

type Tolerance units = ?tolerance :: Qty units

class ApproximateEquality a units | a -> units where
  (~=) :: Tolerance units => a -> a -> Bool

infix 4 ~=

instance ApproximateEquality (Qty units) units where
  x ~= y = let (Qty delta) = x - y in Qty (Prelude.abs delta) <= ?tolerance

newtype Named (name :: Symbol) value = Named value deriving (Eq)

instance (KnownSymbol name, Show value) => Show (Named name value) where
  showsPrec precedence (Named value) =
    let string = '#' : symbolVal (Proxy :: Proxy name) ++ (' ' : Prelude.showsPrec 10 value [])
     in Prelude.showParen (precedence >= 10) (Prelude.showString string)

class Nameable (name :: Symbol) value where
  fromLabel :: value -> Named name value
  fromLabel = Named

instance Nameable (name :: Symbol) value

instance Units.Coercion (Qty units) Float

instance Multiplication Int Int Int where
  (*) = (Prelude.*)

instance Multiplication Sign Sign Sign where
  Positive * sign = sign
  Negative * sign = -sign

class (Product qty2 qty1 qty3, Quotient qty3 qty1 qty2, Quotient qty3 qty2 qty1) => Product qty1 qty2 qty3 | qty1 qty2 -> qty3

instance {-# INCOHERENT #-} Product Float Float Float

instance {-# INCOHERENT #-} Product Float (Qty units) (Qty units)

instance {-# INCOHERENT #-} Product (Qty units) Float (Qty units)

class (Product qty3 qty2 qty1, Quotient qty1 qty3 qty2) => Quotient qty1 qty2 qty3 | qty1 qty2 -> qty3

instance {-# INCOHERENT #-} Quotient Float Float Float

instance {-# INCOHERENT #-} Quotient (Qty units) (Qty units) Float

instance {-# INCOHERENT #-} Quotient (Qty units) Float (Qty units)

instance Product (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (Qty units2) (Qty units3) where
  {-# INLINE (*) #-}
  (Qty x) * (Qty y) = Qty (x Prelude.* y)

instance Quotient (Qty units1) (Qty units2) (Qty units3) => Division (Qty units1) (Qty units2) (Qty units3) where
  {-# INLINE (/) #-}
  (Qty x) / (Qty y) = Qty (x Prelude./ y)

instance Multiplication Int (Qty units) (Qty units) where
  {-# INLINE (*) #-}
  n * x = float n * x

instance Multiplication (Qty units) Int (Qty units) where
  {-# INLINE (*) #-}
  x * n = x * float n

instance Division Int Int Float where
  {-# INLINE (/) #-}
  n / m = float n / float m

instance Division (Qty units) Int (Qty units) where
  {-# INLINE (/) #-}
  x / n = x / float n

instance Division Float (Qty units1) (Qty units2) => Division Int (Qty units1) (Qty units2) where
  {-# INLINE (/) #-}
  n / x = float n / x

class Product a a b => Squared a b | a -> b, b -> a

instance Squared Float Float

data Radians

type Angle = Qty Radians

data Meters

type Length = Qty Meters

data Seconds

type Duration = Qty Seconds

data MetersPerSecond

type Speed = Qty MetersPerSecond

data MetersPerSecondSquared

type Acceleration = Qty MetersPerSecondSquared

data SquareMeters

type Area = Qty SquareMeters

data CubicMeters

type Volume = Qty CubicMeters

instance Product Length Length Area

instance Product Length Area Volume

instance Product Area Length Volume

instance Product Duration Speed Length

instance Product Speed Duration Length

instance Product Duration Acceleration Speed

instance Product Acceleration Duration Speed

instance Quotient Area Length Length

instance Quotient Volume Area Length

instance Quotient Volume Length Area

instance Quotient Length Duration Speed

instance Quotient Length Speed Duration

instance Quotient Speed Duration Acceleration

instance Quotient Speed Acceleration Duration

instance Squared Length Area
