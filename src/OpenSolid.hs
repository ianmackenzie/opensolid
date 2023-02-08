-- Set in package.yaml, but hlint/HLS doesn't seem to notice it there
-- Can remove this if doing so no longer triggers warnings in ifThenElse
-- (e.g. an update to hlint or HLS notices the extension in package.yaml/opensolid.cabal)
{-# LANGUAGE Strict #-}

module OpenSolid
  ( module Prelude
  , module Data.Void
  , module Data.Kind
  , Composition (..)
  , Bind (..)
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
  , (??)
  , (?=)
  , validate
  , Invalid (..)
  , Tolerance
  , ApproximateEquality (..)
  , Named (..)
  , fromLabel
  , IsProduct
  , IsQuotient
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

import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified
import Data.Void (Void)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Prelude
  ( Applicative
  , Bool (..)
  , Char
  , Enum
  , Eq (..)
  , Functor
  , IO
  , Int
  , Maybe (..)
  , Ord (..)
  , Show
  , not
  , otherwise
  , ($)
  , (&&)
  , (||)
  )
import Prelude qualified

class Composition a b c | a b -> c where
  (>>) :: a -> b -> c
  (<<) :: b -> a -> c
  second << first = first >> second

instance Composition (a -> b) (b -> c) (a -> c) where
  f >> g = g Prelude.. f

class Bind m where
  (>>=) :: m a -> (a -> m b) -> m b

instance Bind Maybe where
  (>>=) = (Prelude.>>=)

instance Bind [] where
  (>>=) = (Prelude.>>=)

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

instance Bind (Result x) where
  Ok value >>= function = function value
  Err err >>= _ = Err err

instance Prelude.Monad (Result x) where
  (>>=) = (>>=)

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

class DotProduct a b c | a b -> c where
  (<>) :: a -> b -> c

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

data Invalid = Invalid

{-# INLINE validate #-}
validate :: (a -> Bool) -> a -> Result Invalid a
validate function value = if function value then Ok value else Err Invalid

class Nullable nullable where
  (??) :: forall applicative a. Applicative applicative => nullable a -> applicative a -> applicative a

instance Nullable Maybe where
  Just value ?? ~_ = Prelude.pure value
  Nothing ?? ~fallback = fallback

instance Nullable (Result x) where
  Ok value ?? ~_ = Prelude.pure value
  Err _ ?? ~fallback = fallback

(?=) :: Nullable nullable => nullable a -> a -> a
(?=) nullable ~fallback = runIdentity (nullable ?? Identity fallback)

infixl 0 |>, ??, ?=

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

instance Multiplication Int Int Int where
  (*) = (Prelude.*)

instance Multiplication Sign Sign Sign where
  Positive * sign = sign
  Negative * sign = -sign

class (IsProduct qty2 qty1 qty3, IsQuotient qty3 qty1 qty2, IsQuotient qty3 qty2 qty1) => IsProduct qty1 qty2 qty3 | qty1 qty2 -> qty3

instance {-# INCOHERENT #-} IsProduct Float Float Float

instance {-# INCOHERENT #-} IsProduct Float (Qty units) (Qty units)

instance {-# INCOHERENT #-} IsProduct (Qty units) Float (Qty units)

class (IsProduct qty3 qty2 qty1, IsQuotient qty1 qty3 qty2) => IsQuotient qty1 qty2 qty3 | qty1 qty2 -> qty3

instance {-# INCOHERENT #-} IsQuotient Float Float Float

instance {-# INCOHERENT #-} IsQuotient (Qty units) (Qty units) Float

instance {-# INCOHERENT #-} IsQuotient (Qty units) Float (Qty units)

instance IsProduct (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (Qty units2) (Qty units3) where
  {-# INLINE (*) #-}
  (Qty x) * (Qty y) = Qty (x Prelude.* y)

instance IsQuotient (Qty units1) (Qty units2) (Qty units3) => Division (Qty units1) (Qty units2) (Qty units3) where
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

class IsProduct a a b => Squared a b | a -> b, b -> a

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

instance IsProduct Length Length Area

instance IsProduct Length Area Volume

instance IsProduct Area Length Volume

instance IsProduct Duration Speed Length

instance IsProduct Speed Duration Length

instance IsProduct Duration Acceleration Speed

instance IsProduct Acceleration Duration Speed

instance IsQuotient Area Length Length

instance IsQuotient Volume Area Length

instance IsQuotient Volume Length Area

instance IsQuotient Length Duration Speed

instance IsQuotient Length Speed Duration

instance IsQuotient Speed Duration Acceleration

instance IsQuotient Speed Acceleration Duration

instance Squared Length Area
