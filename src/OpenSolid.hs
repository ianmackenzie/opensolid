-- Set in package.yaml, but hlint/HLS doesn't seem to notice it there
-- Can remove this if doing so no longer triggers warnings in ifThenElse
-- (e.g. an update to hlint or HLS notices the extension in package.yaml/opensolid.cabal)
{-# LANGUAGE Strict #-}

module OpenSolid
  ( module Prelude
  , module Control.Category
  , Nbr (..)
  , Qty (..)
  , Int
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
  , internalError
  , notImplemented
  , subtract
  , (|>)
  , (<|)
  , ToleranceIn
  , Tolerance
  , ApproximateEquality (..)
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
import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified
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
  , Maybe (..)
  , Monad (..)
  , Ord (..)
  , Show
  , const
  , flip
  , fmap
  , id
  , not
  , otherwise
  , pure
  , ($)
  , (&&)
  , (.)
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

type Nbr :: Type -> Type
newtype Nbr units = Nbr {unNbr :: Prelude.Int} deriving (Eq, Ord, Enum)

type Qty :: Type -> Type
newtype Qty units = Qty {unQty :: Prelude.Double} deriving (Eq, Ord)

instance Show Int where
  show (Nbr n) = Prelude.show n

data Units (symbol :: Symbol)

type Unitless = Units ""

type Int = Nbr Unitless

deriving instance Prelude.Num Int

deriving instance Prelude.Real Int

deriving instance Prelude.Integral Int

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

class Addition p q r | p q -> r where
  (+) :: p a -> q a -> r a

class Subtraction p q r | p q -> r where
  (-) :: p a -> q a -> r a

class Multiplication b a c => Multiplication a b c | a b -> c where
  (*) :: a -> b -> c

class Division a b c | a b -> c where
  (/) :: a -> b -> c

instance Negation (Nbr units) where
  {-# INLINE negate #-}
  negate (Nbr n) = Nbr (Prelude.negate n)

instance Negation (Qty units) where
  {-# INLINE negate #-}
  negate (Qty x) = Qty (Prelude.negate x)

instance Negation Sign where
  negate Positive = Negative
  negate Negative = Positive

instance Generic.Zero Nbr where
  zero = coerce 0

instance Generic.Zero Qty where
  zero = coerce 0.0

instance Addition Nbr Nbr Nbr where
  {-# INLINE (+) #-}
  (Nbr n) + (Nbr m) = Nbr (n Prelude.+ m)

instance Addition Qty Qty Qty where
  {-# INLINE (+) #-}
  (Qty x) + (Qty y) = Qty (x Prelude.+ y)

instance Subtraction Nbr Nbr Nbr where
  {-# INLINE (-) #-}
  (Nbr n) - (Nbr m) = Nbr (n Prelude.- m)

instance Subtraction Qty Qty Qty where
  {-# INLINE (-) #-}
  (Qty x) - (Qty y) = Qty (x Prelude.- y)

{-# INLINE (//) #-}
(//) :: Int -> Int -> Int
(Nbr n) // (Nbr m) = Nbr (Prelude.quot n m)

class DotProduct p q r | p q -> r where
  (<>) :: p a -> q a -> r

class CrossProduct a b c | a b -> c where
  (><) :: a -> b -> c

(++) :: Prelude.Monoid a => a -> a -> a
(++) = Prelude.mappend

{-# INLINE fromInteger #-}
fromInteger :: Prelude.Integer -> Int
fromInteger n = Nbr (Prelude.fromInteger n)

{-# INLINE fromRational #-}
fromRational :: Prelude.Rational -> Float
fromRational x = Qty (Prelude.fromRational x)

fromString :: Prelude.String -> Text
fromString = Data.Text.pack

{-# INLINE float #-}
float :: Int -> Float
float (Nbr n) = Qty (Prelude.fromIntegral n)

ifThenElse :: Bool -> a -> a -> a
ifThenElse True ifBranch ~_ = ifBranch
ifThenElse False ~_ elseBranch = elseBranch

identity :: a -> a
identity = Prelude.id

always :: a -> b -> a
always = const

internalError :: Text -> a
internalError message = Prelude.error (Data.Text.unpack message)

notImplemented :: a
notImplemented = internalError "Not implemented"

subtract :: Subtraction p q r => q a -> p a -> r a
subtract b a = a - b

{-# INLINE (|>) #-}
(|>) :: a -> (a -> b) -> b
(|>) value function = function value

{-# INLINE (<|) #-}
(<|) :: (a -> b) -> a -> b
(<|) = identity

infixr 0 <|

infixl 0 |>

infixl 6 +, -

infixl 7 *, /, //

type ToleranceIn units = ?tolerance :: Qty units

type Tolerance = ToleranceIn Meters

class ApproximateEquality a units | a -> units where
  (~=) :: ToleranceIn units => a -> a -> Bool

infix 4 ~=

instance ApproximateEquality (Qty units) units where
  x ~= y = let (Qty delta) = x - y in Qty (Prelude.abs delta) <= ?tolerance

instance Units.Coercion (Nbr units) Int

instance Units.Coercion (Qty units) Float

instance Multiplication Int Int Int where
  {-# INLINE (*) #-}
  (Nbr n) * (Nbr m) = Nbr (n Prelude.* m)

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

instance {-# OVERLAPS #-} Show Float where
  show (Qty x) = Prelude.show x

instance KnownSymbol symbol => Show (Qty (Units symbol)) where
  showsPrec precedence (Qty x) =
    let string = Prelude.shows x (' ' : symbolVal (Proxy :: Proxy symbol))
     in Prelude.showParen (Nbr precedence >= 10) (Prelude.showString string)

type Radians = Units "rad"

type Angle = Qty Radians

type Meters = Units "m"

type Length = Qty Meters

type Seconds = Units "s"

type Duration = Qty Seconds

type MetersPerSecond = Units "m/s"

type Speed = Qty MetersPerSecond

type MetersPerSecondSquared = Units "m/s^2"

type Acceleration = Qty MetersPerSecondSquared

type SquareMeters = Units "m^2"

type Area = Qty SquareMeters

type CubicMeters = Units "m^3"

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
