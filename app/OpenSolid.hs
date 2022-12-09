module OpenSolid (
    module Prelude,
    Nbr (..),
    Qty (..),
    Int,
    Float,
    String,
    List,
    Result (..),
    Negation (..),
    Addition (..),
    Subtraction (..),
    Multiplication (..),
    Division (..),
    Sqrt,
    (//),
    DotProduct (..),
    CrossProduct (..),
    Concatenation (..),
    fromInteger,
    fromRational,
    fromString,
    float,
    ifThenElse,
    identity,
    always,
    error,
    notImplemented,
    subtract,
    (|>),
    (<|),
    (>>),
    (<<),
    (:::),
    Unitless,
    Angle,
    Radians,
    Length,
    Meters,
    Area,
    SquareMeters,
    Volume,
    CubicMeters,
    Duration,
    Seconds,
    Speed,
    MetersPerSecond,
    Acceleration,
    MetersPerSecondSquared,
) where

import Data.Proxy (Proxy (Proxy))
import Data.Text qualified
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Units qualified
import Prelude (
    Bool (..),
    Char,
    Enum,
    Eq (..),
    IO,
    Maybe (..),
    Ord (..),
    Show,
    const,
    flip,
    fmap,
    id,
    not,
    otherwise,
    pure,
    ($),
    (&&),
    (.),
    (||),
 )
import Prelude qualified

type String = Data.Text.Text

type List a = [a]

data Result x a
    = Ok !a
    | Err !x
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

newtype Nbr units = Nbr {unNbr :: Prelude.Int} deriving (Eq, Ord, Enum)

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

class Negation a where
    negate :: a -> a

class Addition p q r | p q -> r where
    (+) :: p a -> q a -> r a

class Subtraction p q r | p q -> r where
    (-) :: p a -> q a -> r a

class Multiplication a b c | a b -> c where
    (*) :: a -> b -> c

class Division a b c | a b -> c where
    (/) :: a -> b -> c

instance Negation (Nbr units) where
    negate (Nbr n) = Nbr (Prelude.negate n)

instance Negation (Qty units) where
    negate (Qty x) = Qty (Prelude.negate x)

instance Addition Nbr Nbr Nbr where
    (Nbr n) + (Nbr m) = Nbr (n Prelude.+ m)

instance Addition Qty Qty Qty where
    (Qty x) + (Qty y) = Qty (x Prelude.+ y)

instance Subtraction Nbr Nbr Nbr where
    (Nbr n) - (Nbr m) = Nbr (n Prelude.- m)

instance Subtraction Qty Qty Qty where
    (Qty x) - (Qty y) = Qty (x Prelude.- y)

(//) :: Int -> Int -> Int
(Nbr n) // (Nbr m) = Nbr (Prelude.quot n m)

class DotProduct p q r | p q -> r where
    (<>) :: p a -> q a -> r

class CrossProduct p q r | p q -> r where
    (><) :: p a -> q a -> r a

class Concatenation a where
    (++) :: a -> a -> a

instance Concatenation String where
    (++) = Data.Text.append

instance Concatenation (List a) where
    (++) = Prelude.mappend

fromInteger :: Prelude.Integer -> Int
fromInteger n = Nbr (Prelude.fromInteger n)

fromRational :: Prelude.Rational -> Float
fromRational x = Qty (Prelude.fromRational x)

fromString :: Prelude.String -> String
fromString = Data.Text.pack

float :: Int -> Float
float (Nbr n) = Qty (Prelude.fromIntegral n)

ifThenElse :: Bool -> a -> a -> a
ifThenElse True ifBranch _ = ifBranch
ifThenElse False _ elseBranch = elseBranch

identity :: a -> a
identity = Prelude.id

always :: a -> b -> a
always = const

error :: String -> a
error message = Prelude.error (Data.Text.unpack message)

notImplemented :: a
notImplemented = error "Not implemented"

subtract :: Subtraction p q r => q a -> p a -> r a
subtract b a = a - b

(|>) :: a -> (a -> b) -> b
(|>) value function = function value

(<|) :: (a -> b) -> a -> b
(<|) = identity

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) f g x = g (f x)

(<<) :: (b -> c) -> (a -> b) -> (a -> c)
(<<) f g x = f (g x)

infixr 0 <|

infixl 0 |>

infixl 6 +, -

infixl 7 *, /, //

infixl 9 <<

infixr 9 >>

type (name :: Symbol) ::: a = a

instance Units.Coercion (Nbr units) Int

instance Units.Coercion (Qty units) Float

instance Multiplication Int Int Int where
    (Nbr n) * (Nbr m) = Nbr (n Prelude.* m)

class Product qty1 qty2 qty3 | qty1 qty2 -> qty3

instance {-# INCOHERENT #-} Product Float Float Float

instance {-# INCOHERENT #-} Product Float (Qty units) (Qty units)

instance {-# INCOHERENT #-} Product (Qty units) Float (Qty units)

class Quotient qty1 qty2 qty3 | qty1 qty2 -> qty3

instance {-# INCOHERENT #-} Quotient Float Float Float

instance {-# INCOHERENT #-} Quotient (Qty units) (Qty units) Float

instance {-# INCOHERENT #-} Quotient (Qty units) Float (Qty units)

instance Product (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (Qty units2) (Qty units3) where
    (Qty x) * (Qty y) = Qty (x Prelude.* y)

instance Quotient (Qty units1) (Qty units2) (Qty units3) => Division (Qty units1) (Qty units2) (Qty units3) where
    (Qty x) / (Qty y) = Qty (x Prelude./ y)

instance Multiplication Int (Qty units) (Qty units) where
    n * x = float n * x

instance Multiplication (Qty units) Int (Qty units) where
    x * n = x * float n

instance Division Int Int Float where
    n / m = float n / float m

instance Division (Qty units) Int (Qty units) where
    x / n = x / float n

instance Division Float (Qty units1) (Qty units2) => Division Int (Qty units1) (Qty units2) where
    n / x = float n / x

class Sqrt a b | a -> b

instance Sqrt Float Float

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

instance Quotient Speed Duration Acceleration

instance Product Length Angle Length

instance Product Angle Length Length

instance Sqrt Area Length
