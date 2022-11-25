module OpenSolid (
    module Prelude,
    module Result,
    Nbr (..),
    Qty (..),
    Int,
    Float,
    String,
    List,
    Sqrt,
    Negation (..),
    Addition (..),
    Subtraction (..),
    Multiplication (..),
    Division (..),
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
    zero,
    infinity,
    isNaN,
    abs,
    clamp,
    floor,
    ceiling,
    sqrt,
    sin,
    cos,
    tan,
    asin,
    acos,
    atan,
    atan2,
    pi,
    (|>),
    (<|),
    (>>),
    (<<),
    Arg (..),
    fromLabel,
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

import Data.Coerce (coerce)
import Data.Proxy (Proxy (Proxy))
import qualified Data.Text
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Result (Result (..))
import qualified Units
import Prelude (
    Bool (..),
    Char,
    Enum,
    Eq (..),
    IO,
    Maybe (..),
    Ord (..),
    Show (..),
    const,
    flip,
    id,
    not,
    otherwise,
    (&&),
    (||),
 )
import qualified Prelude

type String = Data.Text.Text

type List a = [a]

newtype Nbr units = Nbr {unNbr :: Prelude.Int} deriving (Eq, Ord, Enum)

newtype Qty units = Qty {unQty :: Prelude.Double} deriving (Eq, Ord)

instance Show Int where
    show (Nbr n) =
        show n

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
    negate (Nbr n) =
        Nbr (Prelude.negate n)

instance Negation (Qty units) where
    negate (Qty x) =
        Qty (Prelude.negate x)

instance Addition Nbr Nbr Nbr where
    (Nbr n) + (Nbr m) =
        Nbr (n Prelude.+ m)

instance Addition Qty Qty Qty where
    (Qty x) + (Qty y) =
        Qty (x Prelude.+ y)

instance Subtraction Nbr Nbr Nbr where
    (Nbr n) - (Nbr m) =
        Nbr (n Prelude.- m)

instance Subtraction Qty Qty Qty where
    (Qty x) - (Qty y) =
        Qty (x Prelude.- y)

class Sqrt a b | a -> b

instance Sqrt Float Float

(//) :: Int -> Int -> Int
(Nbr n) // (Nbr m) =
    Nbr (Prelude.quot n m)

class DotProduct p q r | p q -> r where
    (<>) :: p a -> q a -> r

class CrossProduct p q r | p q -> r where
    (><) :: p a -> q a -> r a

class Concatenation a where
    (++) :: a -> a -> a

instance Concatenation String where (++) = Data.Text.append

instance Concatenation (List a) where (++) = Prelude.mappend

fromInteger :: Prelude.Integer -> Int
fromInteger n =
    Nbr (Prelude.fromInteger n)

fromRational :: Prelude.Rational -> Float
fromRational x =
    Qty (Prelude.fromRational x)

fromString :: Prelude.String -> String
fromString =
    Data.Text.pack

float :: Int -> Float
float (Nbr n) =
    Qty (Prelude.fromIntegral n)

{- HLINT ignore ifThenElse "Use if" -}
ifThenElse :: Bool -> a -> a -> a
ifThenElse condition whenTrue whenFalse =
    -- We need to use 'case' here instead of if-then-else, since this function
    -- is literally *implementing* if-then-else
    case condition of
        True -> whenTrue
        False -> whenFalse

identity :: a -> a
identity =
    Prelude.id

always :: a -> b -> a
always =
    Prelude.const

error :: String -> a
error message =
    Prelude.error (Data.Text.unpack message)

notImplemented :: a
notImplemented =
    error "Not implemented"

subtract :: Subtraction p q r => q a -> p a -> r a
subtract b a =
    a - b

zero :: Qty units
zero =
    coerce 0.0

infinity :: Qty units
infinity =
    coerce (1.0 / 0.0)

isNaN :: Qty units -> Bool
isNaN (Qty x) =
    Prelude.isNaN x

abs :: Qty units -> Qty units
abs (Qty x) =
    Qty (Prelude.abs x)

clamp :: Qty units -> Qty units -> Qty units -> Qty units
clamp a b value
    | value < low = low
    | value > high = high
    | otherwise = value
  where
    low = min a b
    high = max a b

floor :: Float -> Int
floor (Qty x) =
    Nbr (Prelude.floor x)

ceiling :: Float -> Int
ceiling (Qty x) =
    Nbr (Prelude.ceiling x)

sqrt :: Sqrt (Qty units1) (Qty units2) => Qty units1 -> Qty units2
sqrt (Qty x) =
    Qty (Prelude.sqrt x)

sin :: Angle -> Float
sin (Qty x) =
    Qty (Prelude.sin x)

cos :: Angle -> Float
cos (Qty x) =
    Qty (Prelude.cos x)

tan :: Angle -> Float
tan (Qty x) =
    Qty (Prelude.tan x)

asin :: Float -> Angle
asin (Qty x) =
    Qty (Prelude.asin x)

acos :: Float -> Angle
acos (Qty x) =
    Qty (Prelude.acos x)

atan :: Float -> Angle
atan (Qty x) =
    Qty (Prelude.atan x)

atan2 :: Qty units -> Qty units -> Angle
atan2 (Qty y) (Qty x) =
    Qty (Prelude.atan2 y x)

pi :: Float
pi =
    Prelude.pi

(|>) :: a -> (a -> b) -> b
(|>) value function =
    function value

(<|) :: (a -> b) -> a -> b
(<|) =
    identity

(>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>) f g x =
    g (f x)

(<<) :: (b -> c) -> (a -> b) -> (a -> c)
(<<) f g x =
    f (g x)

infixr 0 <|

infixl 0 |>

infixl 6 +, -

infixl 7 *, /, //

infixl 9 <<

infixr 9 >>

newtype Arg (label :: Symbol) value = Arg value deriving (Eq, Show)

class NamedArgument (label :: Symbol) value where
    fromLabel :: value -> Arg label value
    fromLabel = Arg

instance NamedArgument (label :: Symbol) value

instance Units.Coercion (Nbr units) Int

instance Units.Coercion (Qty units) Float

instance Multiplication Int Int Int where
    (Nbr n) * (Nbr m) =
        Nbr (n Prelude.* m)

class Product qty1 qty2 qty3 | qty1 qty2 -> qty3

instance {-# INCOHERENT #-} Product Float Float Float

instance {-# INCOHERENT #-} Product Float (Qty units) (Qty units)

instance {-# INCOHERENT #-} Product (Qty units) Float (Qty units)

class Quotient qty1 qty2 qty3 | qty1 qty2 -> qty3

instance {-# INCOHERENT #-} Quotient Float Float Float

instance {-# INCOHERENT #-} Quotient (Qty units) (Qty units) Float

instance {-# INCOHERENT #-} Quotient (Qty units) Float (Qty units)

instance Product (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (Qty units2) (Qty units3) where
    (Qty x) * (Qty y) =
        Qty (x Prelude.* y)

instance Quotient (Qty units1) (Qty units2) (Qty units3) => Division (Qty units1) (Qty units2) (Qty units3) where
    (Qty x) / (Qty y) =
        Qty (x Prelude./ y)

instance Multiplication Int (Qty units) (Qty units) where
    n * x =
        float n * x

instance Multiplication (Qty units) Int (Qty units) where
    x * n =
        x * float n

instance Division Int Int Float where
    n / m =
        float n / float m

instance Division (Qty units) Int (Qty units) where
    x / n =
        x / float n

instance Division Float (Qty units1) (Qty units2) => Division Int (Qty units1) (Qty units2) where
    n / x =
        float n / x

instance KnownSymbol symbol => Show (Qty (Units symbol)) where
    showsPrec precedence (Qty x) =
        case symbolVal (Proxy :: Proxy symbol) of
            [] ->
                showsPrec precedence x
            suffix ->
                let string = Prelude.mappend (show x) (' ' : suffix)
                 in Prelude.showParen (Nbr precedence > 10) (Prelude.showString string)

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
