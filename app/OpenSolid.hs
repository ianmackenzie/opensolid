module OpenSolid (
    module Prelude,
    module Result,
    Float,
    String,
    List,
    Scalar,
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
    (|>),
    (<|),
    (>>>),
    (<<<),
    Quantity (..),
    Length,
    Area,
    Volume,
) where

import Data.Coerce (Coercible)
import qualified Data.Text
import Result (Result (..))
import qualified Units
import Prelude (
    Bool (..),
    Char,
    Eq (..),
    IO,
    Int,
    Maybe (..),
    Ord (..),
    Show (..),
    const,
    fail,
    flip,
    id,
    not,
    otherwise,
    (&&),
    (>>),
    (>>=),
    (||),
 )
import qualified Prelude

type String = Data.Text.Text

type List a = [a]

type Float = Prelude.Double

class
    ( Eq scalar
    , Ord scalar
    , Show scalar
    , Coercible scalar Float
    , Coercible Float scalar
    , Units.Coercion scalar Float
    , Negation scalar
    , Addition scalar
    , Subtraction scalar
    , Multiplication scalar Float scalar
    , Multiplication Float scalar scalar
    , Division scalar Float scalar
    , Division scalar scalar Float
    ) =>
    Scalar scalar

instance Scalar Float

class (Scalar scalar, Scalar sqrtScalar) => Sqrt scalar sqrtScalar | scalar -> sqrtScalar

instance Sqrt Float Float

class Negation a where
    negate :: a -> a

instance Negation Int where negate = Prelude.negate

instance Negation Float where negate = Prelude.negate

class Addition a where
    (+) :: a -> a -> a

instance Addition Int where n + m = n Prelude.+ m

instance Addition Float where x + y = x Prelude.+ y

class Subtraction a where
    (-) :: a -> a -> a

instance Subtraction Int where n - m = n Prelude.- m

instance Subtraction Float where x - y = x Prelude.- y

class Multiplication lhs rhs result | lhs rhs -> result where
    (*) :: lhs -> rhs -> result

instance Multiplication Int Int Int where n * m = n Prelude.* m

instance Multiplication Float Float Float where x * y = x Prelude.* y

class Division lhs rhs result | lhs rhs -> result where
    (/) :: lhs -> rhs -> result

instance Division Float Float Float where x / y = x Prelude./ y

(//) :: Int -> Int -> Int
(//) =
    Prelude.quot

class DotProduct lhs rhs result | lhs rhs -> result where
    (<>) :: lhs coordinates -> rhs coordinates -> result

class CrossProduct lhs rhs result | lhs rhs -> result where
    (><) :: lhs coordinates -> rhs coordinates -> result coordinates

class Concatenation a where
    (++) :: a -> a -> a

instance Concatenation String where (++) = Data.Text.append

instance Concatenation (List a) where (++) = Prelude.mappend

fromInteger :: Prelude.Integer -> Int
fromInteger =
    Prelude.fromInteger

fromRational :: Prelude.Rational -> Float
fromRational =
    Prelude.fromRational

fromString :: Prelude.String -> String
fromString =
    Data.Text.pack

float :: Int -> Float
float =
    Prelude.fromIntegral

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

subtract :: Subtraction a => a -> a -> a
subtract b a =
    a - b

(|>) :: a -> (a -> b) -> b
(|>) value function =
    function value

(<|) :: (a -> b) -> a -> b
(<|) =
    identity

(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>>) f g x =
    g (f x)

(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
(<<<) f g x =
    f (g x)

infixr 0 <|

infixl 0 |>

infixl 6 +, -

infixl 7 *, /, //

infixl 9 <<<

infixr 9 >>>

newtype Quantity units = Quantity Float deriving (Eq, Ord, Show, Negation, Addition, Subtraction)

instance Units.Coercion (Quantity units) Float

instance Multiplication Float (Quantity units) (Quantity units) where
    x * (Quantity y) =
        Quantity (x * y)

instance Multiplication (Quantity units) Float (Quantity units) where
    (Quantity x) * y =
        Quantity (x * y)

instance Division (Quantity units) Float (Quantity units) where
    (Quantity x) / y =
        Quantity (x / y)

instance Division (Quantity units) (Quantity units) Float where
    (Quantity x) / (Quantity y) =
        x / y

instance Scalar (Quantity units)

quantityMultiplication :: Quantity a -> Quantity b -> Quantity c
quantityMultiplication (Quantity x) (Quantity y) =
    Quantity (x * y)

quantityDivision :: Quantity a -> Quantity b -> Quantity c
quantityDivision (Quantity x) (Quantity y) =
    Quantity (x / y)

data Meters

type Length = Quantity Meters

data Seconds

type Duration = Quantity Seconds

data MetersPerSecond

type Speed = Quantity MetersPerSecond

data MetersPerSecondSquared

type Acceleration = Quantity MetersPerSecondSquared

data SquareMeters

type Area = Quantity SquareMeters

data CubicMeters

type Volume = Quantity CubicMeters

instance Multiplication Length Length Area where (*) = quantityMultiplication

instance Multiplication Length Area Volume where (*) = quantityMultiplication

instance Multiplication Area Length Volume where (*) = quantityMultiplication

instance Multiplication Duration Speed Length where (*) = quantityMultiplication

instance Multiplication Speed Duration Length where (*) = quantityMultiplication

instance Multiplication Duration Acceleration Speed where (*) = quantityMultiplication

instance Multiplication Acceleration Duration Speed where (*) = quantityMultiplication

instance Division Area Length Length where (/) = quantityDivision

instance Division Volume Area Length where (/) = quantityDivision

instance Division Volume Length Area where (/) = quantityDivision

instance Division Length Duration Speed where (/) = quantityDivision

instance Division Speed Duration Acceleration where (/) = quantityDivision

instance Sqrt Area Length
