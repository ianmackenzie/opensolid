{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda" #-}

module OpenSolid (
    String,
    List,
    Int,
    Quantity (..),
    Unitless (..),
    Float,
    Negation (..),
    Addition (..),
    Subtraction (..),
    Multiplication (..),
    Division (..),
    Sqrt (..),
    (//),
    DotProduct (..),
    CrossProduct (..),
    Eq (..),
    Show (..),
    IO,
    fromInteger,
    fromRational,
    fromString,
    float,
    ifThenElse,
    Bool (..),
    Ord (..),
    (&&),
    (||),
    not,
    (|>),
    (<|),
    (>>=),
    (>>),
    fail,
    (>>>),
    (<<<),
) where

import qualified Data.Text
import Prelude (Bool (..), Eq (..), IO, Ord (..), Show (..), fail, not, (&&), (>>), (>>=), (||))
import qualified Prelude

type String = Data.Text.Text

type List a = [a]

type Int = Prelude.Int

newtype Quantity units = Quantity Prelude.Double
    deriving (Show, Eq)

data Unitless = Unitless

type Float = Quantity Unitless

class Negation a where
    negate :: a -> a

instance Negation Int where
    negate = Prelude.negate

instance Negation (Quantity units) where
    negate (Quantity x) = Quantity (Prelude.negate x)

class Addition lhs rhs result | lhs rhs -> result where
    (+) :: lhs -> rhs -> result

instance Addition Int Int Int where
    (+) = (Prelude.+)

instance Addition (Quantity units) (Quantity units) (Quantity units) where
    (Quantity x) + (Quantity y) = Quantity (x Prelude.+ y)

class Subtraction lhs rhs result | lhs rhs -> result where
    (-) :: lhs -> rhs -> result

instance Subtraction Int Int Int where
    (-) = (Prelude.-)

instance Subtraction (Quantity units) (Quantity units) (Quantity units) where
    (Quantity x) - (Quantity y) = Quantity (x Prelude.- y)

class Multiplication lhs rhs result | lhs rhs -> result where
    (*) :: lhs -> rhs -> result

instance Multiplication Unitless Unitless Unitless where
    Unitless * Unitless = Unitless

instance Multiplication Int Int Int where
    (*) = (Prelude.*)

instance Multiplication lhsUnits rhsUnits resultUnits => Multiplication (Quantity lhsUnits) (Quantity rhsUnits) (Quantity resultUnits) where
    (Quantity x) * (Quantity y) = Quantity (x Prelude.* y)

class Division lhs rhs result | lhs rhs -> result where
    (/) :: lhs -> rhs -> result

instance Division Unitless Unitless Unitless where
    Unitless / Unitless = Unitless

instance Division lhsUnits rhsUnits resultUnits => Division (Quantity lhsUnits) (Quantity rhsUnits) (Quantity resultUnits) where
    (Quantity x) / (Quantity y) = Quantity (x Prelude./ y)

class Sqrt squared sqrt | squared -> sqrt where
    sqrt :: squared -> sqrt

instance Sqrt squaredUnits units => Sqrt (Quantity squaredUnits) (Quantity units) where
    sqrt (Quantity x) = Quantity (Prelude.sqrt x)

class DotProduct lhs rhs result | lhs rhs -> result where
    (.) :: lhs -> rhs -> result

class CrossProduct lhs rhs result | lhs rhs -> result where
    (><) :: lhs -> rhs -> result

(//) :: Int -> Int -> Int
(//) = Prelude.quot

fromInteger :: Prelude.Integer -> Int
fromInteger = Prelude.fromInteger

fromRational :: Prelude.Rational -> Float
fromRational x = Quantity (Prelude.fromRational x)

fromString :: Prelude.String -> String
fromString = Data.Text.pack

float :: Int -> Float
float n = Quantity (Prelude.fromIntegral n)

ifThenElse :: Bool -> a -> a -> a
ifThenElse condition whenTrue whenFalse =
    case condition of
        True -> whenTrue
        False -> whenFalse

identity :: a -> a
identity = Prelude.id

(|>) :: a -> (a -> b) -> b
(|>) value function = function value

(<|) :: (a -> b) -> a -> b
(<|) = identity

(>>>) :: (a -> b) -> (b -> c) -> (a -> c)
(>>>) f g = \x -> g (f x)

(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
(<<<) f g = \x -> f (g x)

infixr 0 <|

infixl 0 |>

infixl 6 +, -

infixl 7 *, /

infixl 9 <<<

infixr 9 >>>
