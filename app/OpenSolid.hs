module OpenSolid (
    String,
    Char,
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
    Concatenation (..),
    (//),
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
import Prelude (
    Bool (..),
    Eq (..),
    IO,
    Ord (..),
    Show (..),
    fail,
    not,
    (&&),
    (>>),
    (>>=),
    (||),
 )
import qualified Prelude

type String = Data.Text.Text

type Char = Prelude.Char

type List a = [a]

type Int = Prelude.Int

newtype Quantity units = Quantity Prelude.Double
    deriving (Eq)

data Unitless = Unitless

type Float = Quantity Unitless

instance Show Float where
    show (Quantity x) = show x

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

instance Multiplication units1 units2 units3 => Multiplication (Quantity units1) (Quantity units2) (Quantity units3) where
    (Quantity x) * (Quantity y) = Quantity (x Prelude.* y)

class Division lhs rhs result | lhs rhs -> result where
    (/) :: lhs -> rhs -> result

instance Division Unitless Unitless Unitless where
    Unitless / Unitless = Unitless

instance Division units1 units2 units3 => Division (Quantity units1) (Quantity units2) (Quantity units3) where
    (Quantity x) / (Quantity y) = Quantity (x Prelude./ y)

class Sqrt squared sqrt | squared -> sqrt where
    sqrt :: squared -> sqrt

instance Sqrt squaredUnits units => Sqrt (Quantity squaredUnits) (Quantity units) where
    sqrt (Quantity x) = Quantity (Prelude.sqrt x)

(//) :: Int -> Int -> Int
(//) = Prelude.quot

class Concatenation a where
    (++) :: a -> a -> a

instance Concatenation String where
    (++) = Data.Text.append

instance Concatenation (List a) where
    (++) = Prelude.mappend

fromInteger :: Prelude.Integer -> Int
fromInteger = Prelude.fromInteger

fromRational :: Prelude.Rational -> Float
fromRational x = Quantity (Prelude.fromRational x)

fromString :: Prelude.String -> String
fromString = Data.Text.pack

float :: Int -> Float
float n = Quantity (Prelude.fromIntegral n)

{- HLINT ignore ifThenElse "Use if" -}
ifThenElse :: Bool -> a -> a -> a
ifThenElse condition whenTrue whenFalse =
    -- We need to use 'case' here instead of if-then-else, since this function
    -- is literally *implementing* if-then-else
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
(>>>) f g x = g (f x)

(<<<) :: (b -> c) -> (a -> b) -> (a -> c)
(<<<) f g x = f (g x)

infixr 0 <|

infixl 0 |>

infixl 6 +, -

infixl 7 *, /

infixl 9 <<<

infixr 9 >>>
