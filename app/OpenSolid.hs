module OpenSolid (
    module Prelude,
    module Units,
    module Result,
    String,
    List,
    Count (..),
    Quantity (..),
    Int,
    Float,
    Negation (..),
    Addition (..),
    Subtraction (..),
    Multiplication (..),
    Division (..),
    DotProduct (..),
    CrossProduct (..),
    Concatenation (..),
    fromInteger,
    fromRational,
    fromString,
    float,
    abs,
    sqrt,
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
) where

import qualified Data.Text
import Result (Result (..))
import Units (Unitless)
import qualified Units
import Prelude (
    Bool (..),
    Char,
    Eq (..),
    IO,
    Maybe (..),
    Ord (..),
    Show (..),
    const,
    fail,
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

newtype Count units = Count Prelude.Int
    deriving (Eq, Ord)

newtype Quantity units = Quantity Prelude.Double
    deriving (Eq, Ord)

type Int = Count Unitless

type Float = Quantity Unitless

instance Show Int where
    show (Count n) =
        show n

instance Show Float where
    show (Quantity x) =
        show x

class Negation a where
    negate :: a -> a

instance Negation (Count units) where
    negate (Count n) =
        Count (Prelude.negate n)

instance Negation (Quantity units) where
    negate (Quantity x) =
        Quantity (Prelude.negate x)

class Addition a where
    (+) :: a -> a -> a

instance Addition (Count units) where
    (Count n) + (Count m) =
        Count (n Prelude.+ m)

instance Addition (Quantity units) where
    (Quantity x) + (Quantity y) =
        Quantity (x Prelude.+ y)

class Subtraction a where
    (-) :: a -> a -> a

instance Subtraction (Count units) where
    (Count n) - (Count m) =
        Count (n Prelude.- m)

instance Subtraction (Quantity units) where
    (Quantity x) - (Quantity y) =
        Quantity (x Prelude.- y)

class Multiplication lhs rhs where
    type Product lhs rhs
    (*) :: lhs -> rhs -> Product lhs rhs

instance Units.Multiplication units1 units2 => Multiplication (Count units1) (Count units2) where
    type Product (Count units1) (Count units2) = Count (Units.Product units1 units2)
    (Count n) * (Count m) =
        Count (n Prelude.* m)

instance Units.Multiplication units1 units2 => Multiplication (Quantity units1) (Quantity units2) where
    type Product (Quantity units1) (Quantity units2) = Quantity (Units.Product units1 units2)
    (Quantity x) * (Quantity y) =
        Quantity (x Prelude.* y)

class Division lhs rhs where
    type Quotient lhs rhs
    (/) :: lhs -> rhs -> Quotient lhs rhs

instance Units.Division units1 units2 => Division (Quantity units1) (Quantity units2) where
    type Quotient (Quantity units1) (Quantity units2) = Quantity (Units.Quotient units1 units2)
    (Quantity x) / (Quantity y) =
        Quantity (x Prelude./ y)

instance Units.Division units1 units2 => Division (Count units1) (Count units2) where
    type Quotient (Count units1) (Count units2) = Count (Units.Quotient units1 units2)
    (Count n) / (Count m) =
        Count (Prelude.quot n m)

class DotProduct lhs rhs where
    type DotProductResult lhs rhs
    (.) :: lhs coordinates -> rhs coordinates -> DotProductResult lhs rhs

class CrossProduct lhs rhs where
    type CrossProductResult lhs rhs :: * -> *
    (><) :: lhs coordinates -> rhs coordinates -> (CrossProductResult lhs rhs) coordinates

class Concatenation a where
    (++) :: a -> a -> a

instance Concatenation String where
    (++) =
        Data.Text.append

instance Concatenation (List a) where
    (++) =
        Prelude.mappend

fromInteger :: Prelude.Integer -> Int
fromInteger n =
    Count (Prelude.fromInteger n)

fromRational :: Prelude.Rational -> Float
fromRational x =
    Quantity (Prelude.fromRational x)

fromString :: Prelude.String -> String
fromString =
    Data.Text.pack

float :: Int -> Float
float (Count n) =
    Quantity (Prelude.fromIntegral n)

abs :: Quantity units -> Quantity units
abs (Quantity x) =
    Quantity (Prelude.abs x)

sqrt :: Units.Sqrt units => Quantity units -> Quantity (Units.SquareRoot units)
sqrt (Quantity x) =
    Quantity (Prelude.sqrt x)

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

infixl 7 *, /

infixl 9 <<<

infixr 9 >>>
