module OpenSolid (
    String,
    Char,
    List,
    BooleanLogic (..),
    Count (..),
    Quantity (..),
    Unitless (..),
    Int,
    Float,
    Negation (..),
    Addition (..),
    Subtraction (..),
    Multiplication (..),
    Division (..),
    DotProduct (..),
    CrossProduct (..),
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
    identity,
    otherwise,
    Bool (..),
    Comparison (..),
    min,
    max,
    (|>),
    (<|),
    (>>=),
    (>>),
    fail,
    (>>>),
    (<<<),
) where

import qualified Data.Text
import Units (Unitless)
import qualified Units
import Verdict
import Prelude (
    Bool (..),
    Eq (..),
    IO,
    Ord (max, min),
    Show (..),
    fail,
    otherwise,
    (>>),
    (>>=),
 )
import qualified Prelude

type String = Data.Text.Text

type Char = Prelude.Char

type List a = [a]

class BooleanLogic b where
    not :: b -> b
    (&&) :: b -> b -> b
    (||) :: b -> b -> b

instance BooleanLogic Bool where
    not = Prelude.not
    (&&) = (Prelude.&&)
    (||) = (Prelude.||)

instance BooleanLogic Verdict where
    not verdict =
        case verdict of
            Definitely True -> Definitely False
            Definitely False -> Definitely True
            Indeterminate -> Indeterminate

    v1 && v2 =
        case (v1, v2) of
            (Definitely True, _) -> v2
            (_, Definitely True) -> v1
            (Definitely False, _) -> Definitely False
            (_, Definitely False) -> Definitely False
            (Indeterminate, Indeterminate) -> Indeterminate

    v1 || v2 =
        case (v1, v2) of
            (Definitely True, _) -> Definitely True
            (_, Definitely True) -> Definitely True
            (Definitely False, _) -> v2
            (_, Definitely False) -> v1
            (Indeterminate, Indeterminate) -> Indeterminate

class Comparison lhs rhs where
    type ComparisonResult lhs rhs
    (<) :: lhs units -> rhs units -> ComparisonResult lhs rhs
    (<=) :: lhs units -> rhs units -> ComparisonResult lhs rhs
    (>=) :: lhs units -> rhs units -> ComparisonResult lhs rhs
    (>) :: lhs units -> rhs units -> ComparisonResult lhs rhs

newtype Count units = Count Prelude.Int
    deriving (Eq, Prelude.Ord)

newtype Quantity units = Quantity Prelude.Double
    deriving (Eq, Prelude.Ord)

type Int = Count Unitless

type Float = Quantity Unitless

instance Show Int where
    show (Count n) =
        show n

instance Show Float where
    show (Quantity x) =
        show x

instance Comparison Count Count where
    type ComparisonResult Count Count = Bool

    (Count n) < (Count m) =
        n Prelude.< m

    (Count n) <= (Count m) =
        n Prelude.<= m

    (Count n) >= (Count m) =
        n Prelude.>= m

    (Count n) > (Count m) =
        n Prelude.> m

instance Comparison Quantity Quantity where
    type ComparisonResult Quantity Quantity = Bool

    (Quantity x) < (Quantity y) =
        x Prelude.< y

    (Quantity x) <= (Quantity y) =
        x Prelude.<= y

    (Quantity x) >= (Quantity y) =
        x Prelude.>= y

    (Quantity x) > (Quantity y) =
        x Prelude.> y

class Negation a where
    negate :: a -> a

instance Negation (Count units) where
    negate (Count n) =
        Count (Prelude.negate n)

instance Negation (Quantity units) where
    negate (Quantity x) =
        Quantity (Prelude.negate x)

class Addition lhs rhs where
    type Sum lhs rhs :: * -> *
    (+) :: lhs a -> rhs a -> (Sum lhs rhs) a

instance Addition Count Count where
    type Sum Count Count = Count
    (Count n) + (Count m) =
        Count (n Prelude.+ m)

instance Addition Quantity Quantity where
    type Sum Quantity Quantity = Quantity
    (Quantity x) + (Quantity y) =
        Quantity (x Prelude.+ y)

class Subtraction lhs rhs where
    type Difference lhs rhs :: * -> *
    (-) :: lhs a -> rhs a -> (Difference lhs rhs) a

instance Subtraction Count Count where
    type Difference Count Count = Count
    (Count n) - (Count m) =
        Count (n Prelude.- m)

instance Subtraction Quantity Quantity where
    type Difference Quantity Quantity = Quantity
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

class DotProduct lhs rhs where
    type DotProductResult lhs rhs
    (.) :: lhs coordinates -> rhs coordinates -> DotProductResult lhs rhs

class CrossProduct lhs rhs where
    type CrossProductResult lhs rhs :: * -> *
    (><) :: lhs coordinates -> rhs coordinates -> (CrossProductResult lhs rhs) coordinates

class Sqrt a where
    type SquareRoot a
    sqrt :: a -> SquareRoot a

instance Units.Sqrt units => Sqrt (Quantity units) where
    type SquareRoot (Quantity units) = Quantity (Units.SquareRoot units)
    sqrt (Quantity x) =
        Quantity (Prelude.sqrt x)

(//) :: Count units -> Count units -> Int
(Count n) // (Count m) =
    Count (Prelude.quot n m)

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
