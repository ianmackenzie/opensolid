module OpenSolid (
    module Prelude,
    module Result,
    Float,
    String,
    List,
    Quantity,
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
    Length,
    Area,
    Volume,
) where

import Data.Coerce (Coercible, coerce)
import qualified Data.Text
import Result (Result (..))
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

class (Eq a, Ord a, Coercible a Float, Coercible Float a) => Quantity a

instance Quantity Float

class Negation a where
    negate :: a -> a

instance {-# INCOHERENT #-} Negation Int where
    negate =
        Prelude.negate

instance {-# INCOHERENT #-} Quantity quantity => Negation quantity where
    negate quantity =
        coerce (Prelude.negate (coerce quantity :: Float))

class Addition a where
    (+) :: a -> a -> a

instance {-# INCOHERENT #-} Addition Int where
    n + m =
        n Prelude.+ m

instance {-# INCOHERENT #-} Quantity quantity => Addition quantity where
    x + y =
        coerce ((coerce x :: Float) Prelude.+ (coerce y :: Float))

class Subtraction a where
    (-) :: a -> a -> a

instance {-# INCOHERENT #-} Subtraction Int where
    n - m =
        n Prelude.- m

instance {-# INCOHERENT #-} Quantity quantity => Subtraction quantity where
    x - y =
        coerce ((coerce x :: Float) Prelude.- (coerce y :: Float))

class Multiplication lhs rhs result | lhs rhs -> result where
    (*) :: lhs -> rhs -> result

instance {-# INCOHERENT #-} Multiplication Int Int Int where
    n * m =
        n Prelude.* m

instance {-# INCOHERENT #-} Quantity quantity => Multiplication Float quantity quantity where
    x * y =
        coerce (x Prelude.* (coerce y :: Float))

instance {-# INCOHERENT #-} Quantity quantity => Multiplication quantity Float quantity where
    x * y =
        coerce ((coerce x :: Float) Prelude.* y)

class Division lhs rhs result | lhs rhs -> result where
    (/) :: lhs -> rhs -> result

instance {-# INCOHERENT #-} Division Float Float Float where
    x / y =
        x Prelude./ y

instance {-# INCOHERENT #-} Quantity quantity => Division quantity Float quantity where
    x / y =
        coerce (coerce x Prelude./ y)

instance {-# INCOHERENT #-} Quantity quantity => Division quantity quantity Float where
    x / y =
        coerce ((coerce x :: Float) Prelude./ (coerce y :: Float))

(//) :: Int -> Int -> Int
(//) =
    Prelude.quot

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

abs :: Quantity quantity => quantity -> quantity
abs quantity =
    coerce (Prelude.abs (coerce quantity :: Float))

class Sqrt a where
    type SqrtResult a
    sqrt :: a -> SqrtResult a

instance Sqrt Float where
    type SqrtResult Float = Float

    sqrt =
        Prelude.sqrt

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

newtype Length = Length Float
    deriving (Eq, Ord, Show)

instance Quantity Length

newtype Duration = Duration Float
    deriving (Eq, Ord, Show)

instance Quantity Duration

newtype Speed = Speed Float
    deriving (Eq, Ord, Show)

instance Quantity Speed

newtype Acceleration = Acceleration Float
    deriving (Eq, Ord, Show)

instance Quantity Acceleration

newtype Area = Area Float
    deriving (Eq, Ord, Show)

instance Quantity Area

newtype Volume = Volume Float
    deriving (Eq, Ord, Show)

instance Quantity Volume

quantityMultiplication :: (Quantity a, Quantity b, Quantity c) => a -> b -> c
quantityMultiplication a b =
    coerce ((coerce a :: Float) Prelude.* (coerce b :: Float))

quantityDivision :: (Quantity a, Quantity b, Quantity c) => a -> b -> c
quantityDivision a b =
    coerce ((coerce a :: Float) Prelude./ (coerce b :: Float))

quantitySqrt :: (Quantity a, Quantity b) => a -> b
quantitySqrt a =
    coerce (Prelude.sqrt (coerce a :: Float))

instance {-# INCOHERENT #-} Multiplication Length Length Area where
    (*) = quantityMultiplication

instance {-# INCOHERENT #-} Multiplication Length Area Volume where
    (*) = quantityMultiplication

instance {-# INCOHERENT #-} Multiplication Area Length Volume where
    (*) = quantityMultiplication

instance {-# INCOHERENT #-} Multiplication Duration Speed Length where
    (*) = quantityMultiplication

instance {-# INCOHERENT #-} Multiplication Speed Duration Length where
    (*) = quantityMultiplication

instance {-# INCOHERENT #-} Multiplication Duration Acceleration Speed where
    (*) = quantityMultiplication

instance {-# INCOHERENT #-} Multiplication Acceleration Duration Speed where
    (*) = quantityMultiplication

instance {-# INCOHERENT #-} Division Area Length Length where
    (/) = quantityDivision

instance {-# INCOHERENT #-} Division Volume Area Length where
    (/) = quantityDivision

instance {-# INCOHERENT #-} Division Volume Length Area where
    (/) = quantityDivision

instance {-# INCOHERENT #-} Division Length Duration Speed where
    (/) = quantityDivision

instance {-# INCOHERENT #-} Division Speed Duration Acceleration where
    (/) = quantityDivision

instance Sqrt Area where
    type SqrtResult Area = Length
    sqrt = quantitySqrt
