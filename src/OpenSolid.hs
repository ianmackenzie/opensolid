-- Set in package.yaml, but hlint/HLS doesn't seem to notice it there
-- Can remove this if doing so no longer triggers warnings in ifThenElse
-- (e.g. an update to hlint or HLS notices the extension in package.yaml/opensolid.cabal)
{-# LANGUAGE Strict #-}

module OpenSolid
  ( Bool (..)
  , Char
  , Eq (..)
  , Int
  , Maybe (..)
  , Ord (..)
  , Show
  , IO
  , not
  , otherwise
  , ($)
  , (&&)
  , (||)
  , Type
  , Composition (..)
  , Qty (..)
  , Float
  , Sign (..)
  , Indeterminate (..)
  , Text
  , List
  , Result (..)
  , (??)
  , IsError (..)
  , Negation (..)
  , Addition (..)
  , Subtraction (..)
  , Multiplication (..)
  , Division (..)
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
  , validate
  , Invalid (..)
  , Tolerance
  , ApproximateEquality (..)
  , Named (..)
  , fromLabel
  , (.*)
  , (./)
  )
where

import Data.Kind (Type)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Units (Unitless)
import Units qualified
import Prelude
  ( Bool (..)
  , Char
  , Eq (..)
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

instance b ~ b' => Composition (a -> b) (b' -> c) (a -> c) where
  f >> g = g Prelude.. f

instance Composition (List a) Bool (List a) where
  list >> True = list
  _ >> False = []

instance Composition Bool (List a) (List a) where
  True >> list = list
  False >> _ = []

type Text = Data.Text.Text

type List a = [a]

class IsError error where
  errorMessage :: error -> Text

instance IsError Text where
  errorMessage = identity

data Result x a where
  Ok :: a -> Result x a
  Error :: IsError x => x -> Result x a

(??) :: Result x a -> Result y a -> Result y a
Ok value ?? _ = Ok value
Error _ ?? fallback = fallback

infixl 0 ??

deriving instance (Eq x, Eq a) => Eq (Result x a)

deriving instance (Show x, Show a) => Show (Result x a)

type role Qty nominal

type Qty :: Type -> Type
newtype Qty units = Qty Prelude.Double deriving (Eq, Ord, Show)

instance Units.Coercion Qty

type Float = Qty Unitless

deriving instance Prelude.Num Float

deriving instance Prelude.Real Float

deriving instance Prelude.Fractional Float

deriving instance Prelude.RealFrac Float

deriving instance Prelude.Floating Float

deriving instance Prelude.RealFloat Float

data Sign = Positive | Negative deriving (Eq, Show)

data Indeterminate = Indeterminate

instance IsError Indeterminate where
  errorMessage Indeterminate = "Result is indeterminate"

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

class DotProduct b a c => DotProduct a b c | a b -> c where
  (<>) :: a -> b -> c

class CrossProduct b a c => CrossProduct a b c | a b -> c where
  (><) :: a -> b -> c

(++) :: Prelude.Monoid a => a -> a -> a
(++) = Prelude.mappend

{-# INLINE fromInteger #-}
fromInteger :: Prelude.Integer -> Int
fromInteger = Prelude.fromInteger

{-# INLINE fromRational #-}
fromRational :: Prelude.Rational -> Float
fromRational = Prelude.fromRational

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

instance IsError Invalid where
  errorMessage Invalid = "Validation failed"

{-# INLINE validate #-}
validate :: (a -> Bool) -> a -> Result Invalid a
validate function value = if function value then Ok value else Error Invalid

infixl 0 |>

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

instance Units.Product units1 units2 units3 => Multiplication (Qty units1) (Qty units2) (Qty units3) where
  {-# INLINE (*) #-}
  (Qty x) * (Qty y) = Qty (x Prelude.* y)

instance Units.Quotient units1 units2 units3 => Division (Qty units1) (Qty units2) (Qty units3) where
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

instance Units.Quotient Unitless units1 units2 => Division Int (Qty units1) (Qty units2) where
  {-# INLINE (/) #-}
  n / x = float n / x

(.*)
  :: ( Units.Coercion a
     , Units.Coercion b
     , Units.Coercion c
     , Multiplication (a Unitless) (b Unitless) (c Unitless)
     )
  => a units1
  -> b units2
  -> c (Units.GenericProduct units1 units2)
(.*) lhs rhs = Units.add (Units.drop lhs * Units.drop rhs)

(./)
  :: ( Units.Coercion a
     , Units.Coercion b
     , Units.Coercion c
     , Division (a Unitless) (b Unitless) (c Unitless)
     )
  => a units1
  -> b units2
  -> c (Units.GenericQuotient units1 units2)
(./) lhs rhs = Units.add (Units.drop lhs / Units.drop rhs)
