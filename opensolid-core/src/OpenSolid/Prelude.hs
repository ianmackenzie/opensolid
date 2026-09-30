module OpenSolid.Prelude
  ( module Prelude
  , Type
  , List
  , NonEmpty ((:|))
  , Text
  , Fuzzy (Resolved, Unresolved)
  , Err
  , Result (Ok, Err)
  , Unwrap
  , (!!)
  , (??)
  , Exception
  , Void
  , data NonEmpty
  , type (:::) ((:::))
  , SetField (setField)
  , (&)
  , Negation (negate)
  , Addition ((+))
  , Subtraction ((-))
  , subtract
  , Multiplication_ ((?*?))
  , type (?*?)
  , Multiplication ((*))
  , Division_ ((?/?))
  , type (?/?)
  , Division ((/))
  , DotMultiplication_ (dot_)
  , DotMultiplication (dot)
  , CrossMultiplication_ (cross_)
  , CrossMultiplication (cross)
  , DivMod ((//), (%))
  , Exponentiation ((**))
  , Composition ((<<))
  , Tolerance
  , unitless
  , angular
  , spatial
  , ApproximateEquality ((~=))
  , At ((@))
  , On (on)
  , Intersects ((^))
  , Quantity (Quantity, Q#)
  , Unitless
  , Radians
  , Meters
  , Number
  , data Number
  , data N#
  , Sign (Sign, Positive, Negative)
  , fromInteger
  , fromRational
  , fromString
  , string
  , fromLabel
  , getField
  , ifThenElse
  , assert
  , error
  , data TODO
  , recursive
  , forEach
  , forEachWithIndex
  , curry2
  , curry3
  , curry4
  , curry5
  , curry6
  , curry7
  , curry8
  , curry9
  , curry10
  )
where

import Control.Exception (Exception, assert)
import Data.Coerce
import Data.Function ((&))
import Data.Function qualified
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified
import Data.Map.Strict (Map)
import Data.Map.Strict qualified
import Data.Text (Text)
import Data.Text qualified
import Data.Void (Void)
import Foreign.Storable (Storable)
import GHC.Exts (Double (D#), Double#)
import GHC.OverloadedLabels (IsLabel (fromLabel))
import GHC.Records (HasField (getField))
import GHC.Stack (HasCallStack)
import GHC.Stack qualified
import OpenSolid.Err (Err)
import OpenSolid.Params (SetField (setField), type (:::) ((:::)))
import OpenSolid.Show qualified as Show
import OpenSolid.Units (Meters, Radians, Unitless, Units, type (?*?), type (?/?))
import OpenSolid.Units qualified as Units
import OpenSolid.Unwrap (Unwrap (unwrap))
import System.Random.Stateful qualified
import Prelude
  ( Applicative
  , Bool (False, True)
  , Char
  , Double
  , Either (Left, Right)
  , Eq
  , Foldable
  , Functor
  , IO
  , Int
  , Maybe (Just, Nothing)
  , Monad ((>>), (>>=))
  , MonadFail
  , Ord
  , Ordering (EQ, GT, LT)
  , Show (..)
  , Traversable
  , compare
  , const
  , fromIntegral
  , id
  , max
  , min
  , not
  , otherwise
  , seq
  , ($)
  , (&&)
  , (.)
  , (/=)
  , (<)
  , (<$>)
  , (<*>)
  , (<=)
  , (<>)
  , (==)
  , (>)
  , (>=)
  , (||)
  , type (~)
  )
import Prelude qualified

----- Arithmetic -----

class Negation a where
  negate :: a -> a

instance Negation Int where
  {-# INLINE negate #-}
  negate = Prelude.negate

instance Negation Double where
  {-# INLINE negate #-}
  negate = Prelude.negate

class Addition a b c | a b -> c where
  (+) :: a -> b -> c

infixl 6 +

instance Addition Int Int Int where
  {-# INLINE (+) #-}
  (+) = (Prelude.+)

instance Addition Double Double Double where
  {-# INLINE (+) #-}
  (+) = (Prelude.+)

class Subtraction a b c | a b -> c where
  (-) :: a -> b -> c

infixl 6 -

{-# INLINE subtract #-}
subtract :: Subtraction a b c => b -> a -> c
subtract value = (- value)

instance Subtraction Int Int Int where
  {-# INLINE (-) #-}
  (-) = (Prelude.-)

instance Subtraction Double Double Double where
  {-# INLINE (-) #-}
  (-) = (Prelude.-)

class Multiplication_ a b c | a b -> c where
  (?*?) :: a -> b -> c

infixl 7 ?*?

class Multiplication a b c | a b -> c where
  (*) :: a -> b -> c

infixl 7 *

instance Multiplication Int Int Int where
  {-# INLINE (*) #-}
  (*) = (Prelude.*)

instance Multiplication Sign Int Int where
  Positive * value = value
  Negative * value = -value

instance Multiplication Int Sign Int where
  value * Positive = value
  value * Negative = -value

instance Multiplication Double Double Double where
  {-# INLINE (*) #-}
  (*) = (Prelude.*)

instance Multiplication Sign Double Double where
  Positive * value = value
  Negative * value = -value

instance Multiplication Double Sign Double where
  value * Positive = value
  value * Negative = -value

class Division_ a b c | a b -> c where
  (?/?) :: a -> b -> c

infixl 7 ?/?

class Division a b c | a b -> c where
  (/) :: a -> b -> c

infixl 7 /

instance Division Int Int Number where
  {-# INLINE (/) #-}
  n / m = Number (fromIntegral n) / Number (fromIntegral m)

instance Division Double Double Double where
  {-# INLINE (/) #-}
  (/) = (Prelude./)

class DotMultiplication_ a b c | a b -> c where
  dot_ :: a -> b -> c

infixl 7 `dot_`

class DotMultiplication a b c | a b -> c where
  dot :: a -> b -> c

infixl 7 `dot`

class CrossMultiplication_ a b c | a b -> c where
  cross_ :: a -> b -> c

infixl 7 `cross_`

class CrossMultiplication a b c | a b -> c where
  cross :: a -> b -> c

infixl 7 `cross`

class DivMod a where
  (%) :: a -> a -> a
  (//) :: a -> a -> Int

infixl 7 //

infixl 7 %

instance DivMod Int where
  {-# INLINE (//) #-}
  (//) = Prelude.div
  {-# INLINE (%) #-}
  (%) = Prelude.mod

instance DivMod Double where
  {-# INLINE (//) #-}
  x // y = Prelude.floor (x / y)
  {-# INLINE (%) #-}
  x % y = let n :: Double = fromIntegral (x // y) in x - n * y

class Exponentiation a b c | a b -> c where
  (**) :: a -> b -> c

infixr 8 **

instance Exponentiation Int Int Int where
  {-# INLINE (**) #-}
  (**) = (Prelude.^)

instance Exponentiation Number Number Number where
  {-# INLINE (**) #-}
  (**) = (Prelude.**)

instance Exponentiation Number Int Number where
  {-# INLINE (**) #-}
  (**) = (Prelude.^^)

----- Tolerance -----

type Tolerance units = ?tolerance :: Quantity units

{-# INLINE unitless #-}
unitless :: (Tolerance Unitless => a) -> a
unitless expression = let ?tolerance = 1e-9 in expression

{-# INLINE angular #-}
angular :: (Tolerance Radians => a) -> a
angular expression = let ?tolerance = Quantity 1e-9 in expression

{-# INLINE spatial #-}
spatial :: (Tolerance Meters => a) -> a
spatial expression = let ?tolerance = Quantity 1e-9 in expression

----- Approximate equality -----

class ApproximateEquality a units | a -> units where
  (~=) :: Tolerance units => a -> a -> Bool

infix 4 ~=

instance ApproximateEquality (Quantity units) units where
  {-# INLINE (~=) #-}
  x ~= y = x >= y - ?tolerance && x <= y + ?tolerance

instance ApproximateEquality a units => ApproximateEquality (List a) units where
  x : xs ~= y : ys = x ~= y && xs ~= ys
  [] ~= [] = True
  NonEmpty _ ~= [] = False
  [] ~= NonEmpty _ = False

instance ApproximateEquality a units => ApproximateEquality (NonEmpty a) units where
  x :| xs ~= y :| ys = x ~= y && xs ~= ys

instance ApproximateEquality a units => ApproximateEquality (Maybe a) units where
  Just a ~= Just b = a ~= b
  Nothing ~= Nothing = True
  Just _ ~= Nothing = False
  Nothing ~= Just _ = False

instance
  ( ApproximateEquality a1 units1
  , ApproximateEquality a2 units2
  , units1 ~ units2
  ) =>
  ApproximateEquality (a1, a2) units1
  where
  (a1, a2) ~= (b1, b2) = a1 ~= b1 && a2 ~= b2

instance
  ( ApproximateEquality a1 units1
  , ApproximateEquality a2 units2
  , ApproximateEquality a3 units3
  , units1 ~ units2
  , units1 ~ units3
  ) =>
  ApproximateEquality (a1, a2, a3) units1
  where
  (a1, a2, a3) ~= (b1, b2, b3) = a1 ~= b1 && a2 ~= b2 && a3 ~= b3

instance
  ( ApproximateEquality a1 units1
  , ApproximateEquality a2 units2
  , ApproximateEquality a3 units3
  , ApproximateEquality a4 units4
  , units1 ~ units2
  , units1 ~ units3
  , units1 ~ units4
  ) =>
  ApproximateEquality (a1, a2, a3, a4) units1
  where
  (a1, a2, a3, a4) ~= (b1, b2, b3, b4) = a1 ~= b1 && a2 ~= b2 && a3 ~= b3 && a4 ~= b4

----- Composition -----

class Composition constraint f g h | f g -> h, f g -> constraint where
  (<<) :: constraint => f -> g -> h

infixr 9 <<

----- List -----

type List a = [a]

----- NonEmpty -----

{-# COMPLETE [], NonEmpty #-}

pattern NonEmpty :: NonEmpty a -> List a
pattern NonEmpty nonEmpty <- (Data.List.NonEmpty.nonEmpty -> Just nonEmpty)

----- Fuzzy -----

data Fuzzy a = Resolved a | Unresolved deriving (Eq, Show)

instance Functor Fuzzy where
  {-# INLINE fmap #-}
  fmap function (Resolved value) = Resolved (function value)
  fmap _ Unresolved = Unresolved

instance Applicative Fuzzy where
  {-# INLINE pure #-}
  pure = Resolved

  {-# INLINE (<*>) #-}
  Resolved function <*> Resolved value = Resolved (function value)
  Unresolved <*> _ = Unresolved
  Resolved _ <*> Unresolved = Unresolved

instance Monad Fuzzy where
  {-# INLINE (>>=) #-}
  Resolved value >>= function = function value
  Unresolved >>= _ = Unresolved

----- Result -----

data Result x a where
  Ok :: a -> Result x a
  Err :: Err x => x -> Result x a

deriving instance (Eq x, Eq a) => Eq (Result x a)

deriving instance Show a => Show (Result x a)

instance Functor (Result x) where
  {-# INLINE fmap #-}
  fmap function (Ok value) = Ok (function value)
  fmap _ (Err err) = Err err

instance Applicative (Result x) where
  {-# INLINE pure #-}
  pure = Ok

  {-# INLINE (<*>) #-}
  Ok function <*> Ok value = Ok (function value)
  Err err <*> _ = Err err
  _ <*> Err err = Err err

instance Monad (Result x) where
  {-# INLINE (>>=) #-}
  Ok value >>= function = function value
  Err err >>= _ = Err err

instance MonadFail (Result Text) where
  {-# INLINE fail #-}
  fail message = Err (fromString message)

----- Unwrap ----

instance Unwrap (Result x) where
  {-# INLINE unwrap #-}
  unwrap (Ok value) = Just value
  unwrap (Err _) = Nothing

instance Unwrap Fuzzy where
  {-# INLINE unwrap #-}
  unwrap (Resolved value) = Just value
  unwrap Unresolved = Nothing

{-# INLINE (??) #-}
(??) :: (Unwrap m1, Monad m2) => m1 a -> m2 a -> m2 a
first ?? second =
  case unwrap first of
    Just value -> Prelude.return value
    Nothing -> second

infixl 0 ??

{-# INLINE (!!) #-}
(!!) :: Unwrap m => m a -> a -> a
wrapper !! fallback =
  case unwrap wrapper of
    Just value -> value
    Nothing -> fallback

infixl 0 !!

----- Quantity -----

type Quantity :: Type -> Type
newtype Quantity units = Quantity_ Prelude.Double
  deriving (Eq, Ord)
  deriving newtype (Storable, Hashable)

instance Show (Quantity units) where
  showsPrec precedence (Quantity_ double) =
    Show.constructor precedence "Quantity" double

type Number = Quantity Unitless

{-# COMPLETE Quantity #-}

{-# INLINE Quantity #-}
pattern Quantity :: Number -> Quantity units
pattern Quantity number <- (Data.Coerce.coerce -> number)
 where
  Quantity = Data.Coerce.coerce

{-# COMPLETE Q# #-}

{-# INLINE Q# #-}
pattern Q# :: Double# -> Quantity units
pattern Q# x# = Quantity_ (D# x#)

{-# COMPLETE Number #-}

{-# INLINE Number #-}
pattern Number :: Double -> Number
pattern Number double = Quantity_ double

{-# COMPLETE N# #-}

{-# INLINE N# #-}
pattern N# :: Double# -> Number
pattern N# x# = Number (D# x#)

deriving newtype instance Prelude.Num Number

deriving newtype instance Prelude.Real Number

deriving newtype instance Prelude.Fractional Number

deriving newtype instance Prelude.RealFrac Number

deriving newtype instance Prelude.Floating Number

deriving newtype instance Prelude.RealFloat Number

deriving newtype instance Prelude.Read Number

instance Units (Quantity units) units

instance Units.Coercion (Quantity units1) (Quantity units2) where
  {-# INLINE coerce #-}
  coerce = Data.Coerce.coerce

instance Negation (Quantity units) where
  {-# INLINE negate #-}
  negate (Quantity_ x) = Quantity_ (Prelude.negate x)

instance Multiplication Sign (Quantity units) (Quantity units) where
  {-# INLINE (*) #-}
  Sign sign_ * value = sign_ * value

instance Multiplication (Quantity units) Sign (Quantity units) where
  {-# INLINE (*) #-}
  value * Sign sign_ = value * sign_

instance units1 ~ units2 => Addition (Quantity units1) (Quantity units2) (Quantity units1) where
  {-# INLINE (+) #-}
  Quantity_ x + Quantity_ y = Quantity_ (x + y)

instance units1 ~ units2 => Subtraction (Quantity units1) (Quantity units2) (Quantity units1) where
  {-# INLINE (-) #-}
  Quantity_ x - Quantity_ y = Quantity_ (x - y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Quantity units2) (Quantity units3)
  where
  {-# INLINE (*) #-}
  Quantity_ x * Quantity_ y = Quantity_ (x * y)

instance Multiplication_ (Quantity units1) (Quantity units2) (Quantity (units1 ?*? units2)) where
  {-# INLINE (?*?) #-}
  Quantity_ x ?*? Quantity_ y = Quantity_ (x * y)

instance DotMultiplication_ (Quantity units1) (Quantity units2) (Quantity (units1 ?*? units2)) where
  {-# INLINE dot_ #-}
  dot_ = (?*?)

instance
  Units.Product units1 units2 units3 =>
  DotMultiplication (Quantity units1) (Quantity units2) (Quantity units3)
  where
  {-# INLINE dot #-}
  dot = (*)

instance Division_ (Quantity units1) (Quantity units2) (Quantity (units1 ?/? units2)) where
  {-# INLINE (?/?) #-}
  Quantity_ x ?/? Quantity_ y = Quantity_ (x / y)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Quantity units1) (Quantity units2) (Quantity units3)
  where
  {-# INLINE (/) #-}
  Quantity_ x / Quantity_ y = Quantity_ (x / y)

instance DivMod (Quantity units) where
  {-# INLINE (//) #-}
  Quantity_ x // Quantity_ y = x // y
  {-# INLINE (%) #-}
  Quantity_ x % Quantity_ y = Quantity_ (x % y)

----- At -----

class At datum location result | datum location -> result where
  (@) :: datum -> location -> result

infixl 9 @

instance At (List item) Int item where
  {-# INLINE (@) #-}
  (@) = (Prelude.!!)

instance At (NonEmpty item) Int item where
  {-# INLINE (@) #-}
  (@) = (Data.List.NonEmpty.!!)

instance (Ord key1, key1 ~ key2) => At (Map key1 value) key2 value where
  {-# INLINE (@) #-}
  (@) = (Data.Map.Strict.!)

instance (Hashable key1, key1 ~ key2) => At (HashMap key1 value) key2 value where
  {-# INLINE (@) #-}
  (@) = (Data.HashMap.Strict.!)

instance At datum location result => At datum (List location) (List result) where
  datum @ locations = Prelude.fmap (datum @) locations

instance At datum location result => At datum (NonEmpty location) (NonEmpty result) where
  datum @ locations = Prelude.fmap (datum @) locations

----- On -----

class On datum geometry result | datum geometry -> result where
  on :: datum -> geometry -> result

instance
  ( On datum geometry1 result1
  , On datum geometry2 result2
  ) =>
  On datum (geometry1, geometry2) (result1, result2)
  where
  on datum (geometry1, geometry2) = (on datum geometry1, on datum geometry2)

instance
  ( On datum geometry1 result1
  , On datum geometry2 result2
  , On datum geometry3 result3
  ) =>
  On datum (geometry1, geometry2, geometry3) (result1, result2, result3)
  where
  on datum (geometry1, geometry2, geometry3) =
    (on datum geometry1, on datum geometry2, on datum geometry3)

instance
  ( On datum geometry1 result1
  , On datum geometry2 result2
  , On datum geometry3 result3
  , On datum geometry4 result4
  ) =>
  On datum (geometry1, geometry2, geometry3, geometry4) (result1, result2, result3, result4)
  where
  on datum (geometry1, geometry2, geometry3, geometry4) =
    (on datum geometry1, on datum geometry2, on datum geometry3, on datum geometry4)

instance
  ( On datum geometry1 result1
  , On datum geometry2 result2
  , On datum geometry3 result3
  , On datum geometry4 result4
  , On datum geometry5 result5
  ) =>
  On
    datum
    (geometry1, geometry2, geometry3, geometry4, geometry5)
    (result1, result2, result3, result4, result5)
  where
  on datum (geometry1, geometry2, geometry3, geometry4, geometry5) =
    ( on datum geometry1
    , on datum geometry2
    , on datum geometry3
    , on datum geometry4
    , on datum geometry5
    )

instance
  ( On datum geometry1 result1
  , On datum geometry2 result2
  , On datum geometry3 result3
  , On datum geometry4 result4
  , On datum geometry5 result5
  , On datum geometry6 result6
  ) =>
  On
    datum
    (geometry1, geometry2, geometry3, geometry4, geometry5, geometry6)
    (result1, result2, result3, result4, result5, result6)
  where
  on datum (geometry1, geometry2, geometry3, geometry4, geometry5, geometry6) =
    ( on datum geometry1
    , on datum geometry2
    , on datum geometry3
    , on datum geometry4
    , on datum geometry5
    , on datum geometry6
    )

instance On datum geometry result => On datum (Maybe geometry) (Maybe result) where
  on = Prelude.fmap . on

instance On datum geometry result => On datum (Result x geometry) (Result x result) where
  on = Prelude.fmap . on

instance On datum geometry result => On datum (List geometry) (List result) where
  on = Prelude.fmap . on

instance On datum geometry result => On datum (NonEmpty geometry) (NonEmpty result) where
  on = Prelude.fmap . on

----- Intersection -----

class Intersects a b units | a b -> units where
  (^) :: Tolerance units => a -> b -> Bool

infix 4 ^

instance
  ( Intersects a1 a2 unitsA
  , Intersects b1 b2 unitsB
  , unitsA ~ unitsB
  ) =>
  Intersects (a1, b1) (a2, b2) unitsA
  where
  (a1, b1) ^ (a2, b2) = a1 ^ a2 && b1 ^ b2

----- Sign -----

newtype Sign = Sign_ Number deriving (Eq, Ord)

instance Show Sign where
  show Positive = string "Positive"
  show Negative = string "Negative"

instance Units Sign Unitless

instance ApproximateEquality Sign Unitless where (~=) = (==)

{-# COMPLETE Sign #-}

{-# INLINE Sign #-}
pattern Sign :: Number -> Sign
pattern Sign x <- Sign_ x

{-# COMPLETE Negative, Positive #-}

{-# INLINE Negative #-}
pattern Negative :: Sign
pattern Negative = Sign_ -1.0

{-# INLINE Positive #-}
pattern Positive :: Sign
pattern Positive = Sign_ 1.0

instance Prelude.Bounded Sign where
  minBound = Negative
  maxBound = Positive

instance Prelude.Enum Sign where
  toEnum 0 = Negative
  toEnum 1 = Positive
  toEnum _ = error "Out of range"
  fromEnum Negative = 0
  fromEnum Positive = 1

instance System.Random.Stateful.Uniform Sign where
  uniformM = System.Random.Stateful.uniformEnumM

instance Negation Sign where
  {-# INLINE negate #-}
  negate (Sign_ value) = Sign_ (negate value)

instance Multiplication Sign Sign Sign where
  {-# INLINE (*) #-}
  Sign_ value1 * Sign_ value2 = Sign_ (value1 * value2)

instance DotMultiplication Sign Sign Number where
  {-# INLINE dot #-}
  Sign_ value1 `dot` Sign_ value2 = value1 * value2

instance DotMultiplication Sign (Quantity units) (Quantity units) where dot = (*)

instance DotMultiplication (Quantity units) Sign (Quantity units) where dot = (*)

{-# INLINE fromInteger #-}
fromInteger :: Prelude.Integer -> Int
fromInteger = Prelude.fromInteger

{-# INLINE fromRational #-}
fromRational :: Prelude.Rational -> Number
fromRational = Prelude.fromRational

{-# INLINE fromString #-}
fromString :: Prelude.String -> Text
fromString = Data.Text.pack

string :: Text -> Prelude.String
string = Data.Text.unpack

{-# INLINE ifThenElse #-}
ifThenElse :: Bool -> a -> a -> a
ifThenElse True first _ = first
ifThenElse False _ second = second

{-# COMPLETE TODO #-}

pattern TODO :: HasCallStack => a
pattern TODO <- (GHC.Stack.withFrozenCallStack todoImpl -> ())
 where
  TODO = GHC.Stack.withFrozenCallStack todoImpl

todoImpl :: HasCallStack => a
todoImpl = error "Not implemented"

error :: Text -> a
error = Prelude.error . string

{-# INLINE recursive #-}
recursive :: (a -> a) -> a
recursive = Data.Function.fix

{-# INLINE forEach #-}
forEach :: Foldable list => list item -> (item -> acc -> acc) -> acc -> acc
forEach list function init = Prelude.foldl' (Prelude.flip function) init list

data WithIndex acc = WithIndex Int acc

{-# INLINE forEachWithIndex #-}
forEachWithIndex :: Foldable list => list item -> (Int -> item -> acc -> acc) -> acc -> acc
forEachWithIndex list function init = do
  let callback (WithIndex i acc) item = WithIndex (i + 1) (function i item acc)
  let WithIndex _ result = Prelude.foldl' callback (WithIndex 0 init) list
  result

----- Currying -----

{-# INLINE curry2 #-}
curry2 :: ((a, b) -> result) -> (a -> b -> result)
curry2 function a b = function (a, b)

{-# INLINE curry3 #-}
curry3 :: ((a, b, c) -> result) -> (a -> b -> c -> result)
curry3 function a b c = function (a, b, c)

{-# INLINE curry4 #-}
curry4 :: ((a, b, c, d) -> result) -> (a -> b -> c -> d -> result)
curry4 function a b c d = function (a, b, c, d)

{-# INLINE curry5 #-}
curry5 :: ((a, b, c, d, e) -> result) -> (a -> b -> c -> d -> e -> result)
curry5 function a b c d e = function (a, b, c, d, e)

{-# INLINE curry6 #-}
curry6 :: ((a, b, c, d, e, f) -> result) -> (a -> b -> c -> d -> e -> f -> result)
curry6 function a b c d e f = function (a, b, c, d, e, f)

{-# INLINE curry7 #-}
curry7 :: ((a, b, c, d, e, f, g) -> result) -> (a -> b -> c -> d -> e -> f -> g -> result)
curry7 function a b c d e f g = function (a, b, c, d, e, f, g)

{-# INLINE curry8 #-}
curry8 :: ((a, b, c, d, e, f, g, h) -> result) -> (a -> b -> c -> d -> e -> f -> g -> h -> result)
curry8 function a b c d e f g h = function (a, b, c, d, e, f, g, h)

{-# INLINE curry9 #-}
curry9 :: ((a, b, c, d, e, f, g, h, i) -> result) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> result)
curry9 function a b c d e f g h i = function (a, b, c, d, e, f, g, h, i)

{-# INLINE curry10 #-}
curry10 :: ((a, b, c, d, e, f, g, h, i, j) -> result) -> (a -> b -> c -> d -> e -> f -> g -> h -> i -> j -> result)
curry10 function a b c d e f g h i j = function (a, b, c, d, e, f, g, h, i, j)
