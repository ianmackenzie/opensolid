module OpenSolid.Quantity
  ( Quantity (Quantity, Quantity##)
  , (.//.)
  , (.%.)
  , zero
  , unit
  , infinity
  , coerce
  , sign
  , isNaN
  , isInfinite
  , interpolateFrom
  , midpoint
  , squared
  , squared#
  , sqrt
  , sqrt#
  , hypot2
  , hypot3
  , abs
  , min
  , max
  , minmax
  , smaller
  , larger
  , smallerBy
  , largerBy
  , smallest
  , largest
  , smallestBy
  , largestBy
  , clampTo
  , convert
  , unconvert
  , sum
  , sumOf
  , random
  , steps
  , leading
  , trailing
  , inBetween
  , midpoints
  )
where

import Data.Coerce qualified
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text qualified
import Foreign.Storable (Storable)
import OpenSolid.Arithmetic hiding ((*), (+), (-))
import OpenSolid.Bootstrap hiding (max, min)
import {-# SOURCE #-} OpenSolid.Bounds (Bounds)
import {-# SOURCE #-} OpenSolid.Bounds qualified as Bounds
import OpenSolid.Composition
import OpenSolid.HasZero (HasZero)
import OpenSolid.HasZero qualified as HasZero
import OpenSolid.List qualified as List
import {-# SOURCE #-} OpenSolid.Number (Number)
import OpenSolid.Random.Internal qualified as Random
import OpenSolid.Sign (Sign (Negative, Positive))
import OpenSolid.Unboxed.Math
import OpenSolid.Unitless (Unitless)
import OpenSolid.Units (CubicMeters, Meters, Radians, SquareMeters, type (#*#), type (#/#))
import OpenSolid.Units qualified as Units
import System.Random qualified
import Prelude ((*), (+), (-), (/))
import Prelude qualified

type role Quantity phantom

type Quantity :: Type -> Type
newtype Quantity units = Quantity Double deriving (Eq, Ord)

{-# COMPLETE Quantity## #-}

{-# INLINE Quantity## #-}
pattern Quantity## :: Double# -> Quantity units
pattern Quantity## x## = Quantity (D# x##)

instance Show (Quantity Unitless) where
  show (Quantity x) = show x

showsPrecImpl :: Text -> Int -> Quantity units -> ShowS
showsPrecImpl constructor prec (Quantity x) =
  showParen (prec > 10) (showString (Data.Text.unpack (constructor <> " ")) . showsPrec 11 x)

instance Show (Quantity Meters) where
  showsPrec = showsPrecImpl "Length.meters"

instance Show (Quantity Radians) where
  showsPrec = showsPrecImpl "Angle.radians"

instance Show (Quantity SquareMeters) where
  showsPrec = showsPrecImpl "Area.squareMeters"

instance Show (Quantity CubicMeters) where
  showsPrec = showsPrecImpl "Area.cubicMeters"

deriving newtype instance units ~ Unitless => Prelude.Num (Quantity units)

deriving newtype instance units ~ Unitless => Prelude.Real (Quantity units)

deriving newtype instance units ~ Unitless => Prelude.Fractional (Quantity units)

deriving newtype instance units ~ Unitless => Prelude.RealFrac (Quantity units)

deriving newtype instance units ~ Unitless => Prelude.Floating (Quantity units)

deriving newtype instance units ~ Unitless => Prelude.RealFloat (Quantity units)

deriving newtype instance units ~ Unitless => Prelude.Read (Quantity units)

deriving newtype instance Storable (Quantity units)

deriving newtype instance Hashable (Quantity units)

instance HasZero (Quantity units) where
  zero = zero

instance Negation (Quantity units) where
  {-# INLINE negative #-}
  negative (Quantity x) = Quantity (Prelude.negate x)

instance Multiplication Sign (Quantity units) (Quantity units) where
  {-# INLINEABLE (.*.) #-}
  Positive .*. value = value
  Negative .*. value = negative value

instance Multiplication (Quantity units) Sign (Quantity units) where
  {-# INLINEABLE (.*.) #-}
  value .*. Positive = value
  value .*. Negative = negative value

instance units1 ~ units2 => Addition (Quantity units1) (Quantity units2) (Quantity units1) where
  {-# INLINE (.+.) #-}
  Quantity x .+. Quantity y = Quantity (x + y)

instance units1 ~ units2 => Subtraction (Quantity units1) (Quantity units2) (Quantity units1) where
  {-# INLINE (.-.) #-}
  Quantity x .-. Quantity y = Quantity (x - y)

instance Multiplication# (Quantity units1) (Quantity units2) (Quantity (units1 #*# units2)) where
  {-# INLINE (#*#) #-}
  Quantity x #*# Quantity y = Quantity (x * y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Quantity units2) (Quantity units3)
  where
  {-# INLINEABLE (.*.) #-}
  Quantity x .*. Quantity y = Quantity (x * y)

instance Division# (Quantity units1) (Quantity units2) (Quantity (units1 #/# units2)) where
  {-# INLINE (#/#) #-}
  Quantity x #/# Quantity y = Quantity (x / y)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Quantity units1) (Quantity units2) (Quantity units3)
  where
  {-# INLINEABLE (./.) #-}
  Quantity x ./. Quantity y = Quantity (x / y)

{-# INLINE (.//.) #-}
(.//.) :: Quantity units -> Quantity units -> Int
x .//. y = Prelude.floor (x ./. y)

infixl 7 .//.

{-# INLINE (.%.) #-}
(.%.) :: Quantity units -> Quantity units -> Quantity units
x .%. y = x .-. y .* fromIntegral (x .//. y)

infixl 7 .%.

{-# INLINE zero #-}
zero :: Quantity units
zero = Quantity 0.0

{-# INLINE unit #-}
unit :: Quantity units
unit = Quantity 1.0

infinity :: Quantity units
infinity = unit ./. (zero :: Number)

{-# INLINE coerce #-}
coerce :: Quantity units1 -> Quantity units2
coerce = Data.Coerce.coerce

sign :: Quantity units -> Sign
sign value = if value >= zero then Positive else Negative

{-# INLINE isNaN #-}
isNaN :: Quantity units -> Bool
isNaN (Quantity x) = Prelude.isNaN x

{-# INLINE isInfinite #-}
isInfinite :: Quantity units -> Bool
isInfinite (Quantity x) = Prelude.isInfinite x

{-# INLINE squared #-}
squared :: Units.Squared units1 units2 => Quantity units1 -> Quantity units2
squared x = x .*. x

{-# INLINE squared# #-}
squared# :: Quantity units -> Quantity (units #*# units)
squared# x = x #*# x

sqrt :: Units.Squared units1 units2 => Quantity units2 -> Quantity units1
sqrt x | x <= zero = zero
sqrt (Quantity x) = Quantity (Prelude.sqrt x)

sqrt# :: Quantity (units #*# units) -> Quantity units
sqrt# x | x <= zero = zero
sqrt# (Quantity x) = Quantity (Prelude.sqrt x)

hypot2 :: Quantity units -> Quantity units -> Quantity units
hypot2 (Quantity## x##) (Quantity## y##) = Quantity## (hypot2## x## y##)

hypot3 :: Quantity units -> Quantity units -> Quantity units -> Quantity units
hypot3 (Quantity## x##) (Quantity## y##) (Quantity## z##) = Quantity## (hypot3## x## y## z##)

{-# INLINE abs #-}
abs :: Quantity units -> Quantity units
abs (Quantity x) = Quantity (Prelude.abs x)

clampTo :: Bounds units -> Quantity units -> Quantity units
clampTo bounds value = min (max (Bounds.lower bounds) value) (Bounds.upper bounds)

{-# INLINE min #-}
min :: Quantity units -> Quantity units -> Quantity units
min = Prelude.min

{-# INLINE max #-}
max :: Quantity units -> Quantity units -> Quantity units
max = Prelude.max

{-# INLINE minmax #-}
minmax :: (Quantity units, Quantity units) -> (Quantity units, Quantity units)
minmax (a, b) = if a <= b then (a, b) else (b, a)

smaller :: Quantity units -> Quantity units -> Quantity units
smaller x y = if abs x <= abs y then x else y

larger :: Quantity units -> Quantity units -> Quantity units
larger x y = if abs x >= abs y then x else y

smallerBy :: (a -> Quantity units) -> a -> a -> a
smallerBy function first second =
  if abs (function first) <= abs (function second) then first else second

largerBy :: (a -> Quantity units) -> a -> a -> a
largerBy function first second =
  if abs (function first) >= abs (function second) then first else second

smallest :: NonEmpty (Quantity units) -> Quantity units
smallest (x :| xs) = List.foldl smaller x xs

largest :: NonEmpty (Quantity units) -> Quantity units
largest (x :| xs) = List.foldl larger x xs

smallestBy :: (a -> Quantity units) -> NonEmpty a -> a
smallestBy _ (x :| []) = x
smallestBy function (x :| xs) = go x (abs (function x)) xs
 where
  go current _ [] = current
  go current currentAbsValue (next : remaining) = do
    let nextAbsValue = abs (function next)
    if nextAbsValue < currentAbsValue
      then go next nextAbsValue remaining
      else go current currentAbsValue remaining

largestBy :: (a -> Quantity units) -> NonEmpty a -> a
largestBy _ (x :| []) = x
largestBy function (x :| xs) = go x (abs (function x)) xs
 where
  go current _ [] = current
  go current currentAbsValue (next : remaining) = do
    let nextAbsValue = abs (function next)
    if nextAbsValue > currentAbsValue
      then go next nextAbsValue remaining
      else go current currentAbsValue remaining

-- | Interpolate from one value to another, based on a parameter that ranges from 0 to 1.
{-# INLINE interpolateFrom #-}
interpolateFrom :: Quantity units -> Quantity units -> Number -> Quantity units
interpolateFrom a b t = a .+. (b .-. a) .*. t

{-# INLINE midpoint #-}
midpoint :: Quantity units -> Quantity units -> Quantity units
midpoint a b = 0.5 *. (a .+. b)

sum :: List (Quantity units) -> Quantity units
sum = List.foldl (.+.) zero

sumOf :: (a -> Quantity units) -> List a -> Quantity units
sumOf f list = sum (List.map f list)

convert :: Quantity (units2 #/# units1) -> Quantity units1 -> Quantity units2
convert factor value = Units.simplify (value #*# factor)

unconvert :: Quantity (units2 #/# units1) -> Quantity units2 -> Quantity units1
unconvert factor value = Units.simplify (value #/# factor)

random :: Quantity units -> Quantity units -> Random.Generator (Quantity units)
random (Quantity low) (Quantity high) =
  Random.map Quantity (Random.Generator (System.Random.uniformR (low, high)))

{-| Interpolate between two values by subdividing into the given number of steps.

The result is an empty list if the given number of steps is zero (or negative).
Otherwise, the number of values in the resulting list will be equal to one plus the number of steps.
For example, for one step the returned values will just be the given start and end values;
for two steps the returned values will be the start value, the midpoint and then the end value.
-}
steps :: Quantity units -> Quantity units -> Int -> List (Quantity units)
steps start end n = if n > 0 then range start end n [0 .. n] else []

-- | Interpolate between two values like 'steps', but skip the first value.
leading :: Quantity units -> Quantity units -> Int -> List (Quantity units)
leading start end n = range start end n [0 .. n - 1]

-- | Interpolate between two values like 'steps', but skip the last value.
trailing :: Quantity units -> Quantity units -> Int -> List (Quantity units)
trailing start end n = range start end n [1 .. n]

-- | Interpolate between two values like 'steps', but skip the first and last values.
inBetween :: Quantity units -> Quantity units -> Int -> List (Quantity units)
inBetween start end n = range start end n [1 .. n - 1]

{-| Subdivide a given range into the given number of steps, and return the midpoint of each step.

This can be useful if you want to sample a curve or other function at the midpoint of several intervals.
-}
midpoints :: Quantity units -> Quantity units -> Int -> List (Quantity units)
midpoints start end n = range start end (2 * n) [1, 3 .. 2 * n - 1]

range :: Quantity units -> Quantity units -> Int -> List Int -> List (Quantity units)
range start end n indices = do
  let delta = end .-. start
  [start .+. (fromIntegral i / fromIntegral n) *. delta | i <- indices]
