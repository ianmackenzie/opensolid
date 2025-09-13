module OpenSolid.Qty
  ( Qty (Qty, Qty#)
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
  , squared'
  , sqrt
  , sqrt'
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
import OpenSolid.Arithmetic
import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Bounds (Bounds)
import {-# SOURCE #-} OpenSolid.Bounds qualified as Bounds
import OpenSolid.Composition
import {-# SOURCE #-} OpenSolid.Float (Float, fromRational)
import {-# SOURCE #-} OpenSolid.Float qualified as Float
import OpenSolid.HasZero (HasZero)
import OpenSolid.HasZero qualified as HasZero
import OpenSolid.List qualified as List
import OpenSolid.Random.Internal qualified as Random
import OpenSolid.Sign (Sign (Negative, Positive))
import OpenSolid.Unboxed.Math
import OpenSolid.Unitless (Unitless)
import OpenSolid.Units (CubicMeters, Meters, Radians, SquareMeters, (:*:), (:/:))
import OpenSolid.Units qualified as Units
import System.Random qualified
import Prelude qualified

type role Qty phantom

type Qty :: Type -> Type
newtype Qty units = Qty Prelude.Double deriving (Eq, Ord)

{-# COMPLETE Qty# #-}

{-# INLINE Qty# #-}
pattern Qty# :: Double# -> Qty units
pattern Qty# x# = Qty (D# x#)

instance Show (Qty Unitless) where
  show (Qty x) = Prelude.show x

showsPrec :: Text -> Int -> Qty units -> Prelude.ShowS
showsPrec constructor prec (Qty x) =
  Prelude.showParen (prec > 10) $
    Prelude.showString (Data.Text.unpack (constructor <> " ")) . Prelude.showsPrec 11 x

instance Show (Qty Meters) where
  showsPrec = showsPrec "Length.meters"

instance Show (Qty Radians) where
  showsPrec = showsPrec "Angle.radians"

instance Show (Qty SquareMeters) where
  showsPrec = showsPrec "Area.squareMeters"

instance Show (Qty CubicMeters) where
  showsPrec = showsPrec "Area.cubicMeters"

deriving newtype instance Prelude.Num Float

deriving newtype instance Prelude.Real Float

deriving newtype instance Prelude.Fractional Float

deriving newtype instance Prelude.RealFrac Float

deriving newtype instance Prelude.Floating Float

deriving newtype instance Prelude.RealFloat Float

deriving newtype instance Prelude.Read Float

deriving newtype instance Storable (Qty units)

deriving newtype instance Hashable (Qty units)

instance HasZero (Qty units) where
  zero = zero

instance Negation (Qty units) where
  {-# INLINE negate #-}
  negate (Qty x) = Qty (Prelude.negate x)

instance Multiplication Sign (Qty units) (Qty units) where
  {-# INLINEABLE (*) #-}
  Positive * value = value
  Negative * value = -value

instance Multiplication (Qty units) Sign (Qty units) where
  {-# INLINEABLE (*) #-}
  value * Positive = value
  value * Negative = -value

instance units1 ~ units2 => Addition (Qty units1) (Qty units2) (Qty units1) where
  {-# INLINE (+) #-}
  Qty x + Qty y = Qty (x Prelude.+ y)

instance units1 ~ units2 => Subtraction (Qty units1) (Qty units2) (Qty units1) where
  {-# INLINE (-) #-}
  Qty x - Qty y = Qty (x Prelude.- y)

instance Multiplication' (Qty units1) (Qty units2) (Qty (units1 :*: units2)) where
  {-# INLINE (.*.) #-}
  Qty x .*. Qty y = Qty (x Prelude.* y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Qty units2) (Qty units3)
  where
  {-# INLINEABLE (*) #-}
  Qty x * Qty y = Qty (x Prelude.* y)

instance Division' (Qty units1) (Qty units2) (Qty (units1 :/: units2)) where
  {-# INLINE (./.) #-}
  Qty x ./. Qty y = Qty (x Prelude./ y)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Qty units2) (Qty units3)
  where
  {-# INLINEABLE (/) #-}
  Qty x / Qty y = Qty (x Prelude./ y)

instance DivMod (Qty units) where
  {-# INLINE (//) #-}
  x // y = Prelude.floor (x / y)
  {-# INLINE (%) #-}
  x % y = x - y * Float.int (x // y)

{-# INLINE zero #-}
zero :: Qty units
zero = Data.Coerce.coerce 0.0

{-# INLINE unit #-}
unit :: Qty units
unit = Data.Coerce.coerce 1.0

infinity :: Qty units
infinity = Data.Coerce.coerce (1.0 / 0.0)

{-# INLINE coerce #-}
coerce :: Qty units1 -> Qty units2
coerce = Data.Coerce.coerce

sign :: Qty units -> Sign
sign value = if value >= zero then Positive else Negative

{-# INLINE isNaN #-}
isNaN :: Qty units -> Bool
isNaN (Qty x) = Prelude.isNaN x

{-# INLINE isInfinite #-}
isInfinite :: Qty units -> Bool
isInfinite (Qty x) = Prelude.isInfinite x

{-# INLINE squared #-}
squared :: Units.Squared units1 units2 => Qty units1 -> Qty units2
squared x = x * x

{-# INLINE squared' #-}
squared' :: Qty units -> Qty (units :*: units)
squared' x = x .*. x

sqrt :: Units.Squared units1 units2 => Qty units2 -> Qty units1
sqrt x | x <= zero = zero
sqrt (Qty x) = Qty (Prelude.sqrt x)

sqrt' :: Qty (units :*: units) -> Qty units
sqrt' x | x <= zero = zero
sqrt' (Qty x) = Qty (Prelude.sqrt x)

hypot2 :: Qty units -> Qty units -> Qty units
hypot2 x y = sqrt' (squared' x + squared' y)

hypot3 :: Qty units -> Qty units -> Qty units -> Qty units
hypot3 x y z = sqrt' (squared' x + squared' y + squared' z)

{-# INLINE abs #-}
abs :: Qty units -> Qty units
abs (Qty x) = Qty (Prelude.abs x)

clampTo :: Bounds units -> Qty units -> Qty units
clampTo bounds value = min (max (Bounds.lower bounds) value) (Bounds.upper bounds)

{-# INLINE min #-}
min :: Qty units -> Qty units -> Qty units
min = Prelude.min

{-# INLINE max #-}
max :: Qty units -> Qty units -> Qty units
max = Prelude.max

{-# INLINE minmax #-}
minmax :: (Qty units, Qty units) -> (Qty units, Qty units)
minmax (a, b) = if a <= b then (a, b) else (b, a)

smaller :: Qty units -> Qty units -> Qty units
smaller x y = if abs x <= abs y then x else y

larger :: Qty units -> Qty units -> Qty units
larger x y = if abs x >= abs y then x else y

smallerBy :: (a -> Qty units) -> a -> a -> a
smallerBy function first second =
  if abs (function first) <= abs (function second) then first else second

largerBy :: (a -> Qty units) -> a -> a -> a
largerBy function first second =
  if abs (function first) >= abs (function second) then first else second

smallest :: NonEmpty (Qty units) -> Qty units
smallest (x :| xs) = List.foldl smaller x xs

largest :: NonEmpty (Qty units) -> Qty units
largest (x :| xs) = List.foldl larger x xs

smallestBy :: (a -> Qty units) -> NonEmpty a -> a
smallestBy _ (x :| []) = x
smallestBy function (x :| xs) = go x (abs (function x)) xs
 where
  go current _ [] = current
  go current currentAbsValue (next : remaining) = do
    let nextAbsValue = abs (function next)
    if nextAbsValue < currentAbsValue
      then go next nextAbsValue remaining
      else go current currentAbsValue remaining

largestBy :: (a -> Qty units) -> NonEmpty a -> a
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
interpolateFrom :: Qty units -> Qty units -> Float -> Qty units
interpolateFrom a b t = a + (b - a) * t

{-# INLINE midpoint #-}
midpoint :: Qty units -> Qty units -> Qty units
midpoint a b = 0.5 * (a + b)

sum :: List (Qty units) -> Qty units
sum = List.foldl (+) zero

sumOf :: (a -> Qty units) -> List a -> Qty units
sumOf f list = sum (List.map f list)

convert :: Qty (units2 :/: units1) -> Qty units1 -> Qty units2
convert factor value = Units.simplify (value .*. factor)

unconvert :: Qty (units2 :/: units1) -> Qty units2 -> Qty units1
unconvert factor value = Units.simplify (value ./. factor)

random :: Qty units -> Qty units -> Random.Generator (Qty units)
random (Qty low) (Qty high) =
  Random.map Qty (Random.Generator (System.Random.uniformR (low, high)))

{-| Interpolate between two values by subdividing into the given number of steps.

The result is an empty list if the given number of steps is zero (or negative).
Otherwise, the number of values in the resulting list will be equal to one plus the number of steps.
For example, for one step the returned values will just be the given start and end values;
for two steps the returned values will be the start value, the midpoint and then the end value.
-}
steps :: Qty units -> Qty units -> Int -> List (Qty units)
steps start end n = if n > 0 then sequence start end n [0 .. n] else []

-- | Interpolate between two values like 'steps', but skip the first value.
leading :: Qty units -> Qty units -> Int -> List (Qty units)
leading start end n = sequence start end n [0 .. n - 1]

-- | Interpolate between two values like 'steps', but skip the last value.
trailing :: Qty units -> Qty units -> Int -> List (Qty units)
trailing start end n = sequence start end n [1 .. n]

-- | Interpolate between two values like 'steps', but skip the first and last values.
inBetween :: Qty units -> Qty units -> Int -> List (Qty units)
inBetween start end n = sequence start end n [1 .. n - 1]

{-| Subdivide a given range into the given number of steps, and return the midpoint of each step.

This can be useful if you want to sample a curve or other function at the midpoint of several intervals.
-}
midpoints :: Qty units -> Qty units -> Int -> List (Qty units)
midpoints start end n = sequence start end (2 * n) [1, 3 .. 2 * n - 1]

sequence :: Qty units -> Qty units -> Int -> List Int -> List (Qty units)
sequence start end n indices = let delta = end - start in [start + (i / n) * delta | i <- indices]
