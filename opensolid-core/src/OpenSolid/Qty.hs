module OpenSolid.Qty
  ( Qty (Qty)
  , zero
  , unit
  , infinity
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
  )
where

import Data.Coerce qualified
import Data.List.NonEmpty (NonEmpty ((:|)))
import Foreign.Storable (Storable)
import OpenSolid.Arithmetic
import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Float (Float, fromRational)
import {-# SOURCE #-} OpenSolid.Float qualified as Float
import OpenSolid.List qualified as List
import OpenSolid.Random.Internal qualified as Random
import {-# SOURCE #-} OpenSolid.Range (Range)
import {-# SOURCE #-} OpenSolid.Range qualified as Range
import OpenSolid.Sign (Sign (Negative, Positive))
import OpenSolid.Units (Unitless, (:*:), (:/:))
import OpenSolid.Units qualified as Units
import System.Random qualified
import Prelude qualified

type role Qty phantom

type Qty :: Type -> Type
newtype Qty units = Qty Prelude.Double deriving (Eq, Ord, Show)

deriving newtype instance Prelude.Num Float

deriving newtype instance Prelude.Real Float

deriving newtype instance Prelude.Fractional Float

deriving newtype instance Prelude.RealFrac Float

deriving newtype instance Prelude.Floating Float

deriving newtype instance Prelude.RealFloat Float

deriving newtype instance Prelude.Read Float

deriving newtype instance Storable (Qty units)

instance Negation (Qty units) where
  {-# INLINE negate #-}
  negate (Qty x) = Qty (Prelude.negate x)

instance Multiplication' Sign (Qty units) (Qty (Unitless :*: units)) where
  {-# INLINEABLE (.*.) #-}
  Positive .*. value = Units.coerce value
  Negative .*. value = Units.coerce -value

instance Multiplication Sign (Qty units) (Qty units) where
  {-# INLINEABLE (*) #-}
  Positive * value = value
  Negative * value = -value

instance Multiplication' (Qty units) Sign (Qty (units :*: Unitless)) where
  {-# INLINEABLE (.*.) #-}
  value .*. Positive = Units.coerce value
  value .*. Negative = Units.coerce -value

instance Multiplication (Qty units) Sign (Qty units) where
  {-# INLINEABLE (*) #-}
  value * Positive = value
  value * Negative = -value

instance units ~ units_ => Addition (Qty units) (Qty units_) (Qty units) where
  {-# INLINE (+) #-}
  Qty x + Qty y = Qty (x Prelude.+ y)

instance units ~ units_ => Subtraction (Qty units) (Qty units_) (Qty units) where
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

clampTo :: Range units -> Qty units -> Qty units
clampTo range value = min (max (Range.lowerBound range) value) (Range.upperBound range)

{-# INLINE min #-}
min :: Qty units -> Qty units -> Qty units
min = Prelude.min

{-# INLINE max #-}
max :: Qty units -> Qty units -> Qty units
max = Prelude.max

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
convert factor value = value !* factor

unconvert :: Qty (units2 :/: units1) -> Qty units2 -> Qty units1
unconvert factor value = value !/ factor

random :: Qty units -> Qty units -> Random.Generator (Qty units)
random (Qty low) (Qty high) =
  Random.map Qty (Random.Generator (System.Random.uniformR (low, high)))
