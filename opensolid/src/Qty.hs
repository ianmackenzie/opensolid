module Qty
  ( Qty (Qty, Qty_)
  , zero
  , infinity
  , sign
  , isNaN
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
  , clamp
  , convert
  , unconvert
  , sum
  )
where

import Arithmetic
import Basics
import Data.Coerce qualified
import Data.List.NonEmpty (NonEmpty ((:|)))
import {-# SOURCE #-} Float (Float, fromRational)
import {-# SOURCE #-} Float qualified
import Foreign.Storable (Storable)
import List qualified
import Sign (Sign (Negative, Positive))
import Units (Unitless, (:*:), (:/:))
import Units qualified
import Prelude qualified

type role Qty phantom

type Qty :: Type -> Type
newtype Qty units = Qty_ Prelude.Double deriving (Eq, Ord, Show)

{-# COMPLETE Qty #-}

pattern Qty :: Qty Unitless -> Qty units
pattern Qty float <- Qty_ (Qty_ -> float)
  where
    Qty (Qty_ double) = Qty_ double

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
  negate (Qty_ x) = Qty_ (Prelude.negate x)

instance Multiplication Sign (Qty units) where
  type Sign .*. Qty units = Qty (Unitless :*: units)
  Positive .*. value = Units.coerce value
  Negative .*. value = Units.coerce -value

instance Product Sign (Qty units) (Qty units)

instance Multiplication (Qty units) Sign where
  type Qty units .*. Sign = Qty (units :*: Unitless)
  value .*. Positive = Units.coerce value
  value .*. Negative = Units.coerce -value

instance Product (Qty units) Sign (Qty units)

instance units ~ units_ => Addition (Qty units) (Qty units_) (Qty units) where
  {-# INLINE (+) #-}
  Qty_ x + Qty_ y = Qty_ (x Prelude.+ y)

instance units ~ units_ => Subtraction (Qty units) (Qty units_) (Qty units) where
  {-# INLINE (-) #-}
  Qty_ x - Qty_ y = Qty_ (x Prelude.- y)

instance Multiplication (Qty units1) (Qty units2) where
  type Qty units1 .*. Qty units2 = Qty (units1 :*: units2)
  {-# INLINE (.*.) #-}
  Qty_ x .*. Qty_ y = Qty_ (x Prelude.* y)

instance Units.Product units1 units2 units3 => Product (Qty units1) (Qty units2) (Qty units3)

instance Division (Qty units1) (Qty units2) where
  type Qty units1 ./. Qty units2 = Qty (units1 :/: units2)
  {-# INLINE (./.) #-}
  Qty_ x ./. Qty_ y = Qty_ (x Prelude./ y)

instance Units.Quotient units1 units2 units3 => Quotient (Qty units1) (Qty units2) (Qty units3)

instance Multiplication Int (Qty units) where
  type Int .*. Qty units = Qty (Unitless :*: units)
  {-# INLINE (.*.) #-}
  n .*. x = Float.fromInt n .*. x

instance Product Int (Qty units) (Qty units)

instance Multiplication (Qty units) Int where
  type Qty units .*. Int = Qty (units :*: Unitless)
  {-# INLINE (.*.) #-}
  x .*. n = x .*. Float.fromInt n

instance Product (Qty units) Int (Qty units)

instance Division (Qty units) Int where
  type Qty units ./. Int = Qty (units :/: Unitless)
  {-# INLINE (./.) #-}
  x ./. n = x ./. Float.fromInt n

instance Quotient (Qty units) Int (Qty units)

instance Division Int (Qty units) where
  type Int ./. Qty units = Qty (Unitless :/: units)
  {-# INLINE (./.) #-}
  n ./. x = Float.fromInt n ./. x

instance Units.Quotient Unitless units1 units2 => Quotient Int (Qty units1) (Qty units2)

instance DivMod (Qty units) where
  x // y = Prelude.floor (x / y)
  x % y = x - y * (x // y)

zero :: Qty units
zero = Data.Coerce.coerce 0.0

infinity :: Qty units
infinity = Data.Coerce.coerce (1.0 / 0.0)

sign :: Qty units -> Sign
sign value = if value >= zero then Positive else Negative

isNaN :: Qty units -> Bool
isNaN (Qty_ x) = Prelude.isNaN x

{-# INLINE squared #-}
squared :: Units.Squared units1 units2 => Qty units1 -> Qty units2
squared x = x * x

squared' :: Qty units -> Qty (units :*: units)
squared' x = x .*. x

sqrt :: Units.Squared units1 units2 => Qty units2 -> Qty units1
sqrt x | x <= Qty.zero = Qty.zero
sqrt (Qty_ x) = Qty_ (Prelude.sqrt x)

sqrt' :: Qty (units :*: units) -> Qty units
sqrt' x | x <= Qty.zero = Qty.zero
sqrt' (Qty_ x) = Qty_ (Prelude.sqrt x)

hypot2 :: Qty units -> Qty units -> Qty units
hypot2 x y = sqrt' (squared' x + squared' y)

hypot3 :: Qty units -> Qty units -> Qty units -> Qty units
hypot3 x y z = sqrt' (squared' x + squared' y + squared' z)

{-# INLINE abs #-}
abs :: Qty units -> Qty units
abs (Qty_ x) = Qty_ (Prelude.abs x)

clamp :: Qty units -> Qty units -> Qty units -> Qty units
clamp a b value = do
  let low = min a b
  let high = max a b
  if
    | value < low -> low
    | value > high -> high
    | otherwise -> value

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

interpolateFrom :: Qty units -> Qty units -> Float -> Qty units
interpolateFrom a b t =
  if t <= 0.5
    then a + (b - a) * t
    else b + (a - b) * (1.0 - t)

{-# INLINE midpoint #-}
midpoint :: Qty units -> Qty units -> Qty units
midpoint a b = 0.5 * (a + b)

sum :: List (Qty units) -> Qty units
sum = List.foldl (+) zero

convert :: Qty (units2 :/: units1) -> Qty units1 -> Qty units2
convert factor value = value !* factor

unconvert :: Qty (units2 :/: units1) -> Qty units2 -> Qty units1
unconvert factor value = value !/ factor
