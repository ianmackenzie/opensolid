module Qty
  ( Qty (Qty)
  , zero
  , infinity
  , sign
  , isNaN
  , interpolateFrom
  , midpoint
  , squared
  , squared_
  , sqrt
  , sqrt_
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
import Data.Coerce (coerce)
import Data.List.NonEmpty (NonEmpty ((:|)))
import {-# SOURCE #-} Float (Float, fromRational)
import {-# SOURCE #-} Float qualified
import Foreign.Storable (Storable)
import List qualified
import Sign (Sign (Negative, Positive))
import Units (Unitless, convert, unconvert, (.*.), (:*:))
import Units qualified
import Prelude qualified

type role Qty nominal

type Qty :: Type -> Type
newtype Qty units = Qty Prelude.Double deriving (Eq, Ord, Show)

instance
  ( units1 ~ units1'
  , units2 ~ units2'
  ) =>
  Units.Coercion units1 units2 (Qty units1') (Qty units2')

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

instance Multiplication Sign (Qty units) (Qty units) where
  Positive * value = value
  Negative * value = -value

instance Multiplication (Qty units) Sign (Qty units) where
  value * Positive = value
  value * Negative = -value

instance units ~ units' => Addition (Qty units) (Qty units') (Qty units) where
  {-# INLINE (+) #-}
  Qty x + Qty y = Qty (x Prelude.+ y)

instance units ~ units' => Subtraction (Qty units) (Qty units') (Qty units) where
  {-# INLINE (-) #-}
  Qty x - Qty y = Qty (x Prelude.- y)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Qty units2) (Qty units3)
  where
  {-# INLINE (*) #-}
  (Qty x) * (Qty y) = Qty (x Prelude.* y)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Qty units2) (Qty units3)
  where
  {-# INLINE (/) #-}
  (Qty x) / (Qty y) = Qty (x Prelude./ y)

instance Multiplication Int (Qty units) (Qty units) where
  {-# INLINE (*) #-}
  n * x = Float.fromInt n * x

instance Multiplication (Qty units) Int (Qty units) where
  {-# INLINE (*) #-}
  x * n = x * Float.fromInt n

instance Division (Qty units) Int (Qty units) where
  {-# INLINE (/) #-}
  x / n = x / Float.fromInt n

instance
  Units.Quotient Unitless units1 units2 =>
  Division Int (Qty units1) (Qty units2)
  where
  {-# INLINE (/) #-}
  n / x = Float.fromInt n / x

instance DivMod (Qty units) where
  x // y = Prelude.floor (x / y)
  x % y = x - y * (x // y)

zero :: Qty units
zero = coerce 0.0

infinity :: Qty units
infinity = coerce (1.0 / 0.0)

sign :: Qty units -> Sign
sign value = if value >= zero then Positive else Negative

isNaN :: Qty units -> Bool
isNaN (Qty x) = Prelude.isNaN x

{-# INLINE squared #-}
squared :: Units.Squared units1 units2 => Qty units1 -> Qty units2
squared x = x * x

squared_ :: Qty units -> Qty (units :*: units)
squared_ x = x .*. x

sqrt :: Units.Squared units1 units2 => Qty units2 -> Qty units1
sqrt x | x <= Qty.zero = Qty.zero
sqrt (Qty x) = Qty (Prelude.sqrt x)

sqrt_ :: Qty (units :*: units) -> Qty units
sqrt_ x | x <= Qty.zero = Qty.zero
sqrt_ (Qty x) = Qty (Prelude.sqrt x)

hypot2 :: Qty units -> Qty units -> Qty units
hypot2 x y = sqrt_ (squared_ x + squared_ y)

hypot3 :: Qty units -> Qty units -> Qty units -> Qty units
hypot3 x y z = sqrt_ (squared_ x + squared_ y + squared_ z)

{-# INLINE abs #-}
abs :: Qty units -> Qty units
abs (Qty x) = Qty (Prelude.abs x)

clamp :: Qty units -> Qty units -> Qty units -> Qty units
clamp a b value
  | value < low = low
  | value > high = high
  | otherwise = value
 where
  low = min a b
  high = max a b

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
  go current currentAbsValue (next : remaining) =
    let nextAbsValue = abs (function next)
     in if nextAbsValue < currentAbsValue
          then go next nextAbsValue remaining
          else go current currentAbsValue remaining

largestBy :: (a -> Qty units) -> NonEmpty a -> a
largestBy _ (x :| []) = x
largestBy function (x :| xs) = go x (abs (function x)) xs
 where
  go current _ [] = current
  go current currentAbsValue (next : remaining) =
    let nextAbsValue = abs (function next)
     in if nextAbsValue > currentAbsValue
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
