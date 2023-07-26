module Qty
  ( Qty (..)
  , zero
  , infinity
  , sign
  , isNaN
  , interpolateFrom
  , midpoint
  , squared
  , sqrt
  , hypot2
  , hypot3
  , abs
  , min
  , max
  , minBy
  , maxBy
  , smaller
  , larger
  , smallerBy
  , largerBy
  , clamp
  , IsZero (IsZero)
  , nonZero
  , IsNegative (IsNegative)
  , nonNegative
  )
where

import Arithmetic
import Basics
import Data.Coerce (coerce)
import {-# SOURCE #-} Float (Float, fromRational)
import {-# SOURCE #-} Float qualified
import Generic qualified
import Result (ErrorMessage, Result (Error, Ok))
import Sign (Sign (..))
import {-# SOURCE #-} Tolerance (Tolerance, (~=))
import Units (Unitless)
import Units qualified
import Prelude qualified

type role Qty phantom

type Qty :: Type -> Type
newtype Qty units = Qty Prelude.Double deriving (Eq, Ord, Show)

instance (units1 ~ units1', units2 ~ units2') => Units.Coercion units1 units2 (Qty units1') (Qty units2')

instance Generic.Zero (Qty units) where
  zero = coerce 0.0

deriving newtype instance Prelude.Num Float

deriving newtype instance Prelude.Real Float

deriving newtype instance Prelude.Fractional Float

deriving newtype instance Prelude.RealFrac Float

deriving newtype instance Prelude.Floating Float

deriving newtype instance Prelude.RealFloat Float

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

instance Units.Product units1 units2 units3 => Multiplication (Qty units1) (Qty units2) (Qty units3) where
  {-# INLINE (*) #-}
  (Qty x) * (Qty y) = Qty (x Prelude.* y)

instance Units.Quotient units1 units2 units3 => Division (Qty units1) (Qty units2) (Qty units3) where
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

instance Units.Quotient Unitless units1 units2 => Division Int (Qty units1) (Qty units2) where
  {-# INLINE (/) #-}
  n / x = Float.fromInt n / x

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

sqrt :: Units.Squared units1 units2 => Qty units2 -> Qty units1
sqrt x | x <= Qty.zero = Qty.zero
sqrt (Qty x) = Qty (Prelude.sqrt x)

hypot2 :: Qty units -> Qty units -> Qty units
hypot2 x y =
  let xSquared = Qty.squared (Units.generalize x)
      ySquared = Qty.squared (Units.generalize y)
   in Units.specialize (sqrt (xSquared + ySquared))

hypot3 :: Qty units -> Qty units -> Qty units -> Qty units
hypot3 x y z =
  let xSquared = Qty.squared (Units.generalize x)
      ySquared = Qty.squared (Units.generalize y)
      zSquared = Qty.squared (Units.generalize z)
   in Units.specialize (sqrt (xSquared + ySquared + zSquared))

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

min :: Qty units -> Qty units -> Qty units
min = Generic.min

max :: Qty units -> Qty units -> Qty units
max = Generic.max

minBy :: (a -> Qty units) -> a -> a -> a
minBy = Generic.minBy

maxBy :: (a -> Qty units) -> a -> a -> a
maxBy = Generic.maxBy

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

interpolateFrom :: Qty units -> Qty units -> Float -> Qty units
interpolateFrom a b t =
  if t <= 0.5
    then a + (b - a) * t
    else b + (a - b) * (1.0 - t)

{-# INLINE midpoint #-}
midpoint :: Qty units -> Qty units -> Qty units
midpoint a b = 0.5 * (a + b)

data IsZero = IsZero deriving (Eq, Show, ErrorMessage)

nonZero :: Tolerance units => Qty units -> Result IsZero (Qty units)
nonZero value = if value ~= zero then Error IsZero else Ok value

data IsNegative = IsNegative deriving (Eq, Show, ErrorMessage)

nonNegative :: Qty units -> Result IsNegative (Qty units)
nonNegative value = if value >= zero then Ok value else Error IsNegative
