module Range
  ( Range (Range)
  , unsafe
  , constant
  , from
  , hull3
  , minValue
  , maxValue
  , midpoint
  , endpoints
  , width
  , squared
  , contains
  , bisect
  , isAtomic
  , abs
  , sqrt
  , hypot2
  , hypot3
  , aggregate
  , overlaps
  , sin
  , cos
  , interpolate
  , interpolationParameter
  , any
  , all
  )
where

import Angle qualified
import Bounds (Bounds (..))
import Float qualified
import Generic qualified
import OpenSolid
import Qty qualified
import Units qualified

data Range units = Unsafe (Qty units) (Qty units)
  deriving (Eq)

{-# COMPLETE Range #-}

{-# INLINE Range #-}
pattern Range :: Qty units -> Qty units -> Range units
pattern Range low high <- Unsafe low high

deriving instance Show (Qty units) => Show (Range units)

instance Units.Coercion (Range units) (Range Unitless)

instance Negation (Range units) where
  negate (Range low high) = unsafe (negate high) (negate low)

instance Generic.Zero (Range units) where
  zero = constant Qty.zero

instance units ~ units' => Addition (Range units) (Range units') (Range units) where
  (Range low1 high1) + (Range low2 high2) = unsafe (low1 + low2) (high1 + high2)

instance units ~ units' => Addition (Range units) (Qty units') (Range units) where
  (Range low high) + value = unsafe (low + value) (high + value)

instance units ~ units' => Addition (Qty units) (Range units') (Range units) where
  value + (Range low high) = unsafe (value + low) (value + high)

instance units ~ units' => Subtraction (Range units) (Range units') (Range units) where
  (Range low1 high1) - (Range low2 high2) = unsafe (low1 - high2) (high1 - low2)

instance units ~ units' => Subtraction (Range units) (Qty units') (Range units) where
  (Range low high) - value = unsafe (low - value) (high - value)

instance units ~ units' => Subtraction (Qty units) (Range units') (Range units) where
  value - (Range low high) = unsafe (value - high) (value - low)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (Range units2) (Range units3) where
  value * Range low high = from (value * low) (value * high)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Range units1) (Qty units2) (Range units3) where
  Range low high * value = from (low * value) (high * value)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Range units1) (Range units2) (Range units3) where
  Range low1 high1 * Range low2 high2 =
    let ll = low1 * low2
        lh = low1 * high2
        hl = high1 * low2
        hh = high1 * high2
        low = min (min (min ll lh) hl) hh
        high = max (max (max ll lh) hl) hh
     in unsafe low high

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Qty units1) (Range units2) (Range units3) where
  n / Range dl dh =
    if dl > Qty.zero || dh < Qty.zero
      then unsafe (n / dh) (n / dl)
      else unsafe -Qty.infinity Qty.infinity

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Range units1) (Qty units2) (Range units3) where
  Range nl nh / d
    | d > Qty.zero = unsafe (nl / d) (nh / d)
    | d < Qty.zero = unsafe (nh / d) (nl / d)
    | otherwise = unsafe -Qty.infinity Qty.infinity

instance (Division (Qty units1) (Qty units2) (Qty units3)) => Division (Range units1) (Range units2) (Range units3) where
  (Range nl nh) / (Range dl dh)
    | dl > Qty.zero = unsafe (nl / dh) (nh / dl)
    | dh < Qty.zero = unsafe (nh / dh) (nl / dl)
    | otherwise = unsafe -Qty.infinity Qty.infinity

instance Bounds (Range units) where
  aggregate (Range low1 high1) (Range low2 high2) = unsafe (min low1 low2) (max high1 high2)

  overlaps (Range low1 high1) (Range low2 high2) = low1 <= high2 && low2 <= high1

unsafe :: Qty units -> Qty units -> Range units
unsafe = Unsafe

constant :: Qty units -> Range units
constant value = unsafe value value

from :: Qty units -> Qty units -> Range units
from a b = if a <= b then unsafe a b else unsafe b a

hull3 :: Qty units -> Qty units -> Qty units -> Range units
hull3 a b c = unsafe (min a (min b c)) (max a (max b c))

{-# INLINE minValue #-}
minValue :: Range units -> Qty units
minValue (Range low _) = low

{-# INLINE maxValue #-}
maxValue :: Range units -> Qty units
maxValue (Range _ high) = high

{-# INLINE midpoint #-}
midpoint :: Range units -> Qty units
midpoint (Range low high) = Qty.midpoint low high

endpoints :: Range units -> (Qty units, Qty units)
endpoints (Range low high) = (low, high)

{-# INLINE width #-}
width :: Range units -> Qty units
width (Range low high) = high - low

squared :: Squared (Qty units1) (Qty units2) => Range units1 -> Range units2
squared (Range low high)
  | low >= Qty.zero = unsafe ll hh
  | high <= Qty.zero = unsafe hh ll
  | otherwise = unsafe Qty.zero (max ll hh)
 where
  ll = low * low
  hh = high * high

sqrt :: Squared (Qty units1) (Qty units2) => Range units2 -> Range units1
sqrt (Range low high) =
  unsafe
    (Qty.sqrt (max low Qty.zero))
    (Qty.sqrt (max high Qty.zero))

hypot2 :: Range units -> Range units -> Range units
hypot2 (Range xMin xMax) (Range yMin yMax)
  | positiveX && positiveY = unsafe (Qty.hypot2 xMin yMin) maxMagnitude
  | positiveX && negativeY = unsafe (Qty.hypot2 xMin yMax) maxMagnitude
  | negativeX && positiveY = unsafe (Qty.hypot2 xMax yMin) maxMagnitude
  | negativeX && negativeY = unsafe (Qty.hypot2 xMax yMax) maxMagnitude
  | positiveX = unsafe xMin maxMagnitude
  | negativeX = unsafe -xMax maxMagnitude
  | positiveY = unsafe yMin maxMagnitude
  | negativeY = unsafe -yMax maxMagnitude
  | otherwise = unsafe Qty.zero maxMagnitude
 where
  positiveX = xMin >= Qty.zero
  negativeX = xMax <= Qty.zero
  positiveY = yMin >= Qty.zero
  negativeY = yMax <= Qty.zero
  xMagnitude = max (Qty.abs xMin) (Qty.abs xMax)
  yMagnitude = max (Qty.abs yMin) (Qty.abs yMax)
  maxMagnitude = Qty.hypot2 xMagnitude yMagnitude

hypot3 :: Range units -> Range units -> Range units -> Range units
hypot3 (Range xMin xMax) (Range yMin yMax) (Range zMin zMax)
  | positiveX && positiveY && positiveZ = unsafe (Qty.hypot3 xMin yMin zMin) maxMagnitude
  | positiveX && positiveY && negativeZ = unsafe (Qty.hypot3 xMin yMin zMax) maxMagnitude
  | positiveX && negativeY && positiveZ = unsafe (Qty.hypot3 xMin yMax zMin) maxMagnitude
  | positiveX && negativeY && negativeZ = unsafe (Qty.hypot3 xMin yMax zMax) maxMagnitude
  | negativeX && positiveY && positiveZ = unsafe (Qty.hypot3 xMax yMin zMin) maxMagnitude
  | negativeX && positiveY && negativeZ = unsafe (Qty.hypot3 xMax yMin zMax) maxMagnitude
  | negativeX && negativeY && positiveZ = unsafe (Qty.hypot3 xMax yMax zMin) maxMagnitude
  | negativeX && negativeY && negativeZ = unsafe (Qty.hypot3 xMax yMax zMax) maxMagnitude
  | positiveY && positiveZ = unsafe (Qty.hypot2 yMin zMin) maxMagnitude
  | positiveY && negativeZ = unsafe (Qty.hypot2 yMin zMax) maxMagnitude
  | negativeY && positiveZ = unsafe (Qty.hypot2 yMax zMin) maxMagnitude
  | negativeY && negativeZ = unsafe (Qty.hypot2 yMax zMax) maxMagnitude
  | positiveX && positiveZ = unsafe (Qty.hypot2 xMin zMin) maxMagnitude
  | positiveX && negativeZ = unsafe (Qty.hypot2 xMin zMax) maxMagnitude
  | negativeX && positiveZ = unsafe (Qty.hypot2 xMax zMin) maxMagnitude
  | negativeX && negativeZ = unsafe (Qty.hypot2 xMax zMax) maxMagnitude
  | positiveX && positiveY = unsafe (Qty.hypot2 xMin yMin) maxMagnitude
  | positiveX && negativeY = unsafe (Qty.hypot2 xMin yMax) maxMagnitude
  | negativeX && positiveY = unsafe (Qty.hypot2 xMax yMin) maxMagnitude
  | negativeX && negativeY = unsafe (Qty.hypot2 xMax yMax) maxMagnitude
  | positiveX = unsafe xMin maxMagnitude
  | negativeX = unsafe -xMax maxMagnitude
  | positiveY = unsafe yMin maxMagnitude
  | negativeY = unsafe -yMax maxMagnitude
  | otherwise = unsafe Qty.zero maxMagnitude
 where
  positiveX = xMin >= Qty.zero
  negativeX = xMax <= Qty.zero
  positiveY = yMin >= Qty.zero
  negativeY = yMax <= Qty.zero
  positiveZ = zMin >= Qty.zero
  negativeZ = zMax <= Qty.zero
  xMagnitude = max (Qty.abs xMin) (Qty.abs xMax)
  yMagnitude = max (Qty.abs yMin) (Qty.abs yMax)
  zMagnitude = max (Qty.abs zMin) (Qty.abs zMax)
  maxMagnitude = Qty.hypot3 xMagnitude yMagnitude zMagnitude

contains :: Qty units -> Range units -> Bool
contains value (Range low high) = low <= value && value <= high

bisect :: Range units -> (Range units, Range units)
bisect (Range low high) =
  let mid = Qty.midpoint low high
   in (unsafe low mid, unsafe mid high)

isAtomic :: Range units -> Bool
isAtomic (Range low high) =
  let mid = Qty.midpoint low high
   in mid == low || mid == high

abs :: Range units -> Range units
abs range@(Range low high)
  | low >= Qty.zero = range
  | high <= Qty.zero = -range
  | otherwise = unsafe Qty.zero (max high -low)

sin :: Range Radians -> Range Unitless
sin range@(Range low high) =
  let (includesMin, includesMax) = sinIncludesMinMax range
      newLow = if includesMin then -1.0 else min (Angle.sin low) (Angle.sin high)
      newHigh = if includesMax then 1.0 else max (Angle.sin low) (Angle.sin high)
   in unsafe newLow newHigh

cos :: Range Radians -> Range Unitless
cos range@(Range low high) =
  let (includesMin, includesMax) = cosIncludesMinMax range
      newLow = if includesMin then -1.0 else min (Angle.cos low) (Angle.cos high)
      newHigh = if includesMax then 1.0 else max (Angle.cos low) (Angle.cos high)
   in unsafe newLow newHigh

sinIncludesMinMax :: Range Radians -> (Bool, Bool)
sinIncludesMinMax range = cosIncludesMinMax (range - Angle.radians (Float.pi / 2))

cosIncludesMinMax :: Range Radians -> (Bool, Bool)
cosIncludesMinMax range =
  ( cosIncludesMax (range + Angle.radians Float.pi)
  , cosIncludesMax range
  )

cosIncludesMax :: Range Radians -> Bool
cosIncludesMax (Range low high) =
  let twoPi = Angle.radians (2 * Float.pi)
   in Float.floor (low / twoPi) /= Float.floor (high / twoPi)

interpolate :: Range units -> Float -> Qty units
interpolate (Range low high) t =
  Qty.interpolateFrom low high t

interpolationParameter :: Range units -> Qty units -> Float
interpolationParameter (Range low high) value
  | low < high = (value - low) / (high - low)
  | value < low = -Qty.infinity
  | value > high = Qty.infinity
  | otherwise = 0.0

any :: (Range units -> Result Indeterminate Bool) -> Range units -> Result Indeterminate Bool
any assess range =
  let assessment = assess range
   in case assessment of
        Ok _ -> assessment
        Err Indeterminate | Range.isAtomic range -> Err Indeterminate
        Err Indeterminate ->
          let (left, right) = Range.bisect range
           in case any assess left of
                Ok True -> Ok True
                Ok False -> any assess right
                Err Indeterminate ->
                  case any assess right of
                    Ok True -> Ok True
                    Ok False -> Err Indeterminate
                    Err Indeterminate -> Err Indeterminate

all :: (Range units -> Result Indeterminate Bool) -> Range units -> Result Indeterminate Bool
all assess range =
  let assessment = assess range
   in case assessment of
        Ok _ -> assessment
        Err Indeterminate | Range.isAtomic range -> Err Indeterminate
        Err Indeterminate ->
          let (left, right) = Range.bisect range
           in case all assess left of
                Ok True -> all assess right
                Ok False -> Ok False
                Err Indeterminate ->
                  case all assess right of
                    Ok True -> Err Indeterminate
                    Ok False -> Ok False
                    Err Indeterminate -> Err Indeterminate
