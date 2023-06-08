module Range
  ( Range (Range, minValue, maxValue)
  , unsafe
  , constant
  , from
  , unit
  , hull3
  , midpoint
  , endpoints
  , width
  , squared
  , includes
  , approximatelyIncludes
  , contains
  , tolerant
  , bisect
  , isAtomic
  , abs
  , sqrt
  , hypot2
  , hypot3
  , aggregate2
  , sin
  , cos
  , search
  , search2
  , interpolate
  , interpolationParameter
  , any
  , all
  , resolution
  , resolve
  , resolve2
  , recurse
  , recurse2
  , intersects
  , intersection
  , generator
  )
where

import Angle qualified
import Bounds (IsBounds (..))
import Float qualified
import Generic qualified
import OpenSolid
import Qty qualified
import Random qualified
import Units (Radians, Unitless)
import Units qualified

data Range units = Range# {minValue :: Qty units, maxValue :: Qty units}
  deriving (Eq, Show)

{-# COMPLETE Range #-}

{-# INLINE Range #-}
pattern Range :: Qty units -> Qty units -> Range units
pattern Range low high <- Range# low high

instance (units1 ~ units1', units2 ~ units2') => Units.Coercion units1 units2 (Range units1') (Range units2')

instance Negation (Range units) where
  negate (Range low high) = unsafe (negate high) (negate low)

instance Generic.Zero (Range units) where
  zero = constant Qty.zero

instance units ~ units' => Addition (Range units) (Range units') (Range units) where
  Range low1 high1 + Range low2 high2 = unsafe (low1 + low2) (high1 + high2)

instance units ~ units' => Addition (Range units) (Qty units') (Range units) where
  Range low high + value = unsafe (low + value) (high + value)

instance units ~ units' => Addition (Qty units) (Range units') (Range units) where
  value + Range low high = unsafe (value + low) (value + high)

instance units ~ units' => Subtraction (Range units) (Range units') (Range units) where
  Range low1 high1 - Range low2 high2 = unsafe (low1 - high2) (high1 - low2)

instance units ~ units' => Subtraction (Range units) (Qty units') (Range units) where
  Range low high - value = unsafe (low - value) (high - value)

instance units ~ units' => Subtraction (Qty units) (Range units') (Range units) where
  value - Range low high = unsafe (value - high) (value - low)

instance Units.Product units1 units2 units3 => Multiplication (Qty units1) (Range units2) (Range units3) where
  value * Range low high = from (value * low) (value * high)

instance Units.Product units1 units2 units3 => Multiplication (Range units1) (Qty units2) (Range units3) where
  Range low high * value = from (low * value) (high * value)

instance Units.Product units1 units2 units3 => Multiplication (Range units1) (Range units2) (Range units3) where
  Range low1 high1 * Range low2 high2 =
    let ll = low1 * low2
        lh = low1 * high2
        hl = high1 * low2
        hh = high1 * high2
        low = min (min (min ll lh) hl) hh
        high = max (max (max ll lh) hl) hh
     in unsafe low high

instance Units.Quotient units1 units2 units3 => Division (Qty units1) (Range units2) (Range units3) where
  n / Range dl dh =
    if dl > Qty.zero || dh < Qty.zero
      then unsafe (n / dh) (n / dl)
      else unsafe -Qty.infinity Qty.infinity

instance Units.Quotient units1 units2 units3 => Division (Range units1) (Qty units2) (Range units3) where
  Range nl nh / d
    | d > Qty.zero = unsafe (nl / d) (nh / d)
    | d < Qty.zero = unsafe (nh / d) (nl / d)
    | otherwise = unsafe -Qty.infinity Qty.infinity

instance Units.Quotient units1 units2 units3 => Division (Range units1) (Range units2) (Range units3) where
  Range nl nh / Range dl dh
    | dl > Qty.zero = unsafe (nl / dh) (nh / dl)
    | dh < Qty.zero = unsafe (nh / dh) (nl / dl)
    | otherwise = unsafe -Qty.infinity Qty.infinity

instance IsBounds (Range units) where
  aggregate2Impl = aggregate2
  intersectsImpl = intersects
  intersectionImpl = intersection

{-# INLINE unsafe #-}
unsafe :: Qty units -> Qty units -> Range units
unsafe = Range#

{-# INLINE constant #-}
constant :: Qty units -> Range units
constant value = unsafe value value

from :: Qty units -> Qty units -> Range units
from a b = if a <= b then unsafe a b else unsafe b a

unit :: Range Unitless
unit = unsafe 0.0 1.0

aggregate2 :: Range units -> Range units -> Range units
aggregate2 (Range low1 high1) (Range low2 high2) = unsafe (min low1 low2) (max high1 high2)

intersects :: Range units -> Range units -> Bool
intersects (Range low1 high1) (Range low2 high2) = low1 <= high2 && low2 <= high1

intersection :: Range units -> Range units -> Maybe (Range units)
intersection (Range low1 high1) (Range low2 high2)
  | high1 < low2 = Nothing
  | low1 > high2 = Nothing
  | otherwise = Just (unsafe (max low1 low2) (min high1 high2))

hull3 :: Qty units -> Qty units -> Qty units -> Range units
hull3 a b c = unsafe (min a (min b c)) (max a (max b c))

{-# INLINE midpoint #-}
midpoint :: Range units -> Qty units
midpoint (Range low high) = Qty.midpoint low high

instance HasField "midpoint" (Range units) (Qty units) where
  getField = midpoint

endpoints :: Range units -> (Qty units, Qty units)
endpoints (Range low high) = (low, high)

{-# INLINE width #-}
width :: Range units -> Qty units
width (Range low high) = high - low

instance HasField "width" (Range units) (Qty units) where
  getField = width

squared :: Units.Squared units1 units2 => Range units1 -> Range units2
squared (Range low high)
  | low >= Qty.zero = unsafe ll hh
  | high <= Qty.zero = unsafe hh ll
  | otherwise = unsafe Qty.zero (max ll hh)
 where
  ll = low * low
  hh = high * high

sqrt :: Units.Squared units1 units2 => Range units2 -> Range units1
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

includes :: Qty units -> Range units -> Bool
includes value (Range low high) = low <= value && value <= high

approximatelyIncludes :: Tolerance units => Qty units -> Range units -> Bool
approximatelyIncludes value (Range low high) =
  low - ?tolerance <= value && value <= high + ?tolerance

contains :: Range units -> Range units -> Bool
contains (Range low2 high2) (Range low1 high1) = low1 <= low2 && high2 <= high1

tolerant :: Tolerance units => Range units -> Range units
tolerant (Range low high) = unsafe (low - ?tolerance) (high + ?tolerance)

bisect :: Range units -> (Range units, Range units)
bisect (Range low high) =
  let mid = Qty.midpoint low high
   in (unsafe low mid, unsafe mid high)

isAtomic :: Range units -> Bool
isAtomic (Range low high) =
  let mid = Qty.midpoint low high
   in mid == low || mid == high

instance HasField "isAtomic" (Range units) Bool where
  getField = isAtomic

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
sinIncludesMinMax range = cosIncludesMinMax (range - Angle.quarterTurn)

cosIncludesMinMax :: Range Radians -> (Bool, Bool)
cosIncludesMinMax range =
  (cosIncludesMax (range + Angle.halfTurn), cosIncludesMax range)

cosIncludesMax :: Range Radians -> Bool
cosIncludesMax (Range low high) =
  Float.floor (low / Angle.fullTurn) /= Float.floor (high / Angle.fullTurn)

interpolate :: Range units -> Float -> Qty units
interpolate (Range low high) t =
  Qty.interpolateFrom low high t

interpolationParameter :: Range units -> Qty units -> Float
interpolationParameter (Range low high) value
  | low < high = (value - low) / (high - low)
  | value < low = -Qty.infinity
  | value > high = Qty.infinity
  | otherwise = 0.0

resolution :: Range units -> Float
resolution (Range low high)
  | low > Qty.zero = low / high
  | high < Qty.zero = -high / low
  | otherwise = 0.0

any :: (Range units -> Fuzzy Bool) -> Range units -> Fuzzy Bool
any assess range =
  let assessment = assess range
   in case assessment of
        Resolved _ -> assessment
        Unresolved
          | isAtomic range -> Unresolved
          | otherwise ->
              let (left, right) = Range.bisect range
               in case any assess left of
                    Resolved True -> Resolved True
                    Resolved False -> any assess right
                    Unresolved ->
                      case any assess right of
                        Resolved True -> Resolved True
                        Resolved False -> Unresolved
                        Unresolved -> Unresolved

all :: (Range units -> Fuzzy Bool) -> Range units -> Fuzzy Bool
all assess range =
  let assessment = assess range
   in case assessment of
        Resolved _ -> assessment
        Unresolved
          | isAtomic range -> Unresolved
          | otherwise ->
              let (left, right) = Range.bisect range
               in case all assess left of
                    Resolved True -> all assess right
                    Resolved False -> Resolved False
                    Unresolved ->
                      case all assess right of
                        Resolved True -> Unresolved
                        Resolved False -> Resolved False
                        Unresolved -> Unresolved

search :: (Range units -> Bool) -> Range units -> List (Qty units)
search predicate range = searchImpl predicate range []

searchImpl :: (Range units -> Bool) -> Range units -> List (Qty units) -> List (Qty units)
searchImpl predicate range accumulated =
  case predicate range of
    False -> accumulated
    True
      | isAtomic range -> range.minValue : accumulated
      | otherwise ->
          let (left, right) = bisect range
           in accumulated
                |> searchImpl predicate right
                |> searchImpl predicate left

search2
  :: (Range units1 -> Range units2 -> Bool)
  -> Range units1
  -> Range units2
  -> List (Qty units1, Qty units2)
search2 predicate u v = search2Impl predicate u v []

search2Impl
  :: (Range units1 -> Range units2 -> Bool)
  -> Range units1
  -> Range units2
  -> List (Qty units1, Qty units2)
  -> List (Qty units1, Qty units2)
search2Impl predicate u v accumulated =
  case predicate u v of
    False -> accumulated
    True
      | isAtomic u && isAtomic v -> (u.minValue, v.minValue) : accumulated
      | isAtomic u ->
          let (left, right) = bisect v
           in accumulated
                |> search2Impl predicate u right
                |> search2Impl predicate u left
      | isAtomic v ->
          let (left, right) = bisect u
           in accumulated
                |> search2Impl predicate right v
                |> search2Impl predicate left v
      | otherwise ->
          let (leftU, rightU) = bisect u
              (leftV, rightV) = bisect v
           in accumulated
                |> search2Impl predicate rightU rightV
                |> search2Impl predicate rightU leftV
                |> search2Impl predicate leftU rightV
                |> search2Impl predicate leftU leftV

resolve :: (Range units -> Fuzzy a) -> Range units -> Fuzzy (List a)
resolve predicate range = resolveImpl predicate range []

resolveImpl
  :: (Range units -> Fuzzy a)
  -> Range units
  -> List a
  -> Fuzzy (List a)
resolveImpl predicate range accumulated =
  case predicate range of
    Resolved resolved -> Resolved (resolved : accumulated)
    Unresolved
      | isAtomic range -> Unresolved
      | otherwise -> do
          let (left, right) = bisect range
          resultsR <- resolveImpl predicate right accumulated
          resolveImpl predicate left resultsR

resolve2
  :: (Range units1 -> Range units2 -> Fuzzy a)
  -> Range units1
  -> Range units2
  -> Fuzzy (List a)
resolve2 predicate range1 range2 = resolve2Impl predicate range1 range2 []

resolve2Impl
  :: (Range units1 -> Range units2 -> Fuzzy a)
  -> Range units1
  -> Range units2
  -> List a
  -> Fuzzy (List a)
resolve2Impl predicate range1 range2 accumulated =
  case predicate range1 range2 of
    Resolved resolved -> Resolved (resolved : accumulated)
    Unresolved
      | isAtomic range1 && isAtomic range2 -> Unresolved
      | isAtomic range1 -> do
          let (left, right) = bisect range2
          resultsR <- resolve2Impl predicate range1 right accumulated
          resolve2Impl predicate range1 left resultsR
      | isAtomic range2 -> do
          let (left, right) = bisect range1
          resultsR <- resolve2Impl predicate right range2 accumulated
          resolve2Impl predicate left range2 resultsR
      | otherwise -> do
          let (left1, right1) = bisect range1
          let (left2, right2) = bisect range2
          resultsRR <- resolve2Impl predicate right1 right2 accumulated
          resultsRL <- resolve2Impl predicate right1 left2 resultsRR
          resultsLR <- resolve2Impl predicate left1 right2 resultsRL
          resolve2Impl predicate left1 left2 resultsLR

recurse :: (Range units -> a -> a) -> Range units -> a -> a
recurse callback range accumulated =
  let (left, right) = bisect range
   in accumulated
        |> callback right
        |> callback left

recurse2 :: (Range units1 -> Range units2 -> a -> a) -> Range units1 -> Range units2 -> a -> a
recurse2 callback range1 range2 accumulated =
  let (left1, right1) = bisect range1
      (left2, right2) = bisect range2
   in accumulated
        |> callback right1 right2
        |> callback right1 left2
        |> callback left1 right2
        |> callback left1 left2

generator :: Random.Generator (Qty units) -> Random.Generator (Range units)
generator qtyGenerator = do
  a <- qtyGenerator
  b <- qtyGenerator
  Random.return (Range.from a b)
