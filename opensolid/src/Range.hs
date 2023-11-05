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
  , includes
  , approximatelyIncludes
  , contains
  , isContainedIn
  , tolerant
  , bisect
  , isAtomic
  , abs
  , sqrt
  , hypot2
  , hypot3
  , aggregate2
  , aggregate3
  , min
  , max
  , minimum
  , maximum
  , smaller
  , larger
  , smallest
  , largest
  , sin
  , cos
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
  , find
  , find2
  , sample
  )
where

import Angle qualified
import Bounds (IsBounds (..))
import Float qualified
import Generic qualified
import Maybe qualified
import NonEmpty qualified
import OpenSolid
import Qty qualified
import Quadrature qualified
import Random qualified
import Units qualified

data Range units = Range_ (Qty units) (Qty units)
  deriving (Eq, Show)

{-# COMPLETE Range #-}

{-# INLINE Range #-}
pattern Range :: Qty units -> Qty units -> Range units
pattern Range low high <- Range_ low high

instance
  ( units1 ~ units1'
  , units2 ~ units2'
  ) =>
  Units.Coercion units1 units2 (Range units1') (Range units2')

instance (units ~ units') => ApproximateEquality (Range units) (Qty units') units where
  Range low high ~= value = low ~= value && high ~= value

instance (units ~ units') => ApproximateEquality (Qty units) (Range units') units where
  value ~= range = range ~= value

instance (units ~ units') => Intersects (Qty units) (Range units') units where
  value ^ (Range low high) = value >= low - ?tolerance && value <= high + ?tolerance

instance (units ~ units') => Intersects (Range units) (Qty units') units where
  range ^ value = value ^ range

instance (units ~ units') => Intersects (Range units) (Range units') units where
  Range low1 high1 ^ Range low2 high2 = low1 <= high2 + ?tolerance && high1 >= low2 - ?tolerance

instance Negation (Range units) where
  negate (Range low high) = unsafe (negate high) (negate low)

instance Multiplication Sign (Range units) (Range units) where
  Positive * range = range
  Negative * range = -range

instance Multiplication (Range units) Sign (Range units) where
  range * Positive = range
  range * Negative = -range

instance Generic.HasZero (Range units) where
  zeroImpl = constant Qty.zero

instance (units ~ units') => Addition (Range units) (Range units') (Range units) where
  Range low1 high1 + Range low2 high2 = unsafe (low1 + low2) (high1 + high2)

instance (units ~ units') => Addition (Range units) (Qty units') (Range units) where
  Range low high + value = unsafe (low + value) (high + value)

instance (units ~ units') => Addition (Qty units) (Range units') (Range units) where
  value + Range low high = unsafe (value + low) (value + high)

instance (units ~ units') => Subtraction (Range units) (Range units') (Range units) where
  Range low1 high1 - Range low2 high2 = unsafe (low1 - high2) (high1 - low2)

instance (units ~ units') => Subtraction (Range units) (Qty units') (Range units) where
  Range low high - value = unsafe (low - value) (high - value)

instance (units ~ units') => Subtraction (Qty units) (Range units') (Range units) where
  value - Range low high = unsafe (value - high) (value - low)

instance (Units.Product units1 units2 units3) => Multiplication (Qty units1) (Range units2) (Range units3) where
  value * Range low high = from (value * low) (value * high)

instance (Units.Product units1 units2 units3) => Multiplication (Range units1) (Qty units2) (Range units3) where
  Range low high * value = from (low * value) (high * value)

instance (Units.Product units1 units2 units3) => Multiplication (Range units1) (Range units2) (Range units3) where
  Range low1 high1 * Range low2 high2 =
    let ll = low1 * low2
        lh = low1 * high2
        hl = high1 * low2
        hh = high1 * high2
        low = Qty.min (Qty.min (Qty.min ll lh) hl) hh
        high = Qty.max (Qty.max (Qty.max ll lh) hl) hh
     in unsafe low high

instance (Units.Quotient units1 units2 units3) => Division (Qty units1) (Range units2) (Range units3) where
  n / Range dl dh =
    if dl > Qty.zero || dh < Qty.zero
      then unsafe (n / dh) (n / dl)
      else unsafe -Qty.infinity Qty.infinity

instance (Units.Quotient units1 units2 units3) => Division (Range units1) (Qty units2) (Range units3) where
  Range nl nh / d
    | d > Qty.zero = unsafe (nl / d) (nh / d)
    | d < Qty.zero = unsafe (nh / d) (nl / d)
    | otherwise = unsafe -Qty.infinity Qty.infinity

instance (Units.Quotient units1 units2 units3) => Division (Range units1) (Range units2) (Range units3) where
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
unsafe = Range_

{-# INLINE constant #-}
constant :: Qty units -> Range units
constant value = unsafe value value

from :: Qty units -> Qty units -> Range units
from a b = if a <= b then unsafe a b else unsafe b a

aggregate2 :: Range units -> Range units -> Range units
aggregate2 (Range low1 high1) (Range low2 high2) =
  unsafe (Qty.min low1 low2) (Qty.max high1 high2)

aggregate3 :: Range units -> Range units -> Range units -> Range units
aggregate3 (Range low1 high1) (Range low2 high2) (Range low3 high3) =
  unsafe (Qty.min (Qty.min low1 low2) low3) (Qty.max (Qty.max high1 high2) high3)

intersects :: Range units -> Range units -> Bool
intersects (Range low1 high1) (Range low2 high2) = low1 <= high2 && low2 <= high1

intersection :: Range units -> Range units -> Maybe (Range units)
intersection (Range low1 high1) (Range low2 high2)
  | high1 < low2 = Nothing
  | low1 > high2 = Nothing
  | otherwise = Just (unsafe (Qty.max low1 low2) (Qty.min high1 high2))

hull3 :: Qty units -> Qty units -> Qty units -> Range units
hull3 a b c = unsafe (Qty.min a (Qty.min b c)) (Qty.max a (Qty.max b c))

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

{-# INLINE maxAbs #-}
maxAbs :: Range units -> Qty units
maxAbs (Range low high) = Qty.max (Qty.abs low) (Qty.abs high)

{-# INLINE minAbs #-}
minAbs :: Range units -> Qty units
minAbs (Range low high)
  | low >= Qty.zero = low
  | high <= Qty.zero = -high
  | otherwise = Qty.zero

squared :: (Units.Squared units1 units2) => Range units1 -> Range units2
squared (Range low high)
  | low >= Qty.zero = unsafe ll hh
  | high <= Qty.zero = unsafe hh ll
  | otherwise = unsafe Qty.zero (Qty.max ll hh)
 where
  ll = low * low
  hh = high * high

sqrt :: (Units.Squared units1 units2) => Range units2 -> Range units1
sqrt (Range low high) =
  unsafe
    (Qty.sqrt (Qty.max low Qty.zero))
    (Qty.sqrt (Qty.max high Qty.zero))

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
  xMagnitude = Qty.max (Qty.abs xMin) (Qty.abs xMax)
  yMagnitude = Qty.max (Qty.abs yMin) (Qty.abs yMax)
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
  xMagnitude = Qty.max (Qty.abs xMin) (Qty.abs xMax)
  yMagnitude = Qty.max (Qty.abs yMin) (Qty.abs yMax)
  zMagnitude = Qty.max (Qty.abs zMin) (Qty.abs zMax)
  maxMagnitude = Qty.hypot3 xMagnitude yMagnitude zMagnitude

includes :: Qty units -> Range units -> Bool
includes value (Range low high) = low <= value && value <= high

approximatelyIncludes :: (Tolerance units) => Qty units -> Range units -> Bool
approximatelyIncludes value (Range low high) =
  low - ?tolerance <= value && value <= high + ?tolerance

contains :: Range units -> Range units -> Bool
contains (Range low2 high2) (Range low1 high1) = low1 <= low2 && high2 <= high1

isContainedIn :: Range units -> Range units -> Bool
isContainedIn = flip contains

tolerant :: (Tolerance units) => Range units -> Range units
tolerant (Range low high) = unsafe (low - ?tolerance) (high + ?tolerance)

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
  | otherwise = unsafe Qty.zero (Qty.max high -low)

min :: Range units -> Range units -> Range units
min (Range low1 high1) (Range low2 high2) =
  unsafe (Qty.min low1 low2) (Qty.min high1 high2)

max :: Range units -> Range units -> Range units
max (Range low1 high1) (Range low2 high2) =
  unsafe (Qty.max low1 low2) (Qty.max high1 high2)

smaller :: Range units -> Range units -> Range units
smaller first second
  | high1 < low2 = first
  | high2 < low1 = second
  | otherwise =
      let (Range aggregateMin aggregateMax) = aggregate2 first second
          high = Qty.min high1 high2
       in unsafe (Qty.max -high aggregateMin) (Qty.min aggregateMax high)
 where
  (Range low1 high1) = abs first
  (Range low2 high2) = abs second

larger :: Range units -> Range units -> Range units
larger first second
  | low1 > high2 = first
  | low2 > high1 = second
  | aggregateMin > -low = unsafe (Qty.max aggregateMin low) aggregateMax
  | aggregateMax < low = unsafe aggregateMin (Qty.min aggregateMax -low)
  | otherwise = aggregate
 where
  (Range low1 high1) = abs first
  (Range low2 high2) = abs second
  low = Qty.max low1 low2
  aggregate@(Range aggregateMin aggregateMax) = aggregate2 first second

minimum :: NonEmpty (Range units) -> Range units
minimum = NonEmpty.reduceLeft min

maximum :: NonEmpty (Range units) -> Range units
maximum = NonEmpty.reduceLeft max

smallest :: NonEmpty (Range units) -> Range units
smallest ranges =
  let initial = NonEmpty.minimumBy maxAbs ranges
      clipRadius = maxAbs initial
      conditionalAggregate accumulated (Range low high)
        | low > clipRadius || high < -clipRadius = accumulated
        | otherwise = aggregate2 accumulated (unsafe (Qty.max low -clipRadius) (Qty.min high clipRadius))
   in NonEmpty.foldLeft conditionalAggregate initial ranges

largest :: NonEmpty (Range units) -> Range units
largest ranges =
  let initial = NonEmpty.maximumBy minAbs ranges
      clipRadius = minAbs initial
      conditionalAggregate accumulated range@(Range low high)
        | low > -clipRadius && high < clipRadius = accumulated
        | low > -clipRadius = aggregate2 accumulated (unsafe clipRadius high)
        | high < clipRadius = aggregate2 accumulated (unsafe low -clipRadius)
        | otherwise = aggregate2 accumulated range
   in NonEmpty.foldLeft conditionalAggregate initial ranges

sin :: Range Radians -> Range Unitless
sin range@(Range low high) =
  let (includesMin, includesMax) = sinIncludesMinMax range
      newLow = if includesMin then -1.0 else Qty.min (Angle.sin low) (Angle.sin high)
      newHigh = if includesMax then 1.0 else Qty.max (Angle.sin low) (Angle.sin high)
   in unsafe newLow newHigh

cos :: Range Radians -> Range Unitless
cos range@(Range low high) =
  let (includesMin, includesMax) = cosIncludesMinMax range
      newLow = if includesMin then -1.0 else Qty.min (Angle.cos low) (Angle.cos high)
      newHigh = if includesMax then 1.0 else Qty.max (Angle.cos low) (Angle.cos high)
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

any :: (Range units -> Fuzzy Bool) -> Range units -> Bool
any assess range =
  case assess range of
    Resolved assessment -> assessment
    Unresolved
      | isAtomic range -> False
      | otherwise ->
          let (left, right) = Range.bisect range
           in any assess left || any assess right

all :: (Range units -> Fuzzy Bool) -> Range units -> Bool
all assess range =
  case assess range of
    Resolved assessment -> assessment
    Unresolved
      | isAtomic range -> True
      | otherwise ->
          let (left, right) = Range.bisect range
           in all assess left && all assess right

find :: (Range units -> Bool) -> Range units -> Maybe (Qty units)
find isCandidate range =
  case isCandidate range of
    False -> Nothing
    True
      | isAtomic range -> Just (maxValue range)
      | otherwise ->
          let (left, right) = bisect range
              leftResult = find isCandidate left
           in case leftResult of
                Just _ -> leftResult
                Nothing -> find isCandidate right

find2 ::
  (Range units1 -> Range units2 -> Bool) ->
  Range units1 ->
  Range units2 ->
  Maybe (Qty units1, Qty units2)
find2 isCandidate u v =
  case isCandidate u v of
    False -> Nothing
    True
      | isAtomic u && isAtomic v -> Just (maxValue u, maxValue v)
      | isAtomic u -> Maybe.map (maxValue u,) (find (isCandidate u) v)
      | isAtomic v -> Maybe.map (,maxValue v) (find (\u' -> isCandidate u' v) u)
      | otherwise ->
          let (u1, u2) = bisect u
              (v1, v2) = bisect v
           in case find2 isCandidate u1 v1 of
                result@(Just _) -> result
                Nothing ->
                  case find2 isCandidate u1 v2 of
                    result@(Just _) -> result
                    Nothing ->
                      case find2 isCandidate u2 v1 of
                        result@(Just _) -> result
                        Nothing ->
                          find2 isCandidate u2 v2

resolve :: (Range units -> Fuzzy a) -> Range units -> Fuzzy (List a)
resolve predicate range = resolveImpl predicate range []

resolveImpl ::
  (Range units -> Fuzzy a) ->
  Range units ->
  List a ->
  Fuzzy (List a)
resolveImpl predicate range accumulated =
  case predicate range of
    Resolved resolved -> Resolved (resolved : accumulated)
    Unresolved
      | isAtomic range -> Unresolved
      | otherwise -> do
          let (left, right) = bisect range
          resultsR <- resolveImpl predicate right accumulated
          resolveImpl predicate left resultsR

resolve2 ::
  (Range units1 -> Range units2 -> Fuzzy a) ->
  Range units1 ->
  Range units2 ->
  Fuzzy (List a)
resolve2 predicate range1 range2 = resolve2Impl predicate range1 range2 []

resolve2Impl ::
  (Range units1 -> Range units2 -> Fuzzy a) ->
  Range units1 ->
  Range units2 ->
  List a ->
  Fuzzy (List a)
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
recurse callback (Range low high) accumulated =
  let mid = Qty.midpoint low high
   in accumulated
        |> callback (unsafe mid high)
        |> callback (unsafe low mid)

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

{- | Sample a given function at five "random" points within a given range.
This can be useful for checking whether some property (likely) holds true everywhere within a range,
for example:

- Is this function zero everywhere?
- Do these two curves overlap? (Does every point on the first curve lie on the second curve?)

The sampling points are based on Gaussian quadrature. It is of course possible that (for example)
a function could be zero at the five sampling points but non-zero elsewhere,
but should be extremely unlikely.
-}
sample :: (Qty units -> a) -> Range units -> List a
sample function range =
  [ function (Range.interpolate range Quadrature.t1)
  , function (Range.interpolate range Quadrature.t2)
  , function (Range.interpolate range Quadrature.t3)
  , function (Range.interpolate range Quadrature.t4)
  , function (Range.interpolate range Quadrature.t5)
  ]
