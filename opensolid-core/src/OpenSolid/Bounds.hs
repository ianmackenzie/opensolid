module OpenSolid.Bounds
  ( Bounds (Bounds)
  , constant
  , unitInterval
  , coerce
  , zeroTo
  , symmetric
  , infinite
  , hull3
  , hull4
  , hullN
  , lower
  , upper
  , midpoint
  , endpoints
  , width
  , maxAbs
  , squared
  , squared'
  , includes
  , inclusion
  , exclusion
  , contains
  , overlap
  , separation
  , isContainedIn
  , bisect
  , isAtomic
  , isFinite
  , abs
  , sqrt
  , sqrt'
  , hypot2
  , hypot3
  , aggregate2
  , aggregate3
  , aggregateN
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
  , resolve
  , resolution
  , isResolved
  , resolvedSign
  , intersection
  , random
  , sampleValues
  , convert
  , unconvert
  )
where

import Data.Coerce qualified
import OpenSolid.Angle qualified as Angle
import OpenSolid.Debug qualified as Debug
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Float qualified as Float
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import {-# SOURCE #-} OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Random qualified as Random
import OpenSolid.Units qualified as Units

type role Bounds phantom

type Bounds :: Type -> Type

-- | A range of possible values, with a lower bound and upper bound.
data Bounds units = Ordered (Qty units) (Qty units)
  deriving (Eq)

instance HasField "endpoints" (Bounds units) (Qty units, Qty units) where
  getField = endpoints

instance HasField "lower" (Bounds units) (Qty units) where
  getField = lower

instance HasField "upper" (Bounds units) (Qty units) where
  getField = upper

deriving instance Show (Qty units) => Show (Bounds units)

{-# COMPLETE Bounds #-}

{-| Construct a bounding range from two given values (endpoints).

The order of the two arguments does not matter;
the minimum of the two will be used as the lower bound
and the maximum will be used as the upper bound.

If either argument is NaN, then the result will be open/infinite
(with endpoints negative infinity and positive infinity).
-}
{-# INLINE Bounds #-}
pattern Bounds :: Qty units -> Qty units -> Bounds units
pattern Bounds low high <- Ordered low high
  where
    Bounds a b
      | a <= b = Ordered a b
      | b <= a = Ordered b a
      | otherwise = infinite

instance FFI (Bounds Unitless) where
  representation = FFI.classRepresentation "Bounds"

instance FFI (Bounds Radians) where
  representation = FFI.classRepresentation "AngleBounds"

instance FFI (Bounds Meters) where
  representation = FFI.classRepresentation "LengthBounds"

instance FFI (Bounds SquareMeters) where
  representation = FFI.classRepresentation "AreaBounds"

instance HasUnits (Bounds units) units (Bounds Unitless)

instance Units.Coercion (Bounds unitsA) (Bounds unitsB) where
  coerce = Data.Coerce.coerce

instance units1 ~ units2 => ApproximateEquality (Bounds units1) (Qty units2) units1 where
  Bounds low high ~= value = low >= value - ?tolerance && high <= value + ?tolerance

instance units1 ~ units2 => ApproximateEquality (Qty units1) (Bounds units2) units1 where
  value ~= bounds = bounds ~= value

instance units1 ~ units2 => Intersects (Qty units1) (Bounds units2) units1 where
  value ^ bounds = exclusion value bounds <= ?tolerance

instance units1 ~ units2 => Intersects (Bounds units1) (Qty units2) units1 where
  bounds ^ value = value ^ bounds

instance units1 ~ units2 => Intersects (Bounds units1) (Bounds units2) units1 where
  first ^ second = separation first second <= ?tolerance

instance Negation (Bounds units) where
  negate (Bounds low high) = Bounds (negate high) (negate low)

instance Multiplication Sign (Bounds units) (Bounds units) where
  Positive * bounds = bounds
  Negative * bounds = -bounds

instance Multiplication (Bounds units) Sign (Bounds units) where
  bounds * Positive = bounds
  bounds * Negative = -bounds

instance units1 ~ units2 => Addition (Bounds units1) (Bounds units2) (Bounds units1) where
  Bounds low1 high1 + Bounds low2 high2 = Bounds (low1 + low2) (high1 + high2)

instance units1 ~ units2 => Addition (Bounds units1) (Qty units2) (Bounds units1) where
  Bounds low high + value = Bounds (low + value) (high + value)

instance units1 ~ units2 => Addition (Qty units1) (Bounds units2) (Bounds units1) where
  value + Bounds low high = Bounds (value + low) (value + high)

instance units1 ~ units2 => Subtraction (Bounds units1) (Bounds units2) (Bounds units1) where
  Bounds low1 high1 - Bounds low2 high2 = Bounds (low1 - high2) (high1 - low2)

instance units1 ~ units2 => Subtraction (Bounds units1) (Qty units2) (Bounds units1) where
  Bounds low high - value = Bounds (low - value) (high - value)

instance units1 ~ units2 => Subtraction (Qty units1) (Bounds units2) (Bounds units1) where
  value - Bounds low high = Bounds (value - high) (value - low)

instance Multiplication' (Qty units1) (Bounds units2) (Bounds (units1 :*: units2)) where
  value .*. Bounds low high = from (value .*. low) (value .*. high)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Bounds units2) (Bounds units3)
  where
  value * Bounds low high = from (value * low) (value * high)

instance Multiplication' (Bounds units1) (Qty units2) (Bounds (units1 :*: units2)) where
  Bounds low high .*. value = from (low .*. value) (high .*. value)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (Qty units2) (Bounds units3)
  where
  Bounds low high * value = from (low * value) (high * value)

instance Multiplication' (Bounds units1) (Bounds units2) (Bounds (units1 :*: units2)) where
  Bounds low1 high1 .*. Bounds low2 high2 =
    hull4 (low1 .*. low2) (low1 .*. high2) (high1 .*. low2) (high1 .*. high2)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (Bounds units2) (Bounds units3)
  where
  Bounds low1 high1 * Bounds low2 high2 =
    hull4 (low1 * low2) (low1 * high2) (high1 * low2) (high1 * high2)

instance Division' (Qty units1) (Bounds units2) (Bounds (units1 :/: units2)) where
  n ./. Bounds dl dh =
    if dl > Qty.zero || dh < Qty.zero
      then from (n ./. dl) (n ./. dh)
      else infinite

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Bounds units2) (Bounds units3)
  where
  n / Bounds dl dh =
    if dl > Qty.zero || dh < Qty.zero
      then from (n / dl) (n / dh)
      else infinite

instance Division' (Bounds units1) (Qty units2) (Bounds (units1 :/: units2)) where
  Bounds nl nh ./. d =
    if d /= Qty.zero
      then from (nl ./. d) (nh ./. d)
      else infinite

instance
  Units.Quotient units1 units2 units3 =>
  Division (Bounds units1) (Qty units2) (Bounds units3)
  where
  Bounds nl nh / d =
    if d /= Qty.zero
      then from (nl / d) (nh / d)
      else infinite

instance Division' (Bounds units1) (Bounds units2) (Bounds (units1 :/: units2)) where
  Bounds nl nh ./. Bounds dl dh =
    if dl > Qty.zero || dh < Qty.zero
      then hull4 (nl ./. dl) (nl ./. dh) (nh ./. dl) (nh ./. dh)
      else infinite

instance
  Units.Quotient units1 units2 units3 =>
  Division (Bounds units1) (Bounds units2) (Bounds units3)
  where
  Bounds nl nh / Bounds dl dh =
    if dl > Qty.zero || dh < Qty.zero
      then hull4 (nl / dl) (nl / dh) (nh / dl) (nh / dh)
      else infinite

-- | Construct a zero-width bounding range containing a single value.
{-# INLINE constant #-}
constant :: Qty units -> Bounds units
constant value = Bounds value value

-- | The bounding range with endoints [0,1].
unitInterval :: Bounds Unitless
unitInterval = Bounds 0.0 1.0

{-# INLINE coerce #-}
coerce :: Bounds units1 -> Bounds units2
coerce = Data.Coerce.coerce

{-| Construct a range from its lower and upper bounds.

The order of the two arguments does not matter;
the minimum of the two will be used as the lower bound of the range
and the maximum will be used as the upper bound.
-}
{-# INLINE from #-}
from :: Qty units -> Qty units -> Bounds units
from = Bounds

-- | Create a bounding range with zero as one of its endpoints and the given value as the other.
zeroTo :: Qty units -> Bounds units
zeroTo value = Bounds Qty.zero value

{-| Create a bounding range symmetric about zero, with the given width.

The lower bound of the range will be -w/2 and the upper bound will be w/2.
-}
symmetric :: "width" ::: Qty units -> Bounds units
symmetric (Field w) = let r = 0.5 * w in Bounds -r r

infinite :: Bounds units
infinite = Bounds -Qty.infinity Qty.infinity

aggregate2 :: Bounds units -> Bounds units -> Bounds units
aggregate2 (Bounds low1 high1) (Bounds low2 high2) =
  Bounds (Qty.min low1 low2) (Qty.max high1 high2)

aggregate3 :: Bounds units -> Bounds units -> Bounds units -> Bounds units
aggregate3 (Bounds low1 high1) (Bounds low2 high2) (Bounds low3 high3) =
  Bounds (Qty.min (Qty.min low1 low2) low3) (Qty.max (Qty.max high1 high2) high3)

-- | Build a bounding range containing all ranges in the given non-empty list.
aggregateN :: NonEmpty (Bounds units) -> Bounds units
aggregateN (Bounds low1 high1 :| rest) = do
  let go low high [] = Bounds low high
      go low high (Bounds nextLow nextHigh : remaining) =
        go (Qty.min low nextLow) (Qty.max high nextHigh) remaining
  go low1 high1 rest

-- | Attempt to find the intersection of two bounding ranges.
intersection :: Bounds units -> Bounds units -> Maybe (Bounds units)
intersection (Bounds low1 high1) (Bounds low2 high2)
  | high1 < low2 = Nothing
  | low1 > high2 = Nothing
  | otherwise = Just (Bounds (Qty.max low1 low2) (Qty.min high1 high2))

{-# INLINE hull3 #-}
hull3 :: Qty units -> Qty units -> Qty units -> Bounds units
hull3 a b c = Bounds (Qty.min a (Qty.min b c)) (Qty.max a (Qty.max b c))

{-# INLINE hull4 #-}
hull4 :: Qty units -> Qty units -> Qty units -> Qty units -> Bounds units
hull4 a b c d = Bounds (Qty.min a (Qty.min b (Qty.min c d))) (Qty.max a (Qty.max b (Qty.max c d)))

-- | Build a bounding range containing all values in the given non-empty list.
hullN :: NonEmpty (Qty units) -> Bounds units
hullN (first :| rest) = do
  let go low high [] = Bounds low high
      go low high (next : remaining) = go (Qty.min low next) (Qty.max high next) remaining
  go first first rest

-- | Get the lower bound of a range.
{-# INLINE lower #-}
lower :: Bounds units -> Qty units
lower (Bounds low _) = low

-- | Get the upper bound of a range.
{-# INLINE upper #-}
upper :: Bounds units -> Qty units
upper (Bounds _ high) = high

{-# INLINE midpoint #-}
midpoint :: Bounds units -> Qty units
midpoint (Bounds low high) = Qty.midpoint low high

-- | Get the lower and upper bounds of a range.
endpoints :: Bounds units -> (Qty units, Qty units)
endpoints (Bounds low high) = (low, high)

{-# INLINE width #-}
width :: Bounds units -> Qty units
width (Bounds low high) = high - low

{-# INLINE maxAbs #-}
maxAbs :: Bounds units -> Qty units
maxAbs (Bounds low high) = Qty.max (Qty.abs low) (Qty.abs high)

{-# INLINE minAbs #-}
minAbs :: Bounds units -> Qty units
minAbs (Bounds low high)
  | low >= Qty.zero = low
  | high <= Qty.zero = -high
  | otherwise = Qty.zero

squared :: Units.Squared units1 units2 => Bounds units1 -> Bounds units2
squared = Units.specialize . squared'

squared' :: Bounds units -> Bounds (units :*: units)
squared' (Bounds low high) = do
  let ll = low .*. low
  let hh = high .*. high
  if
    | low >= Qty.zero -> Bounds ll hh
    | high <= Qty.zero -> Bounds hh ll
    | otherwise -> Bounds Qty.zero (Qty.max ll hh)

sqrt' :: Bounds (units :*: units) -> Bounds units
sqrt' (Bounds low high) =
  Bounds
    (Qty.sqrt' (Qty.max low Qty.zero))
    (Qty.sqrt' (Qty.max high Qty.zero))

sqrt :: Units.Squared units1 units2 => Bounds units2 -> Bounds units1
sqrt = sqrt' . Units.unspecialize

hypot2 :: Bounds units -> Bounds units -> Bounds units
hypot2 (Bounds xMin xMax) (Bounds yMin yMax) = do
  let positiveX = xMin >= Qty.zero
  let negativeX = xMax <= Qty.zero
  let positiveY = yMin >= Qty.zero
  let negativeY = yMax <= Qty.zero
  let xMagnitude = Qty.max (Qty.abs xMin) (Qty.abs xMax)
  let yMagnitude = Qty.max (Qty.abs yMin) (Qty.abs yMax)
  let maxMagnitude = Qty.hypot2 xMagnitude yMagnitude
  if
    | positiveX && positiveY -> Bounds (Qty.hypot2 xMin yMin) maxMagnitude
    | positiveX && negativeY -> Bounds (Qty.hypot2 xMin yMax) maxMagnitude
    | negativeX && positiveY -> Bounds (Qty.hypot2 xMax yMin) maxMagnitude
    | negativeX && negativeY -> Bounds (Qty.hypot2 xMax yMax) maxMagnitude
    | positiveX -> Bounds xMin maxMagnitude
    | negativeX -> Bounds -xMax maxMagnitude
    | positiveY -> Bounds yMin maxMagnitude
    | negativeY -> Bounds -yMax maxMagnitude
    | otherwise -> Bounds Qty.zero maxMagnitude

hypot3 :: Bounds units -> Bounds units -> Bounds units -> Bounds units
hypot3 (Bounds xMin xMax) (Bounds yMin yMax) (Bounds zMin zMax) = do
  let positiveX = xMin >= Qty.zero
  let negativeX = xMax <= Qty.zero
  let positiveY = yMin >= Qty.zero
  let negativeY = yMax <= Qty.zero
  let positiveZ = zMin >= Qty.zero
  let negativeZ = zMax <= Qty.zero
  let xMagnitude = Qty.max (Qty.abs xMin) (Qty.abs xMax)
  let yMagnitude = Qty.max (Qty.abs yMin) (Qty.abs yMax)
  let zMagnitude = Qty.max (Qty.abs zMin) (Qty.abs zMax)
  let maxMagnitude = Qty.hypot3 xMagnitude yMagnitude zMagnitude
  if
    | positiveX && positiveY && positiveZ -> Bounds (Qty.hypot3 xMin yMin zMin) maxMagnitude
    | positiveX && positiveY && negativeZ -> Bounds (Qty.hypot3 xMin yMin zMax) maxMagnitude
    | positiveX && negativeY && positiveZ -> Bounds (Qty.hypot3 xMin yMax zMin) maxMagnitude
    | positiveX && negativeY && negativeZ -> Bounds (Qty.hypot3 xMin yMax zMax) maxMagnitude
    | negativeX && positiveY && positiveZ -> Bounds (Qty.hypot3 xMax yMin zMin) maxMagnitude
    | negativeX && positiveY && negativeZ -> Bounds (Qty.hypot3 xMax yMin zMax) maxMagnitude
    | negativeX && negativeY && positiveZ -> Bounds (Qty.hypot3 xMax yMax zMin) maxMagnitude
    | negativeX && negativeY && negativeZ -> Bounds (Qty.hypot3 xMax yMax zMax) maxMagnitude
    | positiveY && positiveZ -> Bounds (Qty.hypot2 yMin zMin) maxMagnitude
    | positiveY && negativeZ -> Bounds (Qty.hypot2 yMin zMax) maxMagnitude
    | negativeY && positiveZ -> Bounds (Qty.hypot2 yMax zMin) maxMagnitude
    | negativeY && negativeZ -> Bounds (Qty.hypot2 yMax zMax) maxMagnitude
    | positiveX && positiveZ -> Bounds (Qty.hypot2 xMin zMin) maxMagnitude
    | positiveX && negativeZ -> Bounds (Qty.hypot2 xMin zMax) maxMagnitude
    | negativeX && positiveZ -> Bounds (Qty.hypot2 xMax zMin) maxMagnitude
    | negativeX && negativeZ -> Bounds (Qty.hypot2 xMax zMax) maxMagnitude
    | positiveX && positiveY -> Bounds (Qty.hypot2 xMin yMin) maxMagnitude
    | positiveX && negativeY -> Bounds (Qty.hypot2 xMin yMax) maxMagnitude
    | negativeX && positiveY -> Bounds (Qty.hypot2 xMax yMin) maxMagnitude
    | negativeX && negativeY -> Bounds (Qty.hypot2 xMax yMax) maxMagnitude
    | positiveX -> Bounds xMin maxMagnitude
    | negativeX -> Bounds -xMax maxMagnitude
    | positiveY -> Bounds yMin maxMagnitude
    | negativeY -> Bounds -yMax maxMagnitude
    | otherwise -> Bounds Qty.zero maxMagnitude

{-| Check if a given value is included in a bounding range.

Note that this does *not* use a tolerance, so use with care -
for example, a value *just* outside the range (due to numerical roundoff)
will be reported as not included.
-}
includes :: Qty units -> Bounds units -> Bool
includes value (Bounds low high) = low <= value && value <= high

exclusion :: Qty units -> Bounds units -> Qty units
exclusion value (Bounds low high) = Qty.max (low - value) (value - high)

inclusion :: Qty units -> Bounds units -> Qty units
inclusion value bounds = -(exclusion value bounds)

{-| Check if one bounding range contains another.

Note that this does *not* use a tolerance, so use with care -
for example, a range that extends *just* outside another range (due to numerical
roundoff) will be reported as not contained by that range.
-}
contains :: Bounds units -> Bounds units -> Bool
contains (Bounds low2 high2) (Bounds low1 high1) = low1 <= low2 && high2 <= high1

isContainedIn :: Bounds units -> Bounds units -> Bool
isContainedIn bounds value = contains value bounds

separation :: Bounds units -> Bounds units -> Qty units
separation (Bounds low1 high1) (Bounds low2 high2) = Qty.max (low1 - high2) (low2 - high1)

overlap :: Bounds units -> Bounds units -> Qty units
overlap first second = -(separation first second)

bisect :: Bounds units -> (Bounds units, Bounds units)
bisect (Bounds low high) = do
  let mid
        | low > -Qty.infinity && high < Qty.infinity = Qty.midpoint low high
        | low < Qty.zero && high > Qty.zero = Qty.zero
        | low == Qty.zero = Qty.coerce 1.0
        | high == Qty.zero = Qty.coerce -1.0
        | low > Qty.zero = 2.0 * low
        | high < Qty.zero = 2.0 * high
        | otherwise = internalError "'Impossible' case hit in Bounds.bisect"
  Debug.assert (low < mid)
  Debug.assert (mid < high)
  (Bounds low mid, Bounds mid high)

{-# INLINE isAtomic #-}
isAtomic :: Bounds units -> Bool
isAtomic (Bounds low high) = do
  let mid = Qty.midpoint low high
  mid == low || mid == high

{-# INLINE isFinite #-}
isFinite :: Bounds units -> Bool
isFinite (Bounds low high) = -Qty.infinity < low && high < Qty.infinity

abs :: Bounds units -> Bounds units
abs bounds@(Bounds low high)
  | low >= Qty.zero = bounds
  | high <= Qty.zero = -bounds
  | otherwise = Bounds Qty.zero (Qty.max high -low)

min :: Bounds units -> Bounds units -> Bounds units
min (Bounds low1 high1) (Bounds low2 high2) =
  Bounds (Qty.min low1 low2) (Qty.min high1 high2)

max :: Bounds units -> Bounds units -> Bounds units
max (Bounds low1 high1) (Bounds low2 high2) =
  Bounds (Qty.max low1 low2) (Qty.max high1 high2)

smaller :: Bounds units -> Bounds units -> Bounds units
smaller first second = do
  let Bounds low1 high1 = abs first
  let Bounds low2 high2 = abs second
  if
    | high1 < low2 -> first
    | high2 < low1 -> second
    | otherwise -> do
        let Bounds aggregateMin aggregateMax = aggregate2 first second
        let high = Qty.min high1 high2
        Bounds (Qty.max -high aggregateMin) (Qty.min aggregateMax high)

larger :: Bounds units -> Bounds units -> Bounds units
larger first second = do
  let Bounds low1 high1 = abs first
  let Bounds low2 high2 = abs second
  let low = Qty.max low1 low2
  let aggregate = aggregate2 first second
  let Bounds aggregateMin aggregateMax = aggregate
  if
    | low1 > high2 -> first
    | low2 > high1 -> second
    | aggregateMin > -low -> Bounds (Qty.max aggregateMin low) aggregateMax
    | aggregateMax < low -> Bounds aggregateMin (Qty.min aggregateMax -low)
    | otherwise -> aggregate

minimum :: NonEmpty (Bounds units) -> Bounds units
minimum = NonEmpty.reduce min

maximum :: NonEmpty (Bounds units) -> Bounds units
maximum = NonEmpty.reduce max

smallest :: NonEmpty (Bounds units) -> Bounds units
smallest list = do
  let initial = NonEmpty.minimumBy maxAbs list
  let clipRadius = maxAbs initial
  let conditionalAggregate current (Bounds low high)
        | low > clipRadius || high < -clipRadius = current
        | otherwise =
            aggregate2 current (Bounds (Qty.max low -clipRadius) (Qty.min high clipRadius))
  NonEmpty.foldl conditionalAggregate initial list

largest :: NonEmpty (Bounds units) -> Bounds units
largest list = do
  let initial = NonEmpty.maximumBy minAbs list
  let clipRadius = minAbs initial
  let conditionalAggregate current bounds@(Bounds low high)
        | low > -clipRadius && high < clipRadius = current
        | low > -clipRadius = aggregate2 current (Bounds clipRadius high)
        | high < clipRadius = aggregate2 current (Bounds low -clipRadius)
        | otherwise = aggregate2 current bounds
  NonEmpty.foldl conditionalAggregate initial list

sin :: Bounds Radians -> Bounds Unitless
sin bounds@(Bounds low high) = do
  let (includesMin, includesMax) = sinIncludesMinMax bounds
  let newLow = if includesMin then -1.0 else Qty.min (Angle.sin low) (Angle.sin high)
  let newHigh = if includesMax then 1.0 else Qty.max (Angle.sin low) (Angle.sin high)
  Bounds newLow newHigh

cos :: Bounds Radians -> Bounds Unitless
cos bounds@(Bounds low high) = do
  let (includesMin, includesMax) = cosIncludesMinMax bounds
  let newLow = if includesMin then -1.0 else Qty.min (Angle.cos low) (Angle.cos high)
  let newHigh = if includesMax then 1.0 else Qty.max (Angle.cos low) (Angle.cos high)
  Bounds newLow newHigh

sinIncludesMinMax :: Bounds Radians -> (Bool, Bool)
sinIncludesMinMax bounds = cosIncludesMinMax (bounds - Angle.halfPi)

cosIncludesMinMax :: Bounds Radians -> (Bool, Bool)
cosIncludesMinMax bounds = (cosIncludesMax (bounds + Angle.pi), cosIncludesMax bounds)

cosIncludesMax :: Bounds Radians -> Bool
cosIncludesMax (Bounds low high) =
  (Qty.isInfinite low || Qty.isInfinite high)
    || Float.floor (low / Angle.twoPi) /= Float.floor (high / Angle.twoPi)

interpolate :: Bounds units -> Float -> Qty units
interpolate (Bounds low high) t =
  Qty.interpolateFrom low high t

interpolationParameter :: Bounds units -> Qty units -> Float
interpolationParameter (Bounds low high) value
  | low < high = (value - low) / (high - low)
  | value < low = -Qty.infinity
  | value > high = Qty.infinity
  | otherwise = 0.0

resolution :: Bounds units -> Float
resolution (Bounds low high)
  | low > Qty.zero = low / high
  | high < Qty.zero = -high / low
  | otherwise = 0.0

resolutionThreshold :: Float
resolutionThreshold = 0.5

isResolved :: Bounds units -> Bool
isResolved bounds = Float.abs (resolution bounds) >= resolutionThreshold

resolvedSign :: Bounds units -> Fuzzy Sign
resolvedSign bounds = do
  let boundsResolution = resolution bounds
  if Float.abs boundsResolution >= resolutionThreshold
    then Resolved (Float.sign boundsResolution)
    else Unresolved

any :: (Bounds units -> Fuzzy Bool) -> Bounds units -> Bool
any assess bounds =
  case assess bounds of
    Resolved assessment -> assessment
    Unresolved
      | isAtomic bounds -> False
      | otherwise -> do
          let (left, right) = bisect bounds
          any assess left || any assess right

all :: (Bounds units -> Fuzzy Bool) -> Bounds units -> Bool
all assess bounds =
  case assess bounds of
    Resolved assessment -> assessment
    Unresolved
      | isAtomic bounds -> True
      | otherwise -> do
          let (left, right) = bisect bounds
          all assess left && all assess right

resolve :: Eq a => (Bounds units -> Fuzzy a) -> Bounds units -> Fuzzy a
resolve assess bounds =
  case assess bounds of
    Resolved value -> Resolved value
    Unresolved
      | isAtomic bounds -> Unresolved
      | otherwise -> Fuzzy.do
          let (left, right) = bisect bounds
          leftValue <- resolve assess left
          rightValue <- resolve assess right
          if leftValue == rightValue then Resolved leftValue else Unresolved

random :: Random.Generator (Qty units) -> Random.Generator (Bounds units)
random randomQty = Random.do
  a <- randomQty
  b <- randomQty
  Random.return (from a b)

sampleValues :: Bounds units -> List (Qty units)
sampleValues bounds = List.map (interpolate bounds) Parameter.samples

convert :: Qty (units2 :/: units1) -> Bounds units1 -> Bounds units2
convert factor bounds = bounds !* factor

unconvert :: Qty (units2 :/: units1) -> Bounds units2 -> Bounds units1
unconvert factor bounds = bounds !/ factor
