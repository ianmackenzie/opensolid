module OpenSolid.Range
  ( Range (Range)
  , constant
  , unit
  , zeroTo
  , unbounded
  , radians
  , degrees
  , turns
  , meters
  , centimeters
  , millimeters
  , inches
  , squareMeters
  , hull3
  , hull4
  , hullN
  , lowerBound
  , upperBound
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
  , samples
  , convert
  , unconvert
  )
where

import Data.Coerce qualified
import OpenSolid.Angle qualified as Angle
import OpenSolid.Area qualified as Area
import OpenSolid.Debug qualified as Debug
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Float qualified as Float
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.Length qualified as Length
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import {-# SOURCE #-} OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Random qualified as Random
import OpenSolid.Units (Meters, Radians, SquareMeters)
import OpenSolid.Units qualified as Units

type role Range phantom

type Range :: Type -> Type

-- | A range of possible values, with a lower bound and upper bound.
data Range units = Ordered (Qty units) (Qty units)
  deriving (Eq, Show)

{-# COMPLETE Range #-}

{-| Construct a range from its lower and upper bounds.

The order of the two arguments does not matter;
the minimum of the two will be used as the lower bound of the range
and the maximum will be used as the upper bound.
-}
{-# INLINE Range #-}
pattern Range :: Qty units -> Qty units -> Range units
pattern Range low high <- Ordered low high
  where
    Range a b
      | a <= b = Ordered a b
      | b <= a = Ordered b a
      | otherwise = unbounded

instance FFI (Range Unitless) where
  representation = FFI.classRepresentation "Range"

instance FFI (Range Radians) where
  representation = FFI.classRepresentation "AngleRange"

instance FFI (Range Meters) where
  representation = FFI.classRepresentation "LengthRange"

instance FFI (Range SquareMeters) where
  representation = FFI.classRepresentation "AreaRange"

instance HasUnits (Range units) units (Range Unitless)

instance Units.Coercion (Range unitsA) (Range unitsB) where
  coerce = Data.Coerce.coerce

instance units ~ units_ => ApproximateEquality (Range units) (Qty units_) units where
  Range low high ~= value = low >= value - ?tolerance && high <= value + ?tolerance

instance units ~ units_ => ApproximateEquality (Qty units) (Range units_) units where
  value ~= range = range ~= value

instance units ~ units_ => Intersects (Qty units) (Range units_) units where
  value ^ range = exclusion value range <= ?tolerance

instance units ~ units_ => Intersects (Range units) (Qty units_) units where
  range ^ value = value ^ range

instance units ~ units_ => Intersects (Range units) (Range units_) units where
  first ^ second = separation first second <= ?tolerance

instance Negation (Range units) where
  negate (Range low high) = Ordered (negate high) (negate low)

instance Multiplication Sign (Range units) (Range units) where
  Positive * range = range
  Negative * range = -range

instance Multiplication (Range units) Sign (Range units) where
  range * Positive = range
  range * Negative = -range

instance units ~ units_ => Addition (Range units) (Range units_) (Range units) where
  Range low1 high1 + Range low2 high2 = Ordered (low1 + low2) (high1 + high2)

instance units ~ units_ => Addition (Range units) (Qty units_) (Range units) where
  Range low high + value = Range (low + value) (high + value)

instance units ~ units_ => Addition (Qty units) (Range units_) (Range units) where
  value + Range low high = Range (value + low) (value + high)

instance units ~ units_ => Subtraction (Range units) (Range units_) (Range units) where
  Range low1 high1 - Range low2 high2 = Ordered (low1 - high2) (high1 - low2)

instance units ~ units_ => Subtraction (Range units) (Qty units_) (Range units) where
  Range low high - value = Range (low - value) (high - value)

instance units ~ units_ => Subtraction (Qty units) (Range units_) (Range units) where
  value - Range low high = Range (value - high) (value - low)

instance Multiplication' (Qty units1) (Range units2) (Range (units1 :*: units2)) where
  value .*. Range low high = from (value .*. low) (value .*. high)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Range units2) (Range units3)
  where
  value * Range low high = from (value * low) (value * high)

instance Multiplication' (Range units1) (Qty units2) (Range (units1 :*: units2)) where
  Range low high .*. value = from (low .*. value) (high .*. value)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Range units1) (Qty units2) (Range units3)
  where
  Range low high * value = from (low * value) (high * value)

instance Multiplication' (Range units1) (Range units2) (Range (units1 :*: units2)) where
  Range low1 high1 .*. Range low2 high2 =
    hull4 (low1 .*. low2) (low1 .*. high2) (high1 .*. low2) (high1 .*. high2)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Range units1) (Range units2) (Range units3)
  where
  Range low1 high1 * Range low2 high2 =
    hull4 (low1 * low2) (low1 * high2) (high1 * low2) (high1 * high2)

instance Division' (Qty units1) (Range units2) (Range (units1 :/: units2)) where
  n ./. Range dl dh =
    if dl > Qty.zero || dh < Qty.zero
      then from (n ./. dl) (n ./. dh)
      else unbounded

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Range units2) (Range units3)
  where
  n / Range dl dh =
    if dl > Qty.zero || dh < Qty.zero
      then from (n / dl) (n / dh)
      else unbounded

instance Division' (Range units1) (Qty units2) (Range (units1 :/: units2)) where
  Range nl nh ./. d =
    if d /= Qty.zero
      then from (nl ./. d) (nh ./. d)
      else unbounded

instance
  Units.Quotient units1 units2 units3 =>
  Division (Range units1) (Qty units2) (Range units3)
  where
  Range nl nh / d =
    if d /= Qty.zero
      then from (nl / d) (nh / d)
      else unbounded

instance Division' (Range units1) (Range units2) (Range (units1 :/: units2)) where
  Range nl nh ./. Range dl dh =
    if dl > Qty.zero || dh < Qty.zero
      then hull4 (nl ./. dl) (nl ./. dh) (nh ./. dl) (nh ./. dh)
      else unbounded

instance
  Units.Quotient units1 units2 units3 =>
  Division (Range units1) (Range units2) (Range units3)
  where
  Range nl nh / Range dl dh =
    if dl > Qty.zero || dh < Qty.zero
      then hull4 (nl / dl) (nl / dh) (nh / dl) (nh / dh)
      else unbounded

-- | Construct a zero-width range containing a single value.
{-# INLINE constant #-}
constant :: Qty units -> Range units
constant value = Range value value

-- | The range with endoints [0,1].
unit :: Range Unitless
unit = Ordered 0.0 1.0

{-| Construct a range from its lower and upper bounds.

The order of the two arguments does not matter;
the minimum of the two will be used as the lower bound of the range
and the maximum will be used as the upper bound.
-}
{-# INLINE from #-}
from :: Qty units -> Qty units -> Range units
from = Range

-- | Create a range from zero to the given value.
zeroTo :: Qty units -> Range units
zeroTo value = Range Qty.zero value

unbounded :: Range units
unbounded = Ordered -Qty.infinity Qty.infinity

-- | Construct an angle range from lower and upper bounds given in radians.
radians :: Float -> Float -> Range Radians
radians a b = from (Angle.radians a) (Angle.radians b)

-- | Construct an angle range from lower and upper bounds given in degrees.
degrees :: Float -> Float -> Range Radians
degrees a b = from (Angle.degrees a) (Angle.degrees b)

-- | Construct an angle range from lower and upper bounds given in turns.
turns :: Float -> Float -> Range Radians
turns a b = from (Angle.turns a) (Angle.turns b)

-- | Construct a length range from lower and upper bounds given in meters.
meters :: Float -> Float -> Range Meters
meters a b = from (Length.meters a) (Length.meters b)

-- | Construct a length range from lower and upper bounds given in millimeters.
millimeters :: Float -> Float -> Range Meters
millimeters a b = from (Length.millimeters a) (Length.millimeters b)

-- | Construct a length range from lower and upper bounds given in centimeters.
centimeters :: Float -> Float -> Range Meters
centimeters a b = from (Length.centimeters a) (Length.centimeters b)

-- | Construct a length range from lower and upper bounds given in inches.
inches :: Float -> Float -> Range Meters
inches a b = from (Length.inches a) (Length.inches b)

-- | Construct an area range from lower and upper bounds given in square meters.
squareMeters :: Float -> Float -> Range SquareMeters
squareMeters a b = from (Area.squareMeters a) (Area.squareMeters b)

aggregate2 :: Range units -> Range units -> Range units
aggregate2 (Range low1 high1) (Range low2 high2) =
  Ordered (Qty.min low1 low2) (Qty.max high1 high2)

aggregate3 :: Range units -> Range units -> Range units -> Range units
aggregate3 (Range low1 high1) (Range low2 high2) (Range low3 high3) =
  Ordered (Qty.min (Qty.min low1 low2) low3) (Qty.max (Qty.max high1 high2) high3)

-- | Build a range containing all ranges in the given non-empty list.
aggregateN :: NonEmpty (Range units) -> Range units
aggregateN (Range low1 high1 :| rest) = do
  let go low high [] = Ordered low high
      go low high (Range nextLow nextHigh : remaining) =
        go (Qty.min low nextLow) (Qty.max high nextHigh) remaining
  go low1 high1 rest

-- | Attempt to find the intersection of two ranges.
intersection :: Range units -> Range units -> Maybe (Range units)
intersection (Range low1 high1) (Range low2 high2)
  | high1 < low2 = Nothing
  | low1 > high2 = Nothing
  | otherwise = Just (Ordered (Qty.max low1 low2) (Qty.min high1 high2))

{-# INLINE hull3 #-}
hull3 :: Qty units -> Qty units -> Qty units -> Range units
hull3 a b c = Ordered (Qty.min a (Qty.min b c)) (Qty.max a (Qty.max b c))

{-# INLINE hull4 #-}
hull4 :: Qty units -> Qty units -> Qty units -> Qty units -> Range units
hull4 a b c d = Ordered (Qty.min a (Qty.min b (Qty.min c d))) (Qty.max a (Qty.max b (Qty.max c d)))

-- | Build a range containing all values in the given non-empty list.
hullN :: NonEmpty (Qty units) -> Range units
hullN (first :| rest) = do
  let go low high [] = Ordered low high
      go low high (next : remaining) = go (Qty.min low next) (Qty.max high next) remaining
  go first first rest

-- | Get the lower bound of a range.
{-# INLINE lowerBound #-}
lowerBound :: Range units -> Qty units
lowerBound (Range low _) = low

-- | Get the upper bound of a range.
{-# INLINE upperBound #-}
upperBound :: Range units -> Qty units
upperBound (Range _ high) = high

{-# INLINE midpoint #-}
midpoint :: Range units -> Qty units
midpoint (Range low high) = Qty.midpoint low high

-- | Get the lower and upper bounds of a range.
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

squared :: Units.Squared units1 units2 => Range units1 -> Range units2
squared range = Units.specialize (squared' range)

squared' :: Range units -> Range (units :*: units)
squared' (Range low high) = do
  let ll = low .*. low
  let hh = high .*. high
  if
    | low >= Qty.zero -> Ordered ll hh
    | high <= Qty.zero -> Ordered hh ll
    | otherwise -> Ordered Qty.zero (Qty.max ll hh)

sqrt' :: Range (units :*: units) -> Range units
sqrt' (Range low high) =
  Ordered
    (Qty.sqrt' (Qty.max low Qty.zero))
    (Qty.sqrt' (Qty.max high Qty.zero))

sqrt :: Units.Squared units1 units2 => Range units2 -> Range units1
sqrt range = sqrt' (Units.unspecialize range)

hypot2 :: Range units -> Range units -> Range units
hypot2 (Range xMin xMax) (Range yMin yMax) = do
  let positiveX = xMin >= Qty.zero
  let negativeX = xMax <= Qty.zero
  let positiveY = yMin >= Qty.zero
  let negativeY = yMax <= Qty.zero
  let xMagnitude = Qty.max (Qty.abs xMin) (Qty.abs xMax)
  let yMagnitude = Qty.max (Qty.abs yMin) (Qty.abs yMax)
  let maxMagnitude = Qty.hypot2 xMagnitude yMagnitude
  if
    | positiveX && positiveY -> Ordered (Qty.hypot2 xMin yMin) maxMagnitude
    | positiveX && negativeY -> Ordered (Qty.hypot2 xMin yMax) maxMagnitude
    | negativeX && positiveY -> Ordered (Qty.hypot2 xMax yMin) maxMagnitude
    | negativeX && negativeY -> Ordered (Qty.hypot2 xMax yMax) maxMagnitude
    | positiveX -> Ordered xMin maxMagnitude
    | negativeX -> Ordered -xMax maxMagnitude
    | positiveY -> Ordered yMin maxMagnitude
    | negativeY -> Ordered -yMax maxMagnitude
    | otherwise -> Ordered Qty.zero maxMagnitude

hypot3 :: Range units -> Range units -> Range units -> Range units
hypot3 (Range xMin xMax) (Range yMin yMax) (Range zMin zMax) = do
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
    | positiveX && positiveY && positiveZ -> Ordered (Qty.hypot3 xMin yMin zMin) maxMagnitude
    | positiveX && positiveY && negativeZ -> Ordered (Qty.hypot3 xMin yMin zMax) maxMagnitude
    | positiveX && negativeY && positiveZ -> Ordered (Qty.hypot3 xMin yMax zMin) maxMagnitude
    | positiveX && negativeY && negativeZ -> Ordered (Qty.hypot3 xMin yMax zMax) maxMagnitude
    | negativeX && positiveY && positiveZ -> Ordered (Qty.hypot3 xMax yMin zMin) maxMagnitude
    | negativeX && positiveY && negativeZ -> Ordered (Qty.hypot3 xMax yMin zMax) maxMagnitude
    | negativeX && negativeY && positiveZ -> Ordered (Qty.hypot3 xMax yMax zMin) maxMagnitude
    | negativeX && negativeY && negativeZ -> Ordered (Qty.hypot3 xMax yMax zMax) maxMagnitude
    | positiveY && positiveZ -> Ordered (Qty.hypot2 yMin zMin) maxMagnitude
    | positiveY && negativeZ -> Ordered (Qty.hypot2 yMin zMax) maxMagnitude
    | negativeY && positiveZ -> Ordered (Qty.hypot2 yMax zMin) maxMagnitude
    | negativeY && negativeZ -> Ordered (Qty.hypot2 yMax zMax) maxMagnitude
    | positiveX && positiveZ -> Ordered (Qty.hypot2 xMin zMin) maxMagnitude
    | positiveX && negativeZ -> Ordered (Qty.hypot2 xMin zMax) maxMagnitude
    | negativeX && positiveZ -> Ordered (Qty.hypot2 xMax zMin) maxMagnitude
    | negativeX && negativeZ -> Ordered (Qty.hypot2 xMax zMax) maxMagnitude
    | positiveX && positiveY -> Ordered (Qty.hypot2 xMin yMin) maxMagnitude
    | positiveX && negativeY -> Ordered (Qty.hypot2 xMin yMax) maxMagnitude
    | negativeX && positiveY -> Ordered (Qty.hypot2 xMax yMin) maxMagnitude
    | negativeX && negativeY -> Ordered (Qty.hypot2 xMax yMax) maxMagnitude
    | positiveX -> Ordered xMin maxMagnitude
    | negativeX -> Ordered -xMax maxMagnitude
    | positiveY -> Ordered yMin maxMagnitude
    | negativeY -> Ordered -yMax maxMagnitude
    | otherwise -> Ordered Qty.zero maxMagnitude

{-| Check if a given value is included in a range.

Note that this does *not* use a tolerance, so use with care -
for example, a value *just* outside the range (due to numerical roundoff)
will be reported as not included.
-}
includes :: Qty units -> Range units -> Bool
includes value (Range low high) = low <= value && value <= high

exclusion :: Qty units -> Range units -> Qty units
exclusion value (Range low high) = Qty.max (low - value) (value - high)

inclusion :: Qty units -> Range units -> Qty units
inclusion value range = -(exclusion value range)

{-| Check if one range contains another.

Note that this does *not* use a tolerance, so use with care -
for example, a range that extends *just* outside another range (due to numerical
roundoff) will be reported as not contained by that range.
-}
contains :: Range units -> Range units -> Bool
contains (Range low2 high2) (Range low1 high1) = low1 <= low2 && high2 <= high1

isContainedIn :: Range units -> Range units -> Bool
isContainedIn range value = contains value range

separation :: Range units -> Range units -> Qty units
separation (Range low1 high1) (Range low2 high2) = Qty.max (low1 - high2) (low2 - high1)

overlap :: Range units -> Range units -> Qty units
overlap first second = -(separation first second)

bisect :: Range units -> (Range units, Range units)
bisect (Range low high) = do
  let mid
        | low > -Qty.infinity && high < Qty.infinity = Qty.midpoint low high
        | low < Qty.zero && high > Qty.zero = Qty.zero
        | low == Qty.zero = Units.coerce 1.0
        | high == Qty.zero = Units.coerce -1.0
        | low > Qty.zero = 2.0 * low
        | high < Qty.zero = 2.0 * high
        | otherwise = internalError "'Impossible' case hit in Range.bisect"
  Debug.assert (low < mid)
  Debug.assert (mid < high)
  (Ordered low mid, Ordered mid high)

{-# INLINE isAtomic #-}
isAtomic :: Range units -> Bool
isAtomic (Range low high) = do
  let mid = Qty.midpoint low high
  mid == low || mid == high

{-# INLINE isFinite #-}
isFinite :: Range units -> Bool
isFinite (Range low high) = -Qty.infinity < low && high < Qty.infinity

abs :: Range units -> Range units
abs range@(Range low high)
  | low >= Qty.zero = range
  | high <= Qty.zero = -range
  | otherwise = Ordered Qty.zero (Qty.max high -low)

min :: Range units -> Range units -> Range units
min (Range low1 high1) (Range low2 high2) =
  Ordered (Qty.min low1 low2) (Qty.min high1 high2)

max :: Range units -> Range units -> Range units
max (Range low1 high1) (Range low2 high2) =
  Ordered (Qty.max low1 low2) (Qty.max high1 high2)

smaller :: Range units -> Range units -> Range units
smaller first second = do
  let (Range low1 high1) = abs first
  let (Range low2 high2) = abs second
  if
    | high1 < low2 -> first
    | high2 < low1 -> second
    | otherwise -> do
        let (Range aggregateMin aggregateMax) = aggregate2 first second
        let high = Qty.min high1 high2
        Ordered (Qty.max -high aggregateMin) (Qty.min aggregateMax high)

larger :: Range units -> Range units -> Range units
larger first second = do
  let (Range low1 high1) = abs first
  let (Range low2 high2) = abs second
  let low = Qty.max low1 low2
  let aggregate@(Range aggregateMin aggregateMax) = aggregate2 first second
  if
    | low1 > high2 -> first
    | low2 > high1 -> second
    | aggregateMin > -low -> Ordered (Qty.max aggregateMin low) aggregateMax
    | aggregateMax < low -> Ordered aggregateMin (Qty.min aggregateMax -low)
    | otherwise -> aggregate

minimum :: NonEmpty (Range units) -> Range units
minimum = NonEmpty.reduce min

maximum :: NonEmpty (Range units) -> Range units
maximum = NonEmpty.reduce max

smallest :: NonEmpty (Range units) -> Range units
smallest ranges = do
  let initial = NonEmpty.minimumBy maxAbs ranges
  let clipRadius = maxAbs initial
  let conditionalAggregate current (Range low high)
        | low > clipRadius || high < -clipRadius = current
        | otherwise =
            aggregate2 current (Ordered (Qty.max low -clipRadius) (Qty.min high clipRadius))
  NonEmpty.foldl conditionalAggregate initial ranges

largest :: NonEmpty (Range units) -> Range units
largest ranges = do
  let initial = NonEmpty.maximumBy minAbs ranges
  let clipRadius = minAbs initial
  let conditionalAggregate current range@(Range low high)
        | low > -clipRadius && high < clipRadius = current
        | low > -clipRadius = aggregate2 current (Ordered clipRadius high)
        | high < clipRadius = aggregate2 current (Ordered low -clipRadius)
        | otherwise = aggregate2 current range
  NonEmpty.foldl conditionalAggregate initial ranges

sin :: Range Radians -> Range Unitless
sin range@(Range low high) = do
  let (includesMin, includesMax) = sinIncludesMinMax range
  let newLow = if includesMin then -1.0 else Qty.min (Angle.sin low) (Angle.sin high)
  let newHigh = if includesMax then 1.0 else Qty.max (Angle.sin low) (Angle.sin high)
  Ordered newLow newHigh

cos :: Range Radians -> Range Unitless
cos range@(Range low high) = do
  let (includesMin, includesMax) = cosIncludesMinMax range
  let newLow = if includesMin then -1.0 else Qty.min (Angle.cos low) (Angle.cos high)
  let newHigh = if includesMax then 1.0 else Qty.max (Angle.cos low) (Angle.cos high)
  Ordered newLow newHigh

sinIncludesMinMax :: Range Radians -> (Bool, Bool)
sinIncludesMinMax range = cosIncludesMinMax (range - Angle.quarterTurn)

cosIncludesMinMax :: Range Radians -> (Bool, Bool)
cosIncludesMinMax range =
  (cosIncludesMax (range + Angle.halfTurn), cosIncludesMax range)

cosIncludesMax :: Range Radians -> Bool
cosIncludesMax (Range low high) =
  (Qty.isInfinite low || Qty.isInfinite high)
    || Float.floor (low / Angle.fullTurn) /= Float.floor (high / Angle.fullTurn)

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

resolutionThreshold :: Float
resolutionThreshold = 0.5

isResolved :: Range units -> Bool
isResolved range = Float.abs (resolution range) >= resolutionThreshold

resolvedSign :: Range units -> Fuzzy Sign
resolvedSign range = do
  let rangeResolution = resolution range
  if Float.abs rangeResolution >= resolutionThreshold
    then Resolved (Float.sign rangeResolution)
    else Unresolved

any :: (Range units -> Fuzzy Bool) -> Range units -> Bool
any assess range =
  case assess range of
    Resolved assessment -> assessment
    Unresolved
      | isAtomic range -> False
      | otherwise -> do
          let (left, right) = bisect range
          any assess left || any assess right

all :: (Range units -> Fuzzy Bool) -> Range units -> Bool
all assess range =
  case assess range of
    Resolved assessment -> assessment
    Unresolved
      | isAtomic range -> True
      | otherwise -> do
          let (left, right) = bisect range
          all assess left && all assess right

resolve :: Eq a => (Range units -> Fuzzy a) -> Range units -> Fuzzy a
resolve assess range =
  case assess range of
    Resolved value -> Resolved value
    Unresolved
      | isAtomic range -> Unresolved
      | otherwise -> Fuzzy.do
          let (left, right) = bisect range
          leftValue <- resolve assess left
          rightValue <- resolve assess right
          if leftValue == rightValue then Resolved leftValue else Unresolved

random :: Random.Generator (Qty units) -> Random.Generator (Range units)
random randomQty = Random.do
  a <- randomQty
  b <- randomQty
  Random.return (from a b)

samples :: Range units -> List (Qty units)
samples range = List.map (interpolate range) Parameter.samples

convert :: Qty (units2 :/: units1) -> Range units1 -> Range units2
convert factor range = range !* factor

unconvert :: Qty (units2 :/: units1) -> Range units2 -> Range units1
unconvert factor range = range !/ factor
