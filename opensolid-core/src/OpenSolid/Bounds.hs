{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Bounds
  ( Bounds (Bounds, Bounds#)
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
  , width#
  , maxAbs
  , minAbs
  , squared
  , squared'
  , includes
  , inclusion
  , inclusion#
  , exclusion
  , exclusion#
  , contains
  , overlap
  , separation
  , separation#
  , isContainedIn
  , bisect
  , isAtomic
  , isFinite
  , abs
  , sqrt
  , sqrt'
  , hypot2
  , cubed
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
import OpenSolid.Prelude hiding (max, min)
import OpenSolid.Qty (Qty (Qty#))
import OpenSolid.Qty qualified as Qty
import OpenSolid.Random qualified as Random
import OpenSolid.Unboxed.Math
import OpenSolid.Units qualified as Units

type role Bounds phantom

type Bounds :: Type -> Type

-- | A range of possible values, with a lower bound and upper bound.
data Bounds units = Ordered# Double# Double#
  deriving (Eq)

instance HasField "endpoints" (Bounds units) (Qty units, Qty units) where
  getField = endpoints

instance HasField "lower" (Bounds units) (Qty units) where
  getField = lower

instance HasField "upper" (Bounds units) (Qty units) where
  getField = upper

instance HasField "width" (Bounds units) (Qty units) where
  getField = width

deriving instance Show (Qty units) => Show (Bounds units)

{-| Construct a bounding range from two given values (endpoints).

The order of the two arguments does not matter;
the minimum of the two will be used as the lower bound
and the maximum will be used as the upper bound.

If either argument is NaN, then the result will be open/infinite
(with endpoints negative infinity and positive infinity).
-}
{-# INLINE Bounds #-}
pattern Bounds :: Qty units -> Qty units -> Bounds units
pattern Bounds low high <- (viewBounds# -> (# low, high #))
  where
    Bounds (Qty# a#) (Qty# b#) = Bounds# a# b#

{-# INLINE viewBounds# #-}
viewBounds# :: Bounds units -> (# Qty units, Qty units #)
viewBounds# (Ordered# low high) = (# Qty# low, Qty# high #)

{-# COMPLETE Bounds #-}

{-# INLINE Bounds# #-}
pattern Bounds# :: Double# -> Double# -> Bounds units
pattern Bounds# low# high# <- Ordered# low# high#
  where
    Bounds# a# b# = do
      let !(# low#, high# #) = hull2# a# b#
      Ordered# low# high#

{-# COMPLETE Bounds# #-}

instance FFI (Bounds Unitless) where
  representation = FFI.classRepresentation "Bounds"

instance FFI (Bounds Radians) where
  representation = FFI.classRepresentation "AngleBounds"

instance FFI (Bounds Meters) where
  representation = FFI.classRepresentation "LengthBounds"

instance FFI (Bounds SquareMeters) where
  representation = FFI.classRepresentation "AreaBounds"

instance HasUnits (Bounds units) units

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
  negate (Bounds# low# high#) = Bounds# (negate# high#) (negate# low#)

instance Multiplication Sign (Bounds units) (Bounds units) where
  Positive * bounds = bounds
  Negative * bounds = -bounds

instance Multiplication (Bounds units) Sign (Bounds units) where
  bounds * Positive = bounds
  bounds * Negative = -bounds

instance units1 ~ units2 => Addition (Bounds units1) (Bounds units2) (Bounds units1) where
  Bounds# low1# high1# + Bounds# low2# high2# = Bounds# (low1# +# low2#) (high1# +# high2#)

instance units1 ~ units2 => Addition (Bounds units1) (Qty units2) (Bounds units1) where
  Bounds# low# high# + Qty# value# = Bounds# (low# +# value#) (high# +# value#)

instance units1 ~ units2 => Addition (Qty units1) (Bounds units2) (Bounds units1) where
  Qty# value# + Bounds# low# high# = Bounds# (value# +# low#) (value# +# high#)

instance units1 ~ units2 => Subtraction (Bounds units1) (Bounds units2) (Bounds units1) where
  Bounds# low1# high1# - Bounds# low2# high2# = Bounds# (low1# -# high2#) (high1# -# low2#)

instance units1 ~ units2 => Subtraction (Bounds units1) (Qty units2) (Bounds units1) where
  Bounds# low# high# - Qty# value# = Bounds# (low# -# value#) (high# -# value#)

instance units1 ~ units2 => Subtraction (Qty units1) (Bounds units2) (Bounds units1) where
  Qty# value# - Bounds# low# high# = Bounds# (value# -# high#) (value# -# low#)

instance Multiplication' (Qty units1) (Bounds units2) (Bounds (units1 :*: units2)) where
  Qty# value# .*. Bounds# low# high# = Bounds# (value# *# low#) (value# *# high#)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Bounds units2) (Bounds units3)
  where
  Qty# value# * Bounds# low# high# = Bounds# (value# *# low#) (value# *# high#)

instance Multiplication' (Bounds units1) (Qty units2) (Bounds (units1 :*: units2)) where
  Bounds# low# high# .*. Qty# value# = Bounds# (low# *# value#) (high# *# value#)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (Qty units2) (Bounds units3)
  where
  Bounds# low# high# * Qty# value# = Bounds# (low# *# value#) (high# *# value#)

instance Multiplication' (Bounds units1) (Bounds units2) (Bounds (units1 :*: units2)) where
  Bounds# low1# high1# .*. Bounds# low2# high2# = do
    let !(# low#, high# #) = boundsTimesBounds# low1# high1# low2# high2#
    Ordered# low# high#

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (Bounds units2) (Bounds units3)
  where
  Bounds# low1# high1# * Bounds# low2# high2# = do
    let !(# low#, high# #) = boundsTimesBounds# low1# high1# low2# high2#
    Ordered# low# high#

instance Division' (Qty units1) (Bounds units2) (Bounds (units1 :/: units2)) where
  Qty# n# ./. Bounds# dl# dh# = do
    let !(# low#, high# #) = doubleOverBounds# n# dl# dh#
    Ordered# low# high#

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Bounds units2) (Bounds units3)
  where
  Qty# n# / Bounds# dl# dh# = do
    let !(# low#, high# #) = doubleOverBounds# n# dl# dh#
    Ordered# low# high#

instance Division' (Bounds units1) (Qty units2) (Bounds (units1 :/: units2)) where
  Bounds# nl# nh# ./. Qty# d# = do
    let !(# low#, high# #) = boundsOverDouble# nl# nh# d#
    Ordered# low# high#

instance
  Units.Quotient units1 units2 units3 =>
  Division (Bounds units1) (Qty units2) (Bounds units3)
  where
  Bounds# nl# nh# / Qty# d# = do
    let !(# low#, high# #) = boundsOverDouble# nl# nh# d#
    Ordered# low# high#

instance Division' (Bounds units1) (Bounds units2) (Bounds (units1 :/: units2)) where
  Bounds# nl# nh# ./. Bounds# dl# dh# = do
    let !(# low#, high# #) = boundsOverBounds# nl# nh# dl# dh#
    Ordered# low# high#

instance
  Units.Quotient units1 units2 units3 =>
  Division (Bounds units1) (Bounds units2) (Bounds units3)
  where
  Bounds# nl# nh# / Bounds# dl# dh# = do
    let !(# low#, high# #) = boundsOverBounds# nl# nh# dl# dh#
    Ordered# low# high#

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

-- | Create a bounding range with zero as one of its endpoints and the given value as the other.
zeroTo :: Qty units -> Bounds units
zeroTo value = Bounds Qty.zero value

{-| Create a bounding range symmetric about zero, with the given width.

The lower bound of the range will be -w/2 and the upper bound will be w/2.
-}
symmetric :: "width" ::: Qty units -> Bounds units
symmetric (Named width_) = let r = 0.5 * width_ in Bounds -r r

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
hull4 (Qty# a#) (Qty# b#) (Qty# c#) (Qty# d#) = do
  let !(# low#, high# #) = hull4# a# b# c# d#
  Ordered# low# high#

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
width bounds = Qty# (width# bounds)

{-# INLINE width# #-}
width# :: Bounds units -> Double#
width# (Bounds# low# high#) = high# -# low#

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
hypot2 (Bounds# xMin# xMax#) (Bounds# yMin# yMax#) = do
  let positiveX# = xMin# >=# 0.0##
  let negativeX# = xMax# <=# 0.0##
  let positiveY# = yMin# >=# 0.0##
  let negativeY# = yMax# <=# 0.0##
  let xMagnitude# = max# (abs# xMin#) (abs# xMax#)
  let yMagnitude# = max# (abs# yMin#) (abs# yMax#)
  let maxMagnitude# = hypot2# xMagnitude# yMagnitude#
  case (# positiveX#, negativeX#, positiveY#, negativeY# #) of
    (# 1#, _, 1#, _ #) -> Bounds# (hypot2# xMin# yMin#) maxMagnitude#
    (# 1#, _, _, 1# #) -> Bounds# (hypot2# xMin# yMax#) maxMagnitude#
    (# _, 1#, 1#, _ #) -> Bounds# (hypot2# xMax# yMin#) maxMagnitude#
    (# _, 1#, _, 1# #) -> Bounds# (hypot2# xMax# yMax#) maxMagnitude#
    (# 1#, _, _, _ #) -> Bounds# xMin# maxMagnitude#
    (# _, 1#, _, _ #) -> Bounds# (negate# xMax#) maxMagnitude#
    (# _, _, 1#, _ #) -> Bounds# yMin# maxMagnitude#
    (# _, _, _, 1# #) -> Bounds# (negate# yMax#) maxMagnitude#
    (# _, _, _, _ #) -> Bounds# 0.0## maxMagnitude#

cubed :: Bounds Unitless -> Bounds Unitless
cubed (Bounds low high) = Bounds (low * low * low) (high * high * high)

{-| Check if a given value is included in a bounding range.

Note that this does *not* use a tolerance, so use with care -
for example, a value *just* outside the range (due to numerical roundoff)
will be reported as not included.
-}
includes :: Qty units -> Bounds units -> Bool
includes value (Bounds low high) = low <= value && value <= high

exclusion :: Qty units -> Bounds units -> Qty units
exclusion (Qty# value#) bounds = Qty# (exclusion# value# bounds)

{-# INLINE exclusion# #-}
exclusion# :: Double# -> Bounds units -> Double#
exclusion# value# (Bounds# low# high#) = max# (low# -# value#) (value# -# high#)

inclusion :: Qty units -> Bounds units -> Qty units
inclusion (Qty# value#) bounds = Qty# (inclusion# value# bounds)

{-# INLINE inclusion# #-}
inclusion# :: Double# -> Bounds units -> Double#
inclusion# value# bounds = negate# (exclusion# value# bounds)

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
separation bounds1 bounds2 = Qty# (separation# bounds1 bounds2)

{-# INLINE separation# #-}
separation# :: Bounds units -> Bounds units -> Double#
separation# (Bounds# low1# high1#) (Bounds# low2# high2#) = max# (low1# -# high2#) (low2# -# high1#)

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
  Random.return (Bounds a b)

sampleValues :: Bounds units -> List (Qty units)
sampleValues bounds = List.map (interpolate bounds) Parameter.samples

convert :: Qty (units2 :/: units1) -> Bounds units1 -> Bounds units2
convert factor bounds = Units.simplify (bounds .*. factor)

unconvert :: Qty (units2 :/: units1) -> Bounds units2 -> Bounds units1
unconvert factor bounds = Units.simplify (bounds ./. factor)
