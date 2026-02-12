{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Interval
  ( Interval (Interval, Interval#)
  , constant
  , unit
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
  , squared_
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
  , sqrt_
  , hypot2
  , cubed
  , aggregate2
  , aggregate3
  , aggregateN
  , min
  , max
  , minimum
  , maximum
  , sin
  , cos
  , interpolate
  , interpolationParameter
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
import GHC.Records (HasField)
import OpenSolid.Angle qualified as Angle
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.InternalError (InternalError (InternalError))
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import {-# SOURCE #-} OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude hiding (max, min)
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random qualified as Random
import OpenSolid.Unboxed.Math
import OpenSolid.Units (HasUnits, SquareMeters)
import OpenSolid.Units qualified as Units
import Prelude qualified

type role Interval phantom

type Interval :: Type -> Type

-- | A range of possible values, with a lower bound and upper bound.
data Interval units = Ordered# Double# Double#
  deriving (Eq, Show)

instance HasField "endpoints" (Interval units) (Quantity units, Quantity units) where
  getField = endpoints

instance HasField "lower" (Interval units) (Quantity units) where
  getField = lower

instance HasField "upper" (Interval units) (Quantity units) where
  getField = upper

instance HasField "width" (Interval units) (Quantity units) where
  getField = width

{-| Construct a bounding range from two given values (endpoints).

The order of the two arguments does not matter;
the minimum of the two will be used as the lower bound
and the maximum will be used as the upper bound.

If either argument is NaN, then the result will be open/infinite
(with endpoints negative infinity and positive infinity).
-}
{-# INLINE Interval #-}
pattern Interval :: Quantity units -> Quantity units -> Interval units
pattern Interval low high <- (viewInterval -> (# low, high #))
  where
    Interval (Quantity# a#) (Quantity# b#) = Interval# a# b#

{-# INLINE viewInterval #-}
viewInterval :: Interval units -> (# Quantity units, Quantity units #)
viewInterval (Ordered# low# high#) = (# Quantity# low#, Quantity# high# #)

{-# COMPLETE Interval #-}

{-# INLINE Interval# #-}
pattern Interval# :: Double# -> Double# -> Interval units
pattern Interval# low# high# <- Ordered# low# high#
  where
    Interval# a# b# = do
      let !(# low#, high# #) = hull2# a# b#
      Ordered# low# high#

{-# COMPLETE Interval# #-}

instance FFI (Interval Unitless) where
  representation = FFI.classRepresentation "Interval"

instance FFI (Interval Radians) where
  representation = FFI.classRepresentation "AngleInterval"

instance FFI (Interval Meters) where
  representation = FFI.classRepresentation "LengthInterval"

instance FFI (Interval SquareMeters) where
  representation = FFI.classRepresentation "AreaInterval"

instance HasUnits (Interval units) units

instance Units.Coercion (Interval unitsA) (Interval unitsB) where
  coerce = Data.Coerce.coerce

instance units1 ~ units2 => Intersects (Quantity units1) (Interval units2) units1 where
  value `intersects` interval = exclusion value interval <= ?tolerance

instance units1 ~ units2 => Intersects (Interval units1) (Quantity units2) units1 where
  interval `intersects` value = value `intersects` interval

instance units1 ~ units2 => Intersects (Interval units1) (Interval units2) units1 where
  first `intersects` second = separation first second <= ?tolerance

instance Negation (Interval units) where
  negate (Interval# low# high#) = Interval# (negate# high#) (negate# low#)

instance Multiplication Sign (Interval units) (Interval units) where
  Positive * interval = interval
  Negative * interval = -interval

instance Multiplication (Interval units) Sign (Interval units) where
  interval * Positive = interval
  interval * Negative = -interval

instance units1 ~ units2 => Addition (Interval units1) (Interval units2) (Interval units1) where
  Interval# low1# high1# + Interval# low2# high2# =
    Interval# (low1# +# low2#) (high1# +# high2#)

instance units1 ~ units2 => Addition (Interval units1) (Quantity units2) (Interval units1) where
  Interval# low# high# + Quantity# value# =
    Interval# (low# +# value#) (high# +# value#)

instance units1 ~ units2 => Addition (Quantity units1) (Interval units2) (Interval units1) where
  Quantity# value# + Interval# low# high# =
    Interval# (value# +# low#) (value# +# high#)

instance units1 ~ units2 => Subtraction (Interval units1) (Interval units2) (Interval units1) where
  Interval# low1# high1# - Interval# low2# high2# =
    Interval# (low1# -# high2#) (high1# -# low2#)

instance units1 ~ units2 => Subtraction (Interval units1) (Quantity units2) (Interval units1) where
  Interval# low# high# - Quantity# value# =
    Interval# (low# -# value#) (high# -# value#)

instance units1 ~ units2 => Subtraction (Quantity units1) (Interval units2) (Interval units1) where
  Quantity# value# - Interval# low# high# =
    Interval# (value# -# high#) (value# -# low#)

instance Multiplication_ (Quantity units1) (Interval units2) (Interval (units1 ?*? units2)) where
  Quantity# value# ?*? Interval# low# high# =
    Interval# (value# *# low#) (value# *# high#)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Interval units2) (Interval units3)
  where
  Quantity# value# * Interval# low# high# = Interval# (value# *# low#) (value# *# high#)

instance Multiplication_ (Interval units1) (Quantity units2) (Interval (units1 ?*? units2)) where
  Interval# low# high# ?*? Quantity# value# = Interval# (low# *# value#) (high# *# value#)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Interval units1) (Quantity units2) (Interval units3)
  where
  Interval# low# high# * Quantity# value# = Interval# (low# *# value#) (high# *# value#)

instance Multiplication_ (Interval units1) (Interval units2) (Interval (units1 ?*? units2)) where
  Interval# low1# high1# ?*? Interval# low2# high2# = do
    let !(# low#, high# #) = intervalTimesInterval# low1# high1# low2# high2#
    Ordered# low# high#

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Interval units1) (Interval units2) (Interval units3)
  where
  Interval# low1# high1# * Interval# low2# high2# = do
    let !(# low#, high# #) = intervalTimesInterval# low1# high1# low2# high2#
    Ordered# low# high#

instance Division_ (Quantity units1) (Interval units2) (Interval (units1 ?/? units2)) where
  Quantity# n# ?/? Interval# dl# dh# = do
    let !(# low#, high# #) = doubleOverInterval# n# dl# dh#
    Ordered# low# high#

instance
  Units.Quotient units1 units2 units3 =>
  Division (Quantity units1) (Interval units2) (Interval units3)
  where
  Quantity# n# / Interval# dl# dh# = do
    let !(# low#, high# #) = doubleOverInterval# n# dl# dh#
    Ordered# low# high#

instance Division_ (Interval units1) (Quantity units2) (Interval (units1 ?/? units2)) where
  Interval# nl# nh# ?/? Quantity# d# = do
    let !(# low#, high# #) = intervalOverDouble# nl# nh# d#
    Ordered# low# high#

instance
  Units.Quotient units1 units2 units3 =>
  Division (Interval units1) (Quantity units2) (Interval units3)
  where
  Interval# nl# nh# / Quantity# d# = do
    let !(# low#, high# #) = intervalOverDouble# nl# nh# d#
    Ordered# low# high#

instance Division_ (Interval units1) (Interval units2) (Interval (units1 ?/? units2)) where
  Interval# nl# nh# ?/? Interval# dl# dh# = do
    let !(# low#, high# #) = intervalOverInterval# nl# nh# dl# dh#
    Ordered# low# high#

instance
  Units.Quotient units1 units2 units3 =>
  Division (Interval units1) (Interval units2) (Interval units3)
  where
  Interval# nl# nh# / Interval# dl# dh# = do
    let !(# low#, high# #) = intervalOverInterval# nl# nh# dl# dh#
    Ordered# low# high#

-- | Construct a zero-width bounding range containing a single value.
{-# INLINE constant #-}
constant :: Quantity units -> Interval units
constant value = Interval value value

-- | The bounding range with endoints [0,1].
unit :: Interval Unitless
unit = Interval 0.0 1.0

{-# INLINE coerce #-}
coerce :: Interval units1 -> Interval units2
coerce = Data.Coerce.coerce

-- | Create a bounding range with zero as one of its endpoints and the given value as the other.
zeroTo :: Quantity units -> Interval units
zeroTo value = Interval Quantity.zero value

{-| Create a bounding range symmetric about zero, with the given width.

The lower bound of the range will be -w/2 and the upper bound will be w/2.
-}
symmetric :: "width" # Quantity units -> Interval units
symmetric (Named width_) = let r = 0.5 * width_ in Interval -r r

infinite :: Interval units
infinite = Interval -Quantity.infinity Quantity.infinity

aggregate2 :: Interval units -> Interval units -> Interval units
aggregate2 (Interval low1 high1) (Interval low2 high2) =
  Interval (Prelude.min low1 low2) (Prelude.max high1 high2)

aggregate3 :: Interval units -> Interval units -> Interval units -> Interval units
aggregate3 (Interval low1 high1) (Interval low2 high2) (Interval low3 high3) =
  Interval
    (Prelude.min (Prelude.min low1 low2) low3)
    (Prelude.max (Prelude.max high1 high2) high3)

-- | Build a bounding range containing all ranges in the given non-empty list.
aggregateN :: NonEmpty (Interval units) -> Interval units
aggregateN (Interval low1 high1 :| rest) = do
  let go low high [] = Interval low high
      go low high (Interval nextLow nextHigh : remaining) =
        go (Prelude.min low nextLow) (Prelude.max high nextHigh) remaining
  go low1 high1 rest

-- | Attempt to find the intersection of two bounding ranges.
intersection :: Interval units -> Interval units -> Maybe (Interval units)
intersection (Interval low1 high1) (Interval low2 high2)
  | high1 < low2 = Nothing
  | low1 > high2 = Nothing
  | otherwise = Just (Interval (Prelude.max low1 low2) (Prelude.min high1 high2))

{-# INLINE hull3 #-}
hull3 :: Quantity units -> Quantity units -> Quantity units -> Interval units
hull3 a b c = Interval (Prelude.min a (Prelude.min b c)) (Prelude.max a (Prelude.max b c))

{-# INLINE hull4 #-}
hull4 :: Quantity units -> Quantity units -> Quantity units -> Quantity units -> Interval units
hull4 (Quantity# a#) (Quantity# b#) (Quantity# c#) (Quantity# d#) = do
  let !(# low#, high# #) = hull4# a# b# c# d#
  Ordered# low# high#

-- | Build a bounding range containing all values in the given non-empty list.
hullN :: NonEmpty (Quantity units) -> Interval units
hullN (first :| rest) = do
  let go low high [] = Interval low high
      go low high (next : remaining) =
        go (Prelude.min low next) (Prelude.max high next) remaining
  go first first rest

-- | Get the lower bound of a range.
{-# INLINE lower #-}
lower :: Interval units -> Quantity units
lower (Interval low _) = low

-- | Get the upper bound of a range.
{-# INLINE upper #-}
upper :: Interval units -> Quantity units
upper (Interval _ high) = high

{-# INLINE midpoint #-}
midpoint :: Interval units -> Quantity units
midpoint (Interval low high) = Quantity.midpoint low high

-- | Get the lower and upper bounds of a range.
endpoints :: Interval units -> (Quantity units, Quantity units)
endpoints (Interval low high) = (low, high)

{-# INLINE width #-}
width :: Interval units -> Quantity units
width interval = Quantity# (width# interval)

{-# INLINE width# #-}
width# :: Interval units -> Double#
width# (Interval# low# high#) = high# -# low#

{-# INLINE maxAbs #-}
maxAbs :: Interval units -> Quantity units
maxAbs (Interval low high) = Prelude.max (Quantity.abs low) (Quantity.abs high)

{-# INLINE minAbs #-}
minAbs :: Interval units -> Quantity units
minAbs (Interval low high)
  | low >= Quantity.zero = low
  | high <= Quantity.zero = -high
  | otherwise = Quantity.zero

squared :: Units.Squared units1 units2 => Interval units1 -> Interval units2
squared = Units.specialize . squared_

squared_ :: Interval units -> Interval (units ?*? units)
squared_ (Interval low high) = do
  let ll = low ?*? low
  let hh = high ?*? high
  if
    | low >= Quantity.zero -> Interval ll hh
    | high <= Quantity.zero -> Interval hh ll
    | otherwise -> zeroTo (Prelude.max ll hh)

sqrt_ :: Interval (units ?*? units) -> Interval units
sqrt_ (Interval low high) =
  Interval
    (Quantity.sqrt_ (Prelude.max low Quantity.zero))
    (Quantity.sqrt_ (Prelude.max high Quantity.zero))

sqrt :: Units.Squared units1 units2 => Interval units2 -> Interval units1
sqrt = sqrt_ . Units.unspecialize

hypot2 :: Interval units -> Interval units -> Interval units
hypot2 (Interval# xMin# xMax#) (Interval# yMin# yMax#) = do
  let positiveX# = xMin# >=# 0.0##
  let negativeX# = xMax# <=# 0.0##
  let positiveY# = yMin# >=# 0.0##
  let negativeY# = yMax# <=# 0.0##
  let xMagnitude# = max# (abs# xMin#) (abs# xMax#)
  let yMagnitude# = max# (abs# yMin#) (abs# yMax#)
  let maxMagnitude# = hypot2# xMagnitude# yMagnitude#
  case (# positiveX#, negativeX#, positiveY#, negativeY# #) of
    (# 1#, _, 1#, _ #) -> Interval# (hypot2# xMin# yMin#) maxMagnitude#
    (# 1#, _, _, 1# #) -> Interval# (hypot2# xMin# yMax#) maxMagnitude#
    (# _, 1#, 1#, _ #) -> Interval# (hypot2# xMax# yMin#) maxMagnitude#
    (# _, 1#, _, 1# #) -> Interval# (hypot2# xMax# yMax#) maxMagnitude#
    (# 1#, _, _, _ #) -> Interval# xMin# maxMagnitude#
    (# _, 1#, _, _ #) -> Interval# (negate# xMax#) maxMagnitude#
    (# _, _, 1#, _ #) -> Interval# yMin# maxMagnitude#
    (# _, _, _, 1# #) -> Interval# (negate# yMax#) maxMagnitude#
    (# _, _, _, _ #) -> Interval# 0.0## maxMagnitude#

cubed :: Interval Unitless -> Interval Unitless
cubed (Interval low high) = Interval (low * low * low) (high * high * high)

{-| Check if a given value is included in a bounding range.

Note that this does *not* use a tolerance, so use with care -
for example, a value *just* outside the range (due to numerical roundoff)
will be reported as not included.
-}
includes :: Quantity units -> Interval units -> Bool
includes value (Interval low high) = low <= value && value <= high

exclusion :: Quantity units -> Interval units -> Quantity units
exclusion (Quantity# value#) interval = Quantity# (exclusion# value# interval)

{-# INLINE exclusion# #-}
exclusion# :: Double# -> Interval units -> Double#
exclusion# value# (Interval# low# high#) = max# (low# -# value#) (value# -# high#)

inclusion :: Quantity units -> Interval units -> Quantity units
inclusion (Quantity# value#) interval = Quantity# (inclusion# value# interval)

{-# INLINE inclusion# #-}
inclusion# :: Double# -> Interval units -> Double#
inclusion# value# interval = negate# (exclusion# value# interval)

{-| Check if one bounding range contains another.

Note that this does *not* use a tolerance, so use with care -
for example, a range that extends *just* outside another range (due to numerical
roundoff) will be reported as not contained by that range.
-}
contains :: Interval units -> Interval units -> Bool
contains (Interval low2 high2) (Interval low1 high1) = low1 <= low2 && high2 <= high1

isContainedIn :: Interval units -> Interval units -> Bool
isContainedIn interval value = contains value interval

separation :: Interval units -> Interval units -> Quantity units
separation interval1 interval2 = Quantity# (separation# interval1 interval2)

{-# INLINE separation# #-}
separation# :: Interval units -> Interval units -> Double#
separation# (Interval# low1# high1#) (Interval# low2# high2#) =
  max# (low1# -# high2#) (low2# -# high1#)

overlap :: Interval units -> Interval units -> Quantity units
overlap first second = negate (separation first second)

bisect :: Interval units -> (Interval units, Interval units)
bisect (Interval low high) = do
  let mid
        | low > -Quantity.infinity && high < Quantity.infinity = do
            let value = Quantity.midpoint low high
            assert (low < value && value < high) value
        | low < Quantity.zero && high > Quantity.zero = Quantity.zero
        | low == Quantity.zero = Quantity.unit
        | high == Quantity.zero = -Quantity.unit
        | low > Quantity.zero = 2.0 * low
        | high < Quantity.zero = 2.0 * high
        | otherwise = throw (InternalError "'Impossible' case hit in Interval.bisect")
  (Interval low mid, Interval mid high)

{-# INLINE isAtomic #-}
isAtomic :: Interval units -> Bool
isAtomic (Interval low high) = do
  let mid = Quantity.midpoint low high
  mid == low || mid == high

{-# INLINE isFinite #-}
isFinite :: Interval units -> Bool
isFinite (Interval low high) = -Quantity.infinity < low && high < Quantity.infinity

abs :: Interval units -> Interval units
abs interval@(Interval low high)
  | low >= Quantity.zero = interval
  | high <= Quantity.zero = -interval
  | otherwise = zeroTo (Prelude.max high -low)

min :: Interval units -> Interval units -> Interval units
min (Interval low1 high1) (Interval low2 high2) =
  Interval (Prelude.min low1 low2) (Prelude.min high1 high2)

max :: Interval units -> Interval units -> Interval units
max (Interval low1 high1) (Interval low2 high2) =
  Interval (Prelude.max low1 low2) (Prelude.max high1 high2)

minimum :: NonEmpty (Interval units) -> Interval units
minimum = NonEmpty.reduce min

maximum :: NonEmpty (Interval units) -> Interval units
maximum = NonEmpty.reduce max

sin :: Interval Radians -> Interval Unitless
sin interval@(Interval low high) = do
  let (includesMin, includesMax) = sinIncludesMinMax interval
  let newLow = if includesMin then -1.0 else Prelude.min (Angle.sin low) (Angle.sin high)
  let newHigh = if includesMax then 1.0 else Prelude.max (Angle.sin low) (Angle.sin high)
  Interval newLow newHigh

cos :: Interval Radians -> Interval Unitless
cos interval@(Interval low high) = do
  let (includesMin, includesMax) = cosIncludesMinMax interval
  let newLow = if includesMin then -1.0 else Prelude.min (Angle.cos low) (Angle.cos high)
  let newHigh = if includesMax then 1.0 else Prelude.max (Angle.cos low) (Angle.cos high)
  Interval newLow newHigh

sinIncludesMinMax :: Interval Radians -> (Bool, Bool)
sinIncludesMinMax interval = cosIncludesMinMax (interval - Angle.halfPi)

cosIncludesMinMax :: Interval Radians -> (Bool, Bool)
cosIncludesMinMax interval = (cosIncludesMax (interval + Angle.pi), cosIncludesMax interval)

cosIncludesMax :: Interval Radians -> Bool
cosIncludesMax (Interval low high) =
  (Quantity.isInfinite low || Quantity.isInfinite high)
    || (low // Angle.twoPi /= high // Angle.twoPi)

interpolate :: Interval units -> Number -> Quantity units
interpolate (Interval low high) t =
  Quantity.interpolateFrom low high t

interpolationParameter :: Interval units -> Quantity units -> Number
interpolationParameter (Interval low high) value
  | low < high = (value - low) / (high - low)
  | value < low = -Quantity.infinity
  | value > high = Quantity.infinity
  | otherwise = 0.0

resolution :: Interval units -> Number
resolution (Interval low high)
  | low > Quantity.zero = low / high
  | high < Quantity.zero = -high / low
  | otherwise = 0.0

resolutionThreshold :: Number
resolutionThreshold = 0.5

isResolved :: Interval units -> Bool
isResolved interval = Number.abs (resolution interval) >= resolutionThreshold

resolvedSign :: Interval units -> Fuzzy Sign
resolvedSign interval = do
  let intervalResolution = resolution interval
  if Number.abs intervalResolution >= resolutionThreshold
    then Resolved (Number.sign intervalResolution)
    else Unresolved

random :: Random.Generator (Quantity units) -> Random.Generator (Interval units)
random randomQuantity = do
  a <- randomQuantity
  b <- randomQuantity
  Random.return (Interval a b)

sampleValues :: Interval units -> NonEmpty (Quantity units)
sampleValues interval = NonEmpty.map (interpolate interval) Parameter.samples

convert :: Quantity (units2 ?/? units1) -> Interval units1 -> Interval units2
convert factor interval = Units.simplify (interval ?*? factor)

unconvert :: Quantity (units2 ?/? units1) -> Interval units2 -> Interval units1
unconvert factor interval = Units.simplify (interval ?/? factor)
