{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Interval
  ( Interval (Interval, I#)
  , constant
  , unit
  , coerce
  , erase
  , zeroTo
  , plusMinus
  , infinite
  , hull3
  , hull4
  , hull
  , hullOf
  , lower
  , upper
  , midpoint
  , endpoints
  , width
  , maxAbs
  , minAbs
  , squared
  , squared_
  , member
  , inclusion
  , exclusion
  , contains
  , overlap
  , separation
  , containedIn
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
  , aggregate
  , aggregateOf
  , min
  , max
  , minimum
  , maximum
  , sin
  , cos
  , interpolationParameter
  , resolution
  , isResolved
  , resolvedSign
  , areDistinct
  , isDistinctFrom
  , intersection
  , random
  , sampleValues
  , convert
  , unconvert
  )
where

import Data.Coerce qualified
import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import {-# SOURCE #-} OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude hiding (max, min)
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random qualified as Random
import OpenSolid.Show qualified as Show
import OpenSolid.Unboxed.Math
import OpenSolid.Units (SquareMeters, Units)
import OpenSolid.Units qualified as Units
import Prelude qualified

type role Interval phantom

type Interval :: Type -> Type

-- | A range of possible values, with a lower bound and upper bound.
data Interval units = I# Interval#

instance Show (Interval units) where
  showsPrec precedence (Interval low high) =
    Show.constructor2 precedence "Interval" low high

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
  Interval (Q# a#) (Q# b#) = I# (Interval# a# b#)

{-# INLINE viewInterval #-}
viewInterval :: Interval units -> (# Quantity units, Quantity units #)
viewInterval (I# (Interval# low# high#)) = (# Q# low#, Q# high# #)

{-# COMPLETE Interval #-}

instance FFI (Interval Unitless) where
  representation = FFI.classRepresentation "Interval"

instance FFI (Interval Radians) where
  representation = FFI.classRepresentation "AngleInterval"

instance FFI (Interval Meters) where
  representation = FFI.classRepresentation "LengthInterval"

instance FFI (Interval SquareMeters) where
  representation = FFI.classRepresentation "AreaInterval"

instance Units (Interval units) units

instance units1 ~ units2 => Bounded (Interval units1) (Interval units2) where
  {-# INLINE bounds #-}
  bounds = id

instance units1 ~ units2 => Bounded (Quantity units1) (Interval units2) where
  {-# INLINE bounds #-}
  bounds = constant

instance Units.Coercion (Interval units1) (Interval units2) where
  {-# INLINE coerce #-}
  coerce = Data.Coerce.coerce

instance At (Interval units) Number (Quantity units) where
  {-# INLINE (@) #-}
  I# (Interval# low# high#) @ Q# t# = Q# (interpolateFrom# low# high# t#)

instance units1 ~ units2 => Intersects (Quantity units1) (Interval units2) units1 where
  {-# INLINE (^) #-}
  Q# value# ^ I# interval# = do
    let !(Q# tolerance#) = ?tolerance
    B# (intersects# tolerance# value# interval#)

instance units1 ~ units2 => Intersects (Interval units1) (Quantity units2) units1 where
  {-# INLINE (^) #-}
  interval ^ value = value ^ interval

instance units1 ~ units2 => Intersects (Interval units1) (Interval units2) units1 where
  {-# INLINE (^) #-}
  I# first# ^ I# second# = do
    let !(Q# tolerance#) = ?tolerance
    B# (intersects# tolerance# first# second#)

instance Negation (Interval units) where
  {-# INLINE negate #-}
  negate (I# interval#) = I# (negate# interval#)

instance Multiplication Sign (Interval units) (Interval units) where
  {-# INLINE (*) #-}
  Positive * interval = interval
  Negative * interval = -interval

instance Multiplication (Interval units) Sign (Interval units) where
  {-# INLINE (*) #-}
  interval * Positive = interval
  interval * Negative = -interval

instance units1 ~ units2 => Addition (Interval units1) (Interval units2) (Interval units1) where
  {-# INLINE (+) #-}
  I# first# + I# second# = I# (first# +# second#)

instance units1 ~ units2 => Addition (Interval units1) (Quantity units2) (Interval units1) where
  {-# INLINE (+) #-}
  I# interval# + Q# value# = I# (interval# +# value#)

instance units1 ~ units2 => Addition (Quantity units1) (Interval units2) (Interval units1) where
  {-# INLINE (+) #-}
  Q# value# + I# interval# = I# (value# +# interval#)

instance units1 ~ units2 => Subtraction (Interval units1) (Interval units2) (Interval units1) where
  {-# INLINE (-) #-}
  I# first# - I# second# = I# (first# -# second#)

instance units1 ~ units2 => Subtraction (Interval units1) (Quantity units2) (Interval units1) where
  {-# INLINE (-) #-}
  I# interval# - Q# value# = I# (interval# -# value#)

instance units1 ~ units2 => Subtraction (Quantity units1) (Interval units2) (Interval units1) where
  {-# INLINE (-) #-}
  Q# value# - I# interval# = I# (value# -# interval#)

instance Multiplication_ (Quantity units1) (Interval units2) (Interval (units1 ?*? units2)) where
  {-# INLINE (?*?) #-}
  Q# value# ?*? I# interval# = I# (value# *# interval#)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Interval units2) (Interval units3)
  where
  {-# INLINE (*) #-}
  Q# value# * I# interval# = I# (value# *# interval#)

instance Multiplication_ (Interval units1) (Quantity units2) (Interval (units1 ?*? units2)) where
  {-# INLINE (?*?) #-}
  I# interval# ?*? Q# value# = I# (interval# *# value#)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Interval units1) (Quantity units2) (Interval units3)
  where
  {-# INLINE (*) #-}
  I# interval# * Q# value# = I# (interval# *# value#)

instance Multiplication_ (Interval units1) (Interval units2) (Interval (units1 ?*? units2)) where
  {-# INLINE (?*?) #-}
  I# first# ?*? I# second# = I# (first# *# second#)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Interval units1) (Interval units2) (Interval units3)
  where
  {-# INLINE (*) #-}
  I# first# * I# second# = I# (first# *# second#)

instance DotMultiplication_ (Quantity units1) (Interval units2) (Interval (units1 ?*? units2)) where
  {-# INLINE dot_ #-}
  dot_ = (?*?)

instance
  Units.Product units1 units2 units3 =>
  DotMultiplication (Quantity units1) (Interval units2) (Interval units3)
  where
  {-# INLINE dot #-}
  dot = (*)

instance DotMultiplication_ (Interval units1) (Quantity units2) (Interval (units1 ?*? units2)) where
  {-# INLINE dot_ #-}
  dot_ = (?*?)

instance
  Units.Product units1 units2 units3 =>
  DotMultiplication (Interval units1) (Quantity units2) (Interval units3)
  where
  {-# INLINE dot #-}
  dot = (*)

instance DotMultiplication_ (Interval units1) (Interval units2) (Interval (units1 ?*? units2)) where
  {-# INLINE dot_ #-}
  dot_ = (?*?)

instance
  Units.Product units1 units2 units3 =>
  DotMultiplication (Interval units1) (Interval units2) (Interval units3)
  where
  {-# INLINE dot #-}
  dot = (*)

instance Division_ (Quantity units1) (Interval units2) (Interval (units1 ?/? units2)) where
  {-# INLINE (?/?) #-}
  Q# value# ?/? I# interval# = I# (value# /# interval#)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Quantity units1) (Interval units2) (Interval units3)
  where
  {-# INLINE (/) #-}
  Q# value# / I# interval# = I# (value# /# interval#)

instance Division_ (Interval units1) (Quantity units2) (Interval (units1 ?/? units2)) where
  {-# INLINE (?/?) #-}
  I# interval# ?/? Q# value# = I# (interval# /# value#)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Interval units1) (Quantity units2) (Interval units3)
  where
  {-# INLINE (/) #-}
  I# interval# / Q# value# = I# (interval# /# value#)

instance Division_ (Interval units1) (Interval units2) (Interval (units1 ?/? units2)) where
  {-# INLINE (?/?) #-}
  I# first# ?/? I# second# = I# (first# /# second#)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Interval units1) (Interval units2) (Interval units3)
  where
  {-# INLINE (/) #-}
  I# first# / I# second# = I# (first# /# second#)

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

{-# INLINE erase #-}
erase :: Interval units -> Interval Unitless
erase = coerce

-- | Create a bounding range with zero as one of its endpoints and the given value as the other.
zeroTo :: Quantity units -> Interval units
zeroTo value = Interval Quantity.zero value

-- | Create an interval symmetric about zero, with the given radius.
plusMinus :: Quantity units -> Interval units
plusMinus value = Interval -value value

infinite :: Interval units
infinite = Interval -Quantity.infinity Quantity.infinity

aggregate2 :: Interval units -> Interval units -> Interval units
aggregate2 (I# (Interval# low1# high1#)) (I# (Interval# low2# high2#)) =
  I# (Interval# (min# low1# low2#) (max# high1# high2#))

aggregate3 :: Interval units -> Interval units -> Interval units -> Interval units
aggregate3 first second third = do
  let !(I# (Interval# low1# high1#)) = first
  let !(I# (Interval# low2# high2#)) = second
  let !(I# (Interval# low3# high3#)) = third
  I# (Interval# (low1# `min#` low2# `min#` low3#) (high1# `max#` high2# `max#` high3#))

-- | Build a bounding range containing all ranges in the given non-empty list.
aggregate :: NonEmpty (Interval units) -> Interval units
aggregate (I# (Interval# low# high#) :| rest) = aggregateImpl low# high# rest

aggregateImpl :: Double# -> Double# -> List (Interval units) -> Interval units
aggregateImpl low# high# [] = I# (Interval# low# high#)
aggregateImpl low# high# (I# (Interval# nextLow# nextHigh#) : remaining) =
  aggregateImpl (min# low# nextLow#) (max# high# nextHigh#) remaining

aggregateOf :: (a -> Interval units) -> NonEmpty a -> Interval units
aggregateOf function nonEmpty = aggregate (NonEmpty.map function nonEmpty)

-- | Attempt to find the intersection of two bounding ranges.
intersection :: Interval units -> Interval units -> Maybe (Interval units)
intersection (Interval low1 high1) (Interval low2 high2)
  | high1 < low2 = Nothing
  | low1 > high2 = Nothing
  | otherwise = Just (Interval (Prelude.max low1 low2) (Prelude.min high1 high2))

{-# INLINE hull3 #-}
hull3 :: Quantity units -> Quantity units -> Quantity units -> Interval units
hull3 (Q# a#) (Q# b#) (Q# c#) = I# (hull3# a# b# c#)

{-# INLINE hull4 #-}
hull4 :: Quantity units -> Quantity units -> Quantity units -> Quantity units -> Interval units
hull4 (Q# a#) (Q# b#) (Q# c#) (Q# d#) = I# (hull4# a# b# c# d#)

-- | Build a bounding range containing all values in the given non-empty list.
hull :: NonEmpty (Quantity units) -> Interval units
hull (Q# first# :| rest) = hullImpl first# first# rest

hullImpl :: Double# -> Double# -> List (Quantity units) -> Interval units
hullImpl low# high# [] = I# (Interval# low# high#)
hullImpl low# high# (Q# next# : remaining) =
  hullImpl (min# low# next#) (max# high# next#) remaining

hullOf :: (a -> Quantity units) -> NonEmpty a -> Interval units
hullOf function nonEmpty = hull (NonEmpty.map function nonEmpty)

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
width (I# interval#) = Q# (diameter# interval#)

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
hypot2 (I# (Interval# xMin# xMax#)) (I# (Interval# yMin# yMax#)) = do
  let positiveX# = xMin# >=# 0.0##
  let negativeX# = xMax# <=# 0.0##
  let positiveY# = yMin# >=# 0.0##
  let negativeY# = yMax# <=# 0.0##
  let xMagnitude# = max# (abs# xMin#) (abs# xMax#)
  let yMagnitude# = max# (abs# yMin#) (abs# yMax#)
  let maxMagnitude# = hypot2# xMagnitude# yMagnitude#
  I# $ case (# positiveX#, negativeX#, positiveY#, negativeY# #) of
    (# True#, _, True#, _ #) -> Interval# (hypot2# xMin# yMin#) maxMagnitude#
    (# True#, _, _, True# #) -> Interval# (hypot2# xMin# yMax#) maxMagnitude#
    (# _, True#, True#, _ #) -> Interval# (hypot2# xMax# yMin#) maxMagnitude#
    (# _, True#, _, True# #) -> Interval# (hypot2# xMax# yMax#) maxMagnitude#
    (# True#, _, _, _ #) -> Interval# xMin# maxMagnitude#
    (# _, True#, _, _ #) -> Interval# (negate# xMax#) maxMagnitude#
    (# _, _, True#, _ #) -> Interval# yMin# maxMagnitude#
    (# _, _, _, True# #) -> Interval# (negate# yMax#) maxMagnitude#
    (# _, _, _, _ #) -> Interval# 0.0## maxMagnitude#

cubed :: Interval Unitless -> Interval Unitless
cubed (Interval low high) = Interval (low * low * low) (high * high * high)

{-| Check if a given value is included in a bounding range.

Note that this does *not* use a tolerance, so use with care -
for example, a value *just* outside the range (due to numerical roundoff)
will be reported as not included.
-}
member :: Quantity units -> Interval units -> Bool
member value (Interval low high) = low <= value && value <= high

exclusion :: Quantity units -> Interval units -> Quantity units
exclusion (Q# value#) (I# interval#) = Q# (exclusion# value# interval#)

inclusion :: Quantity units -> Interval units -> Quantity units
inclusion (Q# value#) (I# interval#) = Q# (inclusion# value# interval#)

{-| Check if one bounding range contains another.

Note that this does *not* use a tolerance, so use with care -
for example, a range that extends *just* outside another range (due to numerical
roundoff) will be reported as not contained by that range.
-}
contains :: Interval units -> Interval units -> Bool
contains (Interval low2 high2) (Interval low1 high1) = low1 <= low2 && high2 <= high1

containedIn :: Interval units -> Interval units -> Bool
containedIn interval value = contains value interval

separation :: Interval units -> Interval units -> Quantity units
separation (I# first#) (I# second#) = Q# (separation# first# second#)

overlap :: Interval units -> Interval units -> Quantity units
overlap (I# first#) (I# second#) = Q# (overlap# first# second#)

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
        | otherwise = error "'Impossible' case hit in Interval.bisect"
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
resolutionThreshold = 0.25

isResolved :: Interval units -> Bool
isResolved interval = Number.abs (resolution interval) >= resolutionThreshold

resolvedSign :: Interval units -> Fuzzy Sign
resolvedSign interval = do
  let intervalResolution = resolution interval
  if Number.abs intervalResolution >= resolutionThreshold
    then Resolved (Number.sign intervalResolution)
    else Unresolved

areDistinct :: Interval units -> Interval units -> Bool
areDistinct first second = isResolved (second - first)

isDistinctFrom :: Quantity units -> Interval units -> Bool
isDistinctFrom value interval = isResolved (interval - value)

random :: Random.Generator (Quantity units) -> Random.Generator (Interval units)
random randomQuantity = Random.map2 Interval randomQuantity randomQuantity

sampleValues :: Interval units -> NonEmpty (Quantity units)
sampleValues interval = interval @ Parameter.samples

convert :: Quantity (units2 ?/? units1) -> Interval units1 -> Interval units2
convert factor interval = Units.simplify (interval ?*? factor)

unconvert :: Quantity (units2 ?/? units1) -> Interval units2 -> Interval units1
unconvert factor interval = Units.simplify (interval ?/? factor)
