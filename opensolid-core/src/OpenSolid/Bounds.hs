{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Bounds
  ( Bounds (Bounds, Bounds##)
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
  , width##
  , maxAbs
  , minAbs
  , squared
  , squared#
  , includes
  , inclusion
  , inclusion##
  , exclusion
  , exclusion##
  , contains
  , overlap
  , separation
  , separation##
  , isContainedIn
  , bisect
  , isAtomic
  , isFinite
  , abs
  , sqrt
  , sqrt#
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
import GHC.Records (HasField (getField))
import OpenSolid.Angle qualified as Angle
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.InternalError (InternalError (InternalError))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import {-# SOURCE #-} OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude hiding (max, min)
import OpenSolid.Quantity (Quantity (Quantity##))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random qualified as Random
import OpenSolid.Unboxed.Math
import OpenSolid.Units (HasUnits, SquareMeters)
import OpenSolid.Units qualified as Units
import Prelude qualified

type role Bounds phantom

type Bounds :: Type -> Type

-- | A range of possible values, with a lower bound and upper bound.
data Bounds units = Ordered## Double# Double#
  deriving (Eq, Show)

instance HasField "endpoints" (Bounds units) (Quantity units, Quantity units) where
  getField = endpoints

instance HasField "lower" (Bounds units) (Quantity units) where
  getField = lower

instance HasField "upper" (Bounds units) (Quantity units) where
  getField = upper

instance HasField "width" (Bounds units) (Quantity units) where
  getField = width

{-| Construct a bounding range from two given values (endpoints).

The order of the two arguments does not matter;
the minimum of the two will be used as the lower bound
and the maximum will be used as the upper bound.

If either argument is NaN, then the result will be open/infinite
(with endpoints negative infinity and positive infinity).
-}
{-# INLINE Bounds #-}
pattern Bounds :: Quantity units -> Quantity units -> Bounds units
pattern Bounds low high <- (viewBounds -> (# low, high #))
  where
    Bounds (Quantity## a##) (Quantity## b##) = Bounds## a## b##

{-# INLINE viewBounds #-}
viewBounds :: Bounds units -> (# Quantity units, Quantity units #)
viewBounds (Ordered## low## high##) = (# Quantity## low##, Quantity## high## #)

{-# COMPLETE Bounds #-}

{-# INLINE Bounds## #-}
pattern Bounds## :: Double# -> Double# -> Bounds units
pattern Bounds## low## high## <- Ordered## low## high##
  where
    Bounds## a## b## = do
      let !(# low##, high## #) = hull2## a## b##
      Ordered## low## high##

{-# COMPLETE Bounds## #-}

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

instance units1 ~ units2 => Intersects (Quantity units1) (Bounds units2) units1 where
  value `intersects` bounds = exclusion value bounds <= ?tolerance

instance units1 ~ units2 => Intersects (Bounds units1) (Quantity units2) units1 where
  bounds `intersects` value = value `intersects` bounds

instance units1 ~ units2 => Intersects (Bounds units1) (Bounds units2) units1 where
  first `intersects` second = separation first second <= ?tolerance

instance Negation (Bounds units) where
  negative (Bounds## low## high##) = Bounds## (negate## high##) (negate## low##)

instance Multiplication Sign (Bounds units) (Bounds units) where
  Positive .*. bounds = bounds
  Negative .*. bounds = negative bounds

instance Multiplication (Bounds units) Sign (Bounds units) where
  bounds .*. Positive = bounds
  bounds .*. Negative = negative bounds

instance units1 ~ units2 => Addition (Bounds units1) (Bounds units2) (Bounds units1) where
  Bounds## low1## high1## .+. Bounds## low2## high2## =
    Bounds## (low1## +## low2##) (high1## +## high2##)

instance units1 ~ units2 => Addition (Bounds units1) (Quantity units2) (Bounds units1) where
  Bounds## low## high## .+. Quantity## value## =
    Bounds## (low## +## value##) (high## +## value##)

instance units1 ~ units2 => Addition (Quantity units1) (Bounds units2) (Bounds units1) where
  Quantity## value## .+. Bounds## low## high## =
    Bounds## (value## +## low##) (value## +## high##)

instance units1 ~ units2 => Subtraction (Bounds units1) (Bounds units2) (Bounds units1) where
  Bounds## low1## high1## .-. Bounds## low2## high2## =
    Bounds## (low1## -## high2##) (high1## -## low2##)

instance units1 ~ units2 => Subtraction (Bounds units1) (Quantity units2) (Bounds units1) where
  Bounds## low## high## .-. Quantity## value## =
    Bounds## (low## -## value##) (high## -## value##)

instance units1 ~ units2 => Subtraction (Quantity units1) (Bounds units2) (Bounds units1) where
  Quantity## value## .-. Bounds## low## high## =
    Bounds## (value## -## high##) (value## -## low##)

instance Multiplication# (Quantity units1) (Bounds units2) (Bounds (units1 #*# units2)) where
  Quantity## value## #*# Bounds## low## high## =
    Bounds## (value## *## low##) (value## *## high##)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (Bounds units2) (Bounds units3)
  where
  Quantity## value## .*. Bounds## low## high## = Bounds## (value## *## low##) (value## *## high##)

instance Multiplication# (Bounds units1) (Quantity units2) (Bounds (units1 #*# units2)) where
  Bounds## low## high## #*# Quantity## value## = Bounds## (low## *## value##) (high## *## value##)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (Quantity units2) (Bounds units3)
  where
  Bounds## low## high## .*. Quantity## value## = Bounds## (low## *## value##) (high## *## value##)

instance Multiplication# (Bounds units1) (Bounds units2) (Bounds (units1 #*# units2)) where
  Bounds## low1## high1## #*# Bounds## low2## high2## = do
    let !(# low##, high## #) = boundsTimesBounds## low1## high1## low2## high2##
    Ordered## low## high##

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Bounds units1) (Bounds units2) (Bounds units3)
  where
  Bounds## low1## high1## .*. Bounds## low2## high2## = do
    let !(# low##, high## #) = boundsTimesBounds## low1## high1## low2## high2##
    Ordered## low## high##

instance Division# (Quantity units1) (Bounds units2) (Bounds (units1 #/# units2)) where
  Quantity## n## #/# Bounds## dl## dh## = do
    let !(# low##, high## #) = doubleOverBounds## n## dl## dh##
    Ordered## low## high##

instance
  Units.Quotient units1 units2 units3 =>
  Division (Quantity units1) (Bounds units2) (Bounds units3)
  where
  Quantity## n## ./. Bounds## dl## dh## = do
    let !(# low##, high## #) = doubleOverBounds## n## dl## dh##
    Ordered## low## high##

instance Division# (Bounds units1) (Quantity units2) (Bounds (units1 #/# units2)) where
  Bounds## nl## nh## #/# Quantity## d## = do
    let !(# low##, high## #) = boundsOverDouble## nl## nh## d##
    Ordered## low## high##

instance
  Units.Quotient units1 units2 units3 =>
  Division (Bounds units1) (Quantity units2) (Bounds units3)
  where
  Bounds## nl## nh## ./. Quantity## d## = do
    let !(# low##, high## #) = boundsOverDouble## nl## nh## d##
    Ordered## low## high##

instance Division# (Bounds units1) (Bounds units2) (Bounds (units1 #/# units2)) where
  Bounds## nl## nh## #/# Bounds## dl## dh## = do
    let !(# low##, high## #) = boundsOverBounds## nl## nh## dl## dh##
    Ordered## low## high##

instance
  Units.Quotient units1 units2 units3 =>
  Division (Bounds units1) (Bounds units2) (Bounds units3)
  where
  Bounds## nl## nh## ./. Bounds## dl## dh## = do
    let !(# low##, high## #) = boundsOverBounds## nl## nh## dl## dh##
    Ordered## low## high##

-- | Construct a zero-width bounding range containing a single value.
{-# INLINE constant #-}
constant :: Quantity units -> Bounds units
constant value = Bounds value value

-- | The bounding range with endoints [0,1].
unitInterval :: Bounds Unitless
unitInterval = Bounds 0 1

{-# INLINE coerce #-}
coerce :: Bounds units1 -> Bounds units2
coerce = Data.Coerce.coerce

-- | Create a bounding range with zero as one of its endpoints and the given value as the other.
zeroTo :: Quantity units -> Bounds units
zeroTo value = Bounds Quantity.zero value

{-| Create a bounding range symmetric about zero, with the given width.

The lower bound of the range will be -w/2 and the upper bound will be w/2.
-}
symmetric :: "width" ::: Quantity units -> Bounds units
symmetric (Named width_) = let r = 0.5 *. width_ in Bounds (negative r) r

infinite :: Bounds units
infinite = Bounds (negative Quantity.infinity) Quantity.infinity

aggregate2 :: Bounds units -> Bounds units -> Bounds units
aggregate2 (Bounds low1 high1) (Bounds low2 high2) =
  Bounds (Prelude.min low1 low2) (Prelude.max high1 high2)

aggregate3 :: Bounds units -> Bounds units -> Bounds units -> Bounds units
aggregate3 (Bounds low1 high1) (Bounds low2 high2) (Bounds low3 high3) =
  Bounds
    (Prelude.min (Prelude.min low1 low2) low3)
    (Prelude.max (Prelude.max high1 high2) high3)

-- | Build a bounding range containing all ranges in the given non-empty list.
aggregateN :: NonEmpty (Bounds units) -> Bounds units
aggregateN (Bounds low1 high1 :| rest) = do
  let go low high [] = Bounds low high
      go low high (Bounds nextLow nextHigh : remaining) =
        go (Prelude.min low nextLow) (Prelude.max high nextHigh) remaining
  go low1 high1 rest

-- | Attempt to find the intersection of two bounding ranges.
intersection :: Bounds units -> Bounds units -> Maybe (Bounds units)
intersection (Bounds low1 high1) (Bounds low2 high2)
  | high1 < low2 = Nothing
  | low1 > high2 = Nothing
  | otherwise = Just (Bounds (Prelude.max low1 low2) (Prelude.min high1 high2))

{-# INLINE hull3 #-}
hull3 :: Quantity units -> Quantity units -> Quantity units -> Bounds units
hull3 a b c = Bounds (Prelude.min a (Prelude.min b c)) (Prelude.max a (Prelude.max b c))

{-# INLINE hull4 #-}
hull4 :: Quantity units -> Quantity units -> Quantity units -> Quantity units -> Bounds units
hull4 (Quantity## a##) (Quantity## b##) (Quantity## c##) (Quantity## d##) = do
  let !(# low##, high## #) = hull4## a## b## c## d##
  Ordered## low## high##

-- | Build a bounding range containing all values in the given non-empty list.
hullN :: NonEmpty (Quantity units) -> Bounds units
hullN (first :| rest) = do
  let go low high [] = Bounds low high
      go low high (next : remaining) =
        go (Prelude.min low next) (Prelude.max high next) remaining
  go first first rest

-- | Get the lower bound of a range.
{-# INLINE lower #-}
lower :: Bounds units -> Quantity units
lower (Bounds low _) = low

-- | Get the upper bound of a range.
{-# INLINE upper #-}
upper :: Bounds units -> Quantity units
upper (Bounds _ high) = high

{-# INLINE midpoint #-}
midpoint :: Bounds units -> Quantity units
midpoint (Bounds low high) = Quantity.midpoint low high

-- | Get the lower and upper bounds of a range.
endpoints :: Bounds units -> (Quantity units, Quantity units)
endpoints (Bounds low high) = (low, high)

{-# INLINE width #-}
width :: Bounds units -> Quantity units
width bounds = Quantity## (width## bounds)

{-# INLINE width## #-}
width## :: Bounds units -> Double#
width## (Bounds## low## high##) = high## -## low##

{-# INLINE maxAbs #-}
maxAbs :: Bounds units -> Quantity units
maxAbs (Bounds low high) = Prelude.max (Quantity.abs low) (Quantity.abs high)

{-# INLINE minAbs #-}
minAbs :: Bounds units -> Quantity units
minAbs (Bounds low high)
  | low >= Quantity.zero = low
  | high <= Quantity.zero = negative high
  | otherwise = Quantity.zero

squared :: Units.Squared units1 units2 => Bounds units1 -> Bounds units2
squared = Units.specialize . squared#

squared# :: Bounds units -> Bounds (units #*# units)
squared# (Bounds low high) = do
  let ll = low #*# low
  let hh = high #*# high
  if
    | low >= Quantity.zero -> Bounds ll hh
    | high <= Quantity.zero -> Bounds hh ll
    | otherwise -> Bounds Quantity.zero (Prelude.max ll hh)

sqrt# :: Bounds (units #*# units) -> Bounds units
sqrt# (Bounds low high) =
  Bounds
    (Quantity.sqrt# (Prelude.max low Quantity.zero))
    (Quantity.sqrt# (Prelude.max high Quantity.zero))

sqrt :: Units.Squared units1 units2 => Bounds units2 -> Bounds units1
sqrt = sqrt# . Units.unspecialize

hypot2 :: Bounds units -> Bounds units -> Bounds units
hypot2 (Bounds## xMin## xMax##) (Bounds## yMin## yMax##) = do
  let positiveX## = xMin## >=## 0.0##
  let negativeX## = xMax## <=## 0.0##
  let positiveY## = yMin## >=## 0.0##
  let negativeY## = yMax## <=## 0.0##
  let xMagnitude## = max## (abs## xMin##) (abs## xMax##)
  let yMagnitude## = max## (abs## yMin##) (abs## yMax##)
  let maxMagnitude## = hypot2## xMagnitude## yMagnitude##
  case (# positiveX##, negativeX##, positiveY##, negativeY## #) of
    (# 1#, _, 1#, _ #) -> Bounds## (hypot2## xMin## yMin##) maxMagnitude##
    (# 1#, _, _, 1# #) -> Bounds## (hypot2## xMin## yMax##) maxMagnitude##
    (# _, 1#, 1#, _ #) -> Bounds## (hypot2## xMax## yMin##) maxMagnitude##
    (# _, 1#, _, 1# #) -> Bounds## (hypot2## xMax## yMax##) maxMagnitude##
    (# 1#, _, _, _ #) -> Bounds## xMin## maxMagnitude##
    (# _, 1#, _, _ #) -> Bounds## (negate## xMax##) maxMagnitude##
    (# _, _, 1#, _ #) -> Bounds## yMin## maxMagnitude##
    (# _, _, _, 1# #) -> Bounds## (negate## yMax##) maxMagnitude##
    (# _, _, _, _ #) -> Bounds## 0.0## maxMagnitude##

cubed :: Bounds Unitless -> Bounds Unitless
cubed (Bounds low high) = Bounds (low .*. low .*. low) (high .*. high .*. high)

{-| Check if a given value is included in a bounding range.

Note that this does *not* use a tolerance, so use with care -
for example, a value *just* outside the range (due to numerical roundoff)
will be reported as not included.
-}
includes :: Quantity units -> Bounds units -> Bool
includes value (Bounds low high) = low <= value && value <= high

exclusion :: Quantity units -> Bounds units -> Quantity units
exclusion (Quantity## value##) bounds = Quantity## (exclusion## value## bounds)

{-# INLINE exclusion## #-}
exclusion## :: Double# -> Bounds units -> Double#
exclusion## value## (Bounds## low## high##) = max## (low## -## value##) (value## -## high##)

inclusion :: Quantity units -> Bounds units -> Quantity units
inclusion (Quantity## value##) bounds = Quantity## (inclusion## value## bounds)

{-# INLINE inclusion## #-}
inclusion## :: Double# -> Bounds units -> Double#
inclusion## value## bounds = negate## (exclusion## value## bounds)

{-| Check if one bounding range contains another.

Note that this does *not* use a tolerance, so use with care -
for example, a range that extends *just* outside another range (due to numerical
roundoff) will be reported as not contained by that range.
-}
contains :: Bounds units -> Bounds units -> Bool
contains (Bounds low2 high2) (Bounds low1 high1) = low1 <= low2 && high2 <= high1

isContainedIn :: Bounds units -> Bounds units -> Bool
isContainedIn bounds value = contains value bounds

separation :: Bounds units -> Bounds units -> Quantity units
separation bounds1 bounds2 = Quantity## (separation## bounds1 bounds2)

{-# INLINE separation## #-}
separation## :: Bounds units -> Bounds units -> Double#
separation## (Bounds## low1## high1##) (Bounds## low2## high2##) =
  max## (low1## -## high2##) (low2## -## high1##)

overlap :: Bounds units -> Bounds units -> Quantity units
overlap first second = negative (separation first second)

bisect :: Bounds units -> (Bounds units, Bounds units)
bisect (Bounds low high) = do
  let mid
        | low > negative Quantity.infinity && high < Quantity.infinity = do
            let value = Quantity.midpoint low high
            assert (low < value && value < high) value
        | low < Quantity.zero && high > Quantity.zero = Quantity.zero
        | low == Quantity.zero = Quantity 1
        | high == Quantity.zero = Quantity -1
        | low > Quantity.zero = 2 *. low
        | high < Quantity.zero = 2 *. high
        | otherwise = throw (InternalError "'Impossible' case hit in Bounds.bisect")
  (Bounds low mid, Bounds mid high)

{-# INLINE isAtomic #-}
isAtomic :: Bounds units -> Bool
isAtomic (Bounds low high) = do
  let mid = Quantity.midpoint low high
  mid == low || mid == high

{-# INLINE isFinite #-}
isFinite :: Bounds units -> Bool
isFinite (Bounds low high) = negative Quantity.infinity < low && high < Quantity.infinity

abs :: Bounds units -> Bounds units
abs bounds@(Bounds low high)
  | low >= Quantity.zero = bounds
  | high <= Quantity.zero = negative bounds
  | otherwise = Bounds Quantity.zero (Prelude.max high (negative low))

min :: Bounds units -> Bounds units -> Bounds units
min (Bounds low1 high1) (Bounds low2 high2) =
  Bounds (Prelude.min low1 low2) (Prelude.min high1 high2)

max :: Bounds units -> Bounds units -> Bounds units
max (Bounds low1 high1) (Bounds low2 high2) =
  Bounds (Prelude.max low1 low2) (Prelude.max high1 high2)

smaller :: Bounds units -> Bounds units -> Bounds units
smaller first second = do
  let Bounds low1 high1 = abs first
  let Bounds low2 high2 = abs second
  if
    | high1 < low2 -> first
    | high2 < low1 -> second
    | otherwise -> do
        let Bounds aggregateMin aggregateMax = aggregate2 first second
        let high = Prelude.min high1 high2
        Bounds (Prelude.max (negative high) aggregateMin) (Prelude.min aggregateMax high)

larger :: Bounds units -> Bounds units -> Bounds units
larger first second = do
  let Bounds low1 high1 = abs first
  let Bounds low2 high2 = abs second
  let low = Prelude.max low1 low2
  let aggregate = aggregate2 first second
  let Bounds aggregateMin aggregateMax = aggregate
  if
    | low1 > high2 -> first
    | low2 > high1 -> second
    | aggregateMin > negative low -> Bounds (Prelude.max aggregateMin low) aggregateMax
    | aggregateMax < low -> Bounds aggregateMin (Prelude.min aggregateMax (negative low))
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
        | low > clipRadius || high < negative clipRadius = current
        | otherwise =
            aggregate2 current $
              Bounds (Prelude.max low (negative clipRadius)) (Prelude.min high clipRadius)
  NonEmpty.foldl conditionalAggregate initial list

largest :: NonEmpty (Bounds units) -> Bounds units
largest list = do
  let initial = NonEmpty.maximumBy minAbs list
  let clipRadius = minAbs initial
  let conditionalAggregate current bounds@(Bounds low high)
        | low > negative clipRadius && high < clipRadius = current
        | low > negative clipRadius = aggregate2 current (Bounds clipRadius high)
        | high < clipRadius = aggregate2 current (Bounds low (negative clipRadius))
        | otherwise = aggregate2 current bounds
  NonEmpty.foldl conditionalAggregate initial list

sin :: Bounds Radians -> Bounds Unitless
sin bounds@(Bounds low high) = do
  let (includesMin, includesMax) = sinIncludesMinMax bounds
  let newLow = if includesMin then -1 else Prelude.min (Angle.sin low) (Angle.sin high)
  let newHigh = if includesMax then 1 else Prelude.max (Angle.sin low) (Angle.sin high)
  Bounds newLow newHigh

cos :: Bounds Radians -> Bounds Unitless
cos bounds@(Bounds low high) = do
  let (includesMin, includesMax) = cosIncludesMinMax bounds
  let newLow = if includesMin then -1 else Prelude.min (Angle.cos low) (Angle.cos high)
  let newHigh = if includesMax then 1 else Prelude.max (Angle.cos low) (Angle.cos high)
  Bounds newLow newHigh

sinIncludesMinMax :: Bounds Radians -> (Bool, Bool)
sinIncludesMinMax bounds = cosIncludesMinMax (bounds .-. Angle.halfPi)

cosIncludesMinMax :: Bounds Radians -> (Bool, Bool)
cosIncludesMinMax bounds = (cosIncludesMax (bounds .+. Angle.pi), cosIncludesMax bounds)

cosIncludesMax :: Bounds Radians -> Bool
cosIncludesMax (Bounds low high) =
  (Quantity.isInfinite low || Quantity.isInfinite high)
    || (low .//. Angle.twoPi /= high .//. Angle.twoPi)

interpolate :: Bounds units -> Number -> Quantity units
interpolate (Bounds low high) t =
  Quantity.interpolateFrom low high t

interpolationParameter :: Bounds units -> Quantity units -> Number
interpolationParameter (Bounds low high) value
  | low < high = (value .-. low) ./. (high .-. low)
  | value < low = negative Quantity.infinity
  | value > high = Quantity.infinity
  | otherwise = 0

resolution :: Bounds units -> Number
resolution (Bounds low high)
  | low > Quantity.zero = low ./. high
  | high < Quantity.zero = negative high ./. low
  | otherwise = 0

resolutionThreshold :: Number
resolutionThreshold = 0.5

isResolved :: Bounds units -> Bool
isResolved bounds = Number.abs (resolution bounds) >= resolutionThreshold

resolvedSign :: Bounds units -> Fuzzy Sign
resolvedSign bounds = do
  let boundsResolution = resolution bounds
  if Number.abs boundsResolution >= resolutionThreshold
    then Resolved (Number.sign boundsResolution)
    else Unresolved

resolve :: Eq a => (Bounds units -> Fuzzy a) -> Bounds units -> Fuzzy a
resolve assess bounds =
  case assess bounds of
    Resolved value -> Resolved value
    Unresolved
      | isAtomic bounds -> Unresolved
      | otherwise -> do
          let (left, right) = bisect bounds
          leftValue <- resolve assess left
          rightValue <- resolve assess right
          if leftValue == rightValue then Resolved leftValue else Unresolved

random :: Random.Generator (Quantity units) -> Random.Generator (Bounds units)
random randomQuantity = do
  a <- randomQuantity
  b <- randomQuantity
  Random.return (Bounds a b)

sampleValues :: Bounds units -> List (Quantity units)
sampleValues bounds = List.map (interpolate bounds) Parameter.samples

convert :: Quantity (units2 #/# units1) -> Bounds units1 -> Bounds units2
convert factor bounds = Units.simplify (bounds #*# factor)

unconvert :: Quantity (units2 #/# units1) -> Bounds units2 -> Bounds units1
unconvert factor bounds = Units.simplify (bounds #/# factor)
