module Curve1d
  ( Curve1d
  , Interface (..)
  , evaluateAt
  , pointOn
  , segmentBounds
  , derivative
  , wrap
  , zero
  , constant
  , t
  , squared
  , squared'
  , sqrt
  , sqrt'
  , sin
  , cos
  , Zeros (ZeroEverywhere, Zeros)
  , HigherOrderZero (HigherOrderZero)
  , zeros
  , reverse
  , integral
  )
where

import Angle qualified
import Curve1d.Integral (Integral (Integral))
import Curve1d.Root (Root (Root))
import Curve1d.Root qualified as Root
import Estimate (Estimate)
import Estimate qualified
import Float qualified
import Int qualified
import List qualified
import OpenSolid
import Qty qualified
import Range (Range (Range))
import Range qualified
import Result qualified
import Stream (Stream (Stream))
import Stream qualified
import Units qualified

class Show curve => Interface curve units | curve -> units where
  evaluateAtImpl :: Float -> curve -> Qty units
  segmentBoundsImpl :: Range Unitless -> curve -> Range units
  derivativeImpl :: curve -> Curve1d units

data Curve1d units where
  Curve1d ::
    Interface curve units =>
    curve ->
    Curve1d units
  Constant ::
    Qty units -> Curve1d units
  Parameter ::
    Curve1d Unitless
  Negated ::
    Curve1d units ->
    Curve1d units
  Sum ::
    Curve1d units ->
    Curve1d units ->
    Curve1d units
  Difference ::
    Curve1d units ->
    Curve1d units ->
    Curve1d units
  Product' ::
    Curve1d units1 ->
    Curve1d units2 ->
    Curve1d (units1 :*: units2)
  Quotient' ::
    Curve1d units1 ->
    Curve1d units2 ->
    Curve1d (units1 :/: units2)
  Squared' ::
    Curve1d units ->
    Curve1d (units :*: units)
  SquareRoot' ::
    Curve1d (units :*: units) ->
    Curve1d units
  Sin ::
    Curve1d Radians ->
    Curve1d Unitless
  Cos ::
    Curve1d Radians ->
    Curve1d Unitless
  Coerce ::
    Curve1d units1 ->
    Curve1d units2

deriving instance Show (Curve1d units)

instance HasUnits (Curve1d units) where
  type Units (Curve1d units) = units
  type Erase (Curve1d units) = Curve1d Unitless

instance Units.Coercion (Curve1d units1) (Curve1d units2) where
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce curve) = Coerce curve
  coerce curve = Coerce curve

instance units ~ units_ => ApproximateEquality (Curve1d units) (Curve1d units_) units where
  curve1 ~= curve2 = isZero (curve1 - curve2)

instance units ~ units_ => ApproximateEquality (Curve1d units) (Qty units_) units where
  curve ~= value = isZero (curve - value)

instance Interface (Curve1d units) units where
  evaluateAtImpl = evaluateAt
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative

wrap :: Interface curve units => curve -> Curve1d units
wrap = Curve1d

zero :: Curve1d units
zero = constant Qty.zero

constant :: Qty units -> Curve1d units
constant = Constant

t :: Curve1d Unitless
t = Parameter

instance Negation (Curve1d units) where
  negate (Constant x) = Constant (negate x)
  negate (Negated curve) = curve
  negate (Difference c1 c2) = Difference c2 c1
  negate (Product' c1 c2) = negate c1 .*. c2
  negate curve = Negated curve

instance Multiplication Sign (Curve1d units) (Curve1d units)

instance Multiplication' Sign (Curve1d units) where
  type Sign .*. Curve1d units = Curve1d (Unitless :*: units)
  Positive .*. curve = Units.coerce curve
  Negative .*. curve = Units.coerce -curve

instance Multiplication (Curve1d units) Sign (Curve1d units)

instance Multiplication' (Curve1d units) Sign where
  type Curve1d units .*. Sign = Curve1d (units :*: Unitless)
  curve .*. Positive = Units.coerce curve
  curve .*. Negative = Units.coerce -curve

instance units ~ units_ => Addition (Curve1d units) (Curve1d units_) (Curve1d units) where
  curve + Constant (Qty 0.0) = curve
  Constant (Qty 0.0) + curve = curve
  Constant x + Constant y = constant (x + y)
  curve1 + curve2 = Sum curve1 curve2

instance units ~ units_ => Addition (Curve1d units) (Qty units_) (Curve1d units) where
  curve + value = curve + constant value

instance units ~ units_ => Addition (Qty units) (Curve1d units_) (Curve1d units) where
  value + curve = constant value + curve

instance Addition (Curve1d Unitless) Int (Curve1d Unitless) where
  curve + value = curve + Float.int value

instance Addition Int (Curve1d Unitless) (Curve1d Unitless) where
  value + curve = Float.int value + curve

instance Subtraction (Curve1d Unitless) Int (Curve1d Unitless) where
  curve - value = curve - Float.int value

instance Subtraction Int (Curve1d Unitless) (Curve1d Unitless) where
  value - curve = Float.int value - curve

instance units ~ units_ => Subtraction (Curve1d units) (Curve1d units_) (Curve1d units) where
  curve - Constant (Qty 0.0) = curve
  Constant (Qty 0.0) - curve = negate curve
  Constant x - Constant y = constant (x - y)
  curve1 - curve2 = Difference curve1 curve2

instance units ~ units_ => Subtraction (Curve1d units) (Qty units_) (Curve1d units) where
  curve - value = curve - constant value

instance units ~ units_ => Subtraction (Qty units) (Curve1d units_) (Curve1d units) where
  value - curve = constant value - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1d units1) (Curve1d units2) (Curve1d units3)

instance Multiplication' (Curve1d units1) (Curve1d units2) where
  type Curve1d units1 .*. Curve1d units2 = Curve1d (units1 :*: units2)
  Constant (Qty 0.0) .*. _ = zero
  _ .*. Constant (Qty 0.0) = zero
  Constant x .*. Constant y = Constant (x .*. y)
  Constant (Qty 1.0) .*. curve = Units.coerce curve
  Constant (Qty -1.0) .*. curve = Units.coerce (negate curve)
  Constant x .*. Negated c = negate x .*. c
  c1 .*. (Constant x) = Units.commute (Constant x .*. c1)
  Constant x .*. Product' (Constant y) c = Units.rightAssociate ((x .*. y) .*. c)
  curve1 .*. curve2 = Product' curve1 curve2

instance Multiplication' Int (Curve1d units) where
  type Int .*. Curve1d units = Curve1d (Unitless :*: units)
  value .*. curve = Float.int value .*. curve

instance Multiplication' (Curve1d units) Int where
  type Curve1d units .*. Int = Curve1d (units :*: Unitless)
  curve .*. value = curve .*. Float.int value

instance Multiplication Int (Curve1d units) (Curve1d units)

instance Multiplication (Curve1d units) Int (Curve1d units)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1d units1) (Qty units2) (Curve1d units3)

instance Multiplication' (Curve1d units1) (Qty units2) where
  type Curve1d units1 .*. Qty units2 = Curve1d (units1 :*: units2)
  curve .*. value = curve .*. constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Curve1d units2) (Curve1d units3)

instance Multiplication' (Qty units1) (Curve1d units2) where
  type Qty units1 .*. Curve1d units2 = Curve1d (units1 :*: units2)
  value .*. curve = constant value .*. curve

instance
  Units.Quotient units1 units2 units3 =>
  Division (Curve1d units1) (Curve1d units2) (Curve1d units3)

instance Division' (Curve1d units1) (Curve1d units2) where
  type Curve1d units1 ./. Curve1d units2 = Curve1d (units1 :/: units2)
  Constant (Qty 0.0) ./. _ = zero
  Constant x ./. Constant y = Constant (x ./. y)
  curve ./. Constant x = (1 ./. x) .*^ curve
  curve1 ./. curve2 = Quotient' curve1 curve2

instance
  Units.Quotient units1 units2 units3 =>
  Division (Curve1d units1) (Qty units2) (Curve1d units3)

instance Division' (Curve1d units1) (Qty units2) where
  type Curve1d units1 ./. Qty units2 = Curve1d (units1 :/: units2)
  curve ./. value = curve ./. constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Curve1d units2) (Curve1d units3)

instance Division' (Qty units1) (Curve1d units2) where
  type Qty units1 ./. Curve1d units2 = Curve1d (units1 :/: units2)
  value ./. curve = constant value ./. curve

instance Division (Curve1d units) Int (Curve1d units)

instance Division' (Curve1d units) Int where
  type Curve1d units ./. Int = Curve1d (units :/: Unitless)
  curve ./. value = curve ./. Float.int value

instance
  Units.Quotient Unitless units1 units2 =>
  Division Int (Curve1d units1) (Curve1d units2)

instance Division' Int (Curve1d units) where
  type Int ./. Curve1d units = Curve1d (Unitless :/: units)
  value ./. curve = Float.int value ./. curve

evaluateAt :: Float -> Curve1d units -> Qty units
evaluateAt tValue curve = case curve of
  Curve1d c -> evaluateAtImpl tValue c
  Constant x -> x
  Parameter -> tValue
  Negated c -> negate (evaluateAt tValue c)
  Sum c1 c2 -> evaluateAt tValue c1 + evaluateAt tValue c2
  Difference c1 c2 -> evaluateAt tValue c1 - evaluateAt tValue c2
  Product' c1 c2 -> evaluateAt tValue c1 .*. evaluateAt tValue c2
  Quotient' c1 c2 -> evaluateAt tValue c1 ./. evaluateAt tValue c2
  Squared' c -> Qty.squared' (evaluateAt tValue c)
  SquareRoot' c' -> Qty.sqrt' (evaluateAt tValue c')
  Sin c -> Angle.sin (evaluateAt tValue c)
  Cos c -> Angle.cos (evaluateAt tValue c)
  Coerce c -> Units.coerce (evaluateAt tValue c)

pointOn :: Curve1d units -> Float -> Qty units
pointOn curve tValue = evaluateAt tValue curve

segmentBounds :: Range Unitless -> Curve1d units -> Range units
segmentBounds tBounds curve = case curve of
  Curve1d c -> segmentBoundsImpl tBounds c
  Constant value -> Range.constant value
  Parameter -> tBounds
  Negated c -> negate (segmentBounds tBounds c)
  Sum c1 c2 -> segmentBounds tBounds c1 + segmentBounds tBounds c2
  Difference c1 c2 -> segmentBounds tBounds c1 - segmentBounds tBounds c2
  Product' c1 c2 -> segmentBounds tBounds c1 .*. segmentBounds tBounds c2
  Quotient' c1 c2 -> segmentBounds tBounds c1 ./. segmentBounds tBounds c2
  Squared' c -> Range.squared' (segmentBounds tBounds c)
  SquareRoot' c' -> Range.sqrt' (segmentBounds tBounds c')
  Sin c -> Range.sin (segmentBounds tBounds c)
  Cos c -> Range.cos (segmentBounds tBounds c)
  Coerce c -> Units.coerce (segmentBounds tBounds c)

derivative :: Curve1d units -> Curve1d units
derivative curve = case curve of
  Curve1d c -> derivativeImpl c
  Constant _ -> zero
  Parameter -> constant 1.0
  Negated c -> negate (derivative c)
  Sum c1 c2 -> derivative c1 + derivative c2
  Difference c1 c2 -> derivative c1 - derivative c2
  Product' c1 c2 -> derivative c1 .*. c2 + c1 .*. derivative c2
  Quotient' c1 c2 -> (derivative c1 .*. c2 - c1 .*. derivative c2) .!/.! squared' c2
  Squared' c -> 2 * c .*. derivative c
  SquareRoot' c' -> derivative c' .!/! (2 * sqrt' c')
  Sin c -> cos c * Angle.unitless (derivative c)
  Cos c -> negate (sin c) * Angle.unitless (derivative c)
  Coerce c -> Units.coerce (derivative c)

newtype Reversed units = Reversed (Curve1d units)

deriving instance Show (Reversed units)

instance Interface (Reversed units) units where
  evaluateAtImpl tValue (Reversed curve) = evaluateAt (1 - tValue) curve
  segmentBoundsImpl tBounds (Reversed curve) = segmentBounds (1 - tBounds) curve
  derivativeImpl (Reversed curve) = -(reverse (derivative curve))

reverse :: Curve1d units -> Curve1d units
reverse curve@(Constant _) = curve
reverse curve = Curve1d (Reversed curve)

squared :: Units.Squared units1 units2 => Curve1d units1 -> Curve1d units2
squared curve = Units.specialize (squared' curve)

squared' :: Curve1d units -> Curve1d (units :*: units)
squared' curve = case curve of
  Constant x -> Constant (x .*. x)
  Negated c -> squared' c
  Cos c -> Units.unspecialize (cosSquared c)
  Sin c -> Units.unspecialize (sinSquared c)
  _ -> Squared' curve

cosSquared :: Curve1d Radians -> Curve1d Unitless
cosSquared c = 0.5 * cos (2 * c) + 0.5

sinSquared :: Curve1d Radians -> Curve1d Unitless
sinSquared c = 0.5 - 0.5 * cos (2 * c)

sqrt :: Units.Squared units1 units2 => Curve1d units2 -> Curve1d units1
sqrt curve = sqrt' (Units.unspecialize curve)

sqrt' :: Curve1d (units :*: units) -> Curve1d units
sqrt' (Constant x) = Constant (Qty.sqrt' x)
sqrt' curve = SquareRoot' curve

sin :: Curve1d Radians -> Curve1d Unitless
sin (Constant x) = constant (Angle.sin x)
sin curve = Sin curve

cos :: Curve1d Radians -> Curve1d Unitless
cos (Constant x) = constant (Angle.cos x)
cos curve = Cos curve

isZero :: Tolerance units => Curve1d units -> Bool
isZero (Constant value) = value ~= Qty.zero
isZero curve = List.all (pointOn curve >> (~= Qty.zero)) (Range.samples Range.unit)

----- ROOT FINDING -----

data HigherOrderZero = HigherOrderZero deriving (Eq, Show, Error)

data Zeros = ZeroEverywhere | Zeros (List Root) deriving (Show)

zeros :: Tolerance units => Curve1d units -> Result HigherOrderZero Zeros
zeros (Constant value) = if value ~= Qty.zero then Ok ZeroEverywhere else Ok (Zeros [])
zeros curve | isZero curve = Ok ZeroEverywhere
zeros curve = Result.do
  let derivatives = Stream.iterate curve derivative
  (roots, _) <- findZeros [0 .. 3] derivatives
  Ok (Zeros (List.sortBy Root.value roots))

findZeros ::
  Tolerance units =>
  List Int ->
  Stream (Curve1d units) ->
  Result HigherOrderZero (List Root, List (Range Unitless))
findZeros orders derivatives = case orders of
  [] -> Ok ([], []) -- No more orders to try
  n : higherOrders -> Result.do
    -- Find higher-order zeros first,
    -- and their corresponding exclusions
    (higherOrderZeros, higherOrderExclusions) <- findZeros higherOrders derivatives
    -- Find roots of the current order,
    -- taking into account exclusions from the higher-order roots
    (rootsOrderN, exclusionsOrderN) <- findZerosOrder n derivatives Range.unit higherOrderExclusions
    -- Combine higher-order and current roots
    Ok (higherOrderZeros + rootsOrderN, higherOrderExclusions + exclusionsOrderN)

findZerosOrder ::
  Tolerance units =>
  Int ->
  Stream (Curve1d units) ->
  Range Unitless ->
  List (Range Unitless) ->
  Result HigherOrderZero (List Root, List (Range Unitless))
findZerosOrder n derivatives domain exclusions =
  -- Attempt to 'resolve' the current domain with respect to the current root order
  -- (see if it's possible to guarantee the presence or absence of a unique zero)
  case resolveOrder n derivatives domain exclusions of
    -- Guaranteed single solution of order n exists in this domain,
    -- with a corresponding exclusion region
    Resolved (Just (root, exclusion)) -> Ok ([root], [exclusion])
    -- Guaranteed no solution of the given order exists in this domain
    Resolved Nothing -> Ok ([], [])
    -- We couldn't determine whether or not a unique solution exists in this domain,
    -- so we need to bisect into subdomains
    Unresolved
      -- We can't actually bisect further, since the domain is too small -
      -- likely means that there's a higher-order root than we were asked to find
      | Range.isAtomic domain -> Error HigherOrderZero
      -- Otherwise, bisect the domain
      -- and recurse into the left and right subdomains
      | otherwise -> Result.do
          let (leftDomain, rightDomain) = Range.bisect domain
          (leftRoots, leftExclusions) <-
            findZerosOrder n derivatives leftDomain $
              -- When solving within the left domain,
              -- we only need to consider exclusions that overlap that domain
              List.filter (overlaps leftDomain) exclusions
          (rightRoots, rightExclusions) <-
            findZerosOrder n derivatives rightDomain $
              -- Make sure to pass exclusions reported from the left domain
              -- into findZeros for the right domain,
              -- so we don't report a duplicate root
              List.filter (overlaps rightDomain) (exclusions + leftExclusions)
          -- Combine roots and exclusions from both subdomains
          Ok (leftRoots + rightRoots, leftExclusions + rightExclusions)

resolveOrder ::
  Tolerance units =>
  Int ->
  Stream (Curve1d units) ->
  Range Unitless ->
  List (Range Unitless) ->
  Fuzzy (Maybe (Root, Range Unitless))
resolveOrder n derivatives domain exclusions
  -- The current domain is contained within an exclusion,
  -- so no (additional) solution exists within this domain
  | List.any (Range.contains domain) exclusions = Resolved Nothing
  -- The curve itself is non-zero, so no solution exists within this domain
  | not (segmentBounds domain (Stream.head derivatives) ^ Qty.zero) = Resolved Nothing
  -- A lower-order derivative is non-zero, so no solution of the given order exists
  | anyResolved n (Stream.tail derivatives) domain = Resolved Nothing
  -- We're overlapping an exclusion, so we need to bisect further
  | List.any (overlaps domain) exclusions = Unresolved
  -- Otherwise, try to solve for a root of the given order
  | otherwise = solveOrder n derivatives domain (expand domain)

anyResolved :: Int -> Stream (Curve1d units) -> Range Unitless -> Bool
anyResolved 0 _ _ = False
anyResolved n (Stream first rest) domain =
  isResolved (segmentBounds domain first) || anyResolved (n - 1) rest domain

solveOrder ::
  Tolerance units =>
  Int ->
  Stream (Curve1d units) ->
  Range Unitless ->
  Range Unitless ->
  Fuzzy (Maybe (Root, Range Unitless))
solveOrder n derivatives domain expandedDomain = do
  -- For a solution of order n,
  -- we need to look at the derivative of order m = n + 1
  let m = n + 1
  let fm = Stream.nth m derivatives
  let fmBounds = segmentBounds expandedDomain fm
  case resolvedSign fmBounds of
    Nothing -> Unresolved
    Just fmSign -> do
      let fmAbs x = Qty.abs (evaluateAt x fm)
      -- Compute the 'radius' of a root of order n
      let radius x = (Int.factorial m * ?tolerance / fmAbs x) ** (1 / m)
      -- Check that the values of all derivatives of order n and lower
      -- (including order 0, the curve itself)
      -- are zero to within an appropriate tolerance based on the root radius
      let curveIsZero x = evaluateAt x (Stream.head derivatives) ~= Qty.zero
      let derivativeValue x k = evaluateAt x (Stream.nth k derivatives)
      let derivativeTolerance x k = fmAbs x * radius x ** (m - k) / Int.factorial (m - k)
      let derivativeIsZero x k = Qty.abs (derivativeValue x k) < derivativeTolerance x k
      let isSolution x = curveIsZero x && List.all (derivativeIsZero x) [1 .. n]
      let solution x = Resolved (Just (Root x n fmSign, expandedDomain))
      if
        -- Snap to 0.0 or 1.0 as a root if possible
        | Range.includes 0.0 domain && isSolution 0.0 -> solution 0.0
        | Range.includes 1.0 domain && isSolution 1.0 -> solution 1.0
        -- Otherwise, solve for zero of order-n derivative
        -- and check if that point is actually a root
        -- (all lower-order derivatives are also zero)
        | otherwise -> do
            case Range.solve (pointOn (Stream.nth n derivatives)) domain of
              Just x
                -- We found a solution!
                | isSolution x -> solution x
                -- The derivative of order n does not have a root within this domain,
                -- so there is no root of order n in this domain
                | otherwise -> Resolved Nothing
              -- No solution for the order-n derivative,
              -- so there can be no root of order n in this domain
              Nothing -> Resolved Nothing

expand :: Range Unitless -> Range Unitless
expand domain = do
  let (Range low high) = domain
  let expansion = 0.5 * (high - low)
  Range.unsafe (Float.max 0.0 (low - expansion)) (Float.min 1.0 (high + expansion))

overlaps :: Range Unitless -> Range Unitless -> Bool
overlaps domain exclusion = Range.overlap domain exclusion > Qty.zero

isResolved :: Range units -> Bool
isResolved range = resolvedSign range /= Nothing

resolvedSign :: Range units -> Maybe Sign
resolvedSign range = do
  let resolution = Range.resolution range
  if Qty.abs resolution >= 0.5 then Just (Qty.sign resolution) else Nothing

integral :: Curve1d units -> Estimate units
integral curve = Estimate.wrap (Integral curve (derivative curve) Range.unit)
