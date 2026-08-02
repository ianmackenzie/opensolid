module OpenSolid.SurfaceFunction1D
  ( SurfaceFunction1D
  , Compiled
  , valueAt
  , valueOf
  , range
  , partialDerivatives
  , partialDerivativesAt
  , partialDerivativeRanges
  , secondPartialDerivatives
  , secondPartialDerivativesAt
  , secondPartialDerivativeRanges
  , derivativeIn
  , compiled
  , zero
  , constant
  , u
  , v
  , Zeros
  , IsZero (IsZero)
  , zeros
  , new
  , squared
  , squared_
  , cubed
  , sin
  , cos
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction2D (Direction2D (Direction2D))
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Domain1D qualified as Domain1D
import OpenSolid.Domain2D (Domain2D (Domain2D))
import OpenSolid.Domain2D qualified as Domain2D
import OpenSolid.Expression qualified as Expression
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.HigherOrderZero (HigherOrderZero (HigherOrderZero))
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.PartialDerivatives qualified as PartialDerivatives
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Solve2D qualified as Solve2D
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D.HorizontalCurve qualified as HorizontalCurve
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D.Nonzero qualified as SurfaceFunction1D.Nonzero
import OpenSolid.SurfaceFunction1D.PartialZeros (PartialZeros)
import OpenSolid.SurfaceFunction1D.PartialZeros qualified as PartialZeros
import OpenSolid.SurfaceFunction1D.SaddleRegion (SaddleRegion)
import OpenSolid.SurfaceFunction1D.SaddleRegion qualified as SaddleRegion
import OpenSolid.SurfaceFunction1D.Subproblem (CornerValues (..), Subproblem (..))
import OpenSolid.SurfaceFunction1D.Subproblem qualified as Subproblem
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D.VerticalCurve qualified as VerticalCurve
import OpenSolid.SurfaceFunction1D.Zeros (Zeros (..))
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvPoint qualified as UvPoint
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds2D (VectorBounds2D (VectorBounds2D))
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

data SurfaceFunction1D units = SurfaceFunction1D
  { compiled :: Compiled units
  , partialDerivatives :: (SurfaceFunction1D units, SurfaceFunction1D units)
  }

type Compiled units = CompiledFunction UvPoint (Quantity units) UvBounds (Interval units)

instance HasUnits (SurfaceFunction1D units) units

instance Units.Coercion (SurfaceFunction1D units1) (SurfaceFunction1D units2) where
  coerce function =
    SurfaceFunction1D
      { compiled = Units.coerce function.compiled
      , partialDerivatives = Pair.map Units.coerce function.partialDerivatives
      }

instance ApproximateEquality (SurfaceFunction1D units) (Tolerance units) where
  function1 ~= function2 = do
    let equalValuesAt uvPoint = valueAt uvPoint function1 ~= valueAt uvPoint function2
    NonEmpty.all equalValuesAt UvPoint.interiorSamples

instance
  units1 ~ units2 =>
  Intersects (SurfaceFunction1D units1) (Quantity units2) (Tolerance units1)
  where
  function `intersects` quantity =
    -- TODO optimize this to use a special Solve2D.find or similar
    -- to efficiently check if there is *a* zero anywhere
    -- instead of finding *all* zeros (and the full geometry of each)
    case zeros (function - quantity) of
      Ok (Zeros [] [] [] []) -> False
      Ok (Zeros{}) -> True
      Err IsZero -> True

instance
  units1 ~ units2 =>
  Intersects (Quantity units1) (SurfaceFunction1D units2) (Tolerance units1)
  where
  quantity `intersects` function = function `intersects` quantity

instance Negation (SurfaceFunction1D units) where
  negate function = new (negate function.compiled) (Pair.map negate function.partialDerivatives)

instance Multiplication Sign (SurfaceFunction1D units) (SurfaceFunction1D units) where
  Positive * function = function
  Negative * function = -function

instance Multiplication (SurfaceFunction1D units) Sign (SurfaceFunction1D units) where
  function * Positive = function
  function * Negative = -function

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction1D units1)
    (SurfaceFunction1D units2)
    (SurfaceFunction1D units1)
  where
  f + g =
    new
      (compiled f + compiled g)
      (Pair.map2 (+) (partialDerivatives f) (partialDerivatives g))

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction1D units1)
    (Quantity units2)
    (SurfaceFunction1D units1)
  where
  function + quantity = function + constant quantity

instance
  units1 ~ units2 =>
  Addition
    (Quantity units1)
    (SurfaceFunction1D units2)
    (SurfaceFunction1D units1)
  where
  quantity + function = constant quantity + function

instance
  units1 ~ units2 =>
  Subtraction (SurfaceFunction1D units1) (SurfaceFunction1D units2) (SurfaceFunction1D units1)
  where
  f - g =
    new
      (compiled f - compiled g)
      (Pair.map2 (-) (partialDerivatives f) (partialDerivatives g))

instance
  units1 ~ units2 =>
  Subtraction (SurfaceFunction1D units1) (Quantity units2) (SurfaceFunction1D units1)
  where
  function - quantity = function - constant quantity

instance
  units1 ~ units2 =>
  Subtraction (Quantity units1) (SurfaceFunction1D units2) (SurfaceFunction1D units1)
  where
  quantity - function = constant quantity - function

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction1D units1) (SurfaceFunction1D units2) (SurfaceFunction1D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (SurfaceFunction1D units1)
    (SurfaceFunction1D units2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  f ?*? g = do
    let (dfdu, dfdv) = partialDerivatives f
    let (dgdu, dgdv) = partialDerivatives g
    let compiledProduct = compiled f ?*? compiled g
    let productPartialDerivatives =
          ( dfdu ?*? g + f ?*? dgdu
          , dfdv ?*? g + f ?*? dgdv
          )
    new compiledProduct productPartialDerivatives

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction1D units1) (Quantity units2) (SurfaceFunction1D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (SurfaceFunction1D units1)
    (Quantity units2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  function ?*? quantity = function ?*? constant quantity

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (SurfaceFunction1D units2) (SurfaceFunction1D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Quantity units1)
    (SurfaceFunction1D units2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  quantity ?*? function = constant quantity ?*? function

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction1D units1)
    (Vector2D units2)
    (VectorSurfaceFunction2D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (SurfaceFunction1D units1)
    (Vector2D units2)
    (VectorSurfaceFunction2D (units1 ?*? units2))
  where
  function ?*? vector = function ?*? VectorSurfaceFunction2D.constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector2D units1)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction2D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Vector2D units1)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction2D (units1 ?*? units2))
  where
  vector ?*? function = VectorSurfaceFunction2D.constant vector ?*? function

instance
  Multiplication
    (SurfaceFunction1D units)
    Direction2D
    (VectorSurfaceFunction2D units)
  where
  lhs * rhs = lhs * Vector2D.unit rhs

instance
  Multiplication
    Direction2D
    (SurfaceFunction1D units)
    (VectorSurfaceFunction2D units)
  where
  lhs * rhs = Vector2D.unit lhs * rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction1D units1)
    (Vector3D units2 space)
    (VectorSurfaceFunction3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (SurfaceFunction1D units1)
    (Vector3D units2 space)
    (VectorSurfaceFunction3D (units1 ?*? units2) space)
  where
  function ?*? vector = function ?*? VectorSurfaceFunction3D.constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector3D units1 space)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Vector3D units1 space)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction3D (units1 ?*? units2) space)
  where
  vector ?*? function = VectorSurfaceFunction3D.constant vector ?*? function

instance
  Multiplication
    (SurfaceFunction1D units)
    (Direction3D space)
    (VectorSurfaceFunction3D units space)
  where
  lhs * rhs = lhs * Vector3D.unit rhs

instance
  Multiplication
    (Direction3D space)
    (SurfaceFunction1D units)
    (VectorSurfaceFunction3D units space)
  where
  lhs * rhs = Vector3D.unit lhs * rhs

instance
  Units.Quotient units1 units2 units3 =>
  Division (SurfaceFunction1D units1) (Quantity units2) (SurfaceFunction1D units3)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (SurfaceFunction1D units1)
    (Quantity units2)
    (SurfaceFunction1D (units1 ?/? units2))
  where
  function ?/? quantity = Units.simplify (function ?*? (1.0 ?/? quantity))

instance Composition (Curve1D units) (SurfaceFunction1D Unitless) (SurfaceFunction1D units) where
  f . g = do
    let dfdt = Curve1D.derivative f . g
    let (dtdu, dtdv) = partialDerivatives g
    new (Curve1D.compiled f . compiled g) (dfdt * dtdu, dfdt * dtdv)

valueAt :: UvPoint -> SurfaceFunction1D units -> Quantity units
valueAt uvPoint function = CompiledFunction.value function.compiled uvPoint

valueOf :: SurfaceFunction1D units -> UvPoint -> Quantity units
valueOf function uvPoint = valueAt uvPoint function

range :: UvBounds -> SurfaceFunction1D units -> Interval units
range uvRange function = CompiledFunction.range function.compiled uvRange

{-# INLINE partialDerivatives #-}
partialDerivatives :: SurfaceFunction1D units -> (SurfaceFunction1D units, SurfaceFunction1D units)
partialDerivatives = (.partialDerivatives)

partialDerivativesAt ::
  UvPoint ->
  SurfaceFunction1D units ->
  (Quantity units, Quantity units)
partialDerivativesAt uvPoint function = Pair.map (valueAt uvPoint) (partialDerivatives function)

partialDerivativeRanges ::
  UvBounds ->
  SurfaceFunction1D units ->
  (Interval units, Interval units)
partialDerivativeRanges uvRange function = Pair.map (range uvRange) (partialDerivatives function)

secondPartialDerivatives ::
  SurfaceFunction1D units ->
  (SurfaceFunction1D units, SurfaceFunction1D units, SurfaceFunction1D units)
secondPartialDerivatives f = do
  let (fu, fv) = partialDerivatives f
  let (fuu, fuv) = partialDerivatives fu
  let fvv = Pair.second (partialDerivatives fv)
  (fuu, fuv, fvv)

secondPartialDerivativesAt ::
  UvPoint ->
  SurfaceFunction1D units ->
  (Quantity units, Quantity units, Quantity units)
secondPartialDerivativesAt uvPoint function = do
  let (fuu, fuv, fvv) = secondPartialDerivatives function
  (valueAt uvPoint fuu, valueAt uvPoint fuv, valueAt uvPoint fvv)

secondPartialDerivativeRanges ::
  UvBounds ->
  SurfaceFunction1D units ->
  (Interval units, Interval units, Interval units)
secondPartialDerivativeRanges uvRange function = do
  let (fuu, fuv, fvv) = secondPartialDerivatives function
  (range uvRange fuu, range uvRange fuv, range uvRange fvv)

derivativeIn :: Direction2D -> SurfaceFunction1D units -> SurfaceFunction1D units
derivativeIn (Direction2D du dv) function = do
  let (dfdu, dfdv) = partialDerivatives function
  du * dfdu + dv * dfdv

{-# INLINE compiled #-}
compiled :: SurfaceFunction1D units -> Compiled units
compiled = (.compiled)

zero :: SurfaceFunction1D units
zero = constant Quantity.zero

one :: SurfaceFunction1D Unitless
one = constant 1.0

constant :: Quantity units -> SurfaceFunction1D units
constant quantity = new (CompiledFunction.constant quantity) (zero, zero)

u :: SurfaceFunction1D Unitless
u = new (CompiledFunction.concrete Expression.u) (one, zero)

v :: SurfaceFunction1D Unitless
v = new (CompiledFunction.concrete Expression.v) (zero, one)

new ::
  Compiled units ->
  (SurfaceFunction1D units, SurfaceFunction1D units) ->
  SurfaceFunction1D units
new givenCompiled givenPartialDerivatives = do
  let mergedPartialDerivatives =
        PartialDerivatives.merge new compiled partialDerivatives givenPartialDerivatives
  SurfaceFunction1D givenCompiled mergedPartialDerivatives

instance HasUnits (Nonzero (SurfaceFunction1D units)) units

instance
  Units.Coercion
    (Nonzero (SurfaceFunction1D units1))
    (Nonzero (SurfaceFunction1D units2))
  where
  coerce (Nonzero function) = Nonzero (Units.coerce function)

instance
  Division_
    (SurfaceFunction1D units1)
    (Nonzero (SurfaceFunction1D units2))
    (SurfaceFunction1D (units1 ?/? units2))
  where
  f ?/? Nonzero g = do
    let compiledQuotient = compiled f ?/? compiled g
    let (dfdu, dfdv) = partialDerivatives f
    let (dgdu, dgdv) = partialDerivatives g
    let gSquared_ = SurfaceFunction1D.Nonzero.squared_ (Nonzero g)
    let quotientPartialDerivatives =
          Pair.map Units.simplify $
            ( (dfdu ?*? g - f ?*? dgdu) ?/? gSquared_
            , (dfdv ?*? g - f ?*? dgdv) ?/? gSquared_
            )
    new compiledQuotient quotientPartialDerivatives

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (SurfaceFunction1D units1)
    (Nonzero (SurfaceFunction1D units2))
    (SurfaceFunction1D units3)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance HasUnits (Nondegenerate (SurfaceFunction1D units)) units

instance
  Units.Coercion
    (Nondegenerate (SurfaceFunction1D units1))
    (Nondegenerate (SurfaceFunction1D units2))
  where
  coerce (Nondegenerate function) = Nondegenerate (Units.coerce function)

squared :: Units.Squared units1 units2 => SurfaceFunction1D units1 -> SurfaceFunction1D units2
squared function = Units.specialize (squared_ function)

squared_ :: SurfaceFunction1D units -> SurfaceFunction1D (units ?*? units)
squared_ f =
  new
    (CompiledFunction.map Expression.squared_ Quantity.squared_ Interval.squared_ f.compiled)
    (Pair.map (2.0 * f ?*?) (partialDerivatives f))

cubed :: SurfaceFunction1D Unitless -> SurfaceFunction1D Unitless
cubed f =
  new
    (CompiledFunction.map Expression.cubed Number.cubed Interval.cubed f.compiled)
    (Pair.map (3.0 * squared f *) (partialDerivatives f))

sin :: SurfaceFunction1D Radians -> SurfaceFunction1D Unitless
sin f = do
  let (dfdu, dfdv) = partialDerivatives f
  let compiledSin = CompiledFunction.map Expression.sin Angle.sin Interval.sin f.compiled
  let sinPartialDerivatives =
        ( cos f * (dfdu / Angle.radian)
        , cos f * (dfdv / Angle.radian)
        )
  new compiledSin sinPartialDerivatives

cos :: SurfaceFunction1D Radians -> SurfaceFunction1D Unitless
cos f = do
  let (dfdu, dfdv) = partialDerivatives f
  let compiledCos = CompiledFunction.map Expression.cos Angle.cos Interval.cos f.compiled
  let cosPartialDerivatives =
        ( negate (sin f) * (dfdu / Angle.radian)
        , negate (sin f) * (dfdv / Angle.radian)
        )
  new compiledCos cosPartialDerivatives

data IsZero = IsZero deriving (Eq, Show, Err)

zeros :: Tolerance units => SurfaceFunction1D units -> Result IsZero Zeros
zeros function
  | function ~= zero = Err IsZero
  | otherwise = do
      let (dfdu, dfdv) = partialDerivatives function
      -- Using Nonzero should be OK here
      -- since we only actually use dudv and dvdu
      -- in subdomains where we know the denominator is non-zero
      let dudv = -dfdv / Nonzero dfdu
      let dvdu = -dfdu / Nonzero dfdv
      case Solve2D.search (findZeros function dudv dvdu) AllZeroTypes of
        Ok solutions -> do
          let partialZeros = PartialZeros.empty & forEach solutions addSolution
          Ok (PartialZeros.finalize function dvdu dudv partialZeros)
        Err Solve2D.InfiniteRecursion -> throw HigherOrderZero

addSolution :: Solution units -> PartialZeros units -> PartialZeros units
addSolution solution partialZeros = case solution of
  CrossingCurveSolution segment ->
    PartialZeros.addCrossingSegment segment partialZeros
  TangentPointSolution tangentPoint ->
    PartialZeros.addTangentPoint tangentPoint partialZeros
  SaddleRegionSolution saddleRegion ->
    PartialZeros.addSaddleRegion saddleRegion partialZeros

data FindZerosContext = AllZeroTypes | CrossingCurvesOnly deriving (Show)

data Solution units
  = CrossingCurveSolution PartialZeros.CrossingSegment
  | TangentPointSolution (UvPoint, Sign)
  | SaddleRegionSolution (SaddleRegion units)

findZeros ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  SurfaceFunction1D Unitless ->
  FindZerosContext ->
  Domain2D ->
  Solve2D.Exclusions exclusions ->
  Solve2D.Action exclusions FindZerosContext (Solution units)
findZeros f dudv dvdu context subdomain exclusions = do
  -- TODO find zeros along unit domain boundaries
  -- (including nasty cases like curves emanating from a saddle point
  -- being along a domain boundary)
  let subproblem = Subproblem.new f dudv dvdu subdomain
  if not (Subproblem.isZeroCandidate subproblem)
    then Solve2D.pass
    else case exclusions of
      Solve2D.SomeExclusions -> Solve2D.recurse context
      Solve2D.NoExclusions ->
        case context of
          CrossingCurvesOnly -> findCrossingCurves subproblem
          AllZeroTypes -> do
            let Subproblem{fuRange, fvRange} = subproblem
            if Interval.isResolved fuRange || Interval.isResolved fvRange
              then findCrossingCurves subproblem
              else findTangentSolutions subproblem

findTangentSolutions ::
  Tolerance units =>
  Subproblem units ->
  Solve2D.Action Solve2D.NoExclusions FindZerosContext (Solution units)
findTangentSolutions subproblem = do
  let Subproblem{f, subdomain, uvRange, fuuRange, fuvRange, fvvRange} = subproblem
  let determinant = fuuRange ?*? fvvRange - fuvRange ?*? fuvRange
  case Interval.resolvedSign determinant of
    Resolved determinantSign -> do
      let (fu, fv) = partialDerivatives f
      let (fuu, fuv, fvv) = secondPartialDerivatives f
      let maybePoint =
            Solve2D.unique
              (\testRange -> VectorBounds2D (range testRange fu) (range testRange fv))
              (\testPoint -> Vector2D (valueAt testPoint fu) (valueAt testPoint fv))
              (\testPoint -> Vector2D (valueAt testPoint fuu) (valueAt testPoint fuv))
              (\testPoint -> Vector2D (valueAt testPoint fuv) (valueAt testPoint fvv))
              uvRange
      case maybePoint of
        Nothing -> Solve2D.recurse CrossingCurvesOnly
        Just point ->
          if Bounds2D.member point (Domain2D.interior subdomain)
            && valueAt point f ~= Quantity.zero
            then case determinantSign of
              Positive -> do
                -- Non-saddle tangent point
                -- Note that fuu and fvv must be either both positive or both negative
                -- to reach this code path, so we can take the sign of either one
                -- to determine the sign of the tangent point
                let sign = Quantity.sign (Interval.lower fuuRange)
                Solve2D.return (TangentPointSolution (point, sign))
              Negative -> do
                -- Saddle region
                let saddleRegion = SaddleRegion.quadratic subproblem point
                Solve2D.return (SaddleRegionSolution saddleRegion)
            else
              Solve2D.recurse CrossingCurvesOnly
    Unresolved ->
      -- TODO check for tangent curves
      Solve2D.recurse AllZeroTypes

findCrossingCurves ::
  Tolerance units =>
  Subproblem units ->
  Solve2D.Action Solve2D.NoExclusions FindZerosContext (Solution units)
findCrossingCurves subproblem =
  case crossingCurve subproblem of
    Unresolved -> Solve2D.recurse CrossingCurvesOnly
    Resolved Nothing -> Solve2D.pass
    Resolved (Just curve) -> Solve2D.return (CrossingCurveSolution curve)

crossingCurve ::
  Tolerance units =>
  Subproblem units ->
  Fuzzy (Maybe PartialZeros.CrossingSegment)
crossingCurve subproblem =
  Fuzzy.oneOf
    [ diagonalCrossingCurve subproblem
    , horizontalCrossingCurve subproblem
    , verticalCrossingCurve subproblem
    ]

diagonalCrossingCurve ::
  Tolerance units =>
  Subproblem units ->
  Fuzzy (Maybe PartialZeros.CrossingSegment)
diagonalCrossingCurve subproblem = do
  let Subproblem{fuRange, fvRange} = subproblem
  fuSign <- Interval.resolvedSign fuRange
  fvSign <- Interval.resolvedSign fvRange
  Resolved $
    case (fuSign, fvSign) of
      (Negative, Negative) -> southeastCrossingCurve subproblem
      (Negative, Positive) -> southwestCrossingCurve subproblem
      (Positive, Negative) -> northeastCrossingCurve subproblem
      (Positive, Positive) -> northwestCrossingCurve subproblem

southeastCrossingCurve :: Tolerance units => Subproblem units -> Maybe PartialZeros.CrossingSegment
southeastCrossingCurve subproblem = do
  let Subproblem{fValues} = subproblem
  let CornerValues{bottomLeft = f11, bottomRight = f21, topLeft = f12, topRight = f22} = fValues
  if f11 <= Quantity.zero || f22 >= Quantity.zero
    then Nothing
    else do
      let start = case compare f12 Quantity.zero of
            LT -> Subproblem.leftEdgePoint subproblem
            EQ -> Subproblem.topLeftPoint subproblem
            GT -> Subproblem.topEdgePoint subproblem
      let end = case compare f21 Quantity.zero of
            LT -> Subproblem.bottomEdgePoint subproblem
            EQ -> Subproblem.bottomRightPoint subproblem
            GT -> Subproblem.rightEdgePoint subproblem
      Just (diagonalSegment start end)

southwestCrossingCurve :: Tolerance units => Subproblem units -> Maybe PartialZeros.CrossingSegment
southwestCrossingCurve subproblem = do
  let Subproblem{fValues} = subproblem
  let CornerValues{bottomLeft = f11, bottomRight = f21, topLeft = f12, topRight = f22} = fValues
  if f12 <= Quantity.zero || f21 >= Quantity.zero
    then Nothing
    else do
      let start = case compare f22 Quantity.zero of
            LT -> Subproblem.topEdgePoint subproblem
            EQ -> Subproblem.topRightPoint subproblem
            GT -> Subproblem.rightEdgePoint subproblem
      let end = case compare f11 Quantity.zero of
            LT -> Subproblem.leftEdgePoint subproblem
            EQ -> Subproblem.bottomLeftPoint subproblem
            GT -> Subproblem.bottomEdgePoint subproblem
      Just (diagonalSegment start end)

northeastCrossingCurve :: Tolerance units => Subproblem units -> Maybe PartialZeros.CrossingSegment
northeastCrossingCurve subproblem = do
  let Subproblem{fValues} = subproblem
  let CornerValues{bottomLeft = f11, bottomRight = f21, topLeft = f12, topRight = f22} = fValues
  if f21 <= Quantity.zero || f12 >= Quantity.zero
    then Nothing
    else do
      let start = case compare f11 Quantity.zero of
            LT -> Subproblem.bottomEdgePoint subproblem
            EQ -> Subproblem.bottomLeftPoint subproblem
            GT -> Subproblem.leftEdgePoint subproblem
      let end = case compare f22 Quantity.zero of
            LT -> Subproblem.rightEdgePoint subproblem
            EQ -> Subproblem.topRightPoint subproblem
            GT -> Subproblem.topEdgePoint subproblem
      Just (diagonalSegment start end)

northwestCrossingCurve :: Tolerance units => Subproblem units -> Maybe PartialZeros.CrossingSegment
northwestCrossingCurve subproblem = do
  let Subproblem{fValues} = subproblem
  let CornerValues{bottomLeft = f11, bottomRight = f21, topLeft = f12, topRight = f22} = fValues
  if f22 <= Quantity.zero || f11 >= Quantity.zero
    then Nothing
    else do
      let start = case compare f21 Quantity.zero of
            LT -> Subproblem.rightEdgePoint subproblem
            EQ -> Subproblem.bottomRightPoint subproblem
            GT -> Subproblem.bottomEdgePoint subproblem
      let end = case compare f12 Quantity.zero of
            LT -> Subproblem.topEdgePoint subproblem
            EQ -> Subproblem.topLeftPoint subproblem
            GT -> Subproblem.leftEdgePoint subproblem
      Just (diagonalSegment start end)

diagonalSegment ::
  Tolerance units =>
  (UvPoint, Domain2D.Boundary) ->
  (UvPoint, Domain2D.Boundary) ->
  PartialZeros.CrossingSegment
diagonalSegment start end = do
  let startPoint = Pair.first start
  let endPoint = Pair.first end
  PartialZeros.diagonalSegment start end (Bounds2D.hull2 startPoint endPoint)

horizontalCrossingCurve ::
  Tolerance units =>
  Subproblem units ->
  Fuzzy (Maybe PartialZeros.CrossingSegment)
horizontalCrossingCurve subproblem = do
  let Subproblem{fvRange} = subproblem
  if Interval.isResolved fvRange
    then do
      let bottomEdgeRange = Subproblem.bottomEdgeRange subproblem
      let topEdgeRange = Subproblem.topEdgeRange subproblem
      bottomEdgeSign <- Interval.resolvedSign bottomEdgeRange
      topEdgeSign <- Interval.resolvedSign topEdgeRange
      case (bottomEdgeSign, topEdgeSign) of
        (Negative, Negative) -> Resolved Nothing
        (Positive, Positive) -> Resolved Nothing
        (Negative, Positive) -> Fuzzy.map Just (westCrossingCurve subproblem)
        (Positive, Negative) -> Fuzzy.map Just (eastCrossingCurve subproblem)
    else Unresolved

eastCrossingCurve :: Tolerance units => Subproblem units -> Fuzzy PartialZeros.CrossingSegment
eastCrossingCurve subproblem = do
  let start = Subproblem.leftEdgePoint subproblem
  let end = Subproblem.rightEdgePoint subproblem
  horizontalCurve subproblem start end

westCrossingCurve :: Tolerance units => Subproblem units -> Fuzzy PartialZeros.CrossingSegment
westCrossingCurve subproblem = do
  let start = Subproblem.rightEdgePoint subproblem
  let end = Subproblem.leftEdgePoint subproblem
  horizontalCurve subproblem start end

horizontalCurve ::
  Tolerance units =>
  Subproblem units ->
  (UvPoint, Domain2D.Boundary) ->
  (UvPoint, Domain2D.Boundary) ->
  Fuzzy PartialZeros.CrossingSegment
horizontalCurve Subproblem{f, dvdu, subdomain, uvRange} start end = do
  let startPoint = Pair.first start
  let endPoint = Pair.first end
  let uStart = Point2D.xCoordinate startPoint
  let uEnd = Point2D.xCoordinate endPoint
  let curve = HorizontalCurve.new f dvdu uStart uEnd (NonEmpty.one uvRange)
  let Domain2D _ vSubdomain = subdomain
  let Bounds2D _ curveVBounds = Curve2D.bounds curve
  if Interval.contains curveVBounds (Domain1D.interior vSubdomain)
    then Resolved (PartialZeros.horizontalSegment start end uvRange)
    else Unresolved

verticalCrossingCurve ::
  Tolerance units =>
  Subproblem units ->
  Fuzzy (Maybe PartialZeros.CrossingSegment)
verticalCrossingCurve subproblem = do
  let Subproblem{fuRange} = subproblem
  if Interval.isResolved fuRange
    then do
      let leftEdgeRange = Subproblem.leftEdgeRange subproblem
      let rightEdgeRange = Subproblem.rightEdgeRange subproblem
      leftEdgeSign <- Interval.resolvedSign leftEdgeRange
      rightEdgeSign <- Interval.resolvedSign rightEdgeRange
      case (leftEdgeSign, rightEdgeSign) of
        (Negative, Negative) -> Resolved Nothing
        (Positive, Positive) -> Resolved Nothing
        (Negative, Positive) -> Fuzzy.map Just (northCrossingCurve subproblem)
        (Positive, Negative) -> Fuzzy.map Just (southCrossingCurve subproblem)
    else Unresolved

southCrossingCurve :: Tolerance units => Subproblem units -> Fuzzy PartialZeros.CrossingSegment
southCrossingCurve subproblem = do
  let start = Subproblem.topEdgePoint subproblem
  let end = Subproblem.bottomEdgePoint subproblem
  verticalCurve subproblem start end

northCrossingCurve :: Tolerance units => Subproblem units -> Fuzzy PartialZeros.CrossingSegment
northCrossingCurve subproblem = do
  let start = Subproblem.bottomEdgePoint subproblem
  let end = Subproblem.topEdgePoint subproblem
  verticalCurve subproblem start end

verticalCurve ::
  Tolerance units =>
  Subproblem units ->
  (UvPoint, Domain2D.Boundary) ->
  (UvPoint, Domain2D.Boundary) ->
  Fuzzy PartialZeros.CrossingSegment
verticalCurve Subproblem{f, dudv, subdomain, uvRange} start end = do
  let startPoint = Pair.first start
  let endPoint = Pair.first end
  let vStart = Point2D.yCoordinate startPoint
  let vEnd = Point2D.yCoordinate endPoint
  let curve = VerticalCurve.new f dudv vStart vEnd (NonEmpty.one uvRange)
  let Domain2D uSubdomain _ = subdomain
  let Bounds2D curveUBounds _ = Curve2D.bounds curve
  if Interval.contains curveUBounds (Domain1D.interior uSubdomain)
    then Resolved (PartialZeros.verticalSegment start end uvRange)
    else Unresolved
