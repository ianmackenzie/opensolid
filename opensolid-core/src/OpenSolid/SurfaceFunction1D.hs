module OpenSolid.SurfaceFunction1D
  ( SurfaceFunction1D (compiled, du, dv)
  , Compiled
  , evaluate
  , evaluateAt
  , evaluateBounds
  , evaluateBoundsWithin
  , derivative
  , derivativeIn
  , compiled
  , zero
  , constant
  , u
  , v
  , parameter
  , Zeros
  , IsZero (IsZero)
  , zeros
  , new
  , recursive
  , quotient
  , quotient_
  , unsafeQuotient
  , unsafeQuotient_
  , squared
  , squared_
  , sqrt
  , sqrt_
  , unsafeSqrt
  , unsafeSqrt_
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
import OpenSolid.Composition
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction2D (Direction2D (Direction2D))
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.DivisionByZero (DivisionByZero)
import OpenSolid.Domain1D qualified as Domain1D
import OpenSolid.Domain2D (Domain2D (Domain2D))
import OpenSolid.Domain2D qualified as Domain2D
import OpenSolid.Expression qualified as Expression
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.HigherOrderZero (HigherOrderZero (HigherOrderZero))
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Solve2D qualified as Solve2D
import OpenSolid.SurfaceFunction1D.Blending qualified as SurfaceFunction1D.Blending
import OpenSolid.SurfaceFunction1D.Desingularization qualified as SurfaceFunction1D.Desingularization
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D.HorizontalCurve qualified as HorizontalCurve
import OpenSolid.SurfaceFunction1D.PartialZeros (PartialZeros)
import OpenSolid.SurfaceFunction1D.PartialZeros qualified as PartialZeros
import OpenSolid.SurfaceFunction1D.Quotient qualified as SurfaceFunction1D.Quotient
import OpenSolid.SurfaceFunction1D.SaddleRegion (SaddleRegion)
import OpenSolid.SurfaceFunction1D.SaddleRegion qualified as SaddleRegion
import OpenSolid.SurfaceFunction1D.Subproblem (CornerValues (..), Subproblem (..))
import OpenSolid.SurfaceFunction1D.Subproblem qualified as Subproblem
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D.VerticalCurve qualified as VerticalCurve
import OpenSolid.SurfaceFunction1D.Zeros (Zeros (..))
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Tolerance qualified as Tolerance
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
  , du :: ~(SurfaceFunction1D units)
  , dv :: ~(SurfaceFunction1D units)
  }

type Compiled units = CompiledFunction UvPoint (Quantity units) UvBounds (Interval units)

instance HasUnits (SurfaceFunction1D units) units

instance Units.Coercion (SurfaceFunction1D unitsA) (SurfaceFunction1D unitsB) where
  coerce (SurfaceFunction1D c du dv) =
    SurfaceFunction1D (Units.coerce c) (Units.coerce du) (Units.coerce dv)

instance ApproximateEquality (SurfaceFunction1D units) units where
  function1 ~= function2 =
    List.allTrue
      [evaluate function1 uvPoint ~= evaluate function2 uvPoint | uvPoint <- UvPoint.samples]

instance
  units1 ~ units2 =>
  Intersects (SurfaceFunction1D units1) (Quantity units2) units1
  where
  function `intersects` value =
    -- TODO optimize this to use a special Solve2D.find or similar
    -- to efficiently check if there is *a* zero anywhere
    -- instead of finding *all* zeros (and the full geometry of each)
    case zeros (function .-. value) of
      Ok (Zeros [] [] [] []) -> False
      Ok (Zeros{}) -> True
      Error IsZero -> True

instance
  units1 ~ units2 =>
  Intersects (Quantity units1) (SurfaceFunction1D units2) units1
  where
  value `intersects` function = function `intersects` value

instance Negation (SurfaceFunction1D units) where
  negative function = new (negative function.compiled) (\p -> negative (derivative p function))

instance Multiplication Sign (SurfaceFunction1D units) (SurfaceFunction1D units) where
  Positive .*. function = function
  Negative .*. function = negative function

instance Multiplication (SurfaceFunction1D units) Sign (SurfaceFunction1D units) where
  function .*. Positive = function
  function .*. Negative = negative function

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction1D units1)
    (SurfaceFunction1D units2)
    (SurfaceFunction1D units1)
  where
  lhs .+. rhs = new (lhs.compiled .+. rhs.compiled) (\p -> derivative p lhs .+. derivative p rhs)

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction1D units1)
    (Quantity units2)
    (SurfaceFunction1D units1)
  where
  function .+. value = function .+. constant value

instance
  units1 ~ units2 =>
  Addition
    (Quantity units1)
    (SurfaceFunction1D units2)
    (SurfaceFunction1D units1)
  where
  value .+. function = constant value .+. function

instance
  units1 ~ units2 =>
  Subtraction (SurfaceFunction1D units1) (SurfaceFunction1D units2) (SurfaceFunction1D units1)
  where
  lhs .-. rhs = new (lhs.compiled .-. rhs.compiled) (\p -> derivative p lhs .-. derivative p rhs)

instance
  units1 ~ units2 =>
  Subtraction (SurfaceFunction1D units1) (Quantity units2) (SurfaceFunction1D units1)
  where
  function .-. value = function .-. constant value

instance
  units1 ~ units2 =>
  Subtraction (Quantity units1) (SurfaceFunction1D units2) (SurfaceFunction1D units1)
  where
  value .-. function = constant value .-. function

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction1D units1) (SurfaceFunction1D units2) (SurfaceFunction1D units3)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (SurfaceFunction1D units1)
    (SurfaceFunction1D units2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? rhs.compiled)
      (\p -> derivative p lhs ?*? rhs .+. lhs ?*? derivative p rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction1D units1) (Quantity units2) (SurfaceFunction1D units3)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (SurfaceFunction1D units1)
    (Quantity units2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  function ?*? value = function ?*? constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (SurfaceFunction1D units2) (SurfaceFunction1D units3)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Quantity units1)
    (SurfaceFunction1D units2)
    (SurfaceFunction1D (units1 ?*? units2))
  where
  value ?*? function = constant value ?*? function

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction1D units1)
    (Vector2D units2 space)
    (VectorSurfaceFunction2D units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (SurfaceFunction1D units1)
    (Vector2D units2 space)
    (VectorSurfaceFunction2D (units1 ?*? units2) space)
  where
  function ?*? vector = function ?*? VectorSurfaceFunction2D.constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector2D units1 space)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction2D units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Vector2D units1 space)
    (SurfaceFunction1D units2)
    (VectorSurfaceFunction2D (units1 ?*? units2) space)
  where
  vector ?*? function = VectorSurfaceFunction2D.constant vector ?*? function

instance
  Multiplication
    (SurfaceFunction1D units)
    (Direction2D space)
    (VectorSurfaceFunction2D units space)
  where
  lhs .*. rhs = lhs .*. Vector2D.unit rhs

instance
  Multiplication
    (Direction2D space)
    (SurfaceFunction1D units)
    (VectorSurfaceFunction2D units space)
  where
  lhs .*. rhs = Vector2D.unit lhs .*. rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction1D units1)
    (Vector3D units2 space)
    (VectorSurfaceFunction3D units3 space)
  where
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

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
  lhs .*. rhs = Units.specialize (lhs ?*? rhs)

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
  lhs .*. rhs = lhs .*. Vector3D.unit rhs

instance
  Multiplication
    (Direction3D space)
    (SurfaceFunction1D units)
    (VectorSurfaceFunction3D units space)
  where
  lhs .*. rhs = Vector3D.unit lhs .*. rhs

instance
  Units.Quotient units1 units2 units3 =>
  Division (SurfaceFunction1D units1) (Quantity units2) (SurfaceFunction1D units3)
  where
  lhs ./. rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (SurfaceFunction1D units1)
    (Quantity units2)
    (SurfaceFunction1D (units1 ?/? units2))
  where
  function ?/? value = Units.simplify (function ?*? (1 /? value))

instance Composition (SurfaceFunction1D Unitless) (Curve1D units) (SurfaceFunction1D units) where
  curve `compose` function =
    new
      (curve.compiled `compose` function.compiled)
      (\p -> curve.derivative `compose` function .*. derivative p function)

evaluate :: SurfaceFunction1D units -> UvPoint -> Quantity units
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

{-# INLINE evaluateAt #-}
evaluateAt :: UvPoint -> SurfaceFunction1D units -> Quantity units
evaluateAt uvPoint function = evaluate function uvPoint

evaluateBounds :: SurfaceFunction1D units -> UvBounds -> Interval units
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

{-# INLINE evaluateBoundsWithin #-}
evaluateBoundsWithin :: UvBounds -> SurfaceFunction1D units -> Interval units
evaluateBoundsWithin uvBounds function = evaluateBounds function uvBounds

derivative :: SurfaceParameter -> SurfaceFunction1D units -> SurfaceFunction1D units
derivative U = (.du)
derivative V = (.dv)

derivativeIn :: Direction2D UvSpace -> SurfaceFunction1D units -> SurfaceFunction1D units
derivativeIn (Direction2D dx dy) function =
  dx .*. function.du .+. dy .*. function.dv

compiled :: SurfaceFunction1D units -> Compiled units
compiled = (.compiled)

zero :: SurfaceFunction1D units
zero = constant Quantity.zero

one :: SurfaceFunction1D Unitless
one = constant 1

constant :: Quantity units -> SurfaceFunction1D units
constant value = new (CompiledFunction.constant value) (const zero)

u :: SurfaceFunction1D Unitless
u = new (CompiledFunction.concrete Expression.u) (\case U -> one; V -> zero)

v :: SurfaceFunction1D Unitless
v = new (CompiledFunction.concrete Expression.v) (\case U -> zero; V -> one)

parameter :: SurfaceParameter -> SurfaceFunction1D Unitless
parameter U = u
parameter V = v

new :: Compiled units -> (SurfaceParameter -> SurfaceFunction1D units) -> SurfaceFunction1D units
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  SurfaceFunction1D c du (SurfaceFunction1D dv.compiled du.dv dv.dv)

recursive ::
  Compiled units ->
  (SurfaceFunction1D units -> SurfaceParameter -> SurfaceFunction1D units) ->
  SurfaceFunction1D units
recursive givenCompiled derivativeFunction =
  let self = new givenCompiled (derivativeFunction self) in self

desingularize ::
  SurfaceFunction1D units ->
  "singularityU0" ::: Maybe (SurfaceFunction1D units, SurfaceFunction1D units) ->
  "singularityU1" ::: Maybe (SurfaceFunction1D units, SurfaceFunction1D units) ->
  "singularityV0" ::: Maybe (SurfaceFunction1D units, SurfaceFunction1D units) ->
  "singularityV1" ::: Maybe (SurfaceFunction1D units, SurfaceFunction1D units) ->
  SurfaceFunction1D units
desingularize = SurfaceFunction1D.Blending.desingularize desingularized

desingularized ::
  SurfaceFunction1D Unitless ->
  SurfaceFunction1D units ->
  SurfaceFunction1D units ->
  SurfaceFunction1D units ->
  SurfaceFunction1D units
desingularized t start middle end =
  new
    (CompiledFunction.desingularized t.compiled start.compiled middle.compiled end.compiled)
    (\p -> desingularized t (derivative p start) (derivative p middle) (derivative p end))

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  SurfaceFunction1D units1 ->
  SurfaceFunction1D units2 ->
  Result DivisionByZero (SurfaceFunction1D units3)
quotient lhs rhs = Units.specialize (quotient_ lhs rhs)

quotient_ ::
  Tolerance units2 =>
  SurfaceFunction1D units1 ->
  SurfaceFunction1D units2 ->
  Result DivisionByZero (SurfaceFunction1D (units1 ?/? units2))
quotient_ numerator denominator = do
  let lhopital p = do
        let numerator' = derivative p numerator
        let numerator'' = derivative p numerator'
        let denominator' = derivative p denominator
        let denominator'' = derivative p denominator'
        let value = unsafeQuotient_ numerator' denominator'
        let firstDerivative =
              Units.simplify $
                unsafeQuotient_
                  (numerator'' ?*? denominator' .-. numerator' ?*? denominator'')
                  (2 *. squared_ denominator')
        (value, firstDerivative)
  SurfaceFunction1D.Quotient.impl unsafeQuotient_ lhopital desingularize numerator denominator

unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  SurfaceFunction1D units1 ->
  SurfaceFunction1D units2 ->
  SurfaceFunction1D units3
unsafeQuotient numerator denominator = Units.specialize (unsafeQuotient_ numerator denominator)

unsafeQuotient_ ::
  SurfaceFunction1D units1 ->
  SurfaceFunction1D units2 ->
  SurfaceFunction1D (units1 ?/? units2)
unsafeQuotient_ lhs rhs = do
  let quotientDerivative self p =
        unsafeQuotient_ (derivative p lhs) rhs .-. self .*. unsafeQuotient (derivative p rhs) rhs
  recursive
    (CompiledFunction.map2 (?/?) (?/?) (?/?) lhs.compiled rhs.compiled)
    quotientDerivative

squared :: Units.Squared units1 units2 => SurfaceFunction1D units1 -> SurfaceFunction1D units2
squared function = Units.specialize (squared_ function)

squared_ :: SurfaceFunction1D units -> SurfaceFunction1D (units ?*? units)
squared_ function =
  new
    (CompiledFunction.map Expression.squared_ Quantity.squared_ Interval.squared_ function.compiled)
    (\p -> 2 *. function ?*? derivative p function)

sqrt ::
  (Tolerance units1, Units.Squared units1 units2) =>
  SurfaceFunction1D units2 ->
  SurfaceFunction1D units1
sqrt function = sqrt_ (Units.unspecialize function)

sqrt_ :: Tolerance units => SurfaceFunction1D (units ?*? units) -> SurfaceFunction1D units
sqrt_ function =
  if Tolerance.using (Quantity.squared_ ?tolerance) (function ~= zero)
    then zero
    else do
      let maybeSingularity param value sign = do
            let firstDerivative = derivative param function
            let secondDerivative = derivative param firstDerivative
            let testPoints = SurfaceFunction1D.Desingularization.testPoints param value
            let functionIsZeroAt testPoint =
                  Tolerance.using (Quantity.squared_ ?tolerance) $
                    evaluate function testPoint ~= Quantity.zero
            let functionIsZero = NonEmpty.allSatisfy functionIsZeroAt testPoints
            let firstDerivativeIsZeroAt testPoint = do
                  let secondDerivativeValue = evaluate secondDerivative testPoint
                  let firstDerivativeTolerance =
                        ?tolerance ?*? Quantity.sqrt_ (2 *. secondDerivativeValue)
                  Tolerance.using firstDerivativeTolerance $
                    evaluate firstDerivative testPoint ~= Quantity.zero
            let firstDerivativeIsZero = NonEmpty.allSatisfy firstDerivativeIsZeroAt testPoints
            if functionIsZero && firstDerivativeIsZero
              then Just (zero, sign .*. unsafeSqrt_ (0.5 *. secondDerivative))
              else Nothing
      desingularize
        (unsafeSqrt_ function)
        (#singularityU0 (maybeSingularity U 0 Positive))
        (#singularityU1 (maybeSingularity U 1 Negative))
        (#singularityV0 (maybeSingularity V 0 Positive))
        (#singularityV1 (maybeSingularity V 1 Negative))

unsafeSqrt :: Units.Squared units1 units2 => SurfaceFunction1D units2 -> SurfaceFunction1D units1
unsafeSqrt function = unsafeSqrt_ (Units.unspecialize function)

unsafeSqrt_ :: SurfaceFunction1D (units ?*? units) -> SurfaceFunction1D units
unsafeSqrt_ function =
  recursive
    (CompiledFunction.map Expression.sqrt_ Quantity.sqrt_ Interval.sqrt_ function.compiled)
    (\self p -> Units.coerce (unsafeQuotient_ (derivative p function) (2 *. self)))

cubed :: SurfaceFunction1D Unitless -> SurfaceFunction1D Unitless
cubed function =
  new
    (CompiledFunction.map Expression.cubed Number.cubed Interval.cubed function.compiled)
    (\p -> 3 *. squared function .*. derivative p function)

sin :: SurfaceFunction1D Radians -> SurfaceFunction1D Unitless
sin function =
  new
    (CompiledFunction.map Expression.sin Angle.sin Interval.sin function.compiled)
    (\p -> cos function .*. (derivative p function ./. Angle.radian))

cos :: SurfaceFunction1D Radians -> SurfaceFunction1D Unitless
cos function =
  new
    (CompiledFunction.map Expression.cos Angle.cos Interval.cos function.compiled)
    (\p -> negative (sin function) .*. (derivative p function ./. Angle.radian))

data IsZero = IsZero deriving (Eq, Show)

zeros :: Tolerance units => SurfaceFunction1D units -> Result IsZero Zeros
zeros function
  | function ~= zero = Error IsZero
  | otherwise = do
      let fu = function.du
      let fv = function.dv
      -- Using unsafeQuotient should be OK here
      -- since we only actually use dudv and dvdu
      -- in subdomains where we know the denominator is non-zero
      let dudv = unsafeQuotient (negative fv) fu
      let dvdu = unsafeQuotient (negative fu) fv
      case Solve2D.search (findZeros function dudv dvdu) AllZeroTypes of
        Ok solutions -> do
          let partialZeros = List.foldl addSolution PartialZeros.empty solutions
          Ok (PartialZeros.finalize function dvdu dudv partialZeros)
        Error Solve2D.InfiniteRecursion -> throw HigherOrderZero

addSolution :: PartialZeros units -> Solution units -> PartialZeros units
addSolution partialZeros solution = case solution of
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
            let Subproblem{fuBounds, fvBounds} = subproblem
            if Interval.isResolved fuBounds || Interval.isResolved fvBounds
              then findCrossingCurves subproblem
              else findTangentSolutions subproblem

findTangentSolutions ::
  Tolerance units =>
  Subproblem units ->
  Solve2D.Action Solve2D.NoExclusions FindZerosContext (Solution units)
findTangentSolutions subproblem = do
  let Subproblem{f, subdomain, uvBounds, fuuBounds, fuvBounds, fvvBounds} = subproblem
  let determinant = fuuBounds ?*? fvvBounds .-. fuvBounds ?*? fuvBounds
  case Interval.resolvedSign determinant of
    Resolved determinantSign -> do
      let fu = f.du
      let fv = f.dv
      let fuu = f.du.du
      let fuv = f.du.dv
      let fvv = f.dv.dv
      let maybePoint =
            Solve2D.unique
              (\bounds -> VectorBounds2D (evaluateBounds fu bounds) (evaluateBounds fv bounds))
              (\point -> Vector2D (evaluate fu point) (evaluate fv point))
              (\point -> Vector2D (evaluate fuu point) (evaluate fuv point))
              (\point -> Vector2D (evaluate fuv point) (evaluate fvv point))
              uvBounds
      case maybePoint of
        Nothing -> Solve2D.recurse CrossingCurvesOnly
        Just point ->
          if Bounds2D.includes point (Domain2D.interior subdomain)
            && evaluate f point ~= Quantity.zero
            then case determinantSign of
              Positive -> do
                -- Non-saddle tangent point
                -- Note that fuu and fvv must be either both positive or both negative
                -- to reach this code path, so we can take the sign of either one
                -- to determine the sign of the tangent point
                let sign = Quantity.sign (Interval.lower fuuBounds)
                Solve2D.return (TangentPointSolution (point, sign))
              Negative -> do
                -- Saddle region
                let saddleRegion = SaddleRegion.quadratic subproblem point
                Solve2D.return (SaddleRegionSolution saddleRegion)
            else do
              Solve2D.recurse CrossingCurvesOnly
    Unresolved -> do
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
crossingCurve subproblem = do
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
  let Subproblem{fuBounds, fvBounds} = subproblem
  fuSign <- Interval.resolvedSign fuBounds
  fvSign <- Interval.resolvedSign fvBounds
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
  let Subproblem{fvBounds} = subproblem
  if Interval.isResolved fvBounds
    then do
      let bottomEdgeBounds = Subproblem.bottomEdgeBounds subproblem
      let topEdgeBounds = Subproblem.topEdgeBounds subproblem
      bottomEdgeSign <- Interval.resolvedSign bottomEdgeBounds
      topEdgeSign <- Interval.resolvedSign topEdgeBounds
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
horizontalCurve Subproblem{f, dvdu, subdomain, uvBounds} start end = do
  let startPoint = Pair.first start
  let endPoint = Pair.first end
  let uStart = Point2D.xCoordinate startPoint
  let uEnd = Point2D.xCoordinate endPoint
  let curve = HorizontalCurve.new f dvdu uStart uEnd (NonEmpty.one uvBounds)
  let Domain2D _ vSubdomain = subdomain
  let Bounds2D _ curveVBounds = Curve2D.bounds curve
  if Interval.contains curveVBounds (Domain1D.interior vSubdomain)
    then Resolved (PartialZeros.horizontalSegment start end uvBounds)
    else Unresolved

verticalCrossingCurve ::
  Tolerance units =>
  Subproblem units ->
  Fuzzy (Maybe PartialZeros.CrossingSegment)
verticalCrossingCurve subproblem = do
  let Subproblem{fuBounds} = subproblem
  if Interval.isResolved fuBounds
    then do
      let leftEdgeBounds = Subproblem.leftEdgeBounds subproblem
      let rightEdgeBounds = Subproblem.rightEdgeBounds subproblem
      leftEdgeSign <- Interval.resolvedSign leftEdgeBounds
      rightEdgeSign <- Interval.resolvedSign rightEdgeBounds
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
verticalCurve Subproblem{f, dudv, subdomain, uvBounds} start end = do
  let startPoint = Pair.first start
  let endPoint = Pair.first end
  let vStart = Point2D.yCoordinate startPoint
  let vEnd = Point2D.yCoordinate endPoint
  let curve = VerticalCurve.new f dudv vStart vEnd (NonEmpty.one uvBounds)
  let Domain2D uSubdomain _ = subdomain
  let Bounds2D curveUBounds _ = Curve2D.bounds curve
  if Interval.contains curveUBounds (Domain1D.interior uSubdomain)
    then Resolved (PartialZeros.verticalSegment start end uvBounds)
    else Unresolved
