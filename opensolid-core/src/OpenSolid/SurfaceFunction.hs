module OpenSolid.SurfaceFunction
  ( SurfaceFunction
  , Compiled
  , evaluate
  , evaluateAt
  , evaluateBounds
  , evaluateBoundsWithin
  , derivative
  , derivativeIn
  , zero
  , constant
  , u
  , v
  , parameter
  , Zeros
  , IsZero (IsZero)
  , zeros
  , new
  , quotient
  , quotient#
  , unsafeQuotient
  , unsafeQuotient#
  , squared
  , squared#
  , sqrt
  , sqrt#
  , unsafeSqrt
  , unsafeSqrt#
  , cubed
  , sin
  , cos
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.DivisionByZero (DivisionByZero)
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Domain2d (Domain2d (Domain2d))
import OpenSolid.Domain2d qualified as Domain2d
import OpenSolid.Error qualified as Error
import OpenSolid.Exception qualified as Exception
import OpenSolid.Expression qualified as Expression
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Solve2d qualified as Solve2d
import OpenSolid.SurfaceFunction.Blending qualified as SurfaceFunction.Blending
import OpenSolid.SurfaceFunction.Desingularization qualified as SurfaceFunction.Desingularization
import {-# SOURCE #-} OpenSolid.SurfaceFunction.HorizontalCurve qualified as HorizontalCurve
import OpenSolid.SurfaceFunction.PartialZeros (PartialZeros)
import OpenSolid.SurfaceFunction.PartialZeros qualified as PartialZeros
import OpenSolid.SurfaceFunction.Quotient qualified as SurfaceFunction.Quotient
import OpenSolid.SurfaceFunction.SaddleRegion (SaddleRegion)
import OpenSolid.SurfaceFunction.SaddleRegion qualified as SaddleRegion
import OpenSolid.SurfaceFunction.Subproblem (CornerValues (..), Subproblem (..))
import OpenSolid.SurfaceFunction.Subproblem qualified as Subproblem
import {-# SOURCE #-} OpenSolid.SurfaceFunction.VerticalCurve qualified as VerticalCurve
import OpenSolid.SurfaceFunction.Zeros (Zeros (..))
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvPoint qualified as UvPoint
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d

data SurfaceFunction units where
  SurfaceFunction ::
    Compiled units ->
    ~(SurfaceFunction units) ->
    ~(SurfaceFunction units) ->
    SurfaceFunction units

instance HasField "du" (SurfaceFunction units) (SurfaceFunction units) where
  getField (SurfaceFunction _ du _) = du

instance HasField "dv" (SurfaceFunction units) (SurfaceFunction units) where
  getField (SurfaceFunction _ _ dv) = dv

type Compiled units = CompiledFunction UvPoint (Quantity units) UvBounds (Bounds units)

instance HasUnits (SurfaceFunction units) units

instance Units.Coercion (SurfaceFunction unitsA) (SurfaceFunction unitsB) where
  coerce (SurfaceFunction c du dv) =
    SurfaceFunction (Units.coerce c) (Units.coerce du) (Units.coerce dv)

instance ApproximateEquality (SurfaceFunction units) units where
  function1 ~= function2 =
    List.allTrue
      [evaluate function1 uvPoint ~= evaluate function2 uvPoint | uvPoint <- UvPoint.samples]

instance
  units1 ~ units2 =>
  Intersects (SurfaceFunction units1) (Quantity units2) units1
  where
  function `intersects` value =
    -- TODO optimize this to use a special Solve2d.find or similar
    -- to efficiently check if there is *a* zero anywhere
    -- instead of finding *all* zeros (and the full geometry of each)
    case zeros (function .-. value) of
      Success (Zeros [] [] [] []) -> False
      Success (Zeros{}) -> True
      Failure IsZero -> True

instance
  units1 ~ units2 =>
  Intersects (Quantity units1) (SurfaceFunction units2) units1
  where
  value `intersects` function = function `intersects` value

instance Negation (SurfaceFunction units) where
  negative function = new (negative function.compiled) (\p -> negative (derivative p function))

instance Multiplication Sign (SurfaceFunction units) (SurfaceFunction units) where
  Positive .*. function = function
  Negative .*. function = negative function

instance Multiplication (SurfaceFunction units) Sign (SurfaceFunction units) where
  function .*. Positive = function
  function .*. Negative = negative function

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction units1)
    (SurfaceFunction units2)
    (SurfaceFunction units1)
  where
  lhs .+. rhs = new (lhs.compiled .+. rhs.compiled) (\p -> derivative p lhs .+. derivative p rhs)

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction units1)
    (Quantity units2)
    (SurfaceFunction units1)
  where
  function .+. value = function .+. constant value

instance
  units1 ~ units2 =>
  Addition
    (Quantity units1)
    (SurfaceFunction units2)
    (SurfaceFunction units1)
  where
  value .+. function = constant value .+. function

instance
  units1 ~ units2 =>
  Subtraction (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units1)
  where
  lhs .-. rhs = new (lhs.compiled .-. rhs.compiled) (\p -> derivative p lhs .-. derivative p rhs)

instance
  units1 ~ units2 =>
  Subtraction (SurfaceFunction units1) (Quantity units2) (SurfaceFunction units1)
  where
  function .-. value = function .-. constant value

instance
  units1 ~ units2 =>
  Subtraction (Quantity units1) (SurfaceFunction units2) (SurfaceFunction units1)
  where
  value .-. function = constant value .-. function

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units3)
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance
  Multiplication#
    (SurfaceFunction units1)
    (SurfaceFunction units2)
    (SurfaceFunction (units1 #*# units2))
  where
  lhs #*# rhs =
    new
      @ lhs.compiled #*# rhs.compiled
      @ \p -> derivative p lhs #*# rhs .+. lhs #*# derivative p rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction units1) (Quantity units2) (SurfaceFunction units3)
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance
  Multiplication#
    (SurfaceFunction units1)
    (Quantity units2)
    (SurfaceFunction (units1 #*# units2))
  where
  function #*# value = function #*# constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Quantity units1) (SurfaceFunction units2) (SurfaceFunction units3)
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance
  Multiplication#
    (Quantity units1)
    (SurfaceFunction units2)
    (SurfaceFunction (units1 #*# units2))
  where
  value #*# function = constant value #*# function

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (Vector2d (space @ units2))
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance
  Multiplication#
    (SurfaceFunction units1)
    (Vector2d (space @ units2))
    (VectorSurfaceFunction2d (space @ (units1 #*# units2)))
  where
  function #*# vector = function #*# VectorSurfaceFunction2d.constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector2d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance
  Multiplication#
    (Vector2d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (space @ (units1 #*# units2)))
  where
  vector #*# function = VectorSurfaceFunction2d.constant vector #*# function

instance
  Multiplication
    (SurfaceFunction units)
    (Direction2d space)
    (VectorSurfaceFunction2d (space @ units))
  where
  lhs .*. rhs = lhs .*. Vector2d.unit rhs

instance
  Multiplication
    (Direction2d space)
    (SurfaceFunction units)
    (VectorSurfaceFunction2d (space @ units))
  where
  lhs .*. rhs = Vector2d.unit lhs .*. rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (Vector3d (space @ units2))
    (VectorSurfaceFunction3d (space @ units3))
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance
  Multiplication#
    (SurfaceFunction units1)
    (Vector3d (space @ units2))
    (VectorSurfaceFunction3d (space @ (units1 #*# units2)))
  where
  function #*# vector = function #*# VectorSurfaceFunction3d.constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector3d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d (space @ units3))
  where
  lhs .*. rhs = Units.specialize (lhs #*# rhs)

instance
  Multiplication#
    (Vector3d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d (space @ (units1 #*# units2)))
  where
  vector #*# function = VectorSurfaceFunction3d.constant vector #*# function

instance
  Multiplication
    (SurfaceFunction units)
    (Direction3d space)
    (VectorSurfaceFunction3d (space @ units))
  where
  lhs .*. rhs = lhs .*. Vector3d.unit rhs

instance
  Multiplication
    (Direction3d space)
    (SurfaceFunction units)
    (VectorSurfaceFunction3d (space @ units))
  where
  lhs .*. rhs = Vector3d.unit lhs .*. rhs

instance
  Units.Quotient units1 units2 units3 =>
  Division (SurfaceFunction units1) (Quantity units2) (SurfaceFunction units3)
  where
  lhs ./. rhs = Units.specialize (lhs #/# rhs)

instance
  Division#
    (SurfaceFunction units1)
    (Quantity units2)
    (SurfaceFunction (units1 #/# units2))
  where
  function #/# value = Units.simplify (function #*# (1 /# value))

instance Composition (SurfaceFunction Unitless) (Curve units) (SurfaceFunction units) where
  curve . function =
    new
      @ curve.compiled . function.compiled
      @ \p -> curve.derivative . function .*. derivative p function

evaluate :: SurfaceFunction units -> UvPoint -> Quantity units
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

{-# INLINE evaluateAt #-}
evaluateAt :: UvPoint -> SurfaceFunction units -> Quantity units
evaluateAt uvPoint function = evaluate function uvPoint

evaluateBounds :: SurfaceFunction units -> UvBounds -> Bounds units
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

{-# INLINE evaluateBoundsWithin #-}
evaluateBoundsWithin :: UvBounds -> SurfaceFunction units -> Bounds units
evaluateBoundsWithin uvBounds function = evaluateBounds function uvBounds

instance HasField "compiled" (SurfaceFunction units) (Compiled units) where
  getField (SurfaceFunction c _ _) = c

derivative :: SurfaceParameter -> SurfaceFunction units -> SurfaceFunction units
derivative U = (.du)
derivative V = (.dv)

derivativeIn :: Direction2d UvSpace -> SurfaceFunction units -> SurfaceFunction units
derivativeIn direction function =
  direction.xComponent .*. function.du .+. direction.yComponent .*. function.dv

zero :: SurfaceFunction units
zero = constant Quantity.zero

one :: SurfaceFunction Unitless
one = constant 1

constant :: Quantity units -> SurfaceFunction units
constant value = new (CompiledFunction.constant value) (const zero)

u :: SurfaceFunction Unitless
u = new (CompiledFunction.concrete Expression.u) (\case U -> one; V -> zero)

v :: SurfaceFunction Unitless
v = new (CompiledFunction.concrete Expression.v) (\case U -> zero; V -> one)

parameter :: SurfaceParameter -> SurfaceFunction Unitless
parameter U = u
parameter V = v

new :: Compiled units -> (SurfaceParameter -> SurfaceFunction units) -> SurfaceFunction units
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  SurfaceFunction c du (SurfaceFunction dv.compiled du.dv dv.dv)

recursive ::
  Compiled units ->
  (SurfaceFunction units -> SurfaceParameter -> SurfaceFunction units) ->
  SurfaceFunction units
recursive givenCompiled derivativeFunction =
  let self = new givenCompiled (derivativeFunction self) in self

desingularize ::
  SurfaceFunction units ->
  "singularityU0" ::: Maybe (SurfaceFunction units, SurfaceFunction units) ->
  "singularityU1" ::: Maybe (SurfaceFunction units, SurfaceFunction units) ->
  "singularityV0" ::: Maybe (SurfaceFunction units, SurfaceFunction units) ->
  "singularityV1" ::: Maybe (SurfaceFunction units, SurfaceFunction units) ->
  SurfaceFunction units
desingularize = SurfaceFunction.Blending.desingularize desingularized

desingularized ::
  SurfaceFunction Unitless ->
  SurfaceFunction units ->
  SurfaceFunction units ->
  SurfaceFunction units ->
  SurfaceFunction units
desingularized t start middle end =
  new
    (CompiledFunction.desingularized t.compiled start.compiled middle.compiled end.compiled)
    (\p -> desingularized t (derivative p start) (derivative p middle) (derivative p end))

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  Result DivisionByZero (SurfaceFunction units3)
quotient lhs rhs = Units.specialize (quotient# lhs rhs)

quotient# ::
  Tolerance units2 =>
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  Result DivisionByZero (SurfaceFunction (units1 #/# units2))
quotient# numerator denominator = do
  let lhopital p = do
        let numerator' = derivative p numerator
        let numerator'' = derivative p numerator'
        let denominator' = derivative p denominator
        let denominator'' = derivative p denominator'
        let value = unsafeQuotient# numerator' denominator'
        let firstDerivative =
              Units.simplify $
                unsafeQuotient#
                  (numerator'' #*# denominator' .-. numerator' #*# denominator'')
                  (2 *. squared# denominator')
        (value, firstDerivative)
  SurfaceFunction.Quotient.impl unsafeQuotient# lhopital desingularize numerator denominator

unsafeQuotient ::
  Units.Quotient units1 units2 units3 =>
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  SurfaceFunction units3
unsafeQuotient numerator denominator = Units.specialize (unsafeQuotient# numerator denominator)

unsafeQuotient# ::
  SurfaceFunction units1 ->
  SurfaceFunction units2 ->
  SurfaceFunction (units1 #/# units2)
unsafeQuotient# lhs rhs =
  recursive
    @ CompiledFunction.map2 (#/#) (#/#) (#/#) lhs.compiled rhs.compiled
    @ \self p ->
      unsafeQuotient# (derivative p lhs) rhs .-. self .*. unsafeQuotient (derivative p rhs) rhs

squared :: Units.Squared units1 units2 => SurfaceFunction units1 -> SurfaceFunction units2
squared function = Units.specialize (squared# function)

squared# :: SurfaceFunction units -> SurfaceFunction (units #*# units)
squared# function =
  new
    @ CompiledFunction.map Expression.squared# Quantity.squared# Bounds.squared# function.compiled
    @ \p -> 2 *. function #*# derivative p function

sqrt ::
  (Tolerance units1, Units.Squared units1 units2) =>
  SurfaceFunction units2 ->
  SurfaceFunction units1
sqrt function = sqrt# (Units.unspecialize function)

sqrt# :: Tolerance units => SurfaceFunction (units #*# units) -> SurfaceFunction units
sqrt# function =
  if Tolerance.using Tolerance.squared# (function ~= zero)
    then zero
    else do
      let maybeSingularity param value sign = do
            let firstDerivative = derivative param function
            let secondDerivative = derivative param firstDerivative
            let testPoints = SurfaceFunction.Desingularization.testPoints param value
            let functionIsZeroAt testPoint =
                  Tolerance.using Tolerance.squared# $
                    evaluate function testPoint ~= Quantity.zero
            let functionIsZero = List.allSatisfy functionIsZeroAt testPoints
            let firstDerivativeIsZeroAt testPoint = do
                  let secondDerivativeValue = evaluate secondDerivative testPoint
                  let firstDerivativeTolerance =
                        ?tolerance #*# Quantity.sqrt# (2 *. secondDerivativeValue)
                  Tolerance.using firstDerivativeTolerance $
                    evaluate firstDerivative testPoint ~= Quantity.zero
            let firstDerivativeIsZero = List.allSatisfy firstDerivativeIsZeroAt testPoints
            if functionIsZero && firstDerivativeIsZero
              then Just (zero, sign .*. unsafeSqrt# (0.5 *. secondDerivative))
              else Nothing
      desingularize (unsafeSqrt# function)
        @ #singularityU0 (maybeSingularity U 0 Positive)
        @ #singularityU1 (maybeSingularity U 1 Negative)
        @ #singularityV0 (maybeSingularity V 0 Positive)
        @ #singularityV1 (maybeSingularity V 1 Negative)

unsafeSqrt :: Units.Squared units1 units2 => SurfaceFunction units2 -> SurfaceFunction units1
unsafeSqrt function = unsafeSqrt# (Units.unspecialize function)

unsafeSqrt# :: SurfaceFunction (units #*# units) -> SurfaceFunction units
unsafeSqrt# function =
  recursive
    @ CompiledFunction.map Expression.sqrt# Quantity.sqrt# Bounds.sqrt# function.compiled
    @ \self p -> Units.coerce (unsafeQuotient# (derivative p function) (2 *. self))

cubed :: SurfaceFunction Unitless -> SurfaceFunction Unitless
cubed function =
  new
    (CompiledFunction.map Expression.cubed Number.cubed Bounds.cubed function.compiled)
    (\p -> 3 *. squared function .*. derivative p function)

sin :: SurfaceFunction Radians -> SurfaceFunction Unitless
sin function =
  new
    @ CompiledFunction.map Expression.sin Angle.sin Bounds.sin function.compiled
    @ \p -> cos function .*. (derivative p function ./. Angle.radian)

cos :: SurfaceFunction Radians -> SurfaceFunction Unitless
cos function =
  new
    @ CompiledFunction.map Expression.cos Angle.cos Bounds.cos function.compiled
    @ \p -> negative (sin function) .*. (derivative p function ./. Angle.radian)

data IsZero = IsZero deriving (Eq, Show, Error.Message)

zeros :: Tolerance units => SurfaceFunction units -> Result IsZero Zeros
zeros function
  | function ~= zero = Failure IsZero
  | otherwise = do
      let fu = function.du
      let fv = function.dv
      -- Using unsafeQuotient should be OK here
      -- since we only actually use dudv and dvdu
      -- in subdomains where we know the denominator is non-zero
      let dudv = unsafeQuotient (negative fv) fu
      let dvdu = unsafeQuotient (negative fu) fv
      case Solve2d.search (findZeros function dudv dvdu) AllZeroTypes of
        Success solutions -> do
          let partialZeros = List.foldl addSolution PartialZeros.empty solutions
          Success (PartialZeros.finalize function dvdu dudv partialZeros)
        Failure Solve2d.InfiniteRecursion -> Exception.higherOrderZero

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
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  SurfaceFunction Unitless ->
  FindZerosContext ->
  Domain2d ->
  Solve2d.Exclusions exclusions ->
  Solve2d.Action exclusions FindZerosContext (Solution units)
findZeros f dudv dvdu context subdomain exclusions = do
  -- TODO find zeros along unit domain boundaries
  -- (including nasty cases like curves emanating from a saddle point
  -- being along a domain boundary)
  let subproblem = Subproblem.new f dudv dvdu subdomain
  if not (Subproblem.isZeroCandidate subproblem)
    then Solve2d.pass
    else case exclusions of
      Solve2d.SomeExclusions -> Solve2d.recurse context
      Solve2d.NoExclusions ->
        case context of
          CrossingCurvesOnly -> findCrossingCurves subproblem
          AllZeroTypes -> do
            let Subproblem{fuBounds, fvBounds} = subproblem
            if Bounds.isResolved fuBounds || Bounds.isResolved fvBounds
              then findCrossingCurves subproblem
              else findTangentSolutions subproblem

findTangentSolutions ::
  Tolerance units =>
  Subproblem units ->
  Solve2d.Action Solve2d.NoExclusions FindZerosContext (Solution units)
findTangentSolutions subproblem = do
  let Subproblem{f, subdomain, uvBounds, fuuBounds, fuvBounds, fvvBounds} = subproblem
  let determinant = fuuBounds #*# fvvBounds .-. fuvBounds #*# fuvBounds
  case Bounds.resolvedSign determinant of
    Resolved determinantSign -> do
      let fu = f.du
      let fv = f.dv
      let fuu = f.du.du
      let fuv = f.du.dv
      let fvv = f.dv.dv
      let maybePoint =
            Solve2d.unique
              (\bounds -> VectorBounds2d (evaluateBounds fu bounds) (evaluateBounds fv bounds))
              (\point -> Vector2d (evaluate fu point) (evaluate fv point))
              (\point -> Vector2d (evaluate fuu point) (evaluate fuv point))
              (\point -> Vector2d (evaluate fuv point) (evaluate fvv point))
              uvBounds
      case maybePoint of
        Nothing -> Solve2d.recurse CrossingCurvesOnly
        Just point ->
          if Bounds2d.includes point (Domain2d.interior subdomain)
            && evaluate f point ~= Quantity.zero
            then case determinantSign of
              Positive -> do
                -- Non-saddle tangent point
                -- Note that fuu and fvv must be either both positive or both negative
                -- to reach this code path, so we can take the sign of either one
                -- to determine the sign of the tangent point
                let sign = Quantity.sign (Bounds.lower fuuBounds)
                Solve2d.return (TangentPointSolution (point, sign))
              Negative -> do
                -- Saddle region
                let saddleRegion = SaddleRegion.quadratic subproblem point
                Solve2d.return (SaddleRegionSolution saddleRegion)
            else do
              Solve2d.recurse CrossingCurvesOnly
    Unresolved -> do
      -- TODO check for tangent curves
      Solve2d.recurse AllZeroTypes

findCrossingCurves ::
  Tolerance units =>
  Subproblem units ->
  Solve2d.Action Solve2d.NoExclusions FindZerosContext (Solution units)
findCrossingCurves subproblem =
  case crossingCurve subproblem of
    Unresolved -> Solve2d.recurse CrossingCurvesOnly
    Resolved Nothing -> Solve2d.pass
    Resolved (Just curve) -> Solve2d.return (CrossingCurveSolution curve)

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
  fuSign <- Bounds.resolvedSign fuBounds
  fvSign <- Bounds.resolvedSign fvBounds
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
  (UvPoint, Domain2d.Boundary) ->
  (UvPoint, Domain2d.Boundary) ->
  PartialZeros.CrossingSegment
diagonalSegment start end = do
  let startPoint = Pair.first start
  let endPoint = Pair.first end
  PartialZeros.diagonalSegment start end (Bounds2d.hull2 startPoint endPoint)

horizontalCrossingCurve ::
  Tolerance units =>
  Subproblem units ->
  Fuzzy (Maybe PartialZeros.CrossingSegment)
horizontalCrossingCurve subproblem = do
  let Subproblem{fvBounds} = subproblem
  if Bounds.isResolved fvBounds
    then do
      let bottomEdgeBounds = Subproblem.bottomEdgeBounds subproblem
      let topEdgeBounds = Subproblem.topEdgeBounds subproblem
      bottomEdgeSign <- Bounds.resolvedSign bottomEdgeBounds
      topEdgeSign <- Bounds.resolvedSign topEdgeBounds
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
  (UvPoint, Domain2d.Boundary) ->
  (UvPoint, Domain2d.Boundary) ->
  Fuzzy PartialZeros.CrossingSegment
horizontalCurve Subproblem{f, dvdu, subdomain, uvBounds} start end = do
  let startPoint = Pair.first start
  let endPoint = Pair.first end
  let uStart = Point2d.xCoordinate startPoint
  let uEnd = Point2d.xCoordinate endPoint
  let curve = HorizontalCurve.new f dvdu uStart uEnd (NonEmpty.one uvBounds)
  let Domain2d _ vSubdomain = subdomain
  let Bounds2d _ curveVBounds = Curve2d.bounds curve
  if Bounds.contains curveVBounds (Domain1d.interior vSubdomain)
    then Resolved (PartialZeros.horizontalSegment start end uvBounds)
    else Unresolved

verticalCrossingCurve ::
  Tolerance units =>
  Subproblem units ->
  Fuzzy (Maybe PartialZeros.CrossingSegment)
verticalCrossingCurve subproblem = do
  let Subproblem{fuBounds} = subproblem
  if Bounds.isResolved fuBounds
    then do
      let leftEdgeBounds = Subproblem.leftEdgeBounds subproblem
      let rightEdgeBounds = Subproblem.rightEdgeBounds subproblem
      leftEdgeSign <- Bounds.resolvedSign leftEdgeBounds
      rightEdgeSign <- Bounds.resolvedSign rightEdgeBounds
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
  (UvPoint, Domain2d.Boundary) ->
  (UvPoint, Domain2d.Boundary) ->
  Fuzzy PartialZeros.CrossingSegment
verticalCurve Subproblem{f, dudv, subdomain, uvBounds} start end = do
  let startPoint = Pair.first start
  let endPoint = Pair.first end
  let vStart = Point2d.yCoordinate startPoint
  let vEnd = Point2d.yCoordinate endPoint
  let curve = VerticalCurve.new f dudv vStart vEnd (NonEmpty.one uvBounds)
  let Domain2d uSubdomain _ = subdomain
  let Bounds2d curveUBounds _ = Curve2d.bounds curve
  if Bounds.contains curveUBounds (Domain1d.interior uSubdomain)
    then Resolved (PartialZeros.verticalSegment start end uvBounds)
    else Unresolved
