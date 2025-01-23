{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module OpenSolid.SurfaceFunction
  ( SurfaceFunction (Parametric)
  , Interface (..)
  , evaluate
  , evaluateBounds
  , derivative
  , derivativeIn
  , zero
  , constant
  , u
  , v
  , parameter
  , Zeros
  , zeros
  , new
  , squared
  , squared'
  , sqrt
  , sqrt'
  , sin
  , cos
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Composition
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import {-# SOURCE #-} OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Domain1d qualified as Domain1d
import OpenSolid.Domain2d (Domain2d (Domain2d))
import OpenSolid.Domain2d qualified as Domain2d
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Fuzzy qualified as Fuzzy
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.Solve2d qualified as Solve2d
import {-# SOURCE #-} OpenSolid.SurfaceFunction.HorizontalCurve qualified as HorizontalCurve
import OpenSolid.SurfaceFunction.PartialZeros (PartialZeros)
import OpenSolid.SurfaceFunction.PartialZeros qualified as PartialZeros
import OpenSolid.SurfaceFunction.SaddleRegion (SaddleRegion)
import OpenSolid.SurfaceFunction.SaddleRegion qualified as SaddleRegion
import OpenSolid.SurfaceFunction.Subproblem (CornerValues (..), Subproblem (..))
import OpenSolid.SurfaceFunction.Subproblem qualified as Subproblem
import {-# SOURCE #-} OpenSolid.SurfaceFunction.VerticalCurve qualified as VerticalCurve
import OpenSolid.SurfaceFunction.Zeros (Zeros (..))
import OpenSolid.SurfaceFunction.Zeros qualified as Zeros
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvDirection, UvPoint)
import OpenSolid.SurfaceParameter qualified as SurfaceParameter
import OpenSolid.Units (Radians)
import OpenSolid.Units qualified as Units
import OpenSolid.Uv.Derivatives (Derivatives)
import OpenSolid.Uv.Derivatives qualified as Derivatives
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d

data SurfaceFunction units where
  SurfaceFunction ::
    Interface function units =>
    function ->
    SurfaceFunction units
  Parametric ::
    Expression UvPoint (Qty units) ->
    SurfaceFunction units
  Negated ::
    SurfaceFunction units ->
    SurfaceFunction units
  Sum ::
    SurfaceFunction units ->
    SurfaceFunction units ->
    SurfaceFunction units
  Difference ::
    SurfaceFunction units ->
    SurfaceFunction units ->
    SurfaceFunction units
  Product' ::
    SurfaceFunction units1 ->
    SurfaceFunction units2 ->
    SurfaceFunction (units1 :*: units2)
  Quotient' ::
    SurfaceFunction units1 ->
    SurfaceFunction units2 ->
    SurfaceFunction (units1 :/: units2)
  Squared' ::
    SurfaceFunction units ->
    SurfaceFunction (units :*: units)
  SquareRoot' ::
    SurfaceFunction (units :*: units) ->
    SurfaceFunction units
  Sin ::
    SurfaceFunction Radians ->
    SurfaceFunction Unitless
  Cos ::
    SurfaceFunction Radians ->
    SurfaceFunction Unitless
  Coerce ::
    SurfaceFunction units1 ->
    SurfaceFunction units2

deriving instance Show (SurfaceFunction units)

class
  Show function =>
  Interface function units
    | function -> units
  where
  evaluateImpl :: function -> UvPoint -> Qty units
  evaluateBoundsImpl :: function -> UvBounds -> Range units
  derivativeImpl :: SurfaceParameter -> function -> SurfaceFunction units

instance HasUnits (SurfaceFunction units) units (SurfaceFunction Unitless)

instance Units.Coercion (SurfaceFunction unitsA) (SurfaceFunction unitsB) where
  coerce (Parametric expression) = Parametric (Units.coerce expression)
  coerce (Coerce function) = Coerce function
  coerce function = Coerce function

instance
  units1 ~ units2 =>
  ApproximateEquality (SurfaceFunction units1) (SurfaceFunction units2) units1
  where
  function1 ~= function2 = function1 - function2 ~= Qty.zero

instance
  units1 ~ units2 =>
  ApproximateEquality (SurfaceFunction units1) (Qty units2) units1
  where
  function ~= value =
    List.allTrue [evaluate function uvPoint ~= value | uvPoint <- SurfaceParameter.samples]

instance
  units1 ~ units2 =>
  Intersects (SurfaceFunction units1) (Qty units2) units1
  where
  function ^ value =
    -- TODO optimize this to use a special Solve2d.find or similar
    -- to efficiently check if there is *a* zero anywhere
    -- instead of finding *all* zeros (and the full geometry of each)
    case zeros (function - value) of
      Success (Zeros [] [] [] []) -> False
      Success (Zeros{}) -> True
      Failure Zeros.ZeroEverywhere -> True
      Failure Zeros.HigherOrderZero -> True

instance
  units1 ~ units2 =>
  Intersects (Qty units1) (SurfaceFunction units2) units1
  where
  value ^ function = function ^ value

instance Negation (SurfaceFunction units) where
  negate (Parametric expression) = Parametric (negate expression)
  negate (Coerce function) = Coerce (negate function)
  negate (Negated function) = function
  negate (Difference f1 f2) = Difference f2 f1
  negate (Product' f1 f2) = negate f1 .*. f2
  negate (Quotient' f1 f2) = negate f1 ./. f2
  negate function = Negated function

instance Multiplication Sign (SurfaceFunction units) (SurfaceFunction units) where
  Positive * function = function
  Negative * function = -function

instance Multiplication' Sign (SurfaceFunction units) (SurfaceFunction (Unitless :*: units)) where
  Positive .*. function = Units.coerce function
  Negative .*. function = Units.coerce -function

instance Multiplication (SurfaceFunction units) Sign (SurfaceFunction units) where
  function * Positive = function
  function * Negative = -function

instance Multiplication' (SurfaceFunction units) Sign (SurfaceFunction (units :*: Unitless)) where
  function .*. Positive = Units.coerce function
  function .*. Negative = Units.coerce -function

instance
  units ~ units_ =>
  Addition
    (SurfaceFunction units)
    (SurfaceFunction units_)
    (SurfaceFunction units)
  where
  Parametric lhs + Parametric rhs = Parametric (lhs + rhs)
  function1 + function2 = Sum function1 function2

instance
  units ~ units_ =>
  Addition
    (SurfaceFunction units)
    (Qty units_)
    (SurfaceFunction units)
  where
  function + value = function + constant value

instance
  units ~ units_ =>
  Addition
    (Qty units)
    (SurfaceFunction units_)
    (SurfaceFunction units)
  where
  value + function = constant value + function

instance
  units1 ~ units2 =>
  Subtraction (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units1)
  where
  Parametric lhs - Parametric rhs = Parametric (lhs - rhs)
  function1 - function2 = Difference function1 function2

instance
  units1 ~ units2 =>
  Subtraction (SurfaceFunction units1) (Qty units2) (SurfaceFunction units1)
  where
  function - value = function - constant value

instance
  units1 ~ units2 =>
  Subtraction (Qty units1) (SurfaceFunction units2) (SurfaceFunction units1)
  where
  value - function = constant value - function

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units3)
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (SurfaceFunction units1)
    (SurfaceFunction units2)
    (SurfaceFunction (units1 :*: units2))
  where
  Parametric lhs .*. Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product' lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (SurfaceFunction units1) (Qty units2) (SurfaceFunction units3)
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (SurfaceFunction units1)
    (Qty units2)
    (SurfaceFunction (units1 :*: units2))
  where
  function .*. value = function .*. constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (SurfaceFunction units2) (SurfaceFunction units3)
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Qty units1)
    (SurfaceFunction units2)
    (SurfaceFunction (units1 :*: units2))
  where
  value .*. function = constant value .*. function

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (Vector2d (space @ units2))
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (SurfaceFunction units1)
    (Vector2d (space @ units2))
    (VectorSurfaceFunction2d (space @ (units1 :*: units2)))
  where
  function .*. vector = function .*. VectorSurfaceFunction2d.constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector2d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Vector2d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction2d (space @ (units1 :*: units2)))
  where
  vector .*. function = VectorSurfaceFunction2d.constant vector .*. function

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (SurfaceFunction units1)
    (Vector3d (space @ units2))
    (VectorSurfaceFunction3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (SurfaceFunction units1)
    (Vector3d (space @ units2))
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  function .*. vector = function .*. VectorSurfaceFunction3d.constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Vector3d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Vector3d (space @ units1))
    (SurfaceFunction units2)
    (VectorSurfaceFunction3d (space @ (units1 :*: units2)))
  where
  vector .*. function = VectorSurfaceFunction3d.constant vector .*. function

instance
  Units.Quotient units1 units2 units3 =>
  Division (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units3)
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (SurfaceFunction units1)
    (SurfaceFunction units2)
    (SurfaceFunction (units1 :/: units2))
  where
  Parametric lhs ./. Parametric rhs = Parametric (lhs ./. rhs)
  lhs ./. rhs = Quotient' lhs rhs

instance
  Units.Quotient units1 units2 units3 =>
  Division (SurfaceFunction units1) (Qty units2) (SurfaceFunction units3)
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (SurfaceFunction units1)
    (Qty units2)
    (SurfaceFunction (units1 :/: units2))
  where
  function ./. value = function ./. constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (SurfaceFunction units2) (SurfaceFunction units3)
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (Qty units1)
    (SurfaceFunction units2)
    (SurfaceFunction (units1 :/: units2))
  where
  value ./. function = constant value ./. function

instance Composition (SurfaceFunction Unitless) (Curve units) (SurfaceFunction units) where
  Curve.Parametric outer . Parametric inner = Parametric (outer . inner)
  outer . inner = new (outer :.: inner)

instance Interface (Curve units :.: SurfaceFunction Unitless) units where
  evaluateImpl (curve :.: function) uvPoint =
    Curve.evaluate curve (evaluate function uvPoint)

  evaluateBoundsImpl (curve :.: function) uvPoint =
    Curve.evaluateBounds curve (evaluateBounds function uvPoint)

  derivativeImpl varyingParameter (curve :.: function) =
    Curve.derivative curve . function * derivative varyingParameter function

evaluate :: SurfaceFunction units -> UvPoint -> Qty units
evaluate function uv = case function of
  SurfaceFunction f -> evaluateImpl f uv
  Parametric x -> Expression.evaluate x uv
  Coerce f -> Units.coerce (evaluate f uv)
  Negated f -> negate (evaluate f uv)
  Sum f1 f2 -> evaluate f1 uv + evaluate f2 uv
  Difference f1 f2 -> evaluate f1 uv - evaluate f2 uv
  Product' f1 f2 -> evaluate f1 uv .*. evaluate f2 uv
  Quotient' f1 f2 -> evaluate f1 uv ./. evaluate f2 uv
  Squared' f -> Qty.squared' (evaluate f uv)
  SquareRoot' f -> Qty.sqrt' (evaluate f uv)
  Sin f -> Angle.sin (evaluate f uv)
  Cos f -> Angle.cos (evaluate f uv)

evaluateBounds :: SurfaceFunction units -> UvBounds -> Range units
evaluateBounds function uv = case function of
  SurfaceFunction f -> evaluateBoundsImpl f uv
  Parametric expression -> Expression.evaluateBounds expression uv
  Coerce f -> Units.coerce (evaluateBounds f uv)
  Negated f -> negate (evaluateBounds f uv)
  Sum f1 f2 -> evaluateBounds f1 uv + evaluateBounds f2 uv
  Difference f1 f2 -> evaluateBounds f1 uv - evaluateBounds f2 uv
  Product' f1 f2 -> evaluateBounds f1 uv .*. evaluateBounds f2 uv
  Quotient' f1 f2 -> evaluateBounds f1 uv ./. evaluateBounds f2 uv
  Squared' f -> Range.squared' (evaluateBounds f uv)
  SquareRoot' f -> Range.sqrt' (evaluateBounds f uv)
  Sin f -> Range.sin (evaluateBounds f uv)
  Cos f -> Range.cos (evaluateBounds f uv)

derivative :: SurfaceParameter -> SurfaceFunction units -> SurfaceFunction units
derivative varyingParameter function = case function of
  SurfaceFunction f -> derivativeImpl varyingParameter f
  Parametric expression -> Parametric (Expression.surfaceDerivative varyingParameter expression)
  Coerce f -> Units.coerce (derivative varyingParameter f)
  Negated f -> negate (derivative varyingParameter f)
  Sum f1 f2 -> derivative varyingParameter f1 + derivative varyingParameter f2
  Difference f1 f2 -> derivative varyingParameter f1 - derivative varyingParameter f2
  Product' f1 f2 -> derivative varyingParameter f1 .*. f2 + f1 .*. derivative varyingParameter f2
  Quotient' f1 f2 ->
    (derivative varyingParameter f1 .*. f2 - f1 .*. derivative varyingParameter f2)
      .!/.! squared' f2
  Squared' f -> 2.0 * f .*. derivative varyingParameter f
  SquareRoot' f -> derivative varyingParameter f .!/! (2.0 * sqrt' f)
  Sin f -> cos f * (derivative varyingParameter f / Angle.radian)
  Cos f -> negate (sin f) * (derivative varyingParameter f / Angle.radian)

derivativeIn :: UvDirection -> SurfaceFunction units -> SurfaceFunction units
derivativeIn direction function =
  Direction2d.xComponent direction * derivative U function
    + Direction2d.yComponent direction * derivative V function

zero :: SurfaceFunction units
zero = constant Qty.zero

constant :: Qty units -> SurfaceFunction units
constant = Parametric . Expression.constant

u :: SurfaceFunction Unitless
u = Parametric Expression.u

v :: SurfaceFunction Unitless
v = Parametric Expression.v

parameter :: SurfaceParameter -> SurfaceFunction Unitless
parameter U = u
parameter V = v

new :: Interface function units => function -> SurfaceFunction units
new = SurfaceFunction

squared :: Units.Squared units1 units2 => SurfaceFunction units1 -> SurfaceFunction units2
squared function = Units.specialize (squared' function)

squared' :: SurfaceFunction units -> SurfaceFunction (units :*: units)
squared' (Parametric expression) = Parametric (Expression.squared' expression)
squared' (Negated f) = squared' f
squared' function = Squared' function

sqrt :: Units.Squared units1 units2 => SurfaceFunction units2 -> SurfaceFunction units1
sqrt function = sqrt' (Units.unspecialize function)

sqrt' :: SurfaceFunction (units :*: units) -> SurfaceFunction units
sqrt' (Parametric expression) = Parametric (Expression.sqrt' expression)
sqrt' function = SquareRoot' function

sin :: SurfaceFunction Radians -> SurfaceFunction Unitless
sin (Parametric expression) = Parametric (Expression.sin expression)
sin function = Sin function

cos :: SurfaceFunction Radians -> SurfaceFunction Unitless
cos (Parametric expression) = Parametric (Expression.cos expression)
cos function = Cos function

zeros :: Tolerance units => SurfaceFunction units -> Result Zeros.Error Zeros
zeros function
  | function ~= Qty.zero = Failure Zeros.ZeroEverywhere
  | otherwise = Result.do
      let derivatives = Derivatives.init function derivative
      let fu = Derivatives.get (derivatives >> U)
      let fv = Derivatives.get (derivatives >> V)
      let dudv = -fv / fu
      let dvdu = -fu / fv
      case Solve2d.search (findZeros derivatives dudv dvdu) AllZeroTypes of
        Success solutions -> do
          let partialZeros = List.foldl addSolution PartialZeros.empty solutions
          Success $
            PartialZeros.finalize
              (HorizontalCurve.new derivatives dvdu)
              (VerticalCurve.new derivatives dudv)
              partialZeros
        Failure Solve2d.InfiniteRecursion -> Failure Zeros.HigherOrderZero

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
  Derivatives (SurfaceFunction units) ->
  SurfaceFunction Unitless ->
  SurfaceFunction Unitless ->
  FindZerosContext ->
  Domain2d ->
  Solve2d.Exclusions exclusions ->
  Solve2d.Action exclusions FindZerosContext (Solution units)
findZeros derivatives dudv dvdu context subdomain exclusions = do
  -- TODO find zeros along unit domain boundaries
  -- (including nasty cases like curves emanating from a saddle point
  -- being along a domain boundary)
  let subproblem = Subproblem.new derivatives dudv dvdu subdomain
  if not (Subproblem.isZeroCandidate subproblem)
    then Solve2d.pass
    else case exclusions of
      Solve2d.SomeExclusions -> Solve2d.recurse context
      Solve2d.NoExclusions ->
        case context of
          CrossingCurvesOnly -> findCrossingCurves subproblem
          AllZeroTypes -> do
            let Subproblem{derivativeBounds} = subproblem
            let fuBounds = Derivatives.get (derivativeBounds >> U)
            let fvBounds = Derivatives.get (derivativeBounds >> V)
            if Range.isResolved fuBounds || Range.isResolved fvBounds
              then findCrossingCurves subproblem
              else findTangentSolutions subproblem

findTangentSolutions ::
  Tolerance units =>
  Subproblem units ->
  Solve2d.Action Solve2d.NoExclusions FindZerosContext (Solution units)
findTangentSolutions subproblem = do
  let Subproblem{derivatives, subdomain, uvBounds, derivativeBounds} = subproblem
  let fuuBounds = Derivatives.get (derivativeBounds >> U >> U)
  let fuvBounds = Derivatives.get (derivativeBounds >> U >> V)
  let fvvBounds = Derivatives.get (derivativeBounds >> V >> V)
  let determinant = fuuBounds .*. fvvBounds - fuvBounds .*. fuvBounds
  case Range.resolvedSign determinant of
    Resolved determinantSign -> do
      let f = Derivatives.get derivatives
      let fu = Derivatives.get (derivatives >> U)
      let fv = Derivatives.get (derivatives >> V)
      let fuu = Derivatives.get (derivatives >> U >> U)
      let fuv = Derivatives.get (derivatives >> U >> V)
      let fvv = Derivatives.get (derivatives >> V >> V)
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
          if Bounds2d.includes point (Domain2d.interior subdomain) && evaluate f point ~= Qty.zero
            then case determinantSign of
              Positive -> do
                -- Non-saddle tangent point
                -- Note that fuu and fvv must be either both positive or both negative
                -- to reach this code path, so we can take the sign of either one
                -- to determine the sign of the tangent point
                let sign = Qty.sign (Range.lowerBound fuuBounds)
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
diagonalCrossingCurve subproblem = Fuzzy.do
  let Subproblem{derivativeBounds} = subproblem
  let fuBounds = Derivatives.get (derivativeBounds >> U)
  let fvBounds = Derivatives.get (derivativeBounds >> V)
  fuSign <- Range.resolvedSign fuBounds
  fvSign <- Range.resolvedSign fvBounds
  Resolved $
    case (fuSign, fvSign) of
      (Negative, Negative) -> southeastCrossingCurve subproblem
      (Negative, Positive) -> southwestCrossingCurve subproblem
      (Positive, Negative) -> northeastCrossingCurve subproblem
      (Positive, Positive) -> northwestCrossingCurve subproblem

southeastCrossingCurve :: Tolerance units => Subproblem units -> Maybe PartialZeros.CrossingSegment
southeastCrossingCurve subproblem = do
  let Subproblem{derivativeValues} = subproblem
  let fValues = Derivatives.get derivativeValues
  let CornerValues{bottomLeft = f11, bottomRight = f21, topLeft = f12, topRight = f22} = fValues
  if f11 <= Qty.zero || f22 >= Qty.zero
    then Nothing
    else do
      let start = case compare f12 Qty.zero of
            LT -> Subproblem.leftEdgePoint subproblem
            EQ -> Subproblem.topLeftPoint subproblem
            GT -> Subproblem.topEdgePoint subproblem
      let end = case compare f21 Qty.zero of
            LT -> Subproblem.bottomEdgePoint subproblem
            EQ -> Subproblem.bottomRightPoint subproblem
            GT -> Subproblem.rightEdgePoint subproblem
      Just (diagonalSegment start end)

southwestCrossingCurve :: Tolerance units => Subproblem units -> Maybe PartialZeros.CrossingSegment
southwestCrossingCurve subproblem = do
  let Subproblem{derivativeValues} = subproblem
  let fValues = Derivatives.get derivativeValues
  let CornerValues{bottomLeft = f11, bottomRight = f21, topLeft = f12, topRight = f22} = fValues
  if f12 <= Qty.zero || f21 >= Qty.zero
    then Nothing
    else do
      let start = case compare f22 Qty.zero of
            LT -> Subproblem.topEdgePoint subproblem
            EQ -> Subproblem.topRightPoint subproblem
            GT -> Subproblem.rightEdgePoint subproblem
      let end = case compare f11 Qty.zero of
            LT -> Subproblem.leftEdgePoint subproblem
            EQ -> Subproblem.bottomLeftPoint subproblem
            GT -> Subproblem.bottomEdgePoint subproblem
      Just (diagonalSegment start end)

northeastCrossingCurve :: Tolerance units => Subproblem units -> Maybe PartialZeros.CrossingSegment
northeastCrossingCurve subproblem = do
  let Subproblem{derivativeValues} = subproblem
  let fValues = Derivatives.get derivativeValues
  let CornerValues{bottomLeft = f11, bottomRight = f21, topLeft = f12, topRight = f22} = fValues
  if f21 <= Qty.zero || f12 >= Qty.zero
    then Nothing
    else do
      let start = case compare f11 Qty.zero of
            LT -> Subproblem.bottomEdgePoint subproblem
            EQ -> Subproblem.bottomLeftPoint subproblem
            GT -> Subproblem.leftEdgePoint subproblem
      let end = case compare f22 Qty.zero of
            LT -> Subproblem.rightEdgePoint subproblem
            EQ -> Subproblem.topRightPoint subproblem
            GT -> Subproblem.topEdgePoint subproblem
      Just (diagonalSegment start end)

northwestCrossingCurve :: Tolerance units => Subproblem units -> Maybe PartialZeros.CrossingSegment
northwestCrossingCurve subproblem = do
  let Subproblem{derivativeValues} = subproblem
  let fValues = Derivatives.get derivativeValues
  let CornerValues{bottomLeft = f11, bottomRight = f21, topLeft = f12, topRight = f22} = fValues
  if f22 <= Qty.zero || f11 >= Qty.zero
    then Nothing
    else do
      let start = case compare f21 Qty.zero of
            LT -> Subproblem.rightEdgePoint subproblem
            EQ -> Subproblem.bottomRightPoint subproblem
            GT -> Subproblem.bottomEdgePoint subproblem
      let end = case compare f12 Qty.zero of
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
horizontalCrossingCurve subproblem = Fuzzy.do
  let Subproblem{derivativeBounds} = subproblem
  let fvBounds = Derivatives.get (derivativeBounds >> V)
  if Range.isResolved fvBounds
    then Fuzzy.do
      let bottomEdgeBounds = Subproblem.bottomEdgeBounds subproblem
      let topEdgeBounds = Subproblem.topEdgeBounds subproblem
      bottomEdgeSign <- Range.resolvedSign bottomEdgeBounds
      topEdgeSign <- Range.resolvedSign topEdgeBounds
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
horizontalCurve Subproblem{derivatives, dvdu, subdomain, uvBounds} start end = do
  let startPoint = Pair.first start
  let endPoint = Pair.first end
  let uStart = Point2d.xCoordinate startPoint
  let uEnd = Point2d.xCoordinate endPoint
  let curve = HorizontalCurve.new derivatives dvdu uStart uEnd (NonEmpty.one uvBounds)
  let Domain2d _ vSubdomain = subdomain
  let Bounds2d _ curveVBounds = Curve2d.bounds curve
  if Range.contains curveVBounds (Domain1d.interior vSubdomain)
    then Resolved (PartialZeros.horizontalSegment start end uvBounds)
    else Unresolved

verticalCrossingCurve ::
  Tolerance units =>
  Subproblem units ->
  Fuzzy (Maybe PartialZeros.CrossingSegment)
verticalCrossingCurve subproblem = Fuzzy.do
  let Subproblem{derivativeBounds} = subproblem
  let fuBounds = Derivatives.get (derivativeBounds >> U)
  if Range.isResolved fuBounds
    then Fuzzy.do
      let leftEdgeBounds = Subproblem.leftEdgeBounds subproblem
      let rightEdgeBounds = Subproblem.rightEdgeBounds subproblem
      leftEdgeSign <- Range.resolvedSign leftEdgeBounds
      rightEdgeSign <- Range.resolvedSign rightEdgeBounds
      case (leftEdgeSign, rightEdgeSign) of
        (Negative, Negative) -> Resolved Nothing
        (Positive, Positive) -> Resolved Nothing
        (Negative, Positive) -> Fuzzy.map Just (northCrossingCurve subproblem)
        (Positive, Negative) -> Fuzzy.map Just (southCrossingCurve subproblem)
    else Unresolved

southCrossingCurve :: Tolerance units => Subproblem units -> Fuzzy PartialZeros.CrossingSegment
southCrossingCurve subproblem = Fuzzy.do
  let start = Subproblem.topEdgePoint subproblem
  let end = Subproblem.bottomEdgePoint subproblem
  verticalCurve subproblem start end

northCrossingCurve :: Tolerance units => Subproblem units -> Fuzzy PartialZeros.CrossingSegment
northCrossingCurve subproblem = Fuzzy.do
  let start = Subproblem.bottomEdgePoint subproblem
  let end = Subproblem.topEdgePoint subproblem
  verticalCurve subproblem start end

verticalCurve ::
  Tolerance units =>
  Subproblem units ->
  (UvPoint, Domain2d.Boundary) ->
  (UvPoint, Domain2d.Boundary) ->
  Fuzzy PartialZeros.CrossingSegment
verticalCurve Subproblem{derivatives, dudv, subdomain, uvBounds} start end = do
  let startPoint = Pair.first start
  let endPoint = Pair.first end
  let vStart = Point2d.yCoordinate startPoint
  let vEnd = Point2d.yCoordinate endPoint
  let curve = VerticalCurve.new derivatives dudv vStart vEnd (NonEmpty.one uvBounds)
  let Domain2d uSubdomain _ = subdomain
  let Bounds2d curveUBounds _ = Curve2d.bounds curve
  if Range.contains curveUBounds (Domain1d.interior uSubdomain)
    then Resolved (PartialZeros.verticalSegment start end uvBounds)
    else Unresolved
