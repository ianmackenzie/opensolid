{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Surface1d.Function
  ( Function (Constant)
  , Interface (..)
  , evaluate
  , bounds
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

import Angle qualified
import Bounds2d (Bounds2d (Bounds2d))
import Bounds2d qualified
import Curve1d (Curve1d)
import Curve1d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Direction2d qualified
import Domain1d qualified
import Domain2d (Domain2d (Domain2d))
import Domain2d qualified
import Float qualified
import Fuzzy qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Point2d qualified
import Qty qualified
import Radians qualified
import Range (Range (Range))
import Range qualified
import Solve2d qualified
import Surface1d.Function.HorizontalCurve qualified as HorizontalCurve
import Surface1d.Function.PartialZeros (PartialZeros)
import Surface1d.Function.PartialZeros qualified as PartialZeros
import Surface1d.Function.SaddleRegion (SaddleRegion)
import Surface1d.Function.SaddleRegion qualified as SaddleRegion
import Surface1d.Function.Subproblem (CornerValues (..), Subproblem (..))
import Surface1d.Function.Subproblem qualified as Subproblem
import Surface1d.Function.VerticalCurve qualified as VerticalCurve
import Surface1d.Function.Zeros (Zeros (..))
import Surface1d.Function.Zeros qualified as Zeros
import Units qualified
import Uv (Parameter (U, V))
import Uv qualified
import Uv.Derivatives (Derivatives)
import Uv.Derivatives qualified as Derivatives
import VectorCurve2d qualified

class Show function => Interface function units | function -> units where
  evaluateImpl :: function -> Uv.Point -> Qty units
  boundsImpl :: function -> Uv.Bounds -> Range units
  derivativeImpl :: Parameter -> function -> Function units

data Function units where
  Function ::
    Interface function units =>
    function ->
    Function units
  Constant ::
    Qty units ->
    Function units
  Parameter ::
    Parameter ->
    Function Unitless
  Negated ::
    Function units ->
    Function units
  Sum ::
    Function units ->
    Function units ->
    Function units
  Difference ::
    Function units ->
    Function units ->
    Function units
  Product' ::
    Function units1 ->
    Function units2 ->
    Function (units1 :*: units2)
  Quotient' ::
    Function units1 ->
    Function units2 ->
    Function (units1 :/: units2)
  Squared' ::
    Function units ->
    Function (units :*: units)
  SquareRoot' ::
    Function (units :*: units) ->
    Function units
  Sin ::
    Function Radians ->
    Function Unitless
  Cos ::
    Function Radians ->
    Function Unitless
  Coerce ::
    Function units1 ->
    Function units2

deriving instance Show (Function units)

instance HasUnits (Function units) where
  type Units (Function units) = units
  type Erase (Function units) = Function Unitless

instance Units.Coercion (Function unitsA) (Function unitsB) where
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce function) = Coerce function
  coerce function = Coerce function

instance units1 ~ units2 => ApproximateEquality (Function units1) (Function units2) units1 where
  function1 ~= function2 = function1 - function2 ~= Qty.zero

instance units1 ~ units2 => ApproximateEquality (Function units1) (Qty units2) units1 where
  Constant value1 ~= value2 = value1 ~= value2
  function ~= value = List.allTrue [evaluate function uvPoint ~= value | uvPoint <- Uv.samples]

instance units1 ~ units2 => Intersects (Function units1) (Qty units2) units1 where
  function ^ value =
    -- TODO optimize this to use a special Solve2d.find or similar
    -- to efficiently check if there is *a* zero anywhere
    -- instead of finding *all* zeros (and the full geometry of each)
    case zeros (function - value) of
      Success (Zeros [] [] [] []) -> False
      Success (Zeros{}) -> True
      Failure Zeros.ZeroEverywhere -> True
      Failure Zeros.HigherOrderZero -> True

instance units1 ~ units2 => Intersects (Qty units1) (Function units2) units1 where
  value ^ function = function ^ value

instance Negation (Function units) where
  negate (Constant x) = Constant (negate x)
  negate (Coerce function) = Coerce (negate function)
  negate (Negated function) = function
  negate (Difference f1 f2) = Difference f2 f1
  negate (Product' f1 f2) = negate f1 .*. f2
  negate (Quotient' f1 f2) = negate f1 ./. f2
  negate function = Negated function

instance Multiplication Sign (Function units) (Function units)

instance Multiplication' Sign (Function units) where
  type Sign .*. Function units = Function (Unitless :*: units)
  Positive .*. function = Units.coerce function
  Negative .*. function = Units.coerce -function

instance Multiplication (Function units) Sign (Function units)

instance Multiplication' (Function units) Sign where
  type Function units .*. Sign = Function (units :*: Unitless)
  function .*. Positive = Units.coerce function
  function .*. Negative = Units.coerce -function

instance units ~ units_ => Addition (Function units) (Function units_) (Function units) where
  Constant x + function | x == Qty.zero = function
  function + Constant x | x == Qty.zero = function
  Constant x + Constant y = constant (x + y)
  function1 + function2 = Sum function1 function2

instance units ~ units_ => Addition (Function units) (Qty units_) (Function units) where
  function + value = function + constant value

instance units ~ units_ => Addition (Qty units) (Function units_) (Function units) where
  value + function = constant value + function

instance units ~ units_ => Subtraction (Function units) (Function units_) (Function units) where
  Constant x - function | x == Qty.zero = negate function
  function - Constant x | x == Qty.zero = function
  Constant x - Constant y = constant (x - y)
  function1 - function2 = Difference function1 function2

instance units ~ units_ => Subtraction (Function units) (Qty units_) (Function units) where
  function - value = function - constant value

instance units ~ units_ => Subtraction (Qty units) (Function units_) (Function units) where
  value - function = constant value - function

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function units1) (Function units2) (Function units3)

instance Multiplication' (Function units1) (Function units2) where
  type Function units1 .*. Function units2 = Function (units1 :*: units2)
  Constant x .*. _ | x == Qty.zero = zero
  _ .*. Constant x | x == Qty.zero = zero
  Constant x .*. Constant y = Constant (x .*. y)
  Constant x .*. function | x == Units.coerce 1.0 = Units.coerce function
  Constant x .*. function | x == Units.coerce -1.0 = Units.coerce (negate function)
  Constant x .*. Negated c = negate x .*. c
  f1 .*. (Constant x) = Units.commute (Constant x .*. f1)
  Constant x .*. Product' (Constant y) c = Units.rightAssociate ((x .*. y) .*. c)
  function1 .*. function2 = Product' function1 function2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function units1) (Qty units2) (Function units3)

instance Multiplication' (Function units1) (Qty units2) where
  type Function units1 .*. Qty units2 = Function (units1 :*: units2)
  function .*. value = function .*. constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Function units2) (Function units3)

instance Multiplication' (Qty units1) (Function units2) where
  type Qty units1 .*. Function units2 = Function (units1 :*: units2)
  value .*. function = constant value .*. function

instance Multiplication (Function units) Int (Function units)

instance Multiplication' (Function units) Int where
  type Function units .*. Int = Function (units :*: Unitless)
  function .*. value = function .*. Float.int value

instance Multiplication Int (Function units) (Function units)

instance Multiplication' Int (Function units) where
  type Int .*. Function units = Function (Unitless :*: units)
  value .*. function = Float.int value .*. function

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function units1) (Function units2) (Function units3)

instance Division' (Function units1) (Function units2) where
  type Function units1 ./. Function units2 = Function (units1 :/: units2)
  Constant x ./. _ | x == Qty.zero = zero
  Constant x ./. Constant y = Constant (x ./. y)
  function ./. Constant x = (1 ./. x) .*^ function
  function1 ./. function2 = Quotient' function1 function2

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function units1) (Qty units2) (Function units3)

instance Division' (Function units1) (Qty units2) where
  type Function units1 ./. Qty units2 = Function (units1 :/: units2)
  function ./. value = function ./. constant value

instance Division (Function units) Int (Function units)

instance Division' (Function units) Int where
  type Function units ./. Int = Function (units :/: Unitless)
  function ./. value = function ./. Float.int value

instance
  Units.Quotient units1 units2 units3 =>
  Division (Qty units1) (Function units2) (Function units3)

instance Division' (Qty units1) (Function units2) where
  type Qty units1 ./. Function units2 = Function (units1 :/: units2)
  value ./. function = constant value ./. function

instance
  Units.Quotient Unitless units1 units2 =>
  Division Int (Function units1) (Function units2)

instance Division' Int (Function units) where
  type Int ./. Function units = Function (Unitless :/: units)
  value ./. function = Float.int value ./. function

evaluate :: Function units -> Uv.Point -> Qty units
evaluate function uv = case function of
  Function f -> evaluateImpl f uv
  Constant x -> x
  Coerce f -> Units.coerce (evaluate f uv)
  Parameter U -> Point2d.xCoordinate uv
  Parameter V -> Point2d.yCoordinate uv
  Negated f -> negate (evaluate f uv)
  Sum f1 f2 -> evaluate f1 uv + evaluate f2 uv
  Difference f1 f2 -> evaluate f1 uv - evaluate f2 uv
  Product' f1 f2 -> evaluate f1 uv .*. evaluate f2 uv
  Quotient' f1 f2 -> evaluate f1 uv ./. evaluate f2 uv
  Squared' f -> Qty.squared' (evaluate f uv)
  SquareRoot' f -> Qty.sqrt' (evaluate f uv)
  Sin f -> Angle.sin (evaluate f uv)
  Cos f -> Angle.cos (evaluate f uv)

bounds :: Function units -> Uv.Bounds -> Range units
bounds function uv = case function of
  Function f -> boundsImpl f uv
  Constant x -> Range.constant x
  Coerce f -> Units.coerce (bounds f uv)
  Parameter U -> Bounds2d.xCoordinate uv
  Parameter V -> Bounds2d.yCoordinate uv
  Negated f -> negate (bounds f uv)
  Sum f1 f2 -> bounds f1 uv + bounds f2 uv
  Difference f1 f2 -> bounds f1 uv - bounds f2 uv
  Product' f1 f2 -> bounds f1 uv .*. bounds f2 uv
  Quotient' f1 f2 -> bounds f1 uv ./. bounds f2 uv
  Squared' f -> Range.squared' (bounds f uv)
  SquareRoot' f -> Range.sqrt' (bounds f uv)
  Sin f -> Range.sin (bounds f uv)
  Cos f -> Range.cos (bounds f uv)

derivative :: Parameter -> Function units -> Function units
derivative varyingParameter function =
  case function of
    Function f -> derivativeImpl varyingParameter f
    Constant _ -> zero
    Coerce f -> Units.coerce (derivative varyingParameter f)
    Parameter p -> if p == varyingParameter then constant 1.0 else zero
    Negated f -> negate (derivative varyingParameter f)
    Sum f1 f2 -> derivative varyingParameter f1 + derivative varyingParameter f2
    Difference f1 f2 -> derivative varyingParameter f1 - derivative varyingParameter f2
    Product' f1 f2 -> derivative varyingParameter f1 .*. f2 + f1 .*. derivative varyingParameter f2
    Quotient' f1 f2 ->
      (derivative varyingParameter f1 .*. f2 - f1 .*. derivative varyingParameter f2)
        .!/.! squared' f2
    Squared' f -> 2 * f .*. derivative varyingParameter f
    SquareRoot' f -> derivative varyingParameter f .!/! (2 * sqrt' f)
    Sin f -> cos f * Radians.toUnitless (derivative varyingParameter f)
    Cos f -> negate (sin f) * Radians.toUnitless (derivative varyingParameter f)

derivativeIn :: Uv.Direction -> Function units -> Function units
derivativeIn direction function =
  Direction2d.xComponent direction * derivative U function
    + Direction2d.yComponent direction * derivative V function

zero :: Function units
zero = constant Qty.zero

constant :: Qty units -> Function units
constant = Constant

u :: Function Unitless
u = parameter U

v :: Function Unitless
v = parameter V

parameter :: Parameter -> Function Unitless
parameter = Parameter

new :: Interface function units => function -> Function units
new = Function

squared :: Units.Squared units1 units2 => Function units1 -> Function units2
squared function = Units.specialize (squared' function)

squared' :: Function units -> Function (units :*: units)
squared' (Constant x) = Constant (x .*. x)
squared' (Negated f) = squared' f
squared' (Cos f) = Units.unspecialize (cosSquared f)
squared' (Sin f) = Units.unspecialize (sinSquared f)
squared' function = Squared' function

cosSquared :: Function Radians -> Function Unitless
cosSquared f = 0.5 * cos (2 * f) + 0.5

sinSquared :: Function Radians -> Function Unitless
sinSquared f = 0.5 - 0.5 * cos (2 * f)

sqrt :: Units.Squared units1 units2 => Function units2 -> Function units1
sqrt function = sqrt' (Units.unspecialize function)

sqrt' :: Function (units :*: units) -> Function units
sqrt' (Constant x) = Constant (Qty.sqrt' x)
sqrt' function = SquareRoot' function

sin :: Function Radians -> Function Unitless
sin (Constant x) = constant (Angle.sin x)
sin function = Sin function

cos :: Function Radians -> Function Unitless
cos (Constant x) = constant (Angle.cos x)
cos function = Cos function

data CurveOnSurface units
  = CurveOnSurface (Curve2d Uv.Coordinates) (Function units)
  deriving (Show)

instance Composition (Curve2d Uv.Coordinates) (Function units) (Curve1d units) where
  function . uvCurve = Curve1d.new (CurveOnSurface uvCurve function)

instance Curve1d.Interface (CurveOnSurface units) units where
  pointOnImpl (CurveOnSurface uvCurve function) t =
    evaluate function (Curve2d.pointOnImpl uvCurve t)

  segmentBoundsImpl (CurveOnSurface uvCurve function) t =
    bounds function (Curve2d.segmentBoundsImpl uvCurve t)

  derivativeImpl (CurveOnSurface uvCurve function) = do
    let fU = derivative U function
    let fV = derivative V function
    let uvT = Curve2d.derivative uvCurve
    let uT = VectorCurve2d.xComponent uvT
    let vT = VectorCurve2d.yComponent uvT
    fU . uvCurve * uT + fV . uvCurve * vT

zeros :: Tolerance units => Function units -> Result Zeros.Error Zeros
zeros function
  | function ~= Qty.zero = Failure Zeros.ZeroEverywhere
  | otherwise = Result.do
      let derivatives = Derivatives.init function derivative
      case Solve2d.search (findZeros derivatives) AllZeroTypes of
        Success solutions -> do
          let partialZeros = List.foldl addSolution PartialZeros.empty solutions
          Success (PartialZeros.finalize partialZeros)
        Failure Solve2d.InfiniteRecursion -> Failure Zeros.HigherOrderZero

addSolution :: PartialZeros units -> Solution units -> PartialZeros units
addSolution partialZeros solution = case solution of
  CrossingCurveSolution curve ->
    PartialZeros.addCrossingCurve curve partialZeros
  TangentPointSolution tangentPoint ->
    PartialZeros.addTangentPoint tangentPoint partialZeros
  SaddleRegionSolution saddleRegion ->
    PartialZeros.addSaddleRegion saddleRegion partialZeros

data FindZerosContext = AllZeroTypes | CrossingCurvesOnly deriving (Show)

data Solution units
  = CrossingCurveSolution PartialZeros.CrossingCurve
  | TangentPointSolution (Uv.Point, Sign, Uv.Bounds)
  | SaddleRegionSolution (SaddleRegion units)

findZeros ::
  Tolerance units =>
  Derivatives (Function units) ->
  FindZerosContext ->
  Domain2d ->
  Solve2d.Exclusions exclusions ->
  Solve2d.Action exclusions FindZerosContext (Solution units)
findZeros derivatives context subdomain exclusions = do
  -- TODO find zeros along unit domain boundaries
  -- (including nasty cases like curves emanating from a saddle point
  -- being along a domain boundary)
  let subproblem = Subproblem.new derivatives subdomain
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
      let fu = Derivatives.get (derivatives >> U)
      let fv = Derivatives.get (derivatives >> V)
      let fuu = Derivatives.get (derivatives >> U >> U)
      let fuv = Derivatives.get (derivatives >> U >> V)
      let fvv = Derivatives.get (derivatives >> V >> V)
      let maybePoint =
            Solve2d.unique
              (bounds fu)
              (evaluate fu)
              (evaluate fuu)
              (evaluate fuv)
              (bounds fv)
              (evaluate fv)
              (evaluate fuv)
              (evaluate fvv)
              uvBounds
      case maybePoint of
        Nothing -> Solve2d.recurse CrossingCurvesOnly
        Just point ->
          if Bounds2d.includes point (Domain2d.interior subdomain)
            then case determinantSign of
              Positive -> do
                -- Non-saddle tangent point
                -- Note that fuu and fvv must be either both positive or both negative
                -- to reach this code path, so we can take the sign of either one
                -- to determine the sign of the tangent point
                let sign = Qty.sign (Range.minValue fuuBounds)
                Solve2d.return (NonEmpty.singleton (TangentPointSolution (point, sign, uvBounds)))
              Negative -> do
                -- Saddle region
                let saddleRegion = SaddleRegion.quadratic subproblem point
                Solve2d.return (NonEmpty.singleton (SaddleRegionSolution saddleRegion))
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
    Resolved (Just curve) -> Solve2d.return (NonEmpty.singleton (CrossingCurveSolution curve))

crossingCurve :: Tolerance units => Subproblem units -> Fuzzy (Maybe PartialZeros.CrossingCurve)
crossingCurve subproblem = do
  Fuzzy.oneOf
    [ diagonalCrossingCurve subproblem
    , horizontalCrossingCurve subproblem
    , verticalCrossingCurve subproblem
    ]

diagonalCrossingCurve ::
  Tolerance units =>
  Subproblem units ->
  Fuzzy (Maybe PartialZeros.CrossingCurve)
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

southeastCrossingCurve :: Tolerance units => Subproblem units -> Maybe PartialZeros.CrossingCurve
southeastCrossingCurve subproblem = do
  let Subproblem{derivatives, uvBounds, derivativeValues} = subproblem
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
      Just (diagonalCurve derivatives uvBounds start end)

southwestCrossingCurve :: Tolerance units => Subproblem units -> Maybe PartialZeros.CrossingCurve
southwestCrossingCurve subproblem = do
  let Subproblem{derivatives, uvBounds, derivativeValues} = subproblem
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
      Just (diagonalCurve derivatives uvBounds start end)

northeastCrossingCurve :: Tolerance units => Subproblem units -> Maybe PartialZeros.CrossingCurve
northeastCrossingCurve subproblem = do
  let Subproblem{derivatives, uvBounds, derivativeValues} = subproblem
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
      Just (diagonalCurve derivatives uvBounds start end)

northwestCrossingCurve :: Tolerance units => Subproblem units -> Maybe PartialZeros.CrossingCurve
northwestCrossingCurve subproblem = do
  let Subproblem{derivatives, uvBounds, derivativeValues} = subproblem
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
      Just (diagonalCurve derivatives uvBounds start end)

diagonalCurve ::
  Tolerance units =>
  Derivatives (Function units) ->
  Uv.Bounds ->
  (Uv.Point, Domain2d.Boundary) ->
  (Uv.Point, Domain2d.Boundary) ->
  PartialZeros.CrossingCurve
diagonalCurve derivatives uvBounds start end = do
  let (Point2d uStart vStart, startBoundary) = start
  let (Point2d uEnd vEnd, endBoundary) = end
  PartialZeros.crossingCurve startBoundary endBoundary uvBounds $
    if Float.abs (uEnd - uStart) >= Float.abs (vEnd - vStart)
      then HorizontalCurve.monotonic derivatives uStart uEnd (Range vStart vEnd)
      else VerticalCurve.monotonic derivatives (Range uStart uEnd) vStart vEnd

horizontalCrossingCurve ::
  Tolerance units =>
  Subproblem units ->
  Fuzzy (Maybe PartialZeros.CrossingCurve)
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
        (Negative, Positive) ->
          Fuzzy.map Just (westCrossingCurve subproblem)
        (Positive, Negative) ->
          Fuzzy.map Just (eastCrossingCurve subproblem)
    else Unresolved

eastCrossingCurve :: Tolerance units => Subproblem units -> Fuzzy PartialZeros.CrossingCurve
eastCrossingCurve subproblem = do
  let start = Subproblem.leftEdgePoint subproblem
  let end = Subproblem.rightEdgePoint subproblem
  horizontalCurve subproblem start end

westCrossingCurve :: Tolerance units => Subproblem units -> Fuzzy PartialZeros.CrossingCurve
westCrossingCurve subproblem = do
  let start = Subproblem.rightEdgePoint subproblem
  let end = Subproblem.leftEdgePoint subproblem
  horizontalCurve subproblem start end

horizontalCurve ::
  Tolerance units =>
  Subproblem units ->
  (Uv.Point, Domain2d.Boundary) ->
  (Uv.Point, Domain2d.Boundary) ->
  Fuzzy PartialZeros.CrossingCurve
horizontalCurve Subproblem{derivatives, subdomain, uvBounds} start end = do
  let (Point2d uStart _, startBoundary) = start
  let (Point2d uEnd _, endBoundary) = end
  let Bounds2d _ vBounds = uvBounds
  let curve = HorizontalCurve.new derivatives uStart uEnd vBounds
  let Domain2d _ vSubdomain = subdomain
  let Bounds2d _ curveVBounds = Curve2d.bounds curve
  if Range.contains curveVBounds (Domain1d.interior vSubdomain)
    then Resolved (PartialZeros.crossingCurve startBoundary endBoundary uvBounds curve)
    else Unresolved

verticalCrossingCurve ::
  Tolerance units =>
  Subproblem units ->
  Fuzzy (Maybe PartialZeros.CrossingCurve)
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
        (Negative, Positive) ->
          Fuzzy.map Just (northCrossingCurve subproblem)
        (Positive, Negative) ->
          Fuzzy.map Just (southCrossingCurve subproblem)
    else Unresolved

southCrossingCurve :: Tolerance units => Subproblem units -> Fuzzy PartialZeros.CrossingCurve
southCrossingCurve subproblem = Fuzzy.do
  let start = Subproblem.topEdgePoint subproblem
  let end = Subproblem.bottomEdgePoint subproblem
  verticalCurve subproblem start end

northCrossingCurve :: Tolerance units => Subproblem units -> Fuzzy PartialZeros.CrossingCurve
northCrossingCurve subproblem = Fuzzy.do
  let start = Subproblem.bottomEdgePoint subproblem
  let end = Subproblem.topEdgePoint subproblem
  verticalCurve subproblem start end

verticalCurve ::
  Tolerance units =>
  Subproblem units ->
  (Uv.Point, Domain2d.Boundary) ->
  (Uv.Point, Domain2d.Boundary) ->
  Fuzzy PartialZeros.CrossingCurve
verticalCurve Subproblem{derivatives, subdomain, uvBounds} start end = do
  let (Point2d _ vStart, startBoundary) = start
  let (Point2d _ vEnd, endBoundary) = end
  let curve = VerticalCurve.new derivatives (Bounds2d.xCoordinate uvBounds) vStart vEnd
  let Domain2d uSubdomain _ = subdomain
  let Bounds2d curveUBounds _ = Curve2d.bounds curve
  if Range.contains curveUBounds (Domain1d.interior uSubdomain)
    then Resolved (PartialZeros.crossingCurve startBoundary endBoundary uvBounds curve)
    else Unresolved
