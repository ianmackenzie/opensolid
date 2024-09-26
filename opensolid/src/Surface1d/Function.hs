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
  , toAst
  , unwrap
  )
where

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
import Jit qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Point2d qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import Solve2d qualified
import Surface1d.Function.Expression (Expression (Expression))
import Surface1d.Function.Expression qualified as Expression
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
import Text qualified
import Units qualified
import Uv (Parameter (U, V))
import Uv qualified
import Uv.Derivatives (Derivatives)
import Uv.Derivatives qualified as Derivatives
import VectorCurve2d qualified
import Prelude qualified

data Function units = Function (Expression units) ~(Uv.Point -> Qty units)

instance Show (Function units) where show _ = Text.unpack "<Surface1d.Function>" -- TODO

pattern Constant :: Qty units -> Function units
pattern Constant value <- Function (Expression.Constant value) _

build :: Expression units -> Function units
build expression = do
  let evaluator = Units.coerce . Jit.compile (Expression.toAst expression)
  Function expression evaluator

unwrap :: Function units -> Expression units
unwrap (Function expression _) = expression

instance Eq (Function units) where
  Function expression1 _ == Function expression2 _ = expression1 == expression2

class Known function => Interface function units | function -> units where
  evaluateImpl :: function -> Uv.Point -> Qty units
  boundsImpl :: function -> Uv.Bounds -> Range units
  derivativeImpl :: Parameter -> function -> Function units
  toAstImpl :: function -> Jit.Ast Uv.Point Float

instance HasUnits (Function units) where
  type UnitsOf (Function units) = units

instance (Known unitsA, Known unitsB) => Units.Coercion (Function unitsA) (Function unitsB) where
  coerce (Function expression _) = build (Units.coerce expression)

instance
  (Known units1, Known units2, units1 ~ units2) =>
  ApproximateEquality (Function units1) (Function units2) units1
  where
  function1 ~= function2 = function1 - function2 ~= Qty.zero

instance
  (Known units1, Known units2, units1 ~ units2) =>
  ApproximateEquality (Function units1) (Qty units2) units1
  where
  Function (Expression.Constant value1) _ ~= value2 = value1 ~= value2
  function ~= value = List.allTrue [evaluate function uvPoint ~= value | uvPoint <- Uv.samples]

instance
  (Known units1, Known units2, units1 ~ units2) =>
  Intersects (Function units1) (Qty units2) units1
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
  (Known units1, Known units2, units1 ~ units2) =>
  Intersects (Qty units1) (Function units2) units1
  where
  value ^ function = function ^ value

instance Known units => Negation (Function units) where
  negate (Function expression _) = build (negate expression)

instance Known units => Multiplication Sign (Function units) (Function units)

instance Known units => Multiplication' Sign (Function units) where
  type Sign .*. Function units = Function (Unitless :*: units)
  sign .*. (Function expression _) = build (sign .*. expression)

instance Known units => Multiplication (Function units) Sign (Function units)

instance Known units => Multiplication' (Function units) Sign where
  type Function units .*. Sign = Function (units :*: Unitless)
  (Function expression _) .*. sign = build (expression .*. sign)

instance units ~ units_ => Addition (Function units) (Function units_) (Function units) where
  Function expression1 _ + Function expression2 _ = build (expression1 + expression2)

instance units ~ units_ => Addition (Function units) (Qty units_) (Function units) where
  Function expression _ + value = build (expression + value)

instance units ~ units_ => Addition (Qty units) (Function units_) (Function units) where
  value + Function expression _ = build (value + expression)

instance
  (Known units1, Known units2, units1 ~ units2) =>
  Subtraction (Function units1) (Function units2) (Function units1)
  where
  Function expression1 _ - Function expression2 _ = build (expression1 - expression2)

instance
  (Known units1, Known units2, units1 ~ units2) =>
  Subtraction (Function units1) (Qty units2) (Function units1)
  where
  Function expression _ - value = build (expression - value)

instance
  (Known units1, Known units2, units1 ~ units2) =>
  Subtraction (Qty units1) (Function units2) (Function units1)
  where
  value - Function expression _ = build (value - expression)

instance
  (Known units1, Known units2, Known units3, Units.Product units1 units2 units3) =>
  Multiplication (Function units1) (Function units2) (Function units3)

instance
  (Known units1, Known units2) =>
  Multiplication' (Function units1) (Function units2)
  where
  type Function units1 .*. Function units2 = Function (units1 :*: units2)
  Function expression1 _ .*. Function expression2 _ = build (expression1 .*. expression2)

instance
  (Known units1, Known units2, Known units3, Units.Product units1 units2 units3) =>
  Multiplication (Function units1) (Qty units2) (Function units3)

instance
  (Known units1, Known units2) =>
  Multiplication' (Function units1) (Qty units2)
  where
  type Function units1 .*. Qty units2 = Function (units1 :*: units2)
  Function expression _ .*. value = build (expression .*. value)

instance
  (Known units1, Known units2, Known units3, Units.Product units1 units2 units3) =>
  Multiplication (Qty units1) (Function units2) (Function units3)

instance
  (Known units1, Known units2) =>
  Multiplication' (Qty units1) (Function units2)
  where
  type Qty units1 .*. Function units2 = Function (units1 :*: units2)
  value .*. Function expression _ = build (value .*. expression)

instance Known units => Multiplication (Function units) Int (Function units)

instance Known units => Multiplication' (Function units) Int where
  type Function units .*. Int = Function (units :*: Unitless)
  Function expression _ .*. value = build (expression .*. value)

instance Known units => Multiplication Int (Function units) (Function units)

instance Known units => Multiplication' Int (Function units) where
  type Int .*. Function units = Function (Unitless :*: units)
  value .*. Function expression _ = build (value .*. expression)

instance
  (Known units1, Known units2, Known units3, Units.Quotient units1 units2 units3) =>
  Division (Function units1) (Function units2) (Function units3)

instance (Known units1, Known units2) => Division' (Function units1) (Function units2) where
  type Function units1 ./. Function units2 = Function (units1 :/: units2)
  Function expression1 _ ./. Function expression2 _ = build (expression1 ./. expression2)

instance
  (Known units1, Known units2, Known units3, Units.Quotient units1 units2 units3) =>
  Division (Function units1) (Qty units2) (Function units3)

instance
  (Known units1, Known units2) =>
  Division' (Function units1) (Qty units2)
  where
  type Function units1 ./. Qty units2 = Function (units1 :/: units2)
  Function expression _ ./. value = build (expression ./. value)

instance Known units => Division (Function units) Int (Function units)

instance Known units => Division' (Function units) Int where
  type Function units ./. Int = Function (units :/: Unitless)
  Function expression _ ./. value = build (expression ./. value)

instance
  (Known units1, Known units2, Known units3, Units.Quotient units1 units2 units3) =>
  Division (Qty units1) (Function units2) (Function units3)

instance
  (Known units1, Known units2) =>
  Division' (Qty units1) (Function units2)
  where
  type Qty units1 ./. Function units2 = Function (units1 :/: units2)
  value ./. Function expression _ = build (value ./. expression)

instance
  (Known units1, Known units2, Units.Inverse units1 units2) =>
  Division Int (Function units1) (Function units2)

instance Known units => Division' Int (Function units) where
  type Int ./. Function units = Function (Unitless :/: units)
  value ./. Function expression _ = build (value ./. expression)

evaluate :: Function units -> Uv.Point -> Qty units
evaluate (Function expression _) uv = Expression.evaluate expression uv

bounds :: Function units -> Uv.Bounds -> Range units
bounds (Function expression _) uv = Expression.bounds expression uv

derivative :: Known units => Parameter -> Function units -> Function units
derivative varyingParameter (Function expression _) =
  build (Expression.derivative varyingParameter expression)

derivativeIn :: Known units => Uv.Direction -> Function units -> Function units
derivativeIn direction function =
  Direction2d.xComponent direction * derivative U function
    + Direction2d.yComponent direction * derivative V function

zero :: Function units
zero = constant Qty.zero

constant :: Qty units -> Function units
constant = build . Expression.Constant

u :: Function Unitless
u = parameter U

v :: Function Unitless
v = parameter V

parameter :: Parameter -> Function Unitless
parameter = build . Expression.Parameter

new :: Interface function units => function -> Function units
new = build . Expression

squared ::
  (Known units1, Known units2, Units.Squared units1 units2) =>
  Function units1 ->
  Function units2
squared function = Units.specialize (squared' function)

squared' :: Known units => Function units -> Function (units :*: units)
squared' (Function expression _) = build (Expression.squared' expression)

sqrt ::
  (Known units1, Known units2, Units.Squared units1 units2) =>
  Function units2 ->
  Function units1
sqrt function = sqrt' (Units.unspecialize function)

sqrt' :: Function (units :*: units) -> Function units
sqrt' (Function expression _) = build (Expression.sqrt' expression)

sin :: Function Radians -> Function Unitless
sin (Function expression _) = build (Expression.sin expression)

cos :: Function Radians -> Function Unitless
cos (Function expression _) = build (Expression.cos expression)

data CurveOnSurface units
  = CurveOnSurface (Curve2d Uv.Coordinates) (Function units)
  deriving (Eq, Show)

instance
  Known units =>
  Composition (Curve2d Uv.Coordinates) (Function units) (Curve1d units)
  where
  function . uvCurve = Curve1d.new (CurveOnSurface uvCurve function)

instance
  Known units =>
  Curve1d.Interface (CurveOnSurface units) units
  where
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

  toAstImpl (CurveOnSurface uvCurve function) = toAst function . Curve2d.toAst uvCurve

toAst :: Function units -> Jit.Ast Uv.Point Float
toAst (Function expression _) = Expression.toAst expression

zeros :: (Known units, Tolerance units) => Function units -> Result Zeros.Error Zeros
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
  (Known units, Tolerance units) =>
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
  (Known units, Tolerance units) =>
  Subproblem units ->
  Solve2d.Action Solve2d.NoExclusions FindZerosContext (Solution units)
findCrossingCurves subproblem =
  case crossingCurve subproblem of
    Unresolved -> Solve2d.recurse CrossingCurvesOnly
    Resolved Nothing -> Solve2d.pass
    Resolved (Just curve) -> Solve2d.return (NonEmpty.singleton (CrossingCurveSolution curve))

crossingCurve ::
  (Known units, Tolerance units) =>
  Subproblem units ->
  Fuzzy (Maybe PartialZeros.CrossingCurve)
crossingCurve subproblem = do
  Fuzzy.oneOf
    [ diagonalCrossingCurve subproblem
    , horizontalCrossingCurve subproblem
    , verticalCrossingCurve subproblem
    ]

diagonalCrossingCurve ::
  (Known units, Tolerance units) =>
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

southeastCrossingCurve ::
  (Known units, Tolerance units) =>
  Subproblem units ->
  Maybe PartialZeros.CrossingCurve
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

southwestCrossingCurve ::
  (Known units, Tolerance units) =>
  Subproblem units ->
  Maybe PartialZeros.CrossingCurve
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

northeastCrossingCurve ::
  (Known units, Tolerance units) =>
  Subproblem units ->
  Maybe PartialZeros.CrossingCurve
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

northwestCrossingCurve ::
  (Known units, Tolerance units) =>
  Subproblem units ->
  Maybe PartialZeros.CrossingCurve
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
  (Known units, Tolerance units) =>
  Derivatives (Function units) ->
  Uv.Bounds ->
  (Uv.Point, Domain2d.Boundary) ->
  (Uv.Point, Domain2d.Boundary) ->
  PartialZeros.CrossingCurve
diagonalCurve derivatives uvBounds start end = do
  let (startPoint, startBoundary) = start
  let (endPoint, endBoundary) = end
  let (uStart, vStart) = Point2d.coordinates startPoint
  let (uEnd, vEnd) = Point2d.coordinates endPoint
  PartialZeros.crossingCurve startBoundary endBoundary uvBounds $
    if Float.abs (uEnd - uStart) >= Float.abs (vEnd - vStart)
      then HorizontalCurve.monotonic derivatives uStart uEnd (Range vStart vEnd)
      else VerticalCurve.monotonic derivatives (Range uStart uEnd) vStart vEnd

horizontalCrossingCurve ::
  (Known units, Tolerance units) =>
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

eastCrossingCurve ::
  (Known units, Tolerance units) =>
  Subproblem units ->
  Fuzzy PartialZeros.CrossingCurve
eastCrossingCurve subproblem = do
  let start = Subproblem.leftEdgePoint subproblem
  let end = Subproblem.rightEdgePoint subproblem
  horizontalCurve subproblem start end

westCrossingCurve ::
  (Known units, Tolerance units) =>
  Subproblem units ->
  Fuzzy PartialZeros.CrossingCurve
westCrossingCurve subproblem = do
  let start = Subproblem.rightEdgePoint subproblem
  let end = Subproblem.leftEdgePoint subproblem
  horizontalCurve subproblem start end

horizontalCurve ::
  (Known units, Tolerance units) =>
  Subproblem units ->
  (Uv.Point, Domain2d.Boundary) ->
  (Uv.Point, Domain2d.Boundary) ->
  Fuzzy PartialZeros.CrossingCurve
horizontalCurve Subproblem{derivatives, subdomain, uvBounds} start end = do
  let (startPoint, startBoundary) = start
  let (endPoint, endBoundary) = end
  let uStart = Point2d.xCoordinate startPoint
  let uEnd = Point2d.xCoordinate endPoint
  let Bounds2d _ vBounds = uvBounds
  let curve = HorizontalCurve.new derivatives uStart uEnd vBounds
  let Domain2d _ vSubdomain = subdomain
  let Bounds2d _ curveVBounds = Curve2d.bounds curve
  if Range.contains curveVBounds (Domain1d.interior vSubdomain)
    then Resolved (PartialZeros.crossingCurve startBoundary endBoundary uvBounds curve)
    else Unresolved

verticalCrossingCurve ::
  (Known units, Tolerance units) =>
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

southCrossingCurve ::
  (Known units, Tolerance units) =>
  Subproblem units ->
  Fuzzy PartialZeros.CrossingCurve
southCrossingCurve subproblem = Fuzzy.do
  let start = Subproblem.topEdgePoint subproblem
  let end = Subproblem.bottomEdgePoint subproblem
  verticalCurve subproblem start end

northCrossingCurve ::
  (Known units, Tolerance units) =>
  Subproblem units ->
  Fuzzy PartialZeros.CrossingCurve
northCrossingCurve subproblem = Fuzzy.do
  let start = Subproblem.bottomEdgePoint subproblem
  let end = Subproblem.topEdgePoint subproblem
  verticalCurve subproblem start end

verticalCurve ::
  (Known units, Tolerance units) =>
  Subproblem units ->
  (Uv.Point, Domain2d.Boundary) ->
  (Uv.Point, Domain2d.Boundary) ->
  Fuzzy PartialZeros.CrossingCurve
verticalCurve Subproblem{derivatives, subdomain, uvBounds} start end = do
  let (startPoint, startBoundary) = start
  let (endPoint, endBoundary) = end
  let vStart = Point2d.yCoordinate startPoint
  let vEnd = Point2d.yCoordinate endPoint
  let curve = VerticalCurve.new derivatives (Bounds2d.xCoordinate uvBounds) vStart vEnd
  let Domain2d uSubdomain _ = subdomain
  let Bounds2d curveUBounds _ = Curve2d.bounds curve
  if Range.contains curveUBounds (Domain1d.interior uSubdomain)
    then Resolved (PartialZeros.crossingCurve startBoundary endBoundary uvBounds curve)
    else Unresolved
