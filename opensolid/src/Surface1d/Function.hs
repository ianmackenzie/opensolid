{-# OPTIONS_GHC -Wno-unused-local-binds #-}

module Surface1d.Function
  ( Function
  , Interface (..)
  , Solution
  , evaluateAt
  , pointOn
  , segmentBounds
  , boundsOn
  , derivative
  , derivativeIn
  , zero
  , constant
  , parameter
  , solve
  , wrap
  , squared
  , sqrt
  , sin
  , cos
  , curveOnSurface
  , isZero
  , findSolutions
  )
where

import Angle qualified
import Bounds2d (Bounds2d (Bounds2d))
import Bounds2d qualified
import Curve1d (Curve1d (Curve1d))
import Curve1d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import Direction2d qualified
import Float qualified
import Generic qualified
import List qualified
import OpenSolid
import Point2d (Point2d (Point2d))
import Point2d qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import Result qualified
import Surface1d.Solution (Solution)
import Surface1d.Solution qualified as Solution
import U qualified
import Units qualified
import Uv (Parameter (U, V))
import Uv qualified
import VectorCurve2d qualified

class (Show function) => Interface function units | function -> units where
  evaluateAtImpl :: Uv.Point -> function -> Qty units
  segmentBoundsImpl :: Uv.Bounds -> function -> Range units
  derivativeImpl :: Parameter -> function -> Function units

data Function units where
  Function ::
    (Interface function units) =>
    function ->
    Function units
  Zero ::
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
  Product ::
    (Units.Product units1 units2 units3) =>
    Function units1 ->
    Function units2 ->
    Function units3
  Quotient ::
    (Units.Quotient units1 units2 units3) =>
    Function units1 ->
    Function units2 ->
    Function units3
  Squared ::
    (Units.Squared units1 units2) =>
    Function units1 ->
    Function units2
  SquareRoot ::
    (Units.Squared units1 units2) =>
    Function units2 ->
    Function units1
  Sin ::
    Function Radians ->
    Function Unitless
  Cos ::
    Function Radians ->
    Function Unitless

deriving instance Show (Function units)

instance
  ( units1 ~ units1'
  , units2 ~ units2'
  ) =>
  Units.Coercion
    units1
    units2
    (Function units1')
    (Function units2')

instance Generic.HasZero (Function units) where
  zeroImpl = zero

instance Negation (Function units) where
  negate Zero = Zero
  negate (Constant x) = Constant (negate x)
  negate (Negated function) = function
  negate (Difference f1 f2) = Difference f2 f1
  negate (Product f1 f2) = negate f1 * f2
  negate function = Negated function

instance Multiplication Sign (Function units) (Function units) where
  Positive * function = function
  Negative * function = -function

instance Multiplication (Function units) Sign (Function units) where
  function * Positive = function
  function * Negative = -function

instance (units ~ units') => Addition (Function units) (Function units') (Function units) where
  Zero + function = function
  function + Zero = function
  Constant x + Constant y = constant (x + y)
  function1 + function2 = Sum function1 function2

instance (units ~ units') => Addition (Function units) (Qty units') (Function units) where
  function + value = function + constant value

instance (units ~ units') => Addition (Qty units) (Function units') (Function units) where
  value + function = constant value + function

instance (units ~ units') => Subtraction (Function units) (Function units') (Function units) where
  Zero - function = negate function
  function - Zero = function
  Constant x - Constant y = constant (x - y)
  function1 - function2 = Difference function1 function2

instance (units ~ units') => Subtraction (Function units) (Qty units') (Function units) where
  function - value = function - constant value

instance (units ~ units') => Subtraction (Qty units) (Function units') (Function units) where
  value - function = constant value - function

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (Function units1)
    (Function units2)
    (Function units3)
  where
  Zero * _ = Zero
  _ * Zero = Zero
  Constant x * Constant y = Constant (x * y)
  Constant x * function | Units.drop x == 1.0 = Units.add (Units.drop function)
  Constant x * function | Units.drop x == -1.0 = Units.add (Units.drop (negate function))
  Constant x * Negated c = negate x * c
  f1 * (Constant x) = Constant x * f1
  Constant x * Product (Constant y) c =
    Units.add (Product (Constant (Units.drop x * Units.drop y)) (Units.drop c))
  function1 * function2 = Product function1 function2

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (Function units1)
    (Qty units2)
    (Function units3)
  where
  function * value = function * constant value

instance
  (Units.Product units1 units2 units3) =>
  Multiplication
    (Qty units1)
    (Function units2)
    (Function units3)
  where
  value * function = constant value * function

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (Function units1)
    (Function units2)
    (Function units3)
  where
  Zero / _ = Zero
  Constant x / Constant y = Constant (x / y)
  function / Constant x =
    Units.specialize $
      (Units.generalize 1.0 ./ Units.generalize x) .* Units.generalize function
  function1 / function2 = Quotient function1 function2

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (Function units1)
    (Qty units2)
    (Function units3)
  where
  function / value = function / constant value

instance
  (Units.Quotient units1 units2 units3) =>
  Division
    (Qty units1)
    (Function units2)
    (Function units3)
  where
  value / function = constant value / function

evaluateAt :: Uv.Point -> Function units -> Qty units
evaluateAt uv function =
  case function of
    Function f -> evaluateAtImpl uv f
    Zero -> Qty.zero
    Constant x -> x
    Parameter U -> Point2d.xCoordinate uv
    Parameter V -> Point2d.yCoordinate uv
    Negated f -> negate (evaluateAt uv f)
    Sum f1 f2 -> evaluateAt uv f1 + evaluateAt uv f2
    Difference f1 f2 -> evaluateAt uv f1 - evaluateAt uv f2
    Product f1 f2 -> evaluateAt uv f1 * evaluateAt uv f2
    Quotient f1 f2 -> evaluateAt uv f1 / evaluateAt uv f2
    Squared f -> Qty.squared (evaluateAt uv f)
    SquareRoot f -> Qty.sqrt (evaluateAt uv f)
    Sin f -> Angle.sin (evaluateAt uv f)
    Cos f -> Angle.cos (evaluateAt uv f)

pointOn :: Function units -> Uv.Point -> Qty units
pointOn function uv = evaluateAt uv function

segmentBounds :: Uv.Bounds -> Function units -> Range units
segmentBounds uv function =
  case function of
    Function f -> segmentBoundsImpl uv f
    Zero -> Range.constant Qty.zero
    Constant x -> Range.constant x
    Parameter U -> Bounds2d.xCoordinate uv
    Parameter V -> Bounds2d.yCoordinate uv
    Negated f -> negate (segmentBounds uv f)
    Sum f1 f2 -> segmentBounds uv f1 + segmentBounds uv f2
    Difference f1 f2 -> segmentBounds uv f1 - segmentBounds uv f2
    Product f1 f2 -> segmentBounds uv f1 * segmentBounds uv f2
    Quotient f1 f2 -> segmentBounds uv f1 / segmentBounds uv f2
    Squared f -> Range.squared (segmentBounds uv f)
    SquareRoot f -> Range.sqrt (segmentBounds uv f)
    Sin f -> Range.sin (segmentBounds uv f)
    Cos f -> Range.cos (segmentBounds uv f)

boundsOn :: Function units -> Uv.Bounds -> Range units
boundsOn function uvBounds = segmentBounds uvBounds function

derivative :: Parameter -> Function units -> Function units
derivative p function =
  case function of
    Function f -> derivativeImpl p f
    Zero -> zero
    Constant _ -> zero
    Parameter p' -> if p == p' then constant 1.0 else zero
    Negated f -> negate (derivative p f)
    Sum f1 f2 -> derivative p f1 + derivative p f2
    Difference f1 f2 -> derivative p f1 - derivative p f2
    Product f1 f2 -> derivative p f1 * f2 + f1 * derivative p f2
    Quotient f1 f2 ->
      let f1' = Units.generalize f1
          f2' = Units.generalize f2
       in Units.specialize ((derivative p f1' .* f2' - f1' .* derivative p f2') ./ squared f2')
    Squared f -> 2.0 * f * derivative p f
    SquareRoot f -> derivative p f / (2.0 * sqrt f)
    Sin f -> cos f * Units.drop (derivative p f)
    Cos f -> negate (sin f) * Units.drop (derivative p f)

derivativeIn :: Uv.Direction -> Function units -> Function units
derivativeIn direction function =
  Direction2d.xComponent direction * derivative U function
    + Direction2d.yComponent direction * derivative V function

zero :: Function units
zero = Zero

constant :: Qty units -> Function units
constant value = if value == Qty.zero then Zero else Constant value

parameter :: Parameter -> Function Unitless
parameter = Parameter

wrap :: (Interface function units) => function -> Function units
wrap = Function

squared :: (Units.Squared units1 units2) => Function units1 -> Function units2
squared Zero = Zero
squared (Constant x) = Constant (x * x)
squared (Negated f) = squared f
squared (Cos f) = Units.add (cosSquared f)
squared (Sin f) = Units.add (sinSquared f)
squared function = Squared function

cosSquared :: Function Radians -> Function Unitless
cosSquared f = 0.5 * cos (2.0 * f) + 0.5

sinSquared :: Function Radians -> Function Unitless
sinSquared f = 0.5 - 0.5 * cos (2.0 * f)

sqrt :: (Units.Squared units1 units2) => Function units2 -> Function units1
sqrt Zero = Zero
sqrt (Constant x) = Constant (Qty.sqrt x)
sqrt function = SquareRoot function

sin :: Function Radians -> Function Unitless
sin Zero = Zero
sin (Constant x) = constant (Angle.sin x)
sin function = Sin function

cos :: Function Radians -> Function Unitless
cos Zero = Constant 1.0
cos (Constant x) = constant (Angle.cos x)
cos function = Cos function

data CurveOnSurface units where
  CurveOnSurface ::
    (Curve2d.Interface curve Uv.Coordinates) =>
    curve ->
    Function units ->
    CurveOnSurface units

deriving instance Show (CurveOnSurface units)

instance Curve1d.Interface (CurveOnSurface units) units where
  evaluateAtImpl t (CurveOnSurface uvCurve function) =
    evaluateAt (Curve2d.evaluateAtImpl t uvCurve) function
  segmentBoundsImpl t (CurveOnSurface uvCurve function) =
    segmentBounds (Curve2d.segmentBoundsImpl t uvCurve) function
  derivativeImpl (CurveOnSurface uvCurve function) =
    let fU = derivative U function
        fV = derivative V function
        uvT = Curve2d.derivativeImpl uvCurve
        uT = VectorCurve2d.xComponent uvT
        vT = VectorCurve2d.yComponent uvT
     in Curve1d (CurveOnSurface uvCurve fU) * uT + Curve1d (CurveOnSurface uvCurve fV) * vT

curveOnSurface :: Curve2d Uv.Coordinates -> Function units -> Curve1d units
curveOnSurface uvCurve function = Curve1d (CurveOnSurface uvCurve function)

isZero :: (Tolerance units) => Function units -> Bool
isZero function = List.all (~= Qty.zero) (Bounds2d.sample (pointOn function) Uv.domain)

data SolveError
  = ZeroEverywhere
  | HigherOrderIntersection
  | DegenerateCurve
  deriving (Eq, Show, ErrorMessage)

solve :: (Tolerance units) => Function units -> Result SolveError (List Solution)
solve Zero = Error ZeroEverywhere
solve (Constant value) = if value ~= Qty.zero then Error ZeroEverywhere else Ok []
solve f | isZero f = Error ZeroEverywhere
solve f = findSolutions f fu fv fuu fvv fuv Uv.domain
 where
  fu = derivative U f
  fv = derivative V f
  fuu = derivative U fu
  fvv = derivative V fv
  fuv = derivative V fu

findSolutions ::
  (Tolerance units) =>
  Function units ->
  Function units ->
  Function units ->
  Function units ->
  Function units ->
  Function units ->
  Uv.Bounds ->
  Result SolveError (List Solution)
findSolutions f fu fv fuu fvv fuv uvBounds
  | not (fBounds ^ Qty.zero) = Ok []
  | Just (Ok c) <- maybeCrossingCurveByU = Ok [Solution.CrossingCurve c]
  | Just (Error Curve2d.DegenerateCurve) <- maybeCrossingCurveByU = Error DegenerateCurve
  -- TODO: other solution types
  | everythingZero = Error HigherOrderIntersection
  | otherwise = do
      let (u1, u2) = Range.bisect uRange
      let (v1, v2) = Range.bisect vRange
      solutions11 <- findSolutions f fu fv fuu fvv fuv (Bounds2d u1 v1)
      solutions12 <- findSolutions f fu fv fuu fvv fuv (Bounds2d u1 v2)
      solutions21 <- findSolutions f fu fv fuu fvv fuv (Bounds2d u2 v1)
      solutions22 <- findSolutions f fu fv fuu fvv fuv (Bounds2d u2 v2)
      return (solutions11 ++ solutions12 ++ solutions21 ++ solutions22)
 where
  (Bounds2d uRange vRange) = uvBounds
  (Range minU maxU) = uRange
  (Range minV maxV) = vRange
  uWidth = Range.width uRange
  vWidth = Range.width vRange
  uLeft = minU - uWidth
  uRight = maxU + uWidth
  vBottom = minV - vWidth
  vTop = maxV + vWidth
  expandedURange = Range.from uLeft uRight
  expandedVRange = Range.from vBottom vTop
  expandedUBounds = Bounds2d expandedURange vRange
  expandedVBounds = Bounds2d uRange expandedVRange
  uLeftSlice = Bounds2d (Range.constant uLeft) vRange
  uRightSlice = Bounds2d (Range.constant uRight) vRange
  vBottomSlice = Bounds2d uRange (Range.constant vBottom)
  vTopSlice = Bounds2d uRange (Range.constant vTop)
  fBounds = segmentBounds uvBounds f

  everythingZero =
    fBounds ~= Qty.zero
      && segmentBounds uvBounds fu ~= Qty.zero
      && segmentBounds uvBounds fv ~= Qty.zero
      && segmentBounds uvBounds fuu ~= Qty.zero
      && segmentBounds uvBounds fvv ~= Qty.zero
      && segmentBounds uvBounds fuv ~= Qty.zero

  maybeCrossingCurveByU
    | fvResolution >= 0.5 && isNegative f vBottomSlice && isPositive f vTopSlice =
        Just (Result.map Curve2d.reverse crossingCurveByU)
    | fvResolution <= -0.5 && isPositive f vBottomSlice && isNegative f vTopSlice =
        Just crossingCurveByU
    | otherwise = Nothing
   where
    fvResolution = Range.resolution (segmentBounds expandedVBounds fv)

  crossingCurveByU :: Result Curve2d.DegenerateCurve (Curve2d Uv.Coordinates)
  crossingCurveByU =
    exactly (Curve2d.from (CrossingCurveByU f (-fu / fv) minU maxU expandedVRange))

-- isTangentCurveByU
--   | segmentBounds vBottomSlice fv ^ Qty.zero = False
--   | segmentBounds vTopSlice fv ^ Qty.zero = False
--   | Qty.abs (Range.resolution (segmentBounds expandedVBounds fvv)) < 0.5 = False
--   | otherwise =
--       let isTangentIntersectionPoint uValue =
--             let fvvalue vValue = evaluateAt (Point2d uValue vValue) fv
--              in case Range.solve fvvalue expandedVRange of
--                   Nothing -> False
--                   Just vValue ->
--                     let uvValue = Point2d uValue vValue
--                      in evaluateAt uvValue f ~= Qty.zero && evaluateAt uvValue fu ~= Qty.zero
--        in List.all isTangentIntersectionPoint (Range.samples uRange)

-- isTangentCurveByV
--   | segmentBounds uLeftSlice fu ^ Qty.zero = False
--   | segmentBounds uRightSlice fu ^ Qty.zero = False
--   | Qty.abs (Range.resolution (segmentBounds expandedUBounds fuu)) < 0.5 = False
--   | otherwise =
--       let isTangentIntersectionPoint vValue =
--             let fuValue uValue = evaluateAt (Point2d uValue vValue) fu
--              in case Range.solve fuValue expandedURange of
--                   Nothing -> False
--                   Just uValue ->
--                     let uvValue = Point2d uValue vValue
--                      in evaluateAt uvValue f ~= Qty.zero && evaluateAt uvValue fv ~= Qty.zero
--        in List.all isTangentIntersectionPoint (Range.samples vRange)

isRangePositive :: (Tolerance units) => Range units -> Fuzzy Bool
isRangePositive range
  | Range.minValue range > ?tolerance = Resolved True
  | Range.maxValue range < ?tolerance = Resolved False
  | otherwise = Unresolved

isRangeNegative :: (Tolerance units) => Range units -> Fuzzy Bool
isRangeNegative range
  | Range.maxValue range < negate ?tolerance = Resolved True
  | Range.minValue range > negate ?tolerance = Resolved False
  | otherwise = Unresolved

isPositive :: (Tolerance units) => Function units -> Uv.Bounds -> Bool
isPositive function uvBounds = Bounds2d.all (isRangePositive . boundsOn function) uvBounds

isNegative :: (Tolerance units) => Function units -> Uv.Bounds -> Bool
isNegative function uvBounds = Bounds2d.all (isRangeNegative . boundsOn function) uvBounds

data CrossingCurveByU units
  = CrossingCurveByU
      (Function units)
      (Function Unitless)
      Float
      Float
      (Range Unitless)
  deriving (Show)

instance Curve2d.Interface (CrossingCurveByU units) Uv.Coordinates where
  startPointImpl = Curve2d.evaluateAtImpl 0.0
  endPointImpl = Curve2d.evaluateAtImpl 1.0

  evaluateAtImpl t (CrossingCurveByU f _ uStart uEnd vRange) =
    let u = Float.interpolateFrom uStart uEnd t
        v = evaluateCrossingCurveByU f vRange u
     in Point2d u v

  segmentBoundsImpl (Range t1 t2) (CrossingCurveByU f vu uStart uEnd vSearchBounds) =
    let u1 = Float.interpolateFrom uStart uEnd t1
        u2 = Float.interpolateFrom uStart uEnd t2
        v1 = evaluateCrossingCurveByU f vSearchBounds u1
        v2 = evaluateCrossingCurveByU f vSearchBounds u2
        slopeBounds = segmentBounds (Bounds2d (Range.from u1 u2) vSearchBounds) vu
        vRange = parallelogramBounds u1 u2 v1 v2 slopeBounds
     in Bounds2d (Range.from u1 u2) vRange

  derivativeImpl c@(CrossingCurveByU _ vu uStart uEnd _) =
    let deltaU = uEnd - uStart
        uT = Curve1d.constant deltaU
        vT = deltaU * Curve1d (CurveOnSurface c vu)
     in VectorCurve2d.xy uT vT

  reverseImpl (CrossingCurveByU f vu uStart uEnd vRange) =
    CrossingCurveByU f vu uEnd uStart vRange

  boundsImpl c = Curve2d.segmentBoundsImpl U.domain c

evaluateCrossingCurveByU :: Function units -> Range Unitless -> Float -> Float
evaluateCrossingCurveByU f vRange u =
  case Range.solve (\v -> evaluateAt (Point2d u v) f) vRange of
    Just v -> v
    Nothing -> internalError "Solution should always exist, by construction"

parallelogramBounds ::
  Float ->
  Float ->
  Float ->
  Float ->
  Range Unitless ->
  Range Unitless
parallelogramBounds x1 x2 y1 y2 (Range minSlope maxSlope) =
  let deltaX = x2 - x1
      deltaY = y2 - y1
      deltaXLow = (maxSlope * deltaX - deltaY) / (maxSlope - minSlope) |> Qty.clamp 0.0 deltaX
      deltaXHigh = (deltaY - minSlope * deltaX) / (maxSlope - minSlope) |> Qty.clamp 0.0 deltaX
      yLow = y1 + minSlope * deltaXLow
      yHigh = y1 + maxSlope * deltaXHigh
   in Range.hull4 y1 y2 yLow yHigh
