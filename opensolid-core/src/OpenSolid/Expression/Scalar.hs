module OpenSolid.Expression.Scalar
  ( Scalar (Constant)
  , zero
  , constant
  , curveParameter
  , surfaceParameter
  , squared
  , sqrt
  , sin
  , cos
  , quadraticSpline
  , cubicSpline
  , quarticSpline
  , quinticSpline
  , bezierCurve
  , curveDerivative
  , surfaceDerivative
  , Ptr
  , ptr
  )
where

import Data.Int (Int64)
import Foreign qualified
import Foreign.Marshal.Array qualified
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.Int qualified as Int
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Qty (Qty (Qty))
import OpenSolid.SurfaceParameter (SurfaceParameter, UvPoint)
import OpenSolid.SurfaceParameter qualified as SurfaceParameter
import OpenSolid.Units qualified as Units
import System.IO.Unsafe (unsafeDupablePerformIO)
import Prelude (Double)

data Scalar input where
  Constant :: Float -> Scalar input
  CurveParameter :: Scalar Float
  SurfaceParameter :: SurfaceParameter -> Scalar UvPoint
  Negated :: Scalar input -> Scalar input
  Sum :: Scalar input -> Scalar input -> Scalar input
  Difference :: Scalar input -> Scalar input -> Scalar input
  Squared :: Scalar input -> Scalar input
  Product :: Scalar input -> Scalar input -> Scalar input
  Quotient :: Scalar input -> Scalar input -> Scalar input
  Sqrt :: Scalar input -> Scalar input
  Sin :: Scalar input -> Scalar input
  Cos :: Scalar input -> Scalar input
  QuadraticSpline :: Float -> Float -> Float -> Scalar input -> Scalar input
  CubicSpline :: Float -> Float -> Float -> Float -> Scalar input -> Scalar input
  QuarticSpline :: Float -> Float -> Float -> Float -> Float -> Scalar input -> Scalar input
  QuinticSpline :: Float -> Float -> Float -> Float -> Float -> Float -> Scalar input -> Scalar input
  BezierCurve :: NonEmpty Float -> Scalar input -> Scalar input

deriving instance Eq (Scalar input)

deriving instance Ord (Scalar input)

instance Composition (Scalar input) (Scalar Float) (Scalar input) where
  Constant value . _ = Constant value
  CurveParameter . input = input
  Negated arg . input = negate (arg . input)
  Sum lhs rhs . input = (lhs . input) + (rhs . input)
  Difference lhs rhs . input = (lhs . input) - (rhs . input)
  Product lhs rhs . input = (lhs . input) * (rhs . input)
  Quotient lhs rhs . input = (lhs . input) / (rhs . input)
  Squared arg . input = squared (arg . input)
  Sqrt arg . input = sqrt (arg . input)
  Sin arg . input = sin (arg . input)
  Cos arg . input = cos (arg . input)
  QuadraticSpline p1 p2 p3 param . input = QuadraticSpline p1 p2 p3 (param . input)
  CubicSpline p1 p2 p3 p4 param . input = CubicSpline p1 p2 p3 p4 (param . input)
  QuarticSpline p1 p2 p3 p4 p5 param . input = QuarticSpline p1 p2 p3 p4 p5 (param . input)
  QuinticSpline p1 p2 p3 p4 p5 p6 param . input = QuinticSpline p1 p2 p3 p4 p5 p6 (param . input)
  BezierCurve controlPoints param . input = BezierCurve controlPoints (param . input)

instance
  input1 ~ input2 =>
  Composition (Scalar input1, Scalar input2) (Scalar UvPoint) (Scalar input1)
  where
  Constant value . _ = Constant value
  SurfaceParameter SurfaceParameter.U . (input, _) = input
  SurfaceParameter SurfaceParameter.V . (_, input) = input
  Negated arg . inputs = negate (arg . inputs)
  Sum lhs rhs . inputs = (lhs . inputs) + (rhs . inputs)
  Difference lhs rhs . inputs = (lhs . inputs) - (rhs . inputs)
  Product lhs rhs . inputs = (lhs . inputs) * (rhs . inputs)
  Quotient lhs rhs . inputs = (lhs . inputs) / (rhs . inputs)
  Squared arg . inputs = squared (arg . inputs)
  Sqrt arg . inputs = sqrt (arg . inputs)
  Sin arg . inputs = sin (arg . inputs)
  Cos arg . inputs = cos (arg . inputs)
  QuadraticSpline p1 p2 p3 param . inputs = QuadraticSpline p1 p2 p3 (param . inputs)
  CubicSpline p1 p2 p3 p4 param . inputs = CubicSpline p1 p2 p3 p4 (param . inputs)
  QuarticSpline p1 p2 p3 p4 p5 param . inputs = QuarticSpline p1 p2 p3 p4 p5 (param . inputs)
  QuinticSpline p1 p2 p3 p4 p5 p6 param . inputs = QuinticSpline p1 p2 p3 p4 p5 p6 (param . inputs)
  BezierCurve controlPoints param . inputs = BezierCurve controlPoints (param . inputs)

zero :: Scalar input
zero = constant 0.0

one :: Scalar input
one = constant 1.0

constant :: Qty units -> Scalar input
constant value = Constant (Units.coerce value)

curveParameter :: Scalar Float
curveParameter = CurveParameter

surfaceParameter :: SurfaceParameter -> Scalar UvPoint
surfaceParameter = SurfaceParameter

instance Negation (Scalar input) where
  -- TODO special cases for spline types
  negate (Constant value) = Constant (negate value)
  negate (Negated expression) = expression
  negate (Difference lhs rhs) = Difference rhs lhs
  negate expression = Negated expression

instance Multiplication Sign (Scalar input) (Scalar input) where
  Positive * scalar = scalar
  Negative * scalar = -scalar

instance Multiplication (Scalar input) Sign (Scalar input) where
  scalar * Positive = scalar
  scalar * Negative = -scalar

instance input1 ~ input2 => Addition (Scalar input1) (Scalar input2) (Scalar input1) where
  -- TODO special cases for spline types
  Constant a + Constant b = Constant (a + b)
  expression + Constant 0.0 = expression
  Constant 0.0 + expression = expression
  Constant a + Sum (Constant b) expression = (a + b) + expression
  Sum (Constant a) expression + Constant b = (a + b) + expression
  Constant a + Difference (Constant b) expression = (a + b) - expression
  Difference (Constant a) expression + Constant b = (a + b) - expression
  Constant a + Difference expression (Constant b) = expression + (a - b)
  Difference expression (Constant a) + Constant b = expression + (b - a)
  -- Canonicalize argument order using lexical ordering
  lhs + rhs = if lhs <= rhs then Sum lhs rhs else Sum rhs lhs

instance Addition (Qty units) (Scalar input) (Scalar input) where
  value + scalar = constant value + scalar

instance Addition (Scalar input) (Qty units) (Scalar input) where
  scalar + value = scalar + constant value

instance input1 ~ input2 => Subtraction (Scalar input1) (Scalar input2) (Scalar input1) where
  -- TODO special cases for spline types
  Constant a - Constant b = constant (a - b)
  expression - Constant 0.0 = expression
  Constant 0.0 - expression = -expression
  Constant a - Sum (Constant b) expression = (a - b) - expression
  Sum (Constant a) expression - Constant b = (a - b) + expression
  Constant a - Difference (Constant b) expression = (a - b) + expression
  Difference (Constant a) expression - Constant b = (a - b) - expression
  Constant a - Difference expression (Constant b) = (a + b) - expression
  Difference expression (Constant a) - Constant b = expression - (a + b)
  lhs - rhs = Difference lhs rhs

instance Subtraction (Qty units) (Scalar input) (Scalar input) where
  value - scalar = constant value - scalar

instance Subtraction (Scalar input) (Qty units) (Scalar input) where
  scalar - value = scalar - constant value

instance input1 ~ input2 => Multiplication (Scalar input1) (Scalar input2) (Scalar input1) where
  -- TODO special cases for spline types
  Constant a * Constant b = constant (a * b)
  _ * Constant 0.0 = zero
  Constant 0.0 * _ = zero
  expression * Constant 1.0 = expression
  Constant 1.0 * expression = expression
  expression * Constant -1.0 = -expression
  Constant -1.0 * expression = -expression
  Constant a * Negated expression = -a * expression
  Negated expression * Constant a = expression * -a
  Constant a * Product (Constant b) expression = (a * b) * expression
  Product (Constant a) expression * Constant b = (a * b) * expression
  Constant a * Quotient (Constant b) expression = (a * b) / expression
  Quotient (Constant a) expression * Constant b = (a * b) / expression
  -- Canonicalize argument order using lexical ordering
  lhs * rhs = if lhs <= rhs then Product lhs rhs else Product rhs lhs

instance Multiplication (Qty units) (Scalar input) (Scalar input) where
  value * scalar = constant value * scalar

instance Multiplication (Scalar input) (Qty units) (Scalar input) where
  scalar * value = scalar * constant value

instance input1 ~ input2 => Division (Scalar input1) (Scalar input2) (Scalar input1) where
  Constant a / Constant b = constant (a / b)
  Constant 0.0 / _ = zero
  expression / Constant a = (1.0 / a) * expression
  expression / Quotient lhs rhs = expression * rhs / lhs
  lhs / rhs = Quotient lhs rhs

instance Division (Qty units) (Scalar input) (Scalar input) where
  value / scalar = constant value / scalar

instance Division (Scalar input) (Qty units) (Scalar input) where
  scalar / value = scalar / constant value

squared :: Scalar input -> Scalar input
squared (Constant value) = constant (Float.squared value)
squared (Negated arg) = squared arg
squared (Sqrt expression) = expression
squared expression = Squared expression

sqrt :: Scalar input -> Scalar input
sqrt (Constant value) = constant (Float.sqrt value)
sqrt expression = Sqrt expression

sin :: Scalar input -> Scalar input
sin (Constant value) = constant (Float.sin value)
sin expression = Sin expression

cos :: Scalar input -> Scalar input
cos (Constant value) = constant (Float.cos value)
cos (Negated expression) = cos expression
cos expression = Cos expression

line :: Qty units -> Qty units -> Scalar input -> Scalar input
line a b param = a + param * (b - a)

quadraticSpline :: Qty units -> Qty units -> Qty units -> Scalar input -> Scalar input
quadraticSpline p1 p2 p3 param =
  QuadraticSpline (Units.coerce p1) (Units.coerce p2) (Units.coerce p3) param

cubicSpline :: Qty units -> Qty units -> Qty units -> Qty units -> Scalar input -> Scalar input
cubicSpline p1 p2 p3 p4 param =
  CubicSpline (Units.coerce p1) (Units.coerce p2) (Units.coerce p3) (Units.coerce p4) param

quarticSpline ::
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Scalar input ->
  Scalar input
quarticSpline p1 p2 p3 p4 p5 param =
  QuarticSpline
    (Units.coerce p1)
    (Units.coerce p2)
    (Units.coerce p3)
    (Units.coerce p4)
    (Units.coerce p5)
    param

quinticSpline ::
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Scalar input ->
  Scalar input
quinticSpline p1 p2 p3 p4 p5 p6 param =
  QuinticSpline
    (Units.coerce p1)
    (Units.coerce p2)
    (Units.coerce p3)
    (Units.coerce p4)
    (Units.coerce p5)
    (Units.coerce p6)
    param

bezierCurve :: NonEmpty (Qty units) -> Scalar input -> Scalar input
bezierCurve controlPoints param = case controlPoints of
  NonEmpty.One value -> constant value
  NonEmpty.Two p1 p2 -> line p1 p2 param
  NonEmpty.Three p1 p2 p3 -> quadraticSpline p1 p2 p3 param
  NonEmpty.Four p1 p2 p3 p4 -> cubicSpline p1 p2 p3 p4 param
  NonEmpty.Five p1 p2 p3 p4 p5 -> quarticSpline p1 p2 p3 p4 p5 param
  NonEmpty.Six p1 p2 p3 p4 p5 p6 -> quinticSpline p1 p2 p3 p4 p5 p6 param
  NonEmpty.SevenOrMore -> BezierCurve (NonEmpty.map Units.coerce controlPoints) param

curveDerivative :: Scalar Float -> Scalar Float
curveDerivative expression = case expression of
  Constant _ -> zero
  CurveParameter -> one
  Negated arg -> negate (curveDerivative arg)
  Sum lhs rhs -> curveDerivative lhs + curveDerivative rhs
  Difference lhs rhs -> curveDerivative lhs - curveDerivative rhs
  Squared arg -> 2.0 * arg * curveDerivative arg
  Product lhs rhs -> curveDerivative lhs * rhs + lhs * curveDerivative rhs
  Quotient lhs rhs -> (curveDerivative lhs * rhs - lhs * curveDerivative rhs) / squared rhs
  Sqrt arg -> curveDerivative arg / (2.0 * expression)
  Sin arg -> cos arg * curveDerivative arg
  Cos arg -> sin arg * negate (curveDerivative arg)
  QuadraticSpline p1 p2 p3 param -> do
    let d1 = 2.0 * (p2 - p1)
    let d2 = 2.0 * (p3 - p2)
    line d1 d2 param * curveDerivative param
  CubicSpline p1 p2 p3 p4 param -> do
    let d1 = 3.0 * (p2 - p1)
    let d2 = 3.0 * (p3 - p2)
    let d3 = 3.0 * (p4 - p3)
    quadraticSpline d1 d2 d3 param * curveDerivative param
  QuarticSpline p1 p2 p3 p4 p5 param -> do
    let d1 = 4.0 * (p2 - p1)
    let d2 = 4.0 * (p3 - p2)
    let d3 = 4.0 * (p4 - p3)
    let d4 = 4.0 * (p5 - p4)
    cubicSpline d1 d2 d3 d4 param * curveDerivative param
  QuinticSpline p1 p2 p3 p4 p5 p6 param -> do
    let d1 = 5.0 * (p2 - p1)
    let d2 = 5.0 * (p3 - p2)
    let d3 = 5.0 * (p4 - p3)
    let d4 = 5.0 * (p5 - p4)
    let d5 = 5.0 * (p6 - p5)
    quarticSpline d1 d2 d3 d4 d5 param * curveDerivative param
  BezierCurve controlPoints param -> do
    let scale = Float.int (NonEmpty.length controlPoints - 1)
    let scaledDifference p1 p2 = scale * (p2 - p1)
    let scaledDifferences = NonEmpty.successive scaledDifference controlPoints
    case scaledDifferences of
      [] -> zero
      NonEmpty derivativeControlPoints ->
        bezierCurve derivativeControlPoints param * curveDerivative param

surfaceDerivative :: SurfaceParameter -> Scalar UvPoint -> Scalar UvPoint
surfaceDerivative varyingParameter expression = case expression of
  Constant _ -> zero
  SurfaceParameter parameter -> if parameter == varyingParameter then one else zero
  Negated arg -> negate (surfaceDerivative varyingParameter arg)
  Sum lhs rhs -> surfaceDerivative varyingParameter lhs + surfaceDerivative varyingParameter rhs
  Difference lhs rhs -> surfaceDerivative varyingParameter lhs - surfaceDerivative varyingParameter rhs
  Squared arg -> 2.0 * arg * surfaceDerivative varyingParameter arg
  Product lhs rhs -> surfaceDerivative varyingParameter lhs * rhs + lhs * surfaceDerivative varyingParameter rhs
  Quotient lhs rhs ->
    (surfaceDerivative varyingParameter lhs * rhs - lhs * surfaceDerivative varyingParameter rhs)
      / squared rhs
  Sqrt arg -> surfaceDerivative varyingParameter arg / (2.0 * expression)
  Sin arg -> cos arg * surfaceDerivative varyingParameter arg
  Cos arg -> sin arg * negate (surfaceDerivative varyingParameter arg)
  QuadraticSpline p1 p2 p3 param -> do
    let d1 = 2.0 * (p2 - p1)
    let d2 = 2.0 * (p3 - p2)
    line d1 d2 param * surfaceDerivative varyingParameter param
  CubicSpline p1 p2 p3 p4 param -> do
    let d1 = 3.0 * (p2 - p1)
    let d2 = 3.0 * (p3 - p2)
    let d3 = 3.0 * (p4 - p3)
    quadraticSpline d1 d2 d3 param * surfaceDerivative varyingParameter param
  QuarticSpline p1 p2 p3 p4 p5 param -> do
    let d1 = 4.0 * (p2 - p1)
    let d2 = 4.0 * (p3 - p2)
    let d3 = 4.0 * (p4 - p3)
    let d4 = 4.0 * (p5 - p4)
    cubicSpline d1 d2 d3 d4 param * surfaceDerivative varyingParameter param
  QuinticSpline p1 p2 p3 p4 p5 p6 param -> do
    let d1 = 5.0 * (p2 - p1)
    let d2 = 5.0 * (p3 - p2)
    let d3 = 5.0 * (p4 - p3)
    let d4 = 5.0 * (p5 - p4)
    let d5 = 5.0 * (p6 - p5)
    quarticSpline d1 d2 d3 d4 d5 param * surfaceDerivative varyingParameter param
  BezierCurve controlPoints param -> do
    let scale = Float.int (NonEmpty.length controlPoints - 1)
    let scaledDifference p1 p2 = scale * (p2 - p1)
    let scaledDifferences = NonEmpty.successive scaledDifference controlPoints
    case scaledDifferences of
      [] -> zero
      NonEmpty derivativeControlPoints ->
        bezierCurve derivativeControlPoints param * surfaceDerivative varyingParameter param

data RustExpression

type Ptr = Foreign.Ptr RustExpression

foreign import ccall unsafe "opensolid_expression_constant"
  opensolid_expression_constant :: Double -> Ptr

foreign import ccall unsafe "opensolid_expression_argument"
  opensolid_expression_argument :: Int64 -> Ptr

foreign import ccall unsafe "opensolid_expression_negate"
  opensolid_expression_negate :: Ptr -> Ptr

foreign import ccall unsafe "opensolid_expression_sum"
  opensolid_expression_sum :: Ptr -> Ptr -> Ptr

foreign import ccall unsafe "opensolid_expression_difference"
  opensolid_expression_difference :: Ptr -> Ptr -> Ptr

foreign import ccall unsafe "opensolid_expression_product"
  opensolid_expression_product :: Ptr -> Ptr -> Ptr

foreign import ccall unsafe "opensolid_expression_quotient"
  opensolid_expression_quotient :: Ptr -> Ptr -> Ptr

foreign import ccall unsafe "opensolid_expression_sqrt"
  opensolid_expression_sqrt :: Ptr -> Ptr

foreign import ccall unsafe "opensolid_expression_squared"
  opensolid_expression_squared :: Ptr -> Ptr

foreign import ccall unsafe "opensolid_expression_sin"
  opensolid_expression_sin :: Ptr -> Ptr

foreign import ccall unsafe "opensolid_expression_cos"
  opensolid_expression_cos :: Ptr -> Ptr

foreign import ccall unsafe "opensolid_expression_quadratic_spline"
  opensolid_expression_quadratic_spline ::
    Double ->
    Double ->
    Double ->
    Ptr ->
    Ptr

foreign import ccall unsafe "opensolid_expression_cubic_spline"
  opensolid_expression_cubic_spline ::
    Double ->
    Double ->
    Double ->
    Double ->
    Ptr ->
    Ptr

foreign import ccall unsafe "opensolid_expression_quartic_spline"
  opensolid_expression_quartic_spline ::
    Double ->
    Double ->
    Double ->
    Double ->
    Double ->
    Ptr ->
    Ptr

foreign import ccall unsafe "opensolid_expression_quintic_spline"
  opensolid_expression_quintic_spline ::
    Double ->
    Double ->
    Double ->
    Double ->
    Double ->
    Double ->
    Ptr ->
    Ptr

foreign import ccall unsafe "opensolid_expression_bezier_curve"
  opensolid_expression_bezier_curve ::
    Int64 ->
    Foreign.Ptr (Qty units) ->
    Ptr ->
    Ptr

-- TODO attach a finalizer to the returned Ptr value,
-- to delete the underlying Rust value?
ptr :: Scalar input -> Ptr
ptr expression = case expression of
  CurveParameter -> opensolid_expression_argument (Int.toInt64 0)
  SurfaceParameter SurfaceParameter.U -> opensolid_expression_argument (Int.toInt64 0)
  SurfaceParameter SurfaceParameter.V -> opensolid_expression_argument (Int.toInt64 1)
  Constant (Qty value) -> opensolid_expression_constant value
  Negated arg -> opensolid_expression_negate (ptr arg)
  Sum lhs rhs -> opensolid_expression_sum (ptr lhs) (ptr rhs)
  Difference lhs rhs -> opensolid_expression_difference (ptr lhs) (ptr rhs)
  Product lhs rhs -> opensolid_expression_product (ptr lhs) (ptr rhs)
  Quotient lhs rhs -> opensolid_expression_quotient (ptr lhs) (ptr rhs)
  Squared arg -> opensolid_expression_squared (ptr arg)
  Sqrt arg -> opensolid_expression_sqrt (ptr arg)
  Sin arg -> opensolid_expression_sin (ptr arg)
  Cos arg -> opensolid_expression_cos (ptr arg)
  QuadraticSpline (Qty p1) (Qty p2) (Qty p3) param ->
    opensolid_expression_quadratic_spline p1 p2 p3 (ptr param)
  CubicSpline (Qty p1) (Qty p2) (Qty p3) (Qty p4) param ->
    opensolid_expression_cubic_spline p1 p2 p3 p4 (ptr param)
  QuarticSpline (Qty p1) (Qty p2) (Qty p3) (Qty p4) (Qty p5) param ->
    opensolid_expression_quartic_spline p1 p2 p3 p4 p5 (ptr param)
  QuinticSpline (Qty p1) (Qty p2) (Qty p3) (Qty p4) (Qty p5) (Qty p6) param ->
    opensolid_expression_quintic_spline p1 p2 p3 p4 p5 p6 (ptr param)
  BezierCurve controlPoints param -> unsafeDupablePerformIO $ IO.do
    let numControlPoints = Int.toInt64 (NonEmpty.length controlPoints)
    Foreign.Marshal.Array.withArray (NonEmpty.toList controlPoints) $ \arrayPtr ->
      IO.succeed (opensolid_expression_bezier_curve numControlPoints arrayPtr (ptr param))
