module Expression.Scalar
  ( Scalar (Constant)
  , zero
  , constant
  , parameter
  , u
  , v
  , negated
  , sum
  , difference
  , product
  , quotient
  , squared
  , sqrt
  , sin
  , cos
  , quadraticSpline
  , cubicSpline
  , bezierCurve
  , curveDerivative
  , surfaceDerivative
  , show
  , showWithPrecedence
  , Ptr
  , ptr
  )
where

import Data.Int (Int64)
import Float qualified
import Foreign qualified
import Foreign.Marshal.Array qualified
import IO qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Qty (Qty (Qty))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Text qualified
import Units qualified
import Uv qualified
import Prelude (Double)

data Scalar input where
  Constant :: Float -> Scalar input
  Parameter :: Scalar Float
  U :: Scalar Uv.Point
  V :: Scalar Uv.Point
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
  BezierCurve :: NonEmpty Float -> Scalar input -> Scalar input

deriving instance Eq (Scalar input)

deriving instance Ord (Scalar input)

instance Composition (Scalar input) (Scalar Float) (Scalar input) where
  Constant value . _ = constant value
  Parameter . input = input
  Negated arg . input = negated (arg . input)
  Sum lhs rhs . input = sum (lhs . input) (rhs . input)
  Difference lhs rhs . input = difference (lhs . input) (rhs . input)
  Product lhs rhs . input = product (lhs . input) (rhs . input)
  Quotient lhs rhs . input = quotient (lhs . input) (rhs . input)
  Squared arg . input = squared (arg . input)
  Sqrt arg . input = sqrt (arg . input)
  Sin arg . input = sin (arg . input)
  Cos arg . input = cos (arg . input)
  QuadraticSpline p1 p2 p3 param . input = QuadraticSpline p1 p2 p3 (param . input)
  CubicSpline p1 p2 p3 p4 param . input = CubicSpline p1 p2 p3 p4 (param . input)
  BezierCurve controlPoints param . input = BezierCurve controlPoints (param . input)

instance
  input1 ~ input2 =>
  Composition
    (Scalar input1, Scalar input2)
    (Scalar Uv.Point)
    (Scalar input1)
  where
  Constant value . _ = constant value
  U . (input, _) = input
  V . (_, input) = input
  Negated arg . inputs = negated (arg . inputs)
  Sum lhs rhs . inputs = sum (lhs . inputs) (rhs . inputs)
  Difference lhs rhs . inputs = difference (lhs . inputs) (rhs . inputs)
  Product lhs rhs . inputs = product (lhs . inputs) (rhs . inputs)
  Quotient lhs rhs . inputs = quotient (lhs . inputs) (rhs . inputs)
  Squared arg . inputs = squared (arg . inputs)
  Sqrt arg . inputs = sqrt (arg . inputs)
  Sin arg . inputs = sin (arg . inputs)
  Cos arg . inputs = cos (arg . inputs)
  QuadraticSpline p1 p2 p3 param . inputs = QuadraticSpline p1 p2 p3 (param . inputs)
  CubicSpline p1 p2 p3 p4 param . inputs = CubicSpline p1 p2 p3 p4 (param . inputs)
  BezierCurve controlPoints param . inputs = BezierCurve controlPoints (param . inputs)

zero :: Scalar input
zero = constant 0.0

one :: Scalar input
one = constant 1.0

constant :: Qty units -> Scalar input
constant value = Constant (Units.coerce value)

parameter :: Scalar Float
parameter = Parameter

u :: Scalar Uv.Point
u = U

v :: Scalar Uv.Point
v = V

negated :: Scalar input -> Scalar input
negated (Constant value) = Constant (negate value)
negated (Negated expression) = expression
negated (Difference lhs rhs) = Difference rhs lhs
negated expression = Negated expression

sum :: Scalar input -> Scalar input -> Scalar input
sum (Constant a) (Constant b) = Constant (a + b)
sum expression (Constant 0.0) = expression -- x + 0 = x
sum (Constant 0.0) expression = expression -- 0 + x = x
sum (Constant a) (Sum (Constant b) expression) = Sum (Constant (a + b)) expression -- a + (b + x) = (a + b) + x
sum (Sum (Constant a) expression) (Constant b) = Sum (Constant (a + b)) expression -- (a + x) + b = (a + b) + x
sum (Constant a) (Difference (Constant b) expression) = Difference (Constant (a + b)) expression -- a + (b - x) = (a + b) - x
sum (Difference (Constant a) expression) (Constant b) = Difference (Constant (a + b)) expression -- (a - x) + b = (a + b) - x
sum (Constant a) (Difference expression (Constant b)) = Sum (Constant (a - b)) expression -- a + (x - b) = (a - b) + x
sum (Difference expression (Constant a)) (Constant b) = Sum (Constant (b - a)) expression -- (x - a) + b = (b - a) + x
sum lhs rhs = if lhs <= rhs then Sum lhs rhs else Sum rhs lhs -- Canonicalize argument order using lexical ordering

difference :: Scalar input -> Scalar input -> Scalar input
difference (Constant a) (Constant b) = constant (a - b)
difference expression (Constant 0.0) = expression -- x - 0 = x
difference (Constant 0.0) expression = negated expression -- 0 - x = -x
difference (Constant a) (Sum (Constant b) expression) = difference (constant (a - b)) expression -- a - (b + x) = (a - b) - x
difference (Sum (Constant a) expression) (Constant b) = sum (constant (a - b)) expression -- (a + x) - b = (a - b) + x
difference (Constant a) (Difference (Constant b) expression) = sum (constant (a - b)) expression -- a - (b - x) = (a - b) + x
difference (Difference (Constant a) expression) (Constant b) = difference (constant (a - b)) expression -- (a - x) - b = (a - b) - x
difference (Constant a) (Difference expression (Constant b)) = difference (constant (a + b)) expression -- a - (x - b) = (a + b) - x
difference (Difference expression (Constant a)) (Constant b) = difference expression (constant (a + b)) -- (x - a) - b = x - (a + b)
difference lhs rhs = Difference lhs rhs

product :: Scalar input -> Scalar input -> Scalar input
product (Constant a) (Constant b) = constant (a * b)
product _ (Constant 0.0) = zero
product (Constant 0.0) _ = zero
product expression (Constant 1.0) = expression
product (Constant 1.0) expression = expression
product expression (Constant -1.0) = negated expression
product (Constant -1.0) expression = negated expression
product (Constant a) (Negated expression) = product (Constant -a) expression
product (Negated expression) (Constant a) = product expression (Constant -a)
product (Constant a) (Product (Constant b) expression) = product (constant (a * b)) expression -- a * (b * x) = (a * b) * x
product (Product (Constant a) expression) (Constant b) = product (constant (a * b)) expression -- (a * x) * b = (a * b) * x
product (Constant a) (Quotient (Constant b) expression) = quotient (constant (a * b)) expression -- a * (b / x) = (a * b) / x
product (Quotient (Constant a) expression) (Constant b) = quotient (constant (a * b)) expression -- (a / x) * b = (a * b) / x
product lhs rhs = if lhs <= rhs then Product lhs rhs else Product rhs lhs -- Canonicalize argument order using lexical ordering

twice :: Scalar input -> Scalar input
twice = product (constant 2.0)

half :: Scalar input -> Scalar input
half = product (constant 0.5)

quotient :: Scalar input -> Scalar input -> Scalar input
quotient (Constant a) (Constant b) = constant (a / b)
quotient (Constant 0.0) _ = zero
quotient expression (Constant a) = product (constant (1.0 / a)) expression
quotient expression (Quotient lhs rhs) = product expression (quotient rhs lhs)
quotient lhs rhs = Quotient lhs rhs

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
line a b param = sum (constant a) (product param (constant (b - a)))

quadraticSpline :: Qty units -> Qty units -> Qty units -> Scalar input -> Scalar input
quadraticSpline p1 p2 p3 param =
  QuadraticSpline (Units.coerce p1) (Units.coerce p2) (Units.coerce p3) param

cubicSpline :: Qty units -> Qty units -> Qty units -> Qty units -> Scalar input -> Scalar input
cubicSpline p1 p2 p3 p4 param =
  CubicSpline (Units.coerce p1) (Units.coerce p2) (Units.coerce p3) (Units.coerce p4) param

bezierCurve :: NonEmpty (Qty units) -> Scalar input -> Scalar input
bezierCurve (NonEmpty.One value) _ = constant value
bezierCurve (NonEmpty.Two p1 p2) param = sum (constant p1) (product param (constant (p2 - p1)))
bezierCurve (NonEmpty.Three p1 p2 p3) param = quadraticSpline p1 p2 p3 param
bezierCurve (NonEmpty.Four p1 p2 p3 p4) param = cubicSpline p1 p2 p3 p4 param
bezierCurve controlPoints param = BezierCurve (NonEmpty.map Units.coerce controlPoints) param

curveDerivative :: Scalar Float -> Scalar Float
curveDerivative expression = case expression of
  Constant _ -> zero
  Parameter -> one
  Negated arg -> negated (curveDerivative arg)
  Sum lhs rhs -> sum (curveDerivative lhs) (curveDerivative rhs)
  Difference lhs rhs -> difference (curveDerivative lhs) (curveDerivative rhs)
  Squared arg -> twice (product (curveDerivative arg) arg)
  Product lhs rhs -> sum (product (curveDerivative lhs) rhs) (product lhs (curveDerivative rhs))
  Quotient lhs rhs ->
    quotient
      (difference (product (curveDerivative lhs) rhs) (product lhs (curveDerivative rhs)))
      (squared rhs)
  Sqrt arg -> quotient (half (curveDerivative arg)) expression
  Sin arg -> product (curveDerivative arg) (cos arg)
  Cos arg -> product (negated (curveDerivative arg)) (sin arg)
  QuadraticSpline p1 p2 p3 param -> do
    let d1 = 2.0 * (p2 - p1)
    let d2 = 2.0 * (p3 - p2)
    product (line d1 d2 param) (curveDerivative param)
  CubicSpline p1 p2 p3 p4 param -> do
    let d1 = 3.0 * (p2 - p1)
    let d2 = 3.0 * (p3 - p2)
    let d3 = 3.0 * (p4 - p3)
    product (quadraticSpline d1 d2 d3 param) (curveDerivative param)
  BezierCurve controlPoints param -> do
    let n = Float.int (NonEmpty.length controlPoints)
    let scaledDifference p1 p2 = (n - 1) * (p2 - p1)
    let scaledDifferences = NonEmpty.successive scaledDifference controlPoints
    case scaledDifferences of
      [] -> zero
      NonEmpty derivativeControlPoints ->
        product (bezierCurve derivativeControlPoints param) (curveDerivative param)

surfaceDerivative :: Uv.Parameter -> Scalar Uv.Point -> Scalar Uv.Point
surfaceDerivative p expression = case expression of
  Constant _ -> zero
  U -> if p == Uv.U then one else zero
  V -> if p == Uv.V then one else zero
  Negated arg -> negated (surfaceDerivative p arg)
  Sum lhs rhs -> sum (surfaceDerivative p lhs) (surfaceDerivative p rhs)
  Difference lhs rhs -> difference (surfaceDerivative p lhs) (surfaceDerivative p rhs)
  Squared arg -> twice (product (surfaceDerivative p arg) arg)
  Product lhs rhs ->
    sum (product (surfaceDerivative p lhs) rhs) (product lhs (surfaceDerivative p rhs))
  Quotient lhs rhs ->
    quotient
      (difference (product (surfaceDerivative p lhs) rhs) (product lhs (surfaceDerivative p rhs)))
      (squared rhs)
  Sqrt arg -> quotient (half (surfaceDerivative p arg)) expression
  Sin arg -> product (surfaceDerivative p arg) (cos arg)
  Cos arg -> product (negated (surfaceDerivative p arg)) (sin arg)
  QuadraticSpline p1 p2 p3 param -> do
    let d1 = 2.0 * (p2 - p1)
    let d2 = 2.0 * (p3 - p2)
    product (line d1 d2 param) (surfaceDerivative p param)
  CubicSpline p1 p2 p3 p4 param -> do
    let d1 = 3.0 * (p2 - p1)
    let d2 = 3.0 * (p3 - p2)
    let d3 = 3.0 * (p4 - p3)
    product (quadraticSpline d1 d2 d3 param) (surfaceDerivative p param)
  BezierCurve controlPoints param -> do
    let n = Float.int (NonEmpty.length controlPoints)
    let scaledDifference p1 p2 = (n - 1) * (p2 - p1)
    let scaledDifferences = NonEmpty.successive scaledDifference controlPoints
    case scaledDifferences of
      [] -> zero
      NonEmpty derivativeControlPoints ->
        product (bezierCurve derivativeControlPoints param) (surfaceDerivative p param)

show :: Scalar input -> Text
show = showWithPrecedence 0

showWithPrecedence :: Int -> Scalar input -> Text
showWithPrecedence precedence expression = case expression of
  Parameter -> "t"
  U -> "u"
  V -> "v"
  Constant value -> Text.float value
  Negated arg -> showParenthesized (precedence >= 6) ("-" + showWithPrecedence 6 arg)
  Sum lhs rhs -> showParenthesized (precedence >= 6) (showWithPrecedence 6 lhs + " + " + showWithPrecedence 6 rhs)
  Difference lhs rhs -> showParenthesized (precedence >= 6) (showWithPrecedence 6 lhs + " - " + showWithPrecedence 6 rhs)
  Product lhs rhs -> showParenthesized (precedence >= 7) (showWithPrecedence 7 lhs + " * " + showWithPrecedence 7 rhs)
  Quotient lhs rhs -> showParenthesized (precedence >= 7) (showWithPrecedence 7 lhs + " / " + showWithPrecedence 6 rhs)
  Squared arg -> showFunctionCall precedence "squared" [arg]
  Sqrt arg -> showFunctionCall precedence "sqrt" [arg]
  Sin arg -> showFunctionCall precedence "sin" [arg]
  Cos arg -> showFunctionCall precedence "cos" [arg]
  QuadraticSpline p1 p2 p3 param -> do
    let args = [constant p1, constant p2, constant p3, param]
    showFunctionCall precedence "quadraticSpline" args
  CubicSpline p1 p2 p3 p4 param -> do
    let args = [constant p1, constant p2, constant p3, constant p4, param]
    showFunctionCall precedence "cubicSpline" args
  BezierCurve controlPoints param ->
    showParenthesized (precedence >= 10) $
      "bezierCurve " + Text.show (NonEmpty.toList controlPoints) + " " + showWithPrecedence 10 param

showFunctionCall :: Int -> Text -> List (Scalar input) -> Text
showFunctionCall precedence functionName arguments =
  showParenthesized (precedence >= 10) $
    Text.join " " (functionName : List.map (showWithPrecedence 10) arguments)

showParenthesized :: Bool -> Text -> Text
showParenthesized True text = "(" + text + ")"
showParenthesized False text = text

data Scalar#

type Ptr = Foreign.Ptr Scalar#

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
  opensolid_expression_quadratic_spline :: Double -> Double -> Double -> Ptr -> Ptr

foreign import ccall unsafe "opensolid_expression_cubic_spline"
  opensolid_expression_cubic_spline :: Double -> Double -> Double -> Double -> Ptr -> Ptr

foreign import ccall unsafe "opensolid_expression_bezier_curve"
  opensolid_expression_bezier_curve :: Int64 -> Foreign.Ptr (Qty units) -> Ptr -> Ptr

-- TODO attach a finalizer to the returned Ptr value,
-- to delete the underlying Rust value?
ptr :: Scalar input -> Ptr
ptr expression = case expression of
  Parameter -> opensolid_expression_argument (fromIntegral 0)
  U -> opensolid_expression_argument (fromIntegral 0)
  V -> opensolid_expression_argument (fromIntegral 1)
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
  BezierCurve controlPoints param -> unsafeDupablePerformIO $ IO.do
    let numControlPoints = fromIntegral (NonEmpty.length controlPoints)
    Foreign.Marshal.Array.withArray (NonEmpty.toList controlPoints) $ \arrayPtr ->
      IO.succeed (opensolid_expression_bezier_curve numControlPoints arrayPtr (ptr param))
