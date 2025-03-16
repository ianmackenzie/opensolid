module OpenSolid.Ast
  ( Ast1d
  , zero1d
  , zero2d
  , constant1d
  , constant2d
  , curveParameter
  , surfaceParameter
  , line1d
  , quadraticSpline1d
  , cubicSpline1d
  , quarticSpline1d
  , quinticSpline1d
  , bezierCurve1d
  , uComponent
  , vComponent
  )
where

import OpenSolid.Float qualified as Float
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (SurfaceParameter, UvPoint)
import OpenSolid.Units qualified as Units

data Ast1d input where
  Constant1d :: Float -> Ast1d input
  CurveParameter :: Ast1d Float
  SurfaceParameter :: SurfaceParameter -> Ast1d UvPoint
  Negated1d :: Ast1d input -> Ast1d input
  Sum1d :: Ast1d input -> Ast1d input -> Ast1d input
  Difference1d :: Ast1d input -> Ast1d input -> Ast1d input
  Squared :: Ast1d input -> Ast1d input
  Product1d :: Ast1d input -> Ast1d input -> Ast1d input
  Quotient1d :: Ast1d input -> Ast1d input -> Ast1d input
  Sqrt :: Ast1d input -> Ast1d input
  Sin :: Ast1d input -> Ast1d input
  Cos :: Ast1d input -> Ast1d input
  QuadraticSpline1d :: Float -> Float -> Float -> Ast1d input -> Ast1d input
  CubicSpline1d :: Float -> Float -> Float -> Float -> Ast1d input -> Ast1d input
  QuarticSpline1d :: Float -> Float -> Float -> Float -> Float -> Ast1d input -> Ast1d input
  QuinticSpline1d :: Float -> Float -> Float -> Float -> Float -> Float -> Ast1d input -> Ast1d input
  BezierCurve1d :: NonEmpty Float -> Ast1d input -> Ast1d input
  UComponent :: Ast2d input -> Ast1d input
  VComponent :: Ast2d input -> Ast1d input

deriving instance Eq (Ast1d input)

deriving instance Ord (Ast1d input)

deriving instance Show (Ast1d input)

data Ast2d input where
  Constant2d :: Float -> Float -> Ast2d input
  UV :: Ast1d input -> Ast1d input -> Ast2d input

deriving instance Eq (Ast2d input)

deriving instance Ord (Ast2d input)

deriving instance Show (Ast2d input)

instance Composition (Ast1d input) (Ast1d Float) (Ast1d input) where
  Constant1d value . _ = Constant1d value
  CurveParameter . input = input
  Negated1d arg . input = negate (arg . input)
  Sum1d lhs rhs . input = (lhs . input) + (rhs . input)
  Difference1d lhs rhs . input = (lhs . input) - (rhs . input)
  Product1d lhs rhs . input = (lhs . input) * (rhs . input)
  Quotient1d lhs rhs . input = (lhs . input) / (rhs . input)
  Squared arg . input = squared (arg . input)
  Sqrt arg . input = sqrt (arg . input)
  Sin arg . input = sin (arg . input)
  Cos arg . input = cos (arg . input)
  QuadraticSpline1d p1 p2 p3 param . input = QuadraticSpline1d p1 p2 p3 (param . input)
  CubicSpline1d p1 p2 p3 p4 param . input = CubicSpline1d p1 p2 p3 p4 (param . input)
  QuarticSpline1d p1 p2 p3 p4 p5 param . input = QuarticSpline1d p1 p2 p3 p4 p5 (param . input)
  QuinticSpline1d p1 p2 p3 p4 p5 p6 param . input = QuinticSpline1d p1 p2 p3 p4 p5 p6 (param . input)
  BezierCurve1d controlPoints param . input = BezierCurve1d controlPoints (param . input)
  UComponent ast2d . input = uComponent (ast2d . input)
  VComponent ast2d . input = vComponent (ast2d . input)

instance Composition (Ast1d input) (Ast2d Float) (Ast2d input) where
  Constant2d u v . _ = Constant2d u v
  UV u v . input = UV (u . input) (v . input)

zero1d :: Ast1d input
zero1d = constant1d 0.0

zero2d :: Ast2d input
zero2d = constant2d 0.0 0.0

constant1d :: Qty units -> Ast1d input
constant1d value = Constant1d (Units.coerce value)

constant2d :: Qty units -> Qty units -> Ast2d input
constant2d u v = Constant2d (Units.coerce u) (Units.coerce v)

curveParameter :: Ast1d Float
curveParameter = CurveParameter

surfaceParameter :: SurfaceParameter -> Ast1d UvPoint
surfaceParameter = SurfaceParameter

instance Negation (Ast1d input) where
  -- TODO special cases for spline types
  negate (Constant1d value) = Constant1d (negate value)
  negate (Negated1d arg) = arg
  negate (Difference1d lhs rhs) = Difference1d rhs lhs
  negate ast = Negated1d ast

instance Multiplication Sign (Ast1d input) (Ast1d input) where
  Positive * ast = ast
  Negative * ast = -ast

instance Multiplication (Ast1d input) Sign (Ast1d input) where
  ast * Positive = ast
  ast * Negative = -ast

instance
  input1 ~ input2 =>
  Addition (Ast1d input1) (Ast1d input2) (Ast1d input1)
  where
  -- TODO special cases for spline types
  Constant1d a + Constant1d b = Constant1d (a + b)
  lhs + Constant1d 0.0 = lhs
  Constant1d 0.0 + rhs = rhs
  Constant1d a + Sum1d (Constant1d b) ast = (a + b) + ast
  Sum1d (Constant1d a) ast + Constant1d b = (a + b) + ast
  Constant1d a + Difference1d (Constant1d b) ast = (a + b) - ast
  Difference1d (Constant1d a) ast + Constant1d b = (a + b) - ast
  Constant1d a + Difference1d ast (Constant1d b) = ast + (a - b)
  Difference1d ast (Constant1d a) + Constant1d b = ast + (b - a)
  -- Canonicalize argument order using lexical ordering
  lhs + rhs = if lhs <= rhs then Sum1d lhs rhs else Sum1d rhs lhs

instance Addition (Qty units) (Ast1d input) (Ast1d input) where
  value + ast = constant1d value + ast

instance
  units1 ~ units2 =>
  Addition (Ast1d input1) (Qty units2) (Ast1d input1)
  where
  ast + value = ast + constant1d value

instance
  input1 ~ input2 =>
  Subtraction (Ast1d input1) (Ast1d input2) (Ast1d input1)
  where
  -- TODO special cases for spline types
  Constant1d a - Constant1d b = constant1d (a - b)
  lhs - Constant1d 0.0 = lhs
  Constant1d 0.0 - rhs = -rhs
  Constant1d a - Sum1d (Constant1d b) ast = (a - b) - ast
  Sum1d (Constant1d a) ast - Constant1d b = (a - b) + ast
  Constant1d a - Difference1d (Constant1d b) ast = (a - b) + ast
  Difference1d (Constant1d a) ast - Constant1d b = (a - b) - ast
  Constant1d a - Difference1d ast (Constant1d b) = (a + b) - ast
  Difference1d ast (Constant1d a) - Constant1d b = ast - (a + b)
  lhs - rhs = Difference1d lhs rhs

instance Subtraction (Qty units) (Ast1d input) (Ast1d input) where
  value - ast = constant1d value - ast

instance
  units1 ~ units2 =>
  Subtraction (Ast1d input1) (Qty units2) (Ast1d input1)
  where
  scalar - ast = scalar - constant1d ast

instance
  input1 ~ input2 =>
  Multiplication (Ast1d input1) (Ast1d input2) (Ast1d input1)
  where
  -- TODO special cases for spline types
  Constant1d a * Constant1d b = constant1d (a * b)
  _ * Constant1d 0.0 = zero1d
  Constant1d 0.0 * _ = zero1d
  lhs * Constant1d 1.0 = lhs
  Constant1d 1.0 * rhs = rhs
  lhs * Constant1d -1.0 = -lhs
  Constant1d -1.0 * rhs = -rhs
  Constant1d a * Negated1d ast = -a * ast
  Negated1d ast * Constant1d a = ast * -a
  Constant1d a * Product1d (Constant1d b) ast = (a * b) * ast
  Product1d (Constant1d a) ast * Constant1d b = (a * b) * ast
  Constant1d a * Quotient1d (Constant1d b) ast = (a * b) / ast
  Quotient1d (Constant1d a) ast * Constant1d b = (a * b) / ast
  -- Canonicalize argument order using lexical ordering
  lhs * rhs = if lhs <= rhs then Product1d lhs rhs else Product1d rhs lhs

instance Multiplication (Qty units) (Ast1d input) (Ast1d input) where
  value * scalar = constant1d value * scalar

instance Multiplication (Ast1d input) (Qty units) (Ast1d input) where
  scalar * value = scalar * constant1d value

instance input1 ~ input2 => Division (Ast1d input1) (Ast1d input2) (Ast1d input1) where
  Constant1d a / Constant1d b = constant1d (a / b)
  Constant1d 0.0 / _ = zero1d
  expression / Constant1d a = (1.0 / a) * expression
  expression / Quotient1d lhs rhs = expression * rhs / lhs
  lhs / rhs = Quotient1d lhs rhs

instance Division (Qty units) (Ast1d input) (Ast1d input) where
  value / scalar = constant1d value / scalar

instance Division (Ast1d input) (Qty units) (Ast1d input) where
  scalar / value = scalar / constant1d value

squared :: Ast1d input -> Ast1d input
squared (Constant1d value) = constant1d (Float.squared value)
squared (Negated1d arg) = squared arg
squared (Sqrt expression) = expression
squared expression = Squared expression

sqrt :: Ast1d input -> Ast1d input
sqrt (Constant1d value) = constant1d (Float.sqrt value)
sqrt expression = Sqrt expression

sin :: Ast1d input -> Ast1d input
sin (Constant1d value) = constant1d (Float.sin value)
sin expression = Sin expression

cos :: Ast1d input -> Ast1d input
cos (Constant1d value) = constant1d (Float.cos value)
cos (Negated1d expression) = cos expression
cos expression = Cos expression

line1d :: Qty units -> Qty units -> Ast1d input -> Ast1d input
line1d a b param = a + param * (b - a)

quadraticSpline1d :: Qty units -> Qty units -> Qty units -> Ast1d input -> Ast1d input
quadraticSpline1d p1 p2 p3 param =
  QuadraticSpline1d (Units.coerce p1) (Units.coerce p2) (Units.coerce p3) param

cubicSpline1d :: Qty units -> Qty units -> Qty units -> Qty units -> Ast1d input -> Ast1d input
cubicSpline1d p1 p2 p3 p4 param =
  CubicSpline1d (Units.coerce p1) (Units.coerce p2) (Units.coerce p3) (Units.coerce p4) param

quarticSpline1d ::
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Ast1d input ->
  Ast1d input
quarticSpline1d p1 p2 p3 p4 p5 param =
  QuarticSpline1d
    (Units.coerce p1)
    (Units.coerce p2)
    (Units.coerce p3)
    (Units.coerce p4)
    (Units.coerce p5)
    param

quinticSpline1d ::
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Ast1d input ->
  Ast1d input
quinticSpline1d p1 p2 p3 p4 p5 p6 param =
  QuinticSpline1d
    (Units.coerce p1)
    (Units.coerce p2)
    (Units.coerce p3)
    (Units.coerce p4)
    (Units.coerce p5)
    (Units.coerce p6)
    param

bezierCurve1d :: NonEmpty (Qty units) -> Ast1d input -> Ast1d input
bezierCurve1d controlPoints param = case controlPoints of
  NonEmpty.One value -> constant1d value
  NonEmpty.Two p1 p2 -> line1d p1 p2 param
  NonEmpty.Three p1 p2 p3 -> quadraticSpline1d p1 p2 p3 param
  NonEmpty.Four p1 p2 p3 p4 -> cubicSpline1d p1 p2 p3 p4 param
  NonEmpty.Five p1 p2 p3 p4 p5 -> quarticSpline1d p1 p2 p3 p4 p5 param
  NonEmpty.Six p1 p2 p3 p4 p5 p6 -> quinticSpline1d p1 p2 p3 p4 p5 p6 param
  NonEmpty.SevenOrMore -> BezierCurve1d (NonEmpty.map Units.coerce controlPoints) param

uComponent :: Ast2d input -> Ast1d input
uComponent (Constant2d u _) = constant1d u
uComponent (UV u _) = u

vComponent :: Ast2d input -> Ast1d input
vComponent (Constant2d _ v) = constant1d v
vComponent (UV _ v) = v
