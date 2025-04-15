module OpenSolid.Bytecode.Ast
  ( Ast1d
  , Ast2d
  , constant1d
  , constant2d
  , curveParameter
  , surfaceParameter
  , xComponent2d
  , yComponent2d
  , squared
  , sqrt
  , sin
  , cos
  , line1d
  , quadraticSpline1d
  , cubicSpline1d
  , quarticSpline1d
  , quinticSpline1d
  , bezierCurve1d
  , compileCurve1d
  )
where

import OpenSolid.Bytecode.Compilation qualified as Compilation
import OpenSolid.Bytecode.Instruction (VariableIndex (VariableIndex))
import OpenSolid.Bytecode.Instruction qualified as Instruction
import OpenSolid.Float qualified as Float
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvPoint)
import OpenSolid.Units qualified as Units

data Ast1d input where
  Constant1d :: Float -> Ast1d input
  Variable1d :: Variable1d input -> Ast1d input

deriving instance Eq (Ast1d input)

deriving instance Ord (Ast1d input)

deriving instance Show (Ast1d input)

data Variable1d input where
  CurveParameter :: Variable1d Float
  SurfaceParameter :: SurfaceParameter -> Variable1d UvPoint
  XComponent2d :: Variable2d input -> Variable1d input
  YComponent2d :: Variable2d input -> Variable1d input
  Negated1d :: Variable1d input -> Variable1d input
  Sum1d :: Variable1d input -> Variable1d input -> Variable1d input
  SumVariableConstant1d :: Variable1d input -> Float -> Variable1d input
  Difference1d :: Variable1d input -> Variable1d input -> Variable1d input
  DifferenceConstantVariable1d :: Float -> Variable1d input -> Variable1d input
  Product1d :: Variable1d input -> Variable1d input -> Variable1d input
  ProductVariableConstant1d :: Variable1d input -> Float -> Variable1d input
  Quotient1d :: Variable1d input -> Variable1d input -> Variable1d input
  QuotientConstantVariable1d :: Float -> Variable1d input -> Variable1d input
  Squared1d :: Variable1d input -> Variable1d input
  Sqrt1d :: Variable1d input -> Variable1d input
  Sin1d :: Variable1d input -> Variable1d input
  Cos1d :: Variable1d input -> Variable1d input
  BezierCurve1d :: NonEmpty Float -> Variable1d input -> Variable1d input
  SquaredNorm2d :: Variable2d input -> Variable1d input
  Norm2d :: Variable2d input -> Variable1d input
  Dot2d :: Variable2d input -> Variable2d input -> Variable1d input
  DotVariableConstant2d :: Variable2d input -> (Float, Float) -> Variable1d input
  Cross2d :: Variable2d input -> Variable2d input -> Variable1d input
  CrossVariableConstant2d :: Variable2d input -> (Float, Float) -> Variable1d input

deriving instance Eq (Variable1d input)

deriving instance Ord (Variable1d input)

deriving instance Show (Variable1d input)

data Ast2d input where
  Constant2d :: (Float, Float) -> Ast2d input
  Variable2d :: Variable2d input -> Ast2d input

deriving instance Eq (Ast2d input)

deriving instance Ord (Ast2d input)

deriving instance Show (Ast2d input)

data Variable2d input where
  XY2d :: Variable1d input -> Variable1d input -> Variable2d input
  XC2d :: Variable1d input -> Float -> Variable2d input
  CY2d :: Float -> Variable1d input -> Variable2d input
  Negated2d :: Variable2d input -> Variable2d input
  Sum2d :: Variable2d input -> Variable2d input -> Variable2d input
  SumVariableConstant2d :: Variable2d input -> (Float, Float) -> Variable2d input
  Difference2d :: Variable2d input -> Variable2d input -> Variable2d input
  DifferenceConstantVariable2d :: (Float, Float) -> Variable2d input -> Variable2d input
  Product2d :: Variable2d input -> Variable1d input -> Variable2d input
  ProductVariableConstant2d :: Variable2d input -> Float -> Variable2d input
  ProductConstantVariable2d :: (Float, Float) -> Variable1d input -> Variable2d input
  Quotient2d :: Variable2d input -> Variable1d input -> Variable2d input
  QuotientConstantVariable2d :: (Float, Float) -> Variable1d input -> Variable2d input
  BezierCurve2d :: NonEmpty (Float, Float) -> Variable1d input -> Variable2d input

deriving instance Eq (Variable2d input)

deriving instance Ord (Variable2d input)

deriving instance Show (Variable2d input)

data Ast3d input where
  Constant3d :: (Float, Float, Float) -> Ast3d input
  Variable3d :: Variable3d input -> Ast3d input

deriving instance Eq (Ast3d input)

deriving instance Ord (Ast3d input)

deriving instance Show (Ast3d input)

data Variable3d input where
  XYZ3d :: Variable1d input -> Variable1d input -> Variable1d input -> Variable3d input
  XYC3d :: Variable1d input -> Variable1d input -> Float -> Variable3d input
  XCZ3d :: Variable1d input -> Float -> Variable1d input -> Variable3d input
  CYZ3d :: Float -> Variable1d input -> Variable1d input -> Variable3d input
  XCC3d :: Variable1d input -> Float -> Float -> Variable3d input
  CYC3d :: Float -> Variable1d input -> Float -> Variable3d input
  CCZ3d :: Float -> Float -> Variable1d input -> Variable3d input
  Negated3d :: Variable3d input -> Variable3d input
  Sum3d :: Variable3d input -> Variable3d input -> Variable3d input
  SumVariableConstant3d :: Variable3d input -> (Float, Float, Float) -> Variable3d input
  Difference3d :: Variable3d input -> Variable3d input -> Variable3d input
  DifferenceConstantVariable3d :: (Float, Float, Float) -> Variable3d input -> Variable3d input
  Product3d :: Variable3d input -> Variable1d input -> Variable3d input
  ProductVariableConstant3d :: Variable3d input -> Float -> Variable3d input
  ProductConstantVariable3d :: (Float, Float, Float) -> Variable1d input -> Variable3d input
  Quotient3d :: Variable3d input -> Variable1d input -> Variable3d input
  QuotientConstantVariable3d :: (Float, Float, Float) -> Variable1d input -> Variable3d input
  Cross3d :: Variable3d input -> Variable3d input -> Variable3d input
  CrossVariableConstant3d :: Variable3d input -> (Float, Float, Float) -> Variable3d input
  BezierCurve3d :: NonEmpty (Float, Float, Float) -> Variable1d input -> Variable3d input

deriving instance Eq (Variable3d input)

deriving instance Ord (Variable3d input)

deriving instance Show (Variable3d input)

instance Composition (Ast1d input) (Ast1d Float) (Ast1d input) where
  Constant1d outer . _ = Constant1d outer
  Variable1d outer . Variable1d inner = Variable1d (outer . inner)
  outer . Constant1d inner = let (f, _) = compileCurve1d outer in Constant1d (f inner)

instance Composition (Variable1d input) (Variable1d Float) (Variable1d input) where
  CurveParameter . input = input
  XComponent2d arg . input = XComponent2d (arg . input)
  YComponent2d arg . input = YComponent2d (arg . input)
  Negated1d arg . input = Negated1d (arg . input)
  Sum1d lhs rhs . input = Sum1d (lhs . input) (rhs . input)
  SumVariableConstant1d lhs rhs . input = SumVariableConstant1d (lhs . input) rhs
  Difference1d lhs rhs . input = Difference1d (lhs . input) (rhs . input)
  DifferenceConstantVariable1d lhs rhs . input = DifferenceConstantVariable1d lhs (rhs . input)
  Product1d lhs rhs . input = Product1d (lhs . input) (rhs . input)
  ProductVariableConstant1d lhs rhs . input = ProductVariableConstant1d (lhs . input) rhs
  Quotient1d lhs rhs . input = Quotient1d (lhs . input) (rhs . input)
  QuotientConstantVariable1d lhs rhs . input = QuotientConstantVariable1d lhs (rhs . input)
  Squared1d arg . input = Squared1d (arg . input)
  Sqrt1d arg . input = Sqrt1d (arg . input)
  Sin1d arg . input = Sin1d (arg . input)
  Cos1d arg . input = Cos1d (arg . input)
  BezierCurve1d controlPoints param . input = BezierCurve1d controlPoints (param . input)
  SquaredNorm2d arg . input = SquaredNorm2d (arg . input)
  Norm2d arg . input = Norm2d (arg . input)
  Dot2d lhs rhs . input = Dot2d (lhs . input) (rhs . input)
  DotVariableConstant2d lhs rhs . input = DotVariableConstant2d (lhs . input) rhs
  Cross2d lhs rhs . input = Cross2d (lhs . input) (rhs . input)
  CrossVariableConstant2d lhs rhs . input = CrossVariableConstant2d (lhs . input) rhs

instance Composition (Ast1d input) (Ast2d Float) (Ast2d input) where
  Constant2d outer . _ = Constant2d outer
  Variable2d outer . Variable1d inner = Variable2d (outer . inner)
  outer . Constant1d inner = let (f, _) = compileCurve2d outer in Constant2d (f inner)

instance Composition (Variable1d input) (Variable2d Float) (Variable2d input) where
  XY2d x y . input = XY2d (x . input) (y . input)
  XC2d x y . input = XC2d (x . input) y
  CY2d x y . input = CY2d x (y . input)
  Negated2d arg . input = Negated2d (arg . input)
  Sum2d lhs rhs . input = Sum2d (lhs . input) (rhs . input)
  SumVariableConstant2d lhs rhs . input = SumVariableConstant2d (lhs . input) rhs
  Difference2d lhs rhs . input = Difference2d (lhs . input) (rhs . input)
  DifferenceConstantVariable2d lhs rhs . input = DifferenceConstantVariable2d lhs (rhs . input)
  Product2d lhs rhs . input = Product2d (lhs . input) (rhs . input)
  ProductVariableConstant2d lhs rhs . input = ProductVariableConstant2d (lhs . input) rhs
  ProductConstantVariable2d lhs rhs . input = ProductConstantVariable2d lhs (rhs . input)
  Quotient2d lhs rhs . input = Quotient2d (lhs . input) (rhs . input)
  QuotientConstantVariable2d lhs rhs . input = QuotientConstantVariable2d lhs (rhs . input)
  BezierCurve2d controlPoints param . input = BezierCurve2d controlPoints (param . input)

instance Composition (Ast1d input) (Ast3d Float) (Ast3d input) where
  Constant3d outer . _ = Constant3d outer
  Variable3d outer . Variable1d inner = Variable3d (outer . inner)
  outer . Constant1d inner = let (f, _) = compileCurve3d outer in Constant3d (f inner)

instance Composition (Variable1d input) (Variable3d Float) (Variable3d input) where
  XYZ3d x y z . input = XYZ3d (x . input) (y . input) (z . input)
  XYC3d x y z . input = XYC3d (x . input) (y . input) z
  XCZ3d x y z . input = XCZ3d (x . input) y (z . input)
  CYZ3d x y z . input = CYZ3d x (y . input) (z . input)
  XCC3d x y z . input = XCC3d (x . input) y z
  CYC3d x y z . input = CYC3d x (y . input) z
  CCZ3d x y z . input = CCZ3d x y (z . input)
  Negated3d arg . input = Negated3d (arg . input)
  Sum3d lhs rhs . input = Sum3d (lhs . input) (rhs . input)
  SumVariableConstant3d lhs rhs . input = SumVariableConstant3d (lhs . input) rhs
  Difference3d lhs rhs . input = Difference3d (lhs . input) (rhs . input)
  DifferenceConstantVariable3d lhs rhs . input = DifferenceConstantVariable3d lhs (rhs . input)
  Product3d lhs rhs . input = Product3d (lhs . input) (rhs . input)
  ProductVariableConstant3d lhs rhs . input = ProductVariableConstant3d (lhs . input) rhs
  ProductConstantVariable3d lhs rhs . input = ProductConstantVariable3d lhs (rhs . input)
  Quotient3d lhs rhs . input = Quotient3d (lhs . input) (rhs . input)
  QuotientConstantVariable3d lhs rhs . input = QuotientConstantVariable3d lhs (rhs . input)
  BezierCurve3d controlPoints param . input = BezierCurve3d controlPoints (param . input)
  Cross3d lhs rhs . input = Cross3d (lhs . input) (rhs . input)
  CrossVariableConstant3d lhs rhs . input = CrossVariableConstant3d (lhs . input) rhs

constant1d :: Qty units -> Ast1d input
constant1d value = Constant1d (Units.coerce value)

constant2d :: Qty units -> Qty units -> Ast2d input
constant2d x y = Constant2d (Units.coerce x, Units.coerce y)

curveParameter :: Ast1d Float
curveParameter = Variable1d CurveParameter

surfaceParameter :: SurfaceParameter -> Ast1d UvPoint
surfaceParameter = Variable1d . SurfaceParameter

xComponent2d :: Ast2d input -> Ast1d input
xComponent2d (Constant2d (x, _)) = Constant1d x
xComponent2d (Variable2d var) = Variable1d (XComponent2d var)

yComponent2d :: Ast2d input -> Ast1d input
yComponent2d (Constant2d (_, y)) = Constant1d y
yComponent2d (Variable2d var) = Variable1d (YComponent2d var)

instance Negation (Ast1d input) where
  negate (Constant1d val) = Constant1d -val
  negate (Variable1d var) = Variable1d -var

instance Negation (Variable1d input) where
  negate (Negated1d arg) = arg
  negate (Difference1d lhs rhs) = Difference1d rhs lhs
  negate var = Negated1d var

instance Multiplication Sign (Ast1d input) (Ast1d input) where
  Positive * ast = ast
  Negative * ast = -ast

instance Multiplication (Ast1d input) Sign (Ast1d input) where
  ast * Positive = ast
  ast * Negative = -ast

instance Multiplication Sign (Variable1d input) (Variable1d input) where
  Positive * var = var
  Negative * var = -var

instance Multiplication (Variable1d input) Sign (Variable1d input) where
  var * Positive = var
  var * Negative = -var

instance input1 ~ input2 => Addition (Ast1d input1) (Ast1d input2) (Ast1d input1) where
  Constant1d 0.0 + rhs = rhs
  lhs + Constant1d 0.0 = lhs
  Constant1d lhs + Constant1d rhs = Constant1d (lhs + rhs)
  Constant1d lhs + Variable1d rhs = Variable1d (SumVariableConstant1d rhs lhs)
  Variable1d lhs + Constant1d rhs = Variable1d (SumVariableConstant1d lhs rhs)
  Variable1d lhs + Variable1d rhs = Variable1d (lhs + rhs)

instance
  input1 ~ input2 =>
  Addition (Variable1d input1) (Variable1d input2) (Variable1d input1)
  where
  lhs + rhs = if lhs <= rhs then Sum1d lhs rhs else Sum1d rhs lhs

instance Addition (Qty units) (Ast1d input) (Ast1d input) where
  lhs + rhs = constant1d lhs + rhs

instance Addition (Ast1d input1) (Qty units) (Ast1d input1) where
  lhs + rhs = lhs + constant1d rhs

instance input1 ~ input2 => Subtraction (Ast1d input1) (Ast1d input2) (Ast1d input1) where
  lhs - Constant1d 0.0 = lhs
  Constant1d 0.0 - rhs = -rhs
  Constant1d lhs - Constant1d rhs = Constant1d (lhs - rhs)
  Constant1d lhs - Variable1d rhs = Variable1d (DifferenceConstantVariable1d lhs rhs)
  Variable1d lhs - Constant1d rhs = Variable1d (SumVariableConstant1d lhs -rhs)
  Variable1d lhs - Variable1d rhs = Variable1d (lhs - rhs)

instance
  input1 ~ input2 =>
  Subtraction (Variable1d input1) (Variable1d input2) (Variable1d input1)
  where
  lhs - rhs = Difference1d lhs rhs

instance Subtraction (Qty units) (Ast1d input) (Ast1d input) where
  lhs - rhs = constant1d lhs - rhs

instance Subtraction (Ast1d input1) (Qty units) (Ast1d input1) where
  lhs - rhs = lhs - constant1d rhs

instance input1 ~ input2 => Multiplication (Ast1d input1) (Ast1d input2) (Ast1d input1) where
  Constant1d lhs * Constant1d rhs = Constant1d (lhs * rhs)
  _ * Constant1d 0.0 = Constant1d 0.0
  Constant1d 0.0 * _ = Constant1d 0.0
  lhs * Constant1d 1.0 = lhs
  Constant1d 1.0 * rhs = rhs
  lhs * Constant1d -1.0 = -lhs
  Constant1d -1.0 * rhs = -rhs
  Variable1d lhs * Constant1d rhs = Variable1d (ProductVariableConstant1d lhs rhs)
  Constant1d lhs * Variable1d rhs = Variable1d (ProductVariableConstant1d rhs lhs)
  Variable1d lhs * Variable1d rhs =
    Variable1d (if lhs <= rhs then Product1d lhs rhs else Product1d rhs lhs)

instance Multiplication (Qty units) (Ast1d input) (Ast1d input) where
  lhs * rhs = constant1d lhs * rhs

instance Multiplication (Ast1d input1) (Qty units) (Ast1d input1) where
  lhs * rhs = lhs * constant1d rhs

instance input1 ~ input2 => Division (Ast1d input1) (Ast1d input2) (Ast1d input1) where
  Constant1d lhs / Constant1d rhs = Constant1d (lhs / rhs)
  Constant1d 0.0 / _ = Constant1d 0.0
  lhs / Constant1d 1.0 = lhs
  lhs / Constant1d -1.0 = -lhs
  Variable1d lhs / Constant1d rhs = Variable1d (ProductVariableConstant1d lhs (1.0 / rhs))
  Constant1d lhs / Variable1d rhs = Variable1d (QuotientConstantVariable1d lhs rhs)
  Variable1d lhs / Variable1d rhs = Variable1d (lhs / rhs)

instance
  input1 ~ input2 =>
  Division (Variable1d input1) (Variable1d input2) (Variable1d input1)
  where
  lhs / rhs = Quotient1d lhs rhs

instance Division (Qty units) (Ast1d input) (Ast1d input) where
  lhs / rhs = constant1d lhs / rhs

instance Division (Ast1d input) (Qty units) (Ast1d input) where
  lhs / rhs = lhs / constant1d rhs

squared :: Ast1d input -> Ast1d input
squared ast = case ast of
  Constant1d val -> Constant1d (Float.squared val)
  Variable1d (Negated1d arg) -> Variable1d (Squared1d arg)
  Variable1d (Sqrt1d arg) -> Variable1d arg
  Variable1d var -> Variable1d (Squared1d var)

sqrt :: Ast1d input -> Ast1d input
sqrt (Constant1d value) = Constant1d (Float.sqrt value)
sqrt (Variable1d var) = Variable1d (Sqrt1d var)

sin :: Ast1d input -> Ast1d input
sin (Constant1d val) = Constant1d (Float.sin val)
sin (Variable1d var) = Variable1d (Sin1d var)

cos :: Ast1d input -> Ast1d input
cos (Constant1d value) = constant1d (Float.cos value)
cos (Variable1d var) = Variable1d (Cos1d var)

line1d :: Qty units -> Qty units -> Ast1d input -> Ast1d input
line1d p1 p2 param = bezierCurve1d (NonEmpty.two p1 p2) param

quadraticSpline1d :: Qty units -> Qty units -> Qty units -> Ast1d input -> Ast1d input
quadraticSpline1d p1 p2 p3 param = bezierCurve1d (NonEmpty.three p1 p2 p3) param

cubicSpline1d :: Qty units -> Qty units -> Qty units -> Qty units -> Ast1d input -> Ast1d input
cubicSpline1d p1 p2 p3 p4 param = bezierCurve1d (NonEmpty.four p1 p2 p3 p4) param

quarticSpline1d ::
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Ast1d input ->
  Ast1d input
quarticSpline1d p1 p2 p3 p4 p5 param = bezierCurve1d (NonEmpty.five p1 p2 p3 p4 p5) param

quinticSpline1d ::
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Qty units ->
  Ast1d input ->
  Ast1d input
quinticSpline1d p1 p2 p3 p4 p5 p6 param = bezierCurve1d (NonEmpty.six p1 p2 p3 p4 p5 p6) param

bezierCurve1d :: NonEmpty (Qty units) -> Ast1d input -> Ast1d input
bezierCurve1d controlPoints param =
  Variable1d (BezierCurve1d (NonEmpty.map Units.coerce controlPoints) CurveParameter) . param

compileVariable1d :: Variable1d input -> Compilation.Step VariableIndex
compileVariable1d variable = case variable of
  CurveParameter -> Compilation.return (VariableIndex 0)
  SurfaceParameter U -> Compilation.return (VariableIndex 0)
  SurfaceParameter V -> Compilation.return (VariableIndex 1)
  XComponent2d arg -> Compilation.do
    argIndex <- compileVariable2d arg
    Compilation.addVariable1d (Instruction.XComponent argIndex)
  YComponent2d arg -> Compilation.do
    argIndex <- compileVariable2d arg
    Compilation.addVariable1d (Instruction.YComponent argIndex)
  Negated1d arg -> Compilation.do
    argIndex <- compileVariable1d arg
    Compilation.addVariable1d (Instruction.Negate1d argIndex)
  Sum1d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable1d (Instruction.Add1d lhsIndex rhsIndex)
  SumVariableConstant1d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- Compilation.addConstant1d rhs
    Compilation.addVariable1d (Instruction.AddVariableConstant1d lhsIndex rhsIndex)
  Difference1d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable1d (Instruction.Subtract1d lhsIndex rhsIndex)
  DifferenceConstantVariable1d lhs rhs -> Compilation.do
    lhsIndex <- Compilation.addConstant1d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable1d (Instruction.SubtractConstantVariable1d lhsIndex rhsIndex)
  Product1d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable1d (Instruction.Multiply1d lhsIndex rhsIndex)
  ProductVariableConstant1d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- Compilation.addConstant1d rhs
    Compilation.addVariable1d (Instruction.MultiplyVariableConstant1d lhsIndex rhsIndex)
  Quotient1d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable1d (Instruction.Divide1d lhsIndex rhsIndex)
  QuotientConstantVariable1d lhs rhs -> Compilation.do
    lhsIndex <- Compilation.addConstant1d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable1d (Instruction.DivideConstantVariable1d lhsIndex rhsIndex)
  Squared1d arg -> Compilation.do
    argIndex <- compileVariable1d arg
    Compilation.addVariable1d (Instruction.Square1d argIndex)
  Sqrt1d arg -> Compilation.do
    argIndex <- compileVariable1d arg
    Compilation.addVariable1d (Instruction.Sqrt1d argIndex)
  Sin1d arg -> Compilation.do
    argIndex <- compileVariable1d arg
    Compilation.addVariable1d (Instruction.Sin1d argIndex)
  Cos1d arg -> Compilation.do
    argIndex <- compileVariable1d arg
    Compilation.addVariable1d (Instruction.Cos1d argIndex)
  BezierCurve1d controlPoints parameter -> Compilation.do
    controlPointsIndex <- Compilation.addConstant controlPoints
    parameterIndex <- compileVariable1d parameter
    let numControlPoints = NonEmpty.length controlPoints
    let instruction = Instruction.Bezier1d numControlPoints controlPointsIndex parameterIndex
    Compilation.addVariable1d instruction
  SquaredNorm2d arg -> Compilation.do
    argIndex <- compileVariable2d arg
    Compilation.addVariable1d (Instruction.SquaredNorm2d argIndex)
  Norm2d arg -> Compilation.do
    argIndex <- compileVariable2d arg
    Compilation.addVariable1d (Instruction.Norm2d argIndex)
  Dot2d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable2d rhs
    Compilation.addVariable1d (Instruction.Dot2d lhsIndex rhsIndex)
  DotVariableConstant2d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- Compilation.addConstant2d rhs
    Compilation.addVariable1d (Instruction.DotVariableConstant2d lhsIndex rhsIndex)
  Cross2d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable2d rhs
    Compilation.addVariable1d (Instruction.Cross2d lhsIndex rhsIndex)
  CrossVariableConstant2d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- Compilation.addConstant2d rhs
    Compilation.addVariable1d (Instruction.CrossVariableConstant2d lhsIndex rhsIndex)

compileVariable2d :: Variable2d input -> Compilation.Step VariableIndex
compileVariable2d variable = case variable of
  XY2d x y -> Compilation.do
    xIndex <- compileVariable1d x
    yIndex <- compileVariable1d y
    Compilation.addVariable2d (Instruction.XY2d xIndex yIndex)
  XC2d x y -> Compilation.do
    xIndex <- compileVariable1d x
    yIndex <- Compilation.addConstant1d y
    Compilation.addVariable2d (Instruction.XC2d xIndex yIndex)
  CY2d x y -> Compilation.do
    xIndex <- Compilation.addConstant1d x
    yIndex <- compileVariable1d y
    Compilation.addVariable2d (Instruction.CY2d xIndex yIndex)
  Negated2d arg -> Compilation.do
    argIndex <- compileVariable2d arg
    Compilation.addVariable2d (Instruction.Negate2d argIndex)
  Sum2d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable2d rhs
    Compilation.addVariable2d (Instruction.Add2d lhsIndex rhsIndex)
  SumVariableConstant2d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- Compilation.addConstant2d rhs
    Compilation.addVariable2d (Instruction.AddVariableConstant2d lhsIndex rhsIndex)
  Difference2d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable2d rhs
    Compilation.addVariable2d (Instruction.Subtract2d lhsIndex rhsIndex)
  DifferenceConstantVariable2d lhs rhs -> Compilation.do
    lhsIndex <- Compilation.addConstant2d lhs
    rhsIndex <- compileVariable2d rhs
    Compilation.addVariable2d (Instruction.SubtractConstantVariable2d lhsIndex rhsIndex)
  Product2d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable2d (Instruction.Multiply2d lhsIndex rhsIndex)
  ProductVariableConstant2d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- Compilation.addConstant1d rhs
    Compilation.addVariable2d (Instruction.MultiplyVariableConstant2d lhsIndex rhsIndex)
  ProductConstantVariable2d lhs rhs -> Compilation.do
    lhsIndex <- Compilation.addConstant2d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable2d (Instruction.MultiplyConstantVariable2d lhsIndex rhsIndex)
  Quotient2d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable2d (Instruction.Divide2d lhsIndex rhsIndex)
  QuotientConstantVariable2d lhs rhs -> Compilation.do
    lhsIndex <- Compilation.addConstant2d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable2d (Instruction.DivideConstantVariable2d lhsIndex rhsIndex)
  BezierCurve2d controlPoints parameter -> Compilation.do
    let (xControlPoints, yControlPoints) = NonEmpty.unzip2 controlPoints
    let flattenedControlPoints = xControlPoints <> yControlPoints
    controlPointsIndex <- Compilation.addConstant flattenedControlPoints
    parameterIndex <- compileVariable1d parameter
    let numControlPoints = NonEmpty.length controlPoints
    let instruction = Instruction.Bezier2d numControlPoints controlPointsIndex parameterIndex
    Compilation.addVariable2d instruction

compileVariable3d :: Variable3d input -> Compilation.Step VariableIndex
compileVariable3d variable = case variable of
  XYZ3d x y z -> Compilation.do
    xIndex <- compileVariable1d x
    yIndex <- compileVariable1d y
    zIndex <- compileVariable1d z
    Compilation.addVariable3d (Instruction.XYZ3d xIndex yIndex zIndex)
  XYC3d x y z -> Compilation.do
    xIndex <- compileVariable1d x
    yIndex <- compileVariable1d y
    zIndex <- Compilation.addConstant1d z
    Compilation.addVariable3d (Instruction.XYC3d xIndex yIndex zIndex)
  XCZ3d x y z -> Compilation.do
    xIndex <- compileVariable1d x
    yIndex <- Compilation.addConstant1d y
    zIndex <- compileVariable1d z
    Compilation.addVariable3d (Instruction.XCZ3d xIndex yIndex zIndex)
  CYZ3d x y z -> Compilation.do
    xIndex <- Compilation.addConstant1d x
    yIndex <- compileVariable1d y
    zIndex <- compileVariable1d z
    Compilation.addVariable3d (Instruction.CYZ3d xIndex yIndex zIndex)
  XCC3d x y z -> Compilation.do
    xIndex <- compileVariable1d x
    yIndex <- Compilation.addConstant1d y
    zIndex <- Compilation.addConstant1d z
    Compilation.addVariable3d (Instruction.XCC3d xIndex yIndex zIndex)
  CYC3d x y z -> Compilation.do
    xIndex <- Compilation.addConstant1d x
    yIndex <- compileVariable1d y
    zIndex <- Compilation.addConstant1d z
    Compilation.addVariable3d (Instruction.CYC3d xIndex yIndex zIndex)
  CCZ3d x y z -> Compilation.do
    xIndex <- Compilation.addConstant1d x
    yIndex <- Compilation.addConstant1d y
    zIndex <- compileVariable1d z
    Compilation.addVariable3d (Instruction.CCZ3d xIndex yIndex zIndex)
  Negated3d arg -> Compilation.do
    argIndex <- compileVariable3d arg
    Compilation.addVariable3d (Instruction.Negate3d argIndex)
  Sum3d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable3d rhs
    Compilation.addVariable3d (Instruction.Add3d lhsIndex rhsIndex)
  SumVariableConstant3d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- Compilation.addConstant3d rhs
    Compilation.addVariable3d (Instruction.AddVariableConstant3d lhsIndex rhsIndex)
  Difference3d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable3d rhs
    Compilation.addVariable3d (Instruction.Subtract3d lhsIndex rhsIndex)
  DifferenceConstantVariable3d lhs rhs -> Compilation.do
    lhsIndex <- Compilation.addConstant3d lhs
    rhsIndex <- compileVariable3d rhs
    Compilation.addVariable3d (Instruction.SubtractConstantVariable3d lhsIndex rhsIndex)
  Product3d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable3d (Instruction.Multiply3d lhsIndex rhsIndex)
  ProductVariableConstant3d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- Compilation.addConstant1d rhs
    Compilation.addVariable3d (Instruction.MultiplyVariableConstant3d lhsIndex rhsIndex)
  ProductConstantVariable3d lhs rhs -> Compilation.do
    lhsIndex <- Compilation.addConstant3d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable3d (Instruction.MultiplyConstantVariable3d lhsIndex rhsIndex)
  Quotient3d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable3d (Instruction.Divide3d lhsIndex rhsIndex)
  QuotientConstantVariable3d lhs rhs -> Compilation.do
    lhsIndex <- Compilation.addConstant3d lhs
    rhsIndex <- compileVariable1d rhs
    Compilation.addVariable3d (Instruction.DivideConstantVariable3d lhsIndex rhsIndex)
  BezierCurve3d controlPoints parameter -> Compilation.do
    let (xControlPoints, yControlPoints, zControlPoints) = NonEmpty.unzip3 controlPoints
    let flattenedControlPoints = xControlPoints <> yControlPoints <> zControlPoints
    controlPointsIndex <- Compilation.addConstant flattenedControlPoints
    parameterIndex <- compileVariable1d parameter
    let numControlPoints = NonEmpty.length controlPoints
    let instruction = Instruction.Bezier3d numControlPoints controlPointsIndex parameterIndex
    Compilation.addVariable3d instruction
  Cross3d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable3d rhs
    Compilation.addVariable3d (Instruction.Cross3d lhsIndex rhsIndex)
  CrossVariableConstant3d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- Compilation.addConstant3d rhs
    Compilation.addVariable3d (Instruction.CrossVariableConstant3d lhsIndex rhsIndex)

compileCurve1d :: Ast1d Float -> (Float -> Float, Range Unitless -> Range Unitless)
compileCurve1d (Constant1d value) = (always value, always (Range.constant value))
compileCurve1d (Variable1d variable) = Compilation.curve1d (compileVariable1d variable)

compileCurve2d ::
  Ast2d Float ->
  ( Float -> (Float, Float)
  , Range Unitless -> (Range Unitless, Range Unitless)
  )
compileCurve2d (Constant2d (x, y)) = (always (x, y), always (Range.constant x, Range.constant y))
compileCurve2d (Variable2d variable) = Compilation.curve2d (compileVariable2d variable)

compileCurve3d ::
  Ast3d Float ->
  ( Float -> (Float, Float, Float)
  , Range Unitless -> (Range Unitless, Range Unitless, Range Unitless)
  )
compileCurve3d (Constant3d (x, y, z)) =
  (always (x, y, z), always (Range.constant x, Range.constant y, Range.constant z))
compileCurve3d (Variable3d variable) = Compilation.curve3d (compileVariable3d variable)
