module OpenSolid.Bytecode.Ast
  ( Ast1d
  , Ast2d
  , Ast3d
  , Compiled (Constant, Bytecode)
  , Coordinates
  , constant1d
  , constant2d
  , constant3d
  , curveParameter
  , surfaceParameter
  , xComponent
  , yComponent
  , rightwardComponent
  , forwardComponent
  , upwardComponent
  , squared
  , sqrt
  , sin
  , cos
  , squaredMagnitude2d
  , squaredMagnitude3d
  , magnitude2d
  , magnitude3d
  , transformVector2d
  , transformPoint2d
  , transformVector3d
  , transformPoint3d
  , placeVector2dIn
  , placePoint2dIn
  , placeVector3dIn
  , placePoint3dIn
  , placeVector2dOn
  , placePoint2dOn
  , projectVector3dInto
  , projectPoint3dInto
  , surfaceParameters
  , xy
  , rightwardForwardUpward
  , line1d
  , quadraticSpline1d
  , cubicSpline1d
  , quarticSpline1d
  , quinticSpline1d
  , bezierCurve1d
  , line2d
  , quadraticSpline2d
  , cubicSpline2d
  , quarticSpline2d
  , quinticSpline2d
  , bezierCurve2d
  , line3d
  , quadraticSpline3d
  , cubicSpline3d
  , quarticSpline3d
  , quinticSpline3d
  , bezierCurve3d
  , compileCurve1d
  , compileCurve2d
  , compileCurve3d
  , compileSurface1d
  , compileSurface2d
  , compileSurface3d
  , debugCurve1d
  , debugCurve2d
  , debugCurve3d
  , debugSurface1d
  , debugSurface2d
  , debugSurface3d
  )
where

import OpenSolid.Bytecode.Compile qualified as Compile
import OpenSolid.Bytecode.Evaluate (Compiled)
import OpenSolid.Bytecode.Evaluate qualified as Evaluate
import OpenSolid.Bytecode.Instruction (ConstantIndex, VariableIndex (VariableIndex))
import OpenSolid.Bytecode.Instruction qualified as Instruction
import OpenSolid.Float qualified as Float
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives (Direction3d (Direction3d), Point3d (Point3d), Vector3d (Vector3d))
import OpenSolid.Qty qualified as Qty
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvPoint)
import OpenSolid.Text qualified as Text
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d qualified as Vector3d

data Space

type Coordinates = Space @ Unitless

type Plane = Plane3d Coordinates (Defines Space)

data Ast1d input where
  Constant1d :: Float -> Ast1d input
  Variable1d :: Variable1d input -> Ast1d input

deriving instance Eq (Ast1d input)

deriving instance Ord (Ast1d input)

deriving instance Show (Ast1d input)

data Variable1d input where
  CurveParameter :: Variable1d Float
  SurfaceParameter :: SurfaceParameter -> Variable1d UvPoint
  XComponent :: Variable2d input -> Variable1d input
  YComponent :: Variable2d input -> Variable1d input
  RightwardComponent :: Variable3d input -> Variable1d input
  ForwardComponent :: Variable3d input -> Variable1d input
  UpwardComponent :: Variable3d input -> Variable1d input
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
  SquaredMagnitude2d :: Variable2d input -> Variable1d input
  SquaredMagnitude3d :: Variable3d input -> Variable1d input
  Magnitude2d :: Variable2d input -> Variable1d input
  Magnitude3d :: Variable3d input -> Variable1d input
  Dot2d :: Variable2d input -> Variable2d input -> Variable1d input
  DotVariableConstant2d :: Variable2d input -> Vector2d Coordinates -> Variable1d input
  Cross2d :: Variable2d input -> Variable2d input -> Variable1d input
  CrossVariableConstant2d :: Variable2d input -> Vector2d Coordinates -> Variable1d input
  Dot3d :: Variable3d input -> Variable3d input -> Variable1d input
  DotVariableConstant3d :: Variable3d input -> Vector3d Coordinates -> Variable1d input

deriving instance Eq (Variable1d input)

deriving instance Ord (Variable1d input)

deriving instance Show (Variable1d input)

data Ast2d input where
  Constant2d :: Vector2d Coordinates -> Ast2d input
  Variable2d :: Variable2d input -> Ast2d input

deriving instance Eq (Ast2d input)

deriving instance Ord (Ast2d input)

deriving instance Show (Ast2d input)

data Variable2d input where
  SurfaceParameters :: Variable2d UvPoint
  XY :: Variable1d input -> Variable1d input -> Variable2d input
  XC :: Variable1d input -> Float -> Variable2d input
  CY :: Float -> Variable1d input -> Variable2d input
  Negated2d :: Variable2d input -> Variable2d input
  Sum2d :: Variable2d input -> Variable2d input -> Variable2d input
  SumVariableConstant2d :: Variable2d input -> Vector2d Coordinates -> Variable2d input
  Difference2d :: Variable2d input -> Variable2d input -> Variable2d input
  DifferenceConstantVariable2d :: Vector2d Coordinates -> Variable2d input -> Variable2d input
  Product2d :: Variable2d input -> Variable1d input -> Variable2d input
  ProductVariableConstant2d :: Variable2d input -> Float -> Variable2d input
  ProductConstantVariable2d :: Vector2d Coordinates -> Variable1d input -> Variable2d input
  Quotient2d :: Variable2d input -> Variable1d input -> Variable2d input
  QuotientConstantVariable2d :: Vector2d Coordinates -> Variable1d input -> Variable2d input
  BezierCurve2d :: NonEmpty (Vector2d Coordinates) -> Variable1d input -> Variable2d input
  TransformVector2d :: Transform2d.Affine Coordinates -> Variable2d input -> Variable2d input
  TransformPoint2d :: Transform2d.Affine Coordinates -> Variable2d input -> Variable2d input
  ProjectVector3d :: Plane -> Variable3d input -> Variable2d input
  ProjectPoint3d :: Plane -> Variable3d input -> Variable2d input

deriving instance Eq (Variable2d input)

deriving instance Ord (Variable2d input)

deriving instance Show (Variable2d input)

data Ast3d input where
  Constant3d :: Vector3d Coordinates -> Ast3d input
  Variable3d :: Variable3d input -> Ast3d input

deriving instance Eq (Ast3d input)

deriving instance Ord (Ast3d input)

deriving instance Show (Ast3d input)

data Variable3d input where
  RFU :: Variable1d input -> Variable1d input -> Variable1d input -> Variable3d input
  RFC :: Variable1d input -> Variable1d input -> Float -> Variable3d input
  RCU :: Variable1d input -> Float -> Variable1d input -> Variable3d input
  CFU :: Float -> Variable1d input -> Variable1d input -> Variable3d input
  RCC :: Variable1d input -> Float -> Float -> Variable3d input
  CFC :: Float -> Variable1d input -> Float -> Variable3d input
  CCU :: Float -> Float -> Variable1d input -> Variable3d input
  Negated3d :: Variable3d input -> Variable3d input
  Sum3d :: Variable3d input -> Variable3d input -> Variable3d input
  SumVariableConstant3d :: Variable3d input -> Vector3d Coordinates -> Variable3d input
  Difference3d :: Variable3d input -> Variable3d input -> Variable3d input
  DifferenceConstantVariable3d :: Vector3d Coordinates -> Variable3d input -> Variable3d input
  Product3d :: Variable3d input -> Variable1d input -> Variable3d input
  ProductVariableConstant3d :: Variable3d input -> Float -> Variable3d input
  ProductConstantVariable3d :: Vector3d Coordinates -> Variable1d input -> Variable3d input
  Quotient3d :: Variable3d input -> Variable1d input -> Variable3d input
  QuotientConstantVariable3d :: Vector3d Coordinates -> Variable1d input -> Variable3d input
  Cross3d :: Variable3d input -> Variable3d input -> Variable3d input
  CrossVariableConstant3d :: Variable3d input -> Vector3d Coordinates -> Variable3d input
  BezierCurve3d :: NonEmpty (Vector3d Coordinates) -> Variable1d input -> Variable3d input
  TransformVector3d :: Transform3d.Affine Coordinates -> Variable3d input -> Variable3d input
  TransformPoint3d :: Transform3d.Affine Coordinates -> Variable3d input -> Variable3d input
  PlaceVector2d :: Plane -> Variable2d input -> Variable3d input
  PlacePoint2d :: Plane -> Variable2d input -> Variable3d input

deriving instance Eq (Variable3d input)

deriving instance Ord (Variable3d input)

deriving instance Show (Variable3d input)

instance Composition (Ast1d input) (Ast1d Float) (Ast1d input) where
  Constant1d outer . _ = Constant1d outer
  Variable1d outer . Variable1d inner = Variable1d (outer . inner)
  outer . Constant1d inner = Constant1d (Evaluate.curve1dValue (compileCurve1d outer) inner)

instance Composition (Variable1d input) (Variable1d Float) (Variable1d input) where
  input . CurveParameter = input
  CurveParameter . input = input
  XComponent arg . input = XComponent (arg . input)
  YComponent arg . input = YComponent (arg . input)
  RightwardComponent arg . input = RightwardComponent (arg . input)
  ForwardComponent arg . input = ForwardComponent (arg . input)
  UpwardComponent arg . input = UpwardComponent (arg . input)
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
  SquaredMagnitude2d arg . input = SquaredMagnitude2d (arg . input)
  SquaredMagnitude3d arg . input = SquaredMagnitude3d (arg . input)
  Magnitude2d arg . input = Magnitude2d (arg . input)
  Magnitude3d arg . input = Magnitude3d (arg . input)
  Dot2d lhs rhs . input = Dot2d (lhs . input) (rhs . input)
  DotVariableConstant2d lhs rhs . input = DotVariableConstant2d (lhs . input) rhs
  Cross2d lhs rhs . input = Cross2d (lhs . input) (rhs . input)
  CrossVariableConstant2d lhs rhs . input = CrossVariableConstant2d (lhs . input) rhs
  Dot3d lhs rhs . input = Dot3d (lhs . input) (rhs . input)
  DotVariableConstant3d lhs rhs . input = DotVariableConstant3d (lhs . input) rhs

instance Composition (Ast1d input) (Ast2d Float) (Ast2d input) where
  Constant2d outer . _ = Constant2d outer
  Variable2d outer . Variable1d inner = Variable2d (outer . inner)
  outer . Constant1d inner = Constant2d (Evaluate.curve2dValue (compileCurve2d outer) inner)

instance Composition (Variable1d input) (Variable2d Float) (Variable2d input) where
  input . CurveParameter = input
  XY x y . input = XY (x . input) (y . input)
  XC x y . input = XC (x . input) y
  CY x y . input = CY x (y . input)
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
  TransformVector2d transform vector . input = TransformVector2d transform (vector . input)
  TransformPoint2d transform point . input = TransformPoint2d transform (point . input)
  ProjectVector3d orientation vector . input = ProjectVector3d orientation (vector . input)
  ProjectPoint3d plane point . input = ProjectPoint3d plane (point . input)

instance Composition (Ast1d input) (Ast3d Float) (Ast3d input) where
  Constant3d outer . _ = Constant3d outer
  Variable3d outer . Variable1d inner = Variable3d (outer . inner)
  outer . Constant1d inner = Constant3d (Evaluate.curve3dValue (compileCurve3d outer) inner)

instance Composition (Variable1d input) (Variable3d Float) (Variable3d input) where
  input . CurveParameter = input
  RFU r f u . input = RFU (r . input) (f . input) (u . input)
  RFC r f u . input = RFC (r . input) (f . input) u
  RCU r f u . input = RCU (r . input) f (u . input)
  CFU r f u . input = CFU r (f . input) (u . input)
  RCC r f u . input = RCC (r . input) f u
  CFC r f u . input = CFC r (f . input) u
  CCU r f u . input = CCU r f (u . input)
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
  TransformVector3d transform vector . input = TransformVector3d transform (vector . input)
  TransformPoint3d transform point . input = TransformPoint3d transform (point . input)
  PlaceVector2d orientation vector . input = PlaceVector2d orientation (vector . input)
  PlacePoint2d plane point . input = PlacePoint2d plane (point . input)

instance Composition (Ast2d input) (Ast1d UvPoint) (Ast1d input) where
  Constant1d outer . _ = Constant1d outer
  Variable1d outer . Variable2d inner = Variable1d (outer . inner)
  outer . Constant2d (Vector2d u v) =
    Constant1d (Evaluate.surface1dValue (compileSurface1d outer) (Point2d u v))

instance Composition (Variable2d input) (Variable1d UvPoint) (Variable1d input) where
  input . SurfaceParameters = input
  SurfaceParameter U . input = XComponent input
  SurfaceParameter V . input = YComponent input
  XComponent arg . input = XComponent (arg . input)
  YComponent arg . input = YComponent (arg . input)
  RightwardComponent arg . input = RightwardComponent (arg . input)
  ForwardComponent arg . input = ForwardComponent (arg . input)
  UpwardComponent arg . input = UpwardComponent (arg . input)
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
  SquaredMagnitude2d arg . input = SquaredMagnitude2d (arg . input)
  SquaredMagnitude3d arg . input = SquaredMagnitude3d (arg . input)
  Magnitude2d arg . input = Magnitude2d (arg . input)
  Magnitude3d arg . input = Magnitude3d (arg . input)
  Dot2d lhs rhs . input = Dot2d (lhs . input) (rhs . input)
  DotVariableConstant2d lhs rhs . input = DotVariableConstant2d (lhs . input) rhs
  Cross2d lhs rhs . input = Cross2d (lhs . input) (rhs . input)
  CrossVariableConstant2d lhs rhs . input = CrossVariableConstant2d (lhs . input) rhs
  Dot3d lhs rhs . input = Dot3d (lhs . input) (rhs . input)
  DotVariableConstant3d lhs rhs . input = DotVariableConstant3d (lhs . input) rhs

instance Composition (Ast2d input) (Ast2d UvPoint) (Ast2d input) where
  Constant2d outer . _ = Constant2d outer
  Variable2d outer . Variable2d inner = Variable2d (outer . inner)
  outer . Constant2d (Vector2d u v) =
    Constant2d (Evaluate.surface2dValue (compileSurface2d outer) (Point2d u v))

instance Composition (Variable2d input) (Variable2d UvPoint) (Variable2d input) where
  input . SurfaceParameters = input
  SurfaceParameters . input = input
  XY x y . input = XY (x . input) (y . input)
  XC x y . input = XC (x . input) y
  CY x y . input = CY x (y . input)
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
  TransformVector2d transform vector . input = TransformVector2d transform (vector . input)
  TransformPoint2d transform point . input = TransformPoint2d transform (point . input)
  ProjectVector3d orientation vector . input = ProjectVector3d orientation (vector . input)
  ProjectPoint3d plane point . input = ProjectPoint3d plane (point . input)

instance Composition (Ast2d input) (Ast3d UvPoint) (Ast3d input) where
  Constant3d outer . _ = Constant3d outer
  Variable3d outer . Variable2d inner = Variable3d (outer . inner)
  outer . Constant2d (Vector2d u v) =
    Constant3d (Evaluate.surface3dValue (compileSurface3d outer) (Point2d u v))

instance Composition (Variable2d input) (Variable3d UvPoint) (Variable3d input) where
  input . SurfaceParameters = input
  RFU r f u . input = RFU (r . input) (f . input) (u . input)
  RFC r f u . input = RFC (r . input) (f . input) u
  RCU r f u . input = RCU (r . input) f (u . input)
  CFU r f u . input = CFU r (f . input) (u . input)
  RCC r f u . input = RCC (r . input) f u
  CFC r f u . input = CFC r (f . input) u
  CCU r f u . input = CCU r f (u . input)
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
  TransformVector3d transform vector . input = TransformVector3d transform (vector . input)
  TransformPoint3d transform point . input = TransformPoint3d transform (point . input)
  PlaceVector2d orientation vector . input = PlaceVector2d orientation (vector . input)
  PlacePoint2d plane point . input = PlacePoint2d plane (point . input)

constant1d :: Qty units -> Ast1d input
constant1d value = Constant1d (Qty.coerce value)

constant2d :: Vector2d (space @ units) -> Ast2d input
constant2d = Constant2d . Vector2d.coerce

constant3d :: Vector3d (space @ units) -> Ast3d input
constant3d = Constant3d . Vector3d.coerce

curveParameter :: Ast1d Float
curveParameter = Variable1d CurveParameter

surfaceParameter :: SurfaceParameter -> Ast1d UvPoint
surfaceParameter = Variable1d . SurfaceParameter

surfaceParameters :: Ast2d UvPoint
surfaceParameters = Variable2d SurfaceParameters

xComponent :: Ast2d input -> Ast1d input
xComponent (Constant2d val) = Constant1d (Vector2d.xComponent val)
xComponent (Variable2d var) = Variable1d (XComponent var)

yComponent :: Ast2d input -> Ast1d input
yComponent (Constant2d val) = Constant1d (Vector2d.yComponent val)
yComponent (Variable2d var) = Variable1d (YComponent var)

rightwardComponent :: Ast3d input -> Ast1d input
rightwardComponent (Constant3d val) = Constant1d (Vector3d.rightwardComponent val)
rightwardComponent (Variable3d var) = Variable1d (RightwardComponent var)

forwardComponent :: Ast3d input -> Ast1d input
forwardComponent (Constant3d val) = Constant1d (Vector3d.forwardComponent val)
forwardComponent (Variable3d var) = Variable1d (ForwardComponent var)

upwardComponent :: Ast3d input -> Ast1d input
upwardComponent (Constant3d val) = Constant1d (Vector3d.upwardComponent val)
upwardComponent (Variable3d var) = Variable1d (UpwardComponent var)

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

instance Negation (Ast2d input) where
  negate (Constant2d val) = Constant2d -val
  negate (Variable2d var) = Variable2d -var

instance Negation (Variable2d input) where
  negate (Negated2d arg) = arg
  negate (Difference2d lhs rhs) = Difference2d rhs lhs
  negate var = Negated2d var

instance Multiplication Sign (Ast2d input) (Ast2d input) where
  Positive * ast = ast
  Negative * ast = -ast

instance Multiplication (Ast2d input) Sign (Ast2d input) where
  ast * Positive = ast
  ast * Negative = -ast

instance Multiplication Sign (Variable2d input) (Variable2d input) where
  Positive * var = var
  Negative * var = -var

instance Multiplication (Variable2d input) Sign (Variable2d input) where
  var * Positive = var
  var * Negative = -var

instance input1 ~ input2 => Addition (Ast2d input1) (Ast2d input2) (Ast2d input1) where
  Constant2d lhs + rhs | lhs == Vector2d.zero = rhs
  lhs + Constant2d rhs | rhs == Vector2d.zero = lhs
  Constant2d lhs + Constant2d rhs = Constant2d (lhs + rhs)
  Constant2d lhs + Variable2d rhs = Variable2d (SumVariableConstant2d rhs lhs)
  Variable2d lhs + Constant2d rhs = Variable2d (SumVariableConstant2d lhs rhs)
  Variable2d lhs + Variable2d rhs = Variable2d (lhs + rhs)

instance
  input1 ~ input2 =>
  Addition (Variable2d input1) (Variable2d input2) (Variable2d input1)
  where
  lhs + rhs = if lhs <= rhs then Sum2d lhs rhs else Sum2d rhs lhs

instance Addition (Vector2d (space @ units)) (Ast2d input) (Ast2d input) where
  lhs + rhs = constant2d lhs + rhs

instance Addition (Ast2d input1) (Vector2d (space @ units)) (Ast2d input1) where
  lhs + rhs = lhs + constant2d rhs

instance input1 ~ input2 => Subtraction (Ast2d input1) (Ast2d input2) (Ast2d input1) where
  lhs - Constant2d rhs | rhs == Vector2d.zero = lhs
  Constant2d lhs - rhs | lhs == Vector2d.zero = -rhs
  Constant2d lhs - Constant2d rhs = Constant2d (lhs - rhs)
  Constant2d lhs - Variable2d rhs = Variable2d (DifferenceConstantVariable2d lhs rhs)
  Variable2d lhs - Constant2d rhs = Variable2d (SumVariableConstant2d lhs -rhs)
  Variable2d lhs - Variable2d rhs = Variable2d (lhs - rhs)

instance
  input1 ~ input2 =>
  Subtraction (Variable2d input1) (Variable2d input2) (Variable2d input1)
  where
  lhs - rhs = Difference2d lhs rhs

instance Subtraction (Vector2d (space @ units)) (Ast2d input) (Ast2d input) where
  lhs - rhs = constant2d lhs - rhs

instance Subtraction (Ast2d input1) (Vector2d (space @ units)) (Ast2d input1) where
  lhs - rhs = lhs - constant2d rhs

instance input1 ~ input2 => Multiplication (Ast2d input1) (Ast1d input2) (Ast2d input1) where
  Constant2d lhs * Constant1d rhs = Constant2d (lhs * rhs)
  _ * Constant1d 0.0 = Constant2d Vector2d.zero
  Constant2d lhs * _ | lhs == Vector2d.zero = Constant2d Vector2d.zero
  lhs * Constant1d 1.0 = lhs
  lhs * Constant1d -1.0 = -lhs
  Variable2d lhs * Constant1d rhs = Variable2d (ProductVariableConstant2d lhs rhs)
  Constant2d lhs * Variable1d rhs = Variable2d (ProductConstantVariable2d lhs rhs)
  Variable2d lhs * Variable1d rhs = Variable2d (Product2d lhs rhs)

instance input1 ~ input2 => Multiplication (Ast1d input1) (Ast2d input2) (Ast2d input1) where
  lhs * rhs = rhs * lhs

instance Multiplication (Ast2d input1) (Qty units) (Ast2d input1) where
  lhs * rhs = lhs * constant1d rhs

instance Multiplication (Qty units) (Ast2d input) (Ast2d input) where
  lhs * rhs = constant1d lhs * rhs

instance input1 ~ input2 => Division (Ast2d input1) (Ast1d input2) (Ast2d input1) where
  Constant2d lhs / Constant1d rhs = Constant2d (lhs / rhs)
  Constant2d lhs / _ | lhs == Vector2d.zero = Constant2d Vector2d.zero
  lhs / Constant1d 1.0 = lhs
  lhs / Constant1d -1.0 = -lhs
  Variable2d lhs / Constant1d rhs = Variable2d (ProductVariableConstant2d lhs (1.0 / rhs))
  Constant2d lhs / Variable1d rhs = Variable2d (QuotientConstantVariable2d lhs rhs)
  Variable2d lhs / Variable1d rhs = Variable2d (Quotient2d lhs rhs)

instance Division (Vector2d (space @ units)) (Ast1d input) (Ast2d input) where
  lhs / rhs = constant2d lhs / rhs

instance Division (Ast2d input) (Qty units) (Ast2d input) where
  lhs / rhs = lhs / constant1d rhs

instance Negation (Ast3d input) where
  negate (Constant3d val) = Constant3d -val
  negate (Variable3d var) = Variable3d -var

instance Negation (Variable3d input) where
  negate (Negated3d arg) = arg
  negate (Difference3d lhs rhs) = Difference3d rhs lhs
  negate var = Negated3d var

instance Multiplication Sign (Ast3d input) (Ast3d input) where
  Positive * ast = ast
  Negative * ast = -ast

instance Multiplication (Ast3d input) Sign (Ast3d input) where
  ast * Positive = ast
  ast * Negative = -ast

instance Multiplication Sign (Variable3d input) (Variable3d input) where
  Positive * var = var
  Negative * var = -var

instance Multiplication (Variable3d input) Sign (Variable3d input) where
  var * Positive = var
  var * Negative = -var

instance input1 ~ input2 => Addition (Ast3d input1) (Ast3d input2) (Ast3d input1) where
  Constant3d lhs + rhs | lhs == Vector3d.zero = rhs
  lhs + Constant3d rhs | rhs == Vector3d.zero = lhs
  Constant3d lhs + Constant3d rhs = Constant3d (lhs + rhs)
  Constant3d lhs + Variable3d rhs = Variable3d (SumVariableConstant3d rhs lhs)
  Variable3d lhs + Constant3d rhs = Variable3d (SumVariableConstant3d lhs rhs)
  Variable3d lhs + Variable3d rhs = Variable3d (lhs + rhs)

instance
  input1 ~ input2 =>
  Addition (Variable3d input1) (Variable3d input2) (Variable3d input1)
  where
  lhs + rhs = if lhs <= rhs then Sum3d lhs rhs else Sum3d rhs lhs

instance Addition (Vector3d (space @ units)) (Ast3d input) (Ast3d input) where
  lhs + rhs = constant3d lhs + rhs

instance Addition (Ast3d input1) (Vector3d (space @ units)) (Ast3d input1) where
  lhs + rhs = lhs + constant3d rhs

instance input1 ~ input2 => Subtraction (Ast3d input1) (Ast3d input2) (Ast3d input1) where
  lhs - Constant3d rhs | rhs == Vector3d.zero = lhs
  Constant3d lhs - rhs | lhs == Vector3d.zero = -rhs
  Constant3d lhs - Constant3d rhs = Constant3d (lhs - rhs)
  Constant3d lhs - Variable3d rhs = Variable3d (DifferenceConstantVariable3d lhs rhs)
  Variable3d lhs - Constant3d rhs = Variable3d (SumVariableConstant3d lhs -rhs)
  Variable3d lhs - Variable3d rhs = Variable3d (lhs - rhs)

instance
  input1 ~ input2 =>
  Subtraction (Variable3d input1) (Variable3d input2) (Variable3d input1)
  where
  lhs - rhs = Difference3d lhs rhs

instance Subtraction (Vector3d (space @ units)) (Ast3d input) (Ast3d input) where
  lhs - rhs = constant3d lhs - rhs

instance Subtraction (Ast3d input1) (Vector3d (space @ units)) (Ast3d input1) where
  lhs - rhs = lhs - constant3d rhs

instance input1 ~ input2 => Multiplication (Ast3d input1) (Ast1d input2) (Ast3d input1) where
  Constant3d lhs * Constant1d rhs = Constant3d (lhs * rhs)
  _ * Constant1d 0.0 = Constant3d Vector3d.zero
  Constant3d lhs * _ | lhs == Vector3d.zero = Constant3d Vector3d.zero
  lhs * Constant1d 1.0 = lhs
  lhs * Constant1d -1.0 = -lhs
  Variable3d lhs * Constant1d rhs = Variable3d (ProductVariableConstant3d lhs rhs)
  Constant3d lhs * Variable1d rhs = Variable3d (ProductConstantVariable3d lhs rhs)
  Variable3d lhs * Variable1d rhs = Variable3d (Product3d lhs rhs)

instance input1 ~ input2 => Multiplication (Ast1d input1) (Ast3d input2) (Ast3d input1) where
  lhs * rhs = rhs * lhs

instance Multiplication (Ast3d input1) (Qty units) (Ast3d input1) where
  lhs * rhs = lhs * constant1d rhs

instance Multiplication (Qty units) (Ast3d input) (Ast3d input) where
  lhs * rhs = constant1d lhs * rhs

instance input1 ~ input2 => Division (Ast3d input1) (Ast1d input2) (Ast3d input1) where
  Constant3d lhs / Constant1d rhs = Constant3d (lhs / rhs)
  Constant3d lhs / _ | lhs == Vector3d.zero = Constant3d Vector3d.zero
  lhs / Constant1d 1.0 = lhs
  lhs / Constant1d -1.0 = -lhs
  Variable3d lhs / Constant1d rhs = Variable3d (ProductVariableConstant3d lhs (1.0 / rhs))
  Constant3d lhs / Variable1d rhs = Variable3d (QuotientConstantVariable3d lhs rhs)
  Variable3d lhs / Variable1d rhs = Variable3d (Quotient3d lhs rhs)

instance Division (Vector3d (space @ units)) (Ast1d input) (Ast3d input) where
  lhs / rhs = constant3d lhs / rhs

instance Division (Ast3d input) (Qty units) (Ast3d input) where
  lhs / rhs = lhs / constant1d rhs

instance input1 ~ input2 => DotMultiplication (Ast2d input1) (Ast2d input2) (Ast1d input1) where
  Constant2d lhs `dot` Constant2d rhs = Constant1d (lhs `dot` rhs)
  Constant2d lhs `dot` _ | lhs == Vector2d.zero = Constant1d 0.0
  _ `dot` Constant2d rhs | rhs == Vector2d.zero = Constant1d 0.0
  Variable2d lhs `dot` Constant2d rhs = Variable1d (DotVariableConstant2d lhs rhs)
  Constant2d lhs `dot` Variable2d rhs = Variable1d (DotVariableConstant2d rhs lhs)
  Variable2d lhs `dot` Variable2d rhs = Variable1d (lhs `dot` rhs)

instance
  input1 ~ input2 =>
  DotMultiplication (Variable2d input1) (Variable2d input2) (Variable1d input1)
  where
  lhs `dot` rhs = if lhs <= rhs then Dot2d lhs rhs else Dot2d rhs lhs

instance DotMultiplication (Vector2d (space @ units)) (Ast2d input) (Ast1d input) where
  lhs `dot` rhs = constant2d lhs `dot` rhs

instance DotMultiplication (Ast2d input) (Vector2d (space @ units)) (Ast1d input) where
  lhs `dot` rhs = lhs `dot` constant2d rhs

instance input1 ~ input2 => CrossMultiplication (Ast2d input1) (Ast2d input2) (Ast1d input1) where
  Constant2d lhs `cross` Constant2d rhs = Constant1d (lhs `cross` rhs)
  Constant2d lhs `cross` _ | lhs == Vector2d.zero = Constant1d 0.0
  _ `cross` Constant2d rhs | rhs == Vector2d.zero = Constant1d 0.0
  Variable2d lhs `cross` Constant2d rhs = Variable1d (CrossVariableConstant2d lhs rhs)
  Constant2d lhs `cross` Variable2d rhs = Variable1d (CrossVariableConstant2d rhs -lhs)
  Variable2d lhs `cross` Variable2d rhs = Variable1d (Cross2d lhs rhs)

instance CrossMultiplication (Vector2d (space @ units)) (Ast2d input) (Ast1d input) where
  lhs `cross` rhs = constant2d lhs `cross` rhs

instance CrossMultiplication (Ast2d input) (Vector2d (space @ units)) (Ast1d input) where
  lhs `cross` rhs = lhs `cross` constant2d rhs

instance input1 ~ input2 => DotMultiplication (Ast3d input1) (Ast3d input2) (Ast1d input1) where
  Constant3d lhs `dot` Constant3d rhs = Constant1d (lhs `dot` rhs)
  Constant3d lhs `dot` _ | lhs == Vector3d.zero = Constant1d 0.0
  _ `dot` Constant3d rhs | rhs == Vector3d.zero = Constant1d 0.0
  Variable3d lhs `dot` Constant3d rhs = Variable1d (DotVariableConstant3d lhs rhs)
  Constant3d lhs `dot` Variable3d rhs = Variable1d (DotVariableConstant3d rhs lhs)
  Variable3d lhs `dot` Variable3d rhs = Variable1d (lhs `dot` rhs)

instance
  input1 ~ input2 =>
  DotMultiplication (Variable3d input1) (Variable3d input2) (Variable1d input1)
  where
  lhs `dot` rhs = if lhs <= rhs then Dot3d lhs rhs else Dot3d rhs lhs

instance DotMultiplication (Vector3d (space @ units)) (Ast3d input) (Ast1d input) where
  lhs `dot` rhs = constant3d lhs `dot` rhs

instance DotMultiplication (Ast3d input) (Vector3d (space @ units)) (Ast1d input) where
  lhs `dot` rhs = lhs `dot` constant3d rhs

instance input1 ~ input2 => CrossMultiplication (Ast3d input1) (Ast3d input2) (Ast3d input1) where
  Constant3d lhs `cross` Constant3d rhs = Constant3d (lhs `cross` rhs)
  Constant3d lhs `cross` _ | lhs == Vector3d.zero = Constant3d Vector3d.zero
  _ `cross` Constant3d rhs | rhs == Vector3d.zero = Constant3d Vector3d.zero
  Variable3d lhs `cross` Constant3d rhs = Variable3d (CrossVariableConstant3d lhs rhs)
  Constant3d lhs `cross` Variable3d rhs = Variable3d (CrossVariableConstant3d rhs -lhs)
  Variable3d lhs `cross` Variable3d rhs = Variable3d (Cross3d lhs rhs)

instance CrossMultiplication (Vector3d (space @ units)) (Ast3d input) (Ast3d input) where
  lhs `cross` rhs = constant3d lhs `cross` rhs

instance CrossMultiplication (Ast3d input) (Vector3d (space @ units)) (Ast3d input) where
  lhs `cross` rhs = lhs `cross` constant3d rhs

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

squaredMagnitude2d :: Ast2d input -> Ast1d input
squaredMagnitude2d ast = case ast of
  Constant2d val -> Constant1d (Vector2d.squaredMagnitude val)
  Variable2d (Negated2d arg) -> Variable1d (SquaredMagnitude2d arg)
  Variable2d var -> Variable1d (SquaredMagnitude2d var)

squaredMagnitude3d :: Ast3d input -> Ast1d input
squaredMagnitude3d ast = case ast of
  Constant3d val -> Constant1d (Vector3d.squaredMagnitude val)
  Variable3d (Negated3d arg) -> Variable1d (SquaredMagnitude3d arg)
  Variable3d var -> Variable1d (SquaredMagnitude3d var)

magnitude2d :: Ast2d input -> Ast1d input
magnitude2d ast = case ast of
  Constant2d val -> Constant1d (Vector2d.magnitude val)
  Variable2d (Negated2d arg) -> Variable1d (Magnitude2d arg)
  Variable2d var -> Variable1d (Magnitude2d var)

magnitude3d :: Ast3d input -> Ast1d input
magnitude3d ast = case ast of
  Constant3d val -> Constant1d (Vector3d.magnitude val)
  Variable3d (Negated3d arg) -> Variable1d (Magnitude3d arg)
  Variable3d var -> Variable1d (Magnitude3d var)

transformVector2d :: Transform2d tag (space @ units) -> Ast2d input -> Ast2d input
transformVector2d transform ast = do
  let erasedTransform = Transform2d.coerce transform
  case ast of
    Constant2d val -> Constant2d (Vector2d.transformBy erasedTransform val)
    Variable2d (TransformVector2d existing var) ->
      Variable2d (TransformVector2d (erasedTransform . existing) var)
    Variable2d var -> Variable2d (TransformVector2d erasedTransform var)

transformVector3d :: Transform3d tag (space @ units) -> Ast3d input -> Ast3d input
transformVector3d transform ast = do
  let erasedTransform = Transform3d.coerce transform
  case ast of
    Constant3d val -> Constant3d (Vector3d.transformBy erasedTransform val)
    Variable3d (TransformVector3d existing var) ->
      Variable3d (TransformVector3d (erasedTransform . existing) var)
    Variable3d var -> Variable3d (TransformVector3d erasedTransform var)

transformPoint2d :: Transform2d tag (space @ units) -> Ast2d input -> Ast2d input
transformPoint2d transform ast = do
  let erasedTransform = Transform2d.coerce transform
  case ast of
    -- TODO avoid adding/subtracting Point2d.origin once Point2d is a newtype over Vector2d
    Constant2d val -> Constant2d (Point2d.transformBy erasedTransform (Point2d.origin + val) - Point2d.origin)
    Variable2d (TransformPoint2d existing var) ->
      Variable2d (TransformPoint2d (erasedTransform . existing) var)
    Variable2d var -> Variable2d (TransformPoint2d erasedTransform var)

transformPoint3d :: Transform3d tag (space @ units) -> Ast3d input -> Ast3d input
transformPoint3d transform ast = do
  let erasedTransform = Transform3d.coerce transform
  case ast of
    -- TODO avoid adding/subtracting Point3d.origin once Point3d is a newtype over Vector3d
    Constant3d val -> Constant3d (Point3d.transformBy erasedTransform (Point3d.origin + val) - Point3d.origin)
    Variable3d (TransformPoint3d existing var) ->
      Variable3d (TransformPoint3d (erasedTransform . existing) var)
    Variable3d var -> Variable3d (TransformPoint3d erasedTransform var)

placementTransform2d :: Frame2d (global @ units) (Defines local) -> Transform2d.Affine Coordinates
placementTransform2d frame =
  Transform2d
    @ Point2d.coerce (Frame2d.originPoint frame)
    @ Vector2d.coerce (Vector2d.unit (Frame2d.xDirection frame))
    @ Vector2d.coerce (Vector2d.unit (Frame2d.yDirection frame))

placementTransform3d :: Frame3d (global @ units) (Defines local) -> Transform3d.Affine Coordinates
placementTransform3d frame =
  Transform3d
    @ Point3d.coerce (Frame3d.originPoint frame)
    @ Vector3d.coerce (Vector3d.unit (Frame3d.rightwardDirection frame))
    @ Vector3d.coerce (Vector3d.unit (Frame3d.forwardDirection frame))
    @ Vector3d.coerce (Vector3d.unit (Frame3d.upwardDirection frame))

placeVector2dIn :: Frame2d (global @ frameUnits) (Defines local) -> Ast2d input -> Ast2d input
placeVector2dIn frame ast = transformVector2d (placementTransform2d frame) ast

placePoint2dIn :: Frame2d (global @ units) (Defines local) -> Ast2d input -> Ast2d input
placePoint2dIn frame ast = transformPoint2d (placementTransform2d frame) ast

placeVector3dIn :: Frame3d (global @ frameUnits) (Defines local) -> Ast3d input -> Ast3d input
placeVector3dIn frame ast = transformVector3d (placementTransform3d frame) ast

placePoint3dIn :: Frame3d (global @ units) (Defines local) -> Ast3d input -> Ast3d input
placePoint3dIn frame ast = transformPoint3d (placementTransform3d frame) ast

placeVector2dOn :: Plane3d (global @ planeUnits) (Defines local) -> Ast2d input -> Ast3d input
placeVector2dOn plane ast = case ast of
  Constant2d val -> Constant3d (Vector2d.on (Plane3d.coerce plane) val)
  Variable2d var -> Variable3d (PlaceVector2d (Plane3d.coerce plane) var)

placePoint2dOn :: Plane3d (global @ units) (Defines local) -> Ast2d input -> Ast3d input
placePoint2dOn plane ast = case ast of
  Constant2d val -> Constant3d (Point2d.on (Plane3d.coerce plane) (Point2d.origin + val) - Point3d.origin)
  Variable2d var -> Variable3d (PlacePoint2d (Plane3d.coerce plane) var)

projectVector3dInto :: Plane3d (global @ planeUnits) (Defines local) -> Ast3d input -> Ast2d input
projectVector3dInto plane ast = case ast of
  Constant3d val -> Constant2d (Vector3d.projectInto (Plane3d.coerce plane) val)
  Variable3d var -> Variable2d (ProjectVector3d (Plane3d.coerce plane) var)

projectPoint3dInto :: Plane3d (global @ units) (Defines local) -> Ast3d input -> Ast2d input
projectPoint3dInto plane ast = case ast of
  Constant3d val -> Constant2d (Point3d.projectInto (Plane3d.coerce plane) (Point3d.origin + val) - Point2d.origin)
  Variable3d var -> Variable2d (ProjectPoint3d (Plane3d.coerce plane) var)

xy :: Ast1d input -> Ast1d input -> Ast2d input
xy (Constant1d x) (Constant1d y) = Constant2d (Vector2d x y)
xy (Constant1d x) (Variable1d y) = Variable2d (CY x y)
xy (Variable1d x) (Constant1d y) = Variable2d (XC x y)
xy (Variable1d x) (Variable1d y) = Variable2d (XY x y)

rightwardForwardUpward :: Ast1d input -> Ast1d input -> Ast1d input -> Ast3d input
rightwardForwardUpward (Constant1d r) (Constant1d f) (Constant1d u) = Constant3d (Vector3d r f u)
rightwardForwardUpward (Constant1d r) (Constant1d f) (Variable1d u) = Variable3d (CCU r f u)
rightwardForwardUpward (Constant1d r) (Variable1d f) (Constant1d u) = Variable3d (CFC r f u)
rightwardForwardUpward (Variable1d r) (Constant1d f) (Constant1d u) = Variable3d (RCC r f u)
rightwardForwardUpward (Constant1d r) (Variable1d f) (Variable1d u) = Variable3d (CFU r f u)
rightwardForwardUpward (Variable1d r) (Constant1d f) (Variable1d u) = Variable3d (RCU r f u)
rightwardForwardUpward (Variable1d r) (Variable1d f) (Constant1d u) = Variable3d (RFC r f u)
rightwardForwardUpward (Variable1d r) (Variable1d f) (Variable1d u) = Variable3d (RFU r f u)

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
bezierCurve1d (NonEmpty.One value) _ = constant1d value
bezierCurve1d controlPoints param =
  Variable1d (BezierCurve1d (NonEmpty.map Qty.coerce controlPoints) CurveParameter) . param

line2d :: Vector2d (space @ units) -> Vector2d (space @ units) -> Ast1d input -> Ast2d input
line2d p1 p2 param = bezierCurve2d (NonEmpty.two p1 p2) param

quadraticSpline2d ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Ast1d input ->
  Ast2d input
quadraticSpline2d p1 p2 p3 param = bezierCurve2d (NonEmpty.three p1 p2 p3) param

cubicSpline2d ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Ast1d input ->
  Ast2d input
cubicSpline2d p1 p2 p3 p4 param = bezierCurve2d (NonEmpty.four p1 p2 p3 p4) param

quarticSpline2d ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Ast1d input ->
  Ast2d input
quarticSpline2d p1 p2 p3 p4 p5 param = bezierCurve2d (NonEmpty.five p1 p2 p3 p4 p5) param

quinticSpline2d ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Ast1d input ->
  Ast2d input
quinticSpline2d p1 p2 p3 p4 p5 p6 param = bezierCurve2d (NonEmpty.six p1 p2 p3 p4 p5 p6) param

bezierCurve2d :: NonEmpty (Vector2d (space @ units)) -> Ast1d input -> Ast2d input
bezierCurve2d (NonEmpty.One value) _ = constant2d value
bezierCurve2d controlPoints param =
  Variable2d (BezierCurve2d (NonEmpty.map Vector2d.coerce controlPoints) CurveParameter) . param

line3d :: Vector3d (space @ units) -> Vector3d (space @ units) -> Ast1d input -> Ast3d input
line3d p1 p2 param = bezierCurve3d (NonEmpty.two p1 p2) param

quadraticSpline3d ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Ast1d input ->
  Ast3d input
quadraticSpline3d p1 p2 p3 param = bezierCurve3d (NonEmpty.three p1 p2 p3) param

cubicSpline3d ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Ast1d input ->
  Ast3d input
cubicSpline3d p1 p2 p3 p4 param = bezierCurve3d (NonEmpty.four p1 p2 p3 p4) param

quarticSpline3d ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Ast1d input ->
  Ast3d input
quarticSpline3d p1 p2 p3 p4 p5 param = bezierCurve3d (NonEmpty.five p1 p2 p3 p4 p5) param

quinticSpline3d ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Ast1d input ->
  Ast3d input
quinticSpline3d p1 p2 p3 p4 p5 p6 param = bezierCurve3d (NonEmpty.six p1 p2 p3 p4 p5 p6) param

bezierCurve3d :: NonEmpty (Vector3d (space @ units)) -> Ast1d input -> Ast3d input
bezierCurve3d (NonEmpty.One value) _ = constant3d value
bezierCurve3d controlPoints param =
  Variable3d (BezierCurve3d (NonEmpty.map Vector3d.coerce controlPoints) CurveParameter) . param

addTransform2d :: Transform2d.Affine Coordinates -> Compile.Step ConstantIndex
addTransform2d (Transform2d origin i j) = do
  let Vector2d iX iY = i
  let Vector2d jX jY = j
  let Point2d oX oY = origin
  Compile.addConstant (iX :| [iY, jX, jY, oX, oY])

addTransform3d :: Transform3d.Affine Coordinates -> Compile.Step ConstantIndex
addTransform3d (Transform3d origin i j k) = do
  let Vector3d iR iF iU = i
  let Vector3d jR jF jU = j
  let Vector3d kR kF kU = k
  let Point3d oR oF oU = origin
  Compile.addConstant (iR :| [iF, iU, jR, jF, jU, kR, kF, kU, oR, oF, oU])

addPlane :: Plane -> Compile.Step ConstantIndex
addPlane plane = do
  let Direction3d iR iF iU = Plane3d.xDirection plane
  let Direction3d jR jF jU = Plane3d.yDirection plane
  let Point3d oR oF oU = Plane3d.originPoint plane
  Compile.addConstant (iR :| [iF, iU, jR, jF, jU, oR, oF, oU])

compileVariable1d :: Variable1d input -> Compile.Step VariableIndex
compileVariable1d variable = case variable of
  CurveParameter -> Compile.return (VariableIndex 0)
  SurfaceParameter U -> Compile.return (VariableIndex 0)
  SurfaceParameter V -> Compile.return (VariableIndex 1)
  XComponent arg -> Compile.do
    argIndex <- compileVariable2d arg
    Compile.addVariable1d (Instruction.Component0 argIndex)
  YComponent arg -> Compile.do
    argIndex <- compileVariable2d arg
    Compile.addVariable1d (Instruction.Component1 argIndex)
  RightwardComponent arg -> Compile.do
    argIndex <- compileVariable3d arg
    Compile.addVariable1d (Instruction.Component0 argIndex)
  ForwardComponent arg -> Compile.do
    argIndex <- compileVariable3d arg
    Compile.addVariable1d (Instruction.Component1 argIndex)
  UpwardComponent arg -> Compile.do
    argIndex <- compileVariable3d arg
    Compile.addVariable1d (Instruction.Component2 argIndex)
  Negated1d arg -> Compile.do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.Negate1d argIndex)
  Sum1d lhs rhs -> Compile.do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable1d (Instruction.Add1d lhsIndex rhsIndex)
  SumVariableConstant1d lhs rhs -> Compile.do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- Compile.addConstant1d rhs
    Compile.addVariable1d (Instruction.AddVariableConstant1d lhsIndex rhsIndex)
  Difference1d lhs rhs -> Compile.do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable1d (Instruction.Subtract1d lhsIndex rhsIndex)
  DifferenceConstantVariable1d lhs rhs -> Compile.do
    lhsIndex <- Compile.addConstant1d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable1d (Instruction.SubtractConstantVariable1d lhsIndex rhsIndex)
  Product1d lhs rhs -> Compile.do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable1d (Instruction.Multiply1d lhsIndex rhsIndex)
  ProductVariableConstant1d lhs rhs -> Compile.do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- Compile.addConstant1d rhs
    Compile.addVariable1d (Instruction.MultiplyVariableConstant1d lhsIndex rhsIndex)
  Quotient1d lhs rhs -> Compile.do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable1d (Instruction.Divide1d lhsIndex rhsIndex)
  QuotientConstantVariable1d lhs rhs -> Compile.do
    lhsIndex <- Compile.addConstant1d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable1d (Instruction.DivideConstantVariable1d lhsIndex rhsIndex)
  Squared1d arg -> Compile.do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.Square1d argIndex)
  Sqrt1d arg -> Compile.do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.Sqrt1d argIndex)
  Sin1d arg -> Compile.do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.Sin1d argIndex)
  Cos1d arg -> Compile.do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.Cos1d argIndex)
  BezierCurve1d controlPoints parameter -> Compile.do
    controlPointsIndex <- Compile.addConstant controlPoints
    parameterIndex <- compileVariable1d parameter
    let numControlPoints = NonEmpty.length controlPoints
    let instruction = Instruction.Bezier1d numControlPoints controlPointsIndex parameterIndex
    Compile.addVariable1d instruction
  SquaredMagnitude2d arg -> Compile.do
    argIndex <- compileVariable2d arg
    Compile.addVariable1d (Instruction.SquaredMagnitude2d argIndex)
  SquaredMagnitude3d arg -> Compile.do
    argIndex <- compileVariable3d arg
    Compile.addVariable1d (Instruction.SquaredMagnitude3d argIndex)
  Magnitude2d arg -> Compile.do
    argIndex <- compileVariable2d arg
    Compile.addVariable1d (Instruction.Magnitude2d argIndex)
  Magnitude3d arg -> Compile.do
    argIndex <- compileVariable3d arg
    Compile.addVariable1d (Instruction.Magnitude3d argIndex)
  Dot2d lhs rhs -> Compile.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable2d rhs
    Compile.addVariable1d (Instruction.Dot2d lhsIndex rhsIndex)
  DotVariableConstant2d lhs rhs -> Compile.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- Compile.addConstant2d rhs
    Compile.addVariable1d (Instruction.DotVariableConstant2d lhsIndex rhsIndex)
  Cross2d lhs rhs -> Compile.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable2d rhs
    Compile.addVariable1d (Instruction.Cross2d lhsIndex rhsIndex)
  CrossVariableConstant2d lhs rhs -> Compile.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- Compile.addConstant2d rhs
    Compile.addVariable1d (Instruction.CrossVariableConstant2d lhsIndex rhsIndex)
  Dot3d lhs rhs -> Compile.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable3d rhs
    Compile.addVariable1d (Instruction.Dot3d lhsIndex rhsIndex)
  DotVariableConstant3d lhs rhs -> Compile.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- Compile.addConstant3d rhs
    Compile.addVariable1d (Instruction.DotVariableConstant3d lhsIndex rhsIndex)

compileVariable2d :: Variable2d input -> Compile.Step VariableIndex
compileVariable2d variable = case variable of
  SurfaceParameters -> Compile.return (VariableIndex 0)
  XY x y -> Compile.do
    xIndex <- compileVariable1d x
    yIndex <- compileVariable1d y
    Compile.addVariable2d (Instruction.XY xIndex yIndex)
  XC x y -> Compile.do
    xIndex <- compileVariable1d x
    yIndex <- Compile.addConstant1d y
    Compile.addVariable2d (Instruction.XC xIndex yIndex)
  CY x y -> Compile.do
    xIndex <- Compile.addConstant1d x
    yIndex <- compileVariable1d y
    Compile.addVariable2d (Instruction.CY xIndex yIndex)
  Negated2d arg -> Compile.do
    argIndex <- compileVariable2d arg
    Compile.addVariable2d (Instruction.Negate2d argIndex)
  Sum2d lhs rhs -> Compile.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable2d rhs
    Compile.addVariable2d (Instruction.Add2d lhsIndex rhsIndex)
  SumVariableConstant2d lhs rhs -> Compile.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- Compile.addConstant2d rhs
    Compile.addVariable2d (Instruction.AddVariableConstant2d lhsIndex rhsIndex)
  Difference2d lhs rhs -> Compile.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable2d rhs
    Compile.addVariable2d (Instruction.Subtract2d lhsIndex rhsIndex)
  DifferenceConstantVariable2d lhs rhs -> Compile.do
    lhsIndex <- Compile.addConstant2d lhs
    rhsIndex <- compileVariable2d rhs
    Compile.addVariable2d (Instruction.SubtractConstantVariable2d lhsIndex rhsIndex)
  Product2d lhs rhs -> Compile.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable2d (Instruction.Multiply2d lhsIndex rhsIndex)
  ProductVariableConstant2d lhs rhs -> Compile.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- Compile.addConstant1d rhs
    Compile.addVariable2d (Instruction.MultiplyVariableConstant2d lhsIndex rhsIndex)
  ProductConstantVariable2d lhs rhs -> Compile.do
    lhsIndex <- Compile.addConstant2d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable2d (Instruction.MultiplyConstantVariable2d lhsIndex rhsIndex)
  Quotient2d lhs rhs -> Compile.do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable2d (Instruction.Divide2d lhsIndex rhsIndex)
  QuotientConstantVariable2d lhs rhs -> Compile.do
    lhsIndex <- Compile.addConstant2d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable2d (Instruction.DivideConstantVariable2d lhsIndex rhsIndex)
  BezierCurve2d controlPoints parameter -> Compile.do
    let (xControlPoints, yControlPoints) = NonEmpty.unzip2 (NonEmpty.map Vector2d.components controlPoints)
    let flattenedControlPoints = xControlPoints <> yControlPoints
    controlPointsIndex <- Compile.addConstant flattenedControlPoints
    parameterIndex <- compileVariable1d parameter
    let numControlPoints = NonEmpty.length controlPoints
    let instruction = Instruction.Bezier2d numControlPoints controlPointsIndex parameterIndex
    Compile.addVariable2d instruction
  TransformVector2d transform vector -> Compile.do
    matrixIndex <- addTransform2d transform
    vectorIndex <- compileVariable2d vector
    Compile.addVariable2d (Instruction.TransformVector2d matrixIndex vectorIndex)
  TransformPoint2d transform point -> Compile.do
    matrixIndex <- addTransform2d transform
    pointIndex <- compileVariable2d point
    Compile.addVariable2d (Instruction.TransformPoint2d matrixIndex pointIndex)
  ProjectVector3d plane vector -> Compile.do
    planeIndex <- addPlane plane
    vectorIndex <- compileVariable3d vector
    Compile.addVariable2d (Instruction.ProjectVector3d planeIndex vectorIndex)
  ProjectPoint3d plane point -> Compile.do
    planeIndex <- addPlane plane
    pointIndex <- compileVariable3d point
    Compile.addVariable2d (Instruction.ProjectPoint3d planeIndex pointIndex)

compileVariable3d :: Variable3d input -> Compile.Step VariableIndex
compileVariable3d variable = case variable of
  RFU r f u -> Compile.do
    rIndex <- compileVariable1d r
    fIndex <- compileVariable1d f
    uIndex <- compileVariable1d u
    Compile.addVariable3d (Instruction.RFU rIndex fIndex uIndex)
  RFC r f u -> Compile.do
    rIndex <- compileVariable1d r
    fIndex <- compileVariable1d f
    uIndex <- Compile.addConstant1d u
    Compile.addVariable3d (Instruction.RFC rIndex fIndex uIndex)
  RCU r f u -> Compile.do
    rIndex <- compileVariable1d r
    fIndex <- Compile.addConstant1d f
    uIndex <- compileVariable1d u
    Compile.addVariable3d (Instruction.RCU rIndex fIndex uIndex)
  CFU r f u -> Compile.do
    rIndex <- Compile.addConstant1d r
    fIndex <- compileVariable1d f
    uIndex <- compileVariable1d u
    Compile.addVariable3d (Instruction.CFU rIndex fIndex uIndex)
  RCC r f u -> Compile.do
    rIndex <- compileVariable1d r
    fIndex <- Compile.addConstant1d f
    uIndex <- Compile.addConstant1d u
    Compile.addVariable3d (Instruction.RCC rIndex fIndex uIndex)
  CFC r f u -> Compile.do
    rIndex <- Compile.addConstant1d r
    fIndex <- compileVariable1d f
    uIndex <- Compile.addConstant1d u
    Compile.addVariable3d (Instruction.CFC rIndex fIndex uIndex)
  CCU r f u -> Compile.do
    rIndex <- Compile.addConstant1d r
    fIndex <- Compile.addConstant1d f
    uIndex <- compileVariable1d u
    Compile.addVariable3d (Instruction.CCU rIndex fIndex uIndex)
  Negated3d arg -> Compile.do
    argIndex <- compileVariable3d arg
    Compile.addVariable3d (Instruction.Negate3d argIndex)
  Sum3d lhs rhs -> Compile.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable3d rhs
    Compile.addVariable3d (Instruction.Add3d lhsIndex rhsIndex)
  SumVariableConstant3d lhs rhs -> Compile.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- Compile.addConstant3d rhs
    Compile.addVariable3d (Instruction.AddVariableConstant3d lhsIndex rhsIndex)
  Difference3d lhs rhs -> Compile.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable3d rhs
    Compile.addVariable3d (Instruction.Subtract3d lhsIndex rhsIndex)
  DifferenceConstantVariable3d lhs rhs -> Compile.do
    lhsIndex <- Compile.addConstant3d lhs
    rhsIndex <- compileVariable3d rhs
    Compile.addVariable3d (Instruction.SubtractConstantVariable3d lhsIndex rhsIndex)
  Product3d lhs rhs -> Compile.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable3d (Instruction.Multiply3d lhsIndex rhsIndex)
  ProductVariableConstant3d lhs rhs -> Compile.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- Compile.addConstant1d rhs
    Compile.addVariable3d (Instruction.MultiplyVariableConstant3d lhsIndex rhsIndex)
  ProductConstantVariable3d lhs rhs -> Compile.do
    lhsIndex <- Compile.addConstant3d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable3d (Instruction.MultiplyConstantVariable3d lhsIndex rhsIndex)
  Quotient3d lhs rhs -> Compile.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable3d (Instruction.Divide3d lhsIndex rhsIndex)
  QuotientConstantVariable3d lhs rhs -> Compile.do
    lhsIndex <- Compile.addConstant3d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable3d (Instruction.DivideConstantVariable3d lhsIndex rhsIndex)
  BezierCurve3d controlPoints parameter -> Compile.do
    let (rControlPoints, fControlPoints, uControlPoints) = NonEmpty.unzip3 (NonEmpty.map unwrap3d controlPoints)
    let flattenedControlPoints = rControlPoints <> fControlPoints <> uControlPoints
    controlPointsIndex <- Compile.addConstant flattenedControlPoints
    parameterIndex <- compileVariable1d parameter
    let numControlPoints = NonEmpty.length controlPoints
    let instruction = Instruction.Bezier3d numControlPoints controlPointsIndex parameterIndex
    Compile.addVariable3d instruction
  Cross3d lhs rhs -> Compile.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable3d rhs
    Compile.addVariable3d (Instruction.Cross3d lhsIndex rhsIndex)
  CrossVariableConstant3d lhs rhs -> Compile.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- Compile.addConstant3d rhs
    Compile.addVariable3d (Instruction.CrossVariableConstant3d lhsIndex rhsIndex)
  TransformVector3d transform vector -> Compile.do
    matrixIndex <- addTransform3d transform
    vectorIndex <- compileVariable3d vector
    Compile.addVariable3d (Instruction.TransformVector3d matrixIndex vectorIndex)
  TransformPoint3d transform point -> Compile.do
    matrixIndex <- addTransform3d transform
    pointIndex <- compileVariable3d point
    Compile.addVariable3d (Instruction.TransformPoint3d matrixIndex pointIndex)
  PlaceVector2d plane vector -> Compile.do
    planeIndex <- addPlane plane
    vectorIndex <- compileVariable2d vector
    Compile.addVariable3d (Instruction.PlaceVector2d planeIndex vectorIndex)
  PlacePoint2d plane point -> Compile.do
    planeIndex <- addPlane plane
    pointIndex <- compileVariable2d point
    Compile.addVariable3d (Instruction.PlacePoint2d planeIndex pointIndex)

unwrap3d :: Vector3d (space @ units) -> (Qty units, Qty units, Qty units)
unwrap3d (Vector3d r f u) = (r, f, u)

compileCurve1d :: Ast1d Float -> Compiled Float Float
compileCurve1d (Constant1d val) = Evaluate.Constant val
compileCurve1d (Variable1d var) = Evaluate.Bytecode (Compile.curve1d (compileVariable1d var))

compileCurve2d :: Ast2d Float -> Compiled Float (Vector2d Coordinates)
compileCurve2d (Constant2d val) = Evaluate.Constant val
compileCurve2d (Variable2d var) = Evaluate.Bytecode (Compile.curve2d (compileVariable2d var))

compileCurve3d :: Ast3d Float -> Compiled Float (Vector3d Coordinates)
compileCurve3d (Constant3d val) = Evaluate.Constant val
compileCurve3d (Variable3d var) = Evaluate.Bytecode (Compile.curve3d (compileVariable3d var))

compileSurface1d :: Ast1d UvPoint -> Compiled UvPoint Float
compileSurface1d (Constant1d val) = Evaluate.Constant val
compileSurface1d (Variable1d var) = Evaluate.Bytecode (Compile.surface1d (compileVariable1d var))

compileSurface2d :: Ast2d UvPoint -> Compiled UvPoint (Vector2d Coordinates)
compileSurface2d (Constant2d val) = Evaluate.Constant val
compileSurface2d (Variable2d var) = Evaluate.Bytecode (Compile.surface2d (compileVariable2d var))

compileSurface3d :: Ast3d UvPoint -> Compiled UvPoint (Vector3d Coordinates)
compileSurface3d (Constant3d val) = Evaluate.Constant val
compileSurface3d (Variable3d var) = Evaluate.Bytecode (Compile.surface3d (compileVariable3d var))

debugCurve1d :: Ast1d Float -> Text
debugCurve1d (Constant1d value) = "Constant: " <> Text.float value
debugCurve1d (Variable1d variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugCurve (compileVariable1d variable))
    ]

debugCurve2d :: Ast2d Float -> Text
debugCurve2d (Constant2d value) = "Constant: " <> Text.show value
debugCurve2d (Variable2d variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugCurve (compileVariable2d variable))
    ]

debugCurve3d :: Ast3d Float -> Text
debugCurve3d (Constant3d value) = "Constant: " <> Text.show value
debugCurve3d (Variable3d variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugCurve (compileVariable3d variable))
    ]

debugSurface1d :: Ast1d UvPoint -> Text
debugSurface1d (Constant1d value) = "Constant: " <> Text.float value
debugSurface1d (Variable1d variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugSurface (compileVariable1d variable))
    ]

debugSurface2d :: Ast2d UvPoint -> Text
debugSurface2d (Constant2d value) = "Constant: " <> Text.show value
debugSurface2d (Variable2d variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugSurface (compileVariable2d variable))
    ]

debugSurface3d :: Ast3d UvPoint -> Text
debugSurface3d (Constant3d value) = "Constant: " <> Text.show value
debugSurface3d (Variable3d variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugSurface (compileVariable3d variable))
    ]
