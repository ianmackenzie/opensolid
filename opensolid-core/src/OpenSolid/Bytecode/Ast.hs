module OpenSolid.Bytecode.Ast
  ( Ast1d
  , Ast2d
  , Ast3d
  , Coordinates
  , constant1d
  , constant2d
  , constant3d
  , curveParameter
  , surfaceParameter
  , xComponent2d
  , yComponent2d
  , xComponent3d
  , yComponent3d
  , zComponent3d
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
  , line1d
  , quadraticSpline1d
  , cubicSpline1d
  , quarticSpline1d
  , quinticSpline1d
  , bezierCurve1d
  , compileCurve1d
  , compileCurve2d
  , compileCurve3d
  , compileSurface1d
  , compileSurface2d
  , compileSurface3d
  )
where

import OpenSolid.Bytecode.Compilation qualified as Compilation
import OpenSolid.Bytecode.Instruction (ConstantIndex, VariableIndex (VariableIndex))
import OpenSolid.Bytecode.Instruction qualified as Instruction
import OpenSolid.Direction3d (Direction3d (Direction3d))
import OpenSolid.Float qualified as Float
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.PlanarBasis3d (PlanarBasis3d)
import OpenSolid.PlanarBasis3d qualified as PlanarBasis3d
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d (Point3d (Point3d))
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvPoint)
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d (Vector3d (Vector3d))
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds2d (VectorBounds2d)
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorBounds3d (VectorBounds3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d

data Space

type Coordinates = Space @ Unitless

type Plane = Plane3d Coordinates (Defines Space)

type PlanarBasis = PlanarBasis3d Space (Defines Space)

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
  XComponent3d :: Variable3d input -> Variable1d input
  YComponent3d :: Variable3d input -> Variable1d input
  ZComponent3d :: Variable3d input -> Variable1d input
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
  XY2d :: Variable1d input -> Variable1d input -> Variable2d input
  XC2d :: Variable1d input -> Float -> Variable2d input
  CY2d :: Float -> Variable1d input -> Variable2d input
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
  ProjectVector3d :: PlanarBasis -> Variable3d input -> Variable2d input
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
  XYZ3d :: Variable1d input -> Variable1d input -> Variable1d input -> Variable3d input
  XYC3d :: Variable1d input -> Variable1d input -> Float -> Variable3d input
  XCZ3d :: Variable1d input -> Float -> Variable1d input -> Variable3d input
  CYZ3d :: Float -> Variable1d input -> Variable1d input -> Variable3d input
  XCC3d :: Variable1d input -> Float -> Float -> Variable3d input
  CYC3d :: Float -> Variable1d input -> Float -> Variable3d input
  CCZ3d :: Float -> Float -> Variable1d input -> Variable3d input
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
  PlaceVector2d :: PlanarBasis -> Variable2d input -> Variable3d input
  PlacePoint2d :: Plane -> Variable2d input -> Variable3d input

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
  XComponent3d arg . input = XComponent3d (arg . input)
  YComponent3d arg . input = YComponent3d (arg . input)
  ZComponent3d arg . input = ZComponent3d (arg . input)
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
  TransformVector2d transform vector . input = TransformVector2d transform (vector . input)
  TransformPoint2d transform point . input = TransformPoint2d transform (point . input)
  ProjectVector3d basis vector . input = ProjectVector3d basis (vector . input)
  ProjectPoint3d plane point . input = ProjectPoint3d plane (point . input)

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
  TransformVector3d transform vector . input = TransformVector3d transform (vector . input)
  TransformPoint3d transform point . input = TransformPoint3d transform (point . input)
  PlaceVector2d basis vector . input = PlaceVector2d basis (vector . input)
  PlacePoint2d plane point . input = PlacePoint2d plane (point . input)

instance Composition (Ast2d input) (Ast1d UvPoint) (Ast1d input) where
  Constant1d outer . _ = Constant1d outer
  Variable1d outer . Variable2d inner = Variable1d (outer . inner)
  outer . Constant2d (Vector2d u v) = do
    let (f, _) = compileSurface1d outer
    Constant1d (f (Point2d u v))

instance Composition (Variable2d input) (Variable1d UvPoint) (Variable1d input) where
  SurfaceParameter U . input = XComponent2d input
  SurfaceParameter V . input = YComponent2d input
  XComponent2d arg . input = XComponent2d (arg . input)
  YComponent2d arg . input = YComponent2d (arg . input)
  XComponent3d arg . input = XComponent3d (arg . input)
  YComponent3d arg . input = YComponent3d (arg . input)
  ZComponent3d arg . input = ZComponent3d (arg . input)
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
  outer . Constant2d (Vector2d u v) = do
    let (f, _) = compileSurface2d outer
    Constant2d (f (Point2d u v))

instance Composition (Variable2d input) (Variable2d UvPoint) (Variable2d input) where
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
  TransformVector2d transform vector . input = TransformVector2d transform (vector . input)
  TransformPoint2d transform point . input = TransformPoint2d transform (point . input)
  ProjectVector3d basis vector . input = ProjectVector3d basis (vector . input)
  ProjectPoint3d plane point . input = ProjectPoint3d plane (point . input)

instance Composition (Ast2d input) (Ast3d UvPoint) (Ast3d input) where
  Constant3d outer . _ = Constant3d outer
  Variable3d outer . Variable2d inner = Variable3d (outer . inner)
  outer . Constant2d (Vector2d u v) = do
    let (f, _) = compileSurface3d outer
    Constant3d (f (Point2d u v))

instance Composition (Variable2d input) (Variable3d UvPoint) (Variable3d input) where
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
  TransformVector3d transform vector . input = TransformVector3d transform (vector . input)
  TransformPoint3d transform point . input = TransformPoint3d transform (point . input)
  PlaceVector2d basis vector . input = PlaceVector2d basis (vector . input)
  PlacePoint2d plane point . input = PlacePoint2d plane (point . input)

constant1d :: Qty units -> Ast1d input
constant1d value = Constant1d (Units.coerce value)

constant2d :: Vector2d (space @ units) -> Ast2d input
constant2d = Constant2d . Vector2d.coerce

constant3d :: Vector3d (space @ units) -> Ast3d input
constant3d = Constant3d . Vector3d.coerce

curveParameter :: Ast1d Float
curveParameter = Variable1d CurveParameter

surfaceParameter :: SurfaceParameter -> Ast1d UvPoint
surfaceParameter = Variable1d . SurfaceParameter

xComponent2d :: Ast2d input -> Ast1d input
xComponent2d (Constant2d val) = Constant1d (Vector2d.xComponent val)
xComponent2d (Variable2d var) = Variable1d (XComponent2d var)

yComponent2d :: Ast2d input -> Ast1d input
yComponent2d (Constant2d val) = Constant1d (Vector2d.yComponent val)
yComponent2d (Variable2d var) = Variable1d (YComponent2d var)

xComponent3d :: Ast3d input -> Ast1d input
xComponent3d (Constant3d val) = Constant1d (Vector3d.xComponent val)
xComponent3d (Variable3d var) = Variable1d (XComponent3d var)

yComponent3d :: Ast3d input -> Ast1d input
yComponent3d (Constant3d val) = Constant1d (Vector3d.yComponent val)
yComponent3d (Variable3d var) = Variable1d (YComponent3d var)

zComponent3d :: Ast3d input -> Ast1d input
zComponent3d (Constant3d val) = Constant1d (Vector3d.zComponent val)
zComponent3d (Variable3d var) = Variable1d (ZComponent3d var)

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

addTransform2d :: Transform2d.Affine Coordinates -> Compilation.Step ConstantIndex
addTransform2d (Transform2d p0 i j) = do
  let Vector2d ix iy = i
  let Vector2d jx jy = j
  let Point2d x0 y0 = p0
  Compilation.addConstant (ix :| [iy, jx, jy, x0, y0])

addTransform3d :: Transform3d.Affine Coordinates -> Compilation.Step ConstantIndex
addTransform3d (Transform3d p0 i j k) = do
  let Vector3d ix iy iz = i
  let Vector3d jx jy jz = j
  let Vector3d kx ky kz = k
  let Point3d x0 y0 z0 = p0
  Compilation.addConstant (ix :| [iy, iz, jx, jy, jz, kx, ky, kz, x0, y0, z0])

addPlanarBasis :: PlanarBasis -> Compilation.Step ConstantIndex
addPlanarBasis basis = do
  let Direction3d ix iy iz = PlanarBasis3d.xDirection basis
  let Direction3d jx jy jz = PlanarBasis3d.yDirection basis
  Compilation.addConstant (NonEmpty.six ix iy iz jx jy jz)

addPlane :: Plane -> Compilation.Step ConstantIndex
addPlane plane = do
  let Direction3d ix iy iz = Plane3d.xDirection plane
  let Direction3d jx jy jz = Plane3d.yDirection plane
  let Point3d x0 y0 z0 = Plane3d.originPoint plane
  Compilation.addConstant (ix :| [iy, iz, jx, jy, jz, x0, y0, z0])

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
  XComponent3d arg -> Compilation.do
    argIndex <- compileVariable3d arg
    Compilation.addVariable1d (Instruction.XComponent argIndex)
  YComponent3d arg -> Compilation.do
    argIndex <- compileVariable3d arg
    Compilation.addVariable1d (Instruction.YComponent argIndex)
  ZComponent3d arg -> Compilation.do
    argIndex <- compileVariable3d arg
    Compilation.addVariable1d (Instruction.ZComponent argIndex)
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
  SquaredMagnitude2d arg -> Compilation.do
    argIndex <- compileVariable2d arg
    Compilation.addVariable1d (Instruction.SquaredMagnitude2d argIndex)
  SquaredMagnitude3d arg -> Compilation.do
    argIndex <- compileVariable3d arg
    Compilation.addVariable1d (Instruction.SquaredMagnitude3d argIndex)
  Magnitude2d arg -> Compilation.do
    argIndex <- compileVariable2d arg
    Compilation.addVariable1d (Instruction.Magnitude2d argIndex)
  Magnitude3d arg -> Compilation.do
    argIndex <- compileVariable3d arg
    Compilation.addVariable1d (Instruction.Magnitude3d argIndex)
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
  Dot3d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable3d rhs
    Compilation.addVariable1d (Instruction.Dot3d lhsIndex rhsIndex)
  DotVariableConstant3d lhs rhs -> Compilation.do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- Compilation.addConstant3d rhs
    Compilation.addVariable1d (Instruction.DotVariableConstant3d lhsIndex rhsIndex)

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
    let (xControlPoints, yControlPoints) = NonEmpty.unzip2 (NonEmpty.map Vector2d.components controlPoints)
    let flattenedControlPoints = xControlPoints <> yControlPoints
    controlPointsIndex <- Compilation.addConstant flattenedControlPoints
    parameterIndex <- compileVariable1d parameter
    let numControlPoints = NonEmpty.length controlPoints
    let instruction = Instruction.Bezier2d numControlPoints controlPointsIndex parameterIndex
    Compilation.addVariable2d instruction
  TransformVector2d transform vector -> Compilation.do
    matrixIndex <- addTransform2d transform
    vectorIndex <- compileVariable2d vector
    Compilation.addVariable2d (Instruction.TransformVector2d matrixIndex vectorIndex)
  TransformPoint2d transform point -> Compilation.do
    matrixIndex <- addTransform2d transform
    pointIndex <- compileVariable2d point
    Compilation.addVariable2d (Instruction.TransformPoint2d matrixIndex pointIndex)
  ProjectVector3d basis vector -> Compilation.do
    basisIndex <- addPlanarBasis basis
    vectorIndex <- compileVariable3d vector
    Compilation.addVariable2d (Instruction.ProjectVector3d basisIndex vectorIndex)
  ProjectPoint3d plane point -> Compilation.do
    planeIndex <- addPlane plane
    pointIndex <- compileVariable3d point
    Compilation.addVariable2d (Instruction.ProjectPoint3d planeIndex pointIndex)

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
    let (xControlPoints, yControlPoints, zControlPoints) = NonEmpty.unzip3 (NonEmpty.map Vector3d.components controlPoints)
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
  TransformVector3d transform vector -> Compilation.do
    matrixIndex <- addTransform3d transform
    vectorIndex <- compileVariable3d vector
    Compilation.addVariable3d (Instruction.TransformVector3d matrixIndex vectorIndex)
  TransformPoint3d transform point -> Compilation.do
    matrixIndex <- addTransform3d transform
    pointIndex <- compileVariable3d point
    Compilation.addVariable3d (Instruction.TransformPoint3d matrixIndex pointIndex)
  PlaceVector2d basis vector -> Compilation.do
    basisIndex <- addPlanarBasis basis
    vectorIndex <- compileVariable2d vector
    Compilation.addVariable3d (Instruction.PlaceVector2d basisIndex vectorIndex)
  PlacePoint2d plane point -> Compilation.do
    planeIndex <- addPlane plane
    pointIndex <- compileVariable2d point
    Compilation.addVariable3d (Instruction.PlacePoint2d planeIndex pointIndex)

compileCurve1d :: Ast1d Float -> (Float -> Float, Range Unitless -> Range Unitless)
compileCurve1d (Constant1d value) = (always value, always (Range.constant value))
compileCurve1d (Variable1d variable) = Compilation.curve1d (compileVariable1d variable)

compileCurve2d ::
  Ast2d Float ->
  (Float -> Vector2d Coordinates, Range Unitless -> VectorBounds2d Coordinates)
compileCurve2d (Constant2d val) = (always val, always (VectorBounds2d.constant val))
compileCurve2d (Variable2d var) = Compilation.curve2d (compileVariable2d var)

compileCurve3d ::
  Ast3d Float ->
  (Float -> Vector3d Coordinates, Range Unitless -> VectorBounds3d Coordinates)
compileCurve3d (Constant3d val) = (always val, always (VectorBounds3d.constant val))
compileCurve3d (Variable3d variable) = Compilation.curve3d (compileVariable3d variable)

compileSurface1d :: Ast1d UvPoint -> (UvPoint -> Float, UvBounds -> Range Unitless)
compileSurface1d (Constant1d value) = (always value, always (Range.constant value))
compileSurface1d (Variable1d variable) = Compilation.surface1d (compileVariable1d variable)

compileSurface2d ::
  Ast2d UvPoint ->
  (UvPoint -> Vector2d Coordinates, UvBounds -> VectorBounds2d Coordinates)
compileSurface2d (Constant2d val) = (always val, always (VectorBounds2d.constant val))
compileSurface2d (Variable2d var) = Compilation.surface2d (compileVariable2d var)

compileSurface3d ::
  Ast3d UvPoint ->
  (UvPoint -> Vector3d Coordinates, UvBounds -> VectorBounds3d Coordinates)
compileSurface3d (Constant3d val) = (always val, always (VectorBounds3d.constant val))
compileSurface3d (Variable3d variable) = Compilation.surface3d (compileVariable3d variable)
