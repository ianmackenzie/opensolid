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
  , squared
  , sqrt
  , cubed
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
  , bezierCurve1d
  , bezierCurve2d
  , bezierCurve3d
  , desingularized1d
  , desingularized2d
  , desingularized3d
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
  , b00
  , b00d1
  , b00d2
  , b00d3
  , b01
  , b01d1
  , b01d2
  , b01d3
  , b02
  , b02d1
  , b02d2
  , b02d3
  , b10
  , b10d1
  , b10d2
  , b10d3
  , b11
  , b11d1
  , b11d2
  , b11d3
  )
where

import Data.Coerce qualified
import OpenSolid.Bytecode.Compile qualified as Compile
import OpenSolid.Bytecode.Evaluate (Compiled)
import OpenSolid.Bytecode.Evaluate qualified as Evaluate
import OpenSolid.Bytecode.Instruction (ConstantIndex, VariableIndex (VariableIndex))
import OpenSolid.Bytecode.Instruction qualified as Instruction
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction3d (Direction3d)
  , Point2d (Point2d, Position2d)
  , Point3d (Point3d, Position3d)
  , Vector3d (Vector3d)
  )
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Text qualified as Text
import OpenSolid.Transform2d (Transform2d (Transform2d))
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Transform3d (Transform3d (Transform3d))
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d qualified as Vector3d

data Space

type Coordinates = Space @ Unitless

type Plane = Plane3d Coordinates (Defines Space)

data Ast1d input where
  Constant1d :: Number -> Ast1d input
  Variable1d :: Variable1d input -> Ast1d input

deriving instance Eq (Ast1d input)

deriving instance Ord (Ast1d input)

deriving instance Show (Ast1d input)

data Variable1d input where
  CurveParameter :: Variable1d Number
  SurfaceParameter :: SurfaceParameter -> Variable1d UvPoint
  XComponent :: Variable2d input -> Variable1d input
  YComponent :: Variable2d input -> Variable1d input
  Negated1d :: Variable1d input -> Variable1d input
  Sum1d :: Variable1d input -> Variable1d input -> Variable1d input
  SumVariableConstant1d :: Variable1d input -> Number -> Variable1d input
  Difference1d :: Variable1d input -> Variable1d input -> Variable1d input
  DifferenceConstantVariable1d :: Number -> Variable1d input -> Variable1d input
  Product1d :: Variable1d input -> Variable1d input -> Variable1d input
  ProductVariableConstant1d :: Variable1d input -> Number -> Variable1d input
  Quotient1d :: Variable1d input -> Variable1d input -> Variable1d input
  QuotientConstantVariable1d :: Number -> Variable1d input -> Variable1d input
  Squared1d :: Variable1d input -> Variable1d input
  Sqrt1d :: Variable1d input -> Variable1d input
  Cubed1d :: Variable1d input -> Variable1d input
  Sin1d :: Variable1d input -> Variable1d input
  Cos1d :: Variable1d input -> Variable1d input
  BezierCurve1d :: NonEmpty Number -> Variable1d input -> Variable1d input
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
  Desingularized1d ::
    Variable1d input ->
    Variable1d input ->
    Variable1d input ->
    Variable1d input ->
    Variable1d input
  B00 :: Variable1d input -> Variable1d input
  B00d1 :: Variable1d input -> Variable1d input
  B00d2 :: Variable1d input -> Variable1d input
  B00d3 :: Variable1d input -> Variable1d input
  B01 :: Variable1d input -> Variable1d input
  B01d1 :: Variable1d input -> Variable1d input
  B01d2 :: Variable1d input -> Variable1d input
  B01d3 :: Variable1d input -> Variable1d input
  B02 :: Variable1d input -> Variable1d input
  B02d1 :: Variable1d input -> Variable1d input
  B02d2 :: Variable1d input -> Variable1d input
  B02d3 :: Variable1d input -> Variable1d input
  B10 :: Variable1d input -> Variable1d input
  B10d1 :: Variable1d input -> Variable1d input
  B10d2 :: Variable1d input -> Variable1d input
  B10d3 :: Variable1d input -> Variable1d input
  B11 :: Variable1d input -> Variable1d input
  B11d1 :: Variable1d input -> Variable1d input
  B11d2 :: Variable1d input -> Variable1d input
  B11d3 :: Variable1d input -> Variable1d input

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
  XC :: Variable1d input -> Number -> Variable2d input
  CY :: Number -> Variable1d input -> Variable2d input
  Negated2d :: Variable2d input -> Variable2d input
  Sum2d :: Variable2d input -> Variable2d input -> Variable2d input
  SumVariableConstant2d :: Variable2d input -> Vector2d Coordinates -> Variable2d input
  Difference2d :: Variable2d input -> Variable2d input -> Variable2d input
  DifferenceConstantVariable2d :: Vector2d Coordinates -> Variable2d input -> Variable2d input
  Product2d :: Variable2d input -> Variable1d input -> Variable2d input
  ProductVariableConstant2d :: Variable2d input -> Number -> Variable2d input
  ProductConstantVariable2d :: Vector2d Coordinates -> Variable1d input -> Variable2d input
  Quotient2d :: Variable2d input -> Variable1d input -> Variable2d input
  QuotientConstantVariable2d :: Vector2d Coordinates -> Variable1d input -> Variable2d input
  BezierCurve2d :: NonEmpty (Vector2d Coordinates) -> Variable1d input -> Variable2d input
  TransformVector2d :: Transform2d.Affine Coordinates -> Variable2d input -> Variable2d input
  TransformPoint2d :: Transform2d.Affine Coordinates -> Variable2d input -> Variable2d input
  ProjectVector3d :: Plane -> Variable3d input -> Variable2d input
  ProjectPoint3d :: Plane -> Variable3d input -> Variable2d input
  Desingularized2d ::
    Variable1d input ->
    Variable2d input ->
    Variable2d input ->
    Variable2d input ->
    Variable2d input

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
  Negated3d :: Variable3d input -> Variable3d input
  Sum3d :: Variable3d input -> Variable3d input -> Variable3d input
  SumVariableConstant3d :: Variable3d input -> Vector3d Coordinates -> Variable3d input
  Difference3d :: Variable3d input -> Variable3d input -> Variable3d input
  DifferenceConstantVariable3d :: Vector3d Coordinates -> Variable3d input -> Variable3d input
  Product3d :: Variable3d input -> Variable1d input -> Variable3d input
  ProductVariableConstant3d :: Variable3d input -> Number -> Variable3d input
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
  Desingularized3d ::
    Variable1d input ->
    Variable3d input ->
    Variable3d input ->
    Variable3d input ->
    Variable3d input

deriving instance Eq (Variable3d input)

deriving instance Ord (Variable3d input)

deriving instance Show (Variable3d input)

uvPoint :: Vector2d Coordinates -> UvPoint
uvPoint position = Point2d.coerce (Position2d position)

instance Composition (Ast1d input) (Ast1d Number) (Ast1d input) where
  Constant1d outer . _ = Constant1d outer
  Variable1d outer . Variable1d inner = outer . inner
  outer . Constant1d inner = Constant1d (evaluateCurve1d outer inner)

instance Composition (Variable1d input) (Variable1d Number) (Ast1d input) where
  input . CurveParameter = Variable1d input
  CurveParameter . input = Variable1d input
  XComponent arg . input = xComponent (arg . input)
  YComponent arg . input = yComponent (arg . input)
  Negated1d arg . input = negative (arg . input)
  Sum1d lhs rhs . input = lhs . input + rhs . input
  SumVariableConstant1d lhs rhs . input = lhs . input + rhs
  Difference1d lhs rhs . input = lhs . input - rhs . input
  DifferenceConstantVariable1d lhs rhs . input = lhs - rhs . input
  Product1d lhs rhs . input = lhs . input * rhs . input
  ProductVariableConstant1d lhs rhs . input = lhs . input * rhs
  Quotient1d lhs rhs . input = lhs . input / rhs . input
  QuotientConstantVariable1d lhs rhs . input = lhs / rhs . input
  Squared1d arg . input = squared (arg . input)
  Cubed1d arg . input = cubed (arg . input)
  Sqrt1d arg . input = sqrt (arg . input)
  Sin1d arg . input = sin (arg . input)
  Cos1d arg . input = cos (arg . input)
  BezierCurve1d controlPoints param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d (bezierCurve1d controlPoints) paramVal)
    Variable1d paramVar -> Variable1d (BezierCurve1d controlPoints paramVar)
  SquaredMagnitude2d arg . input = squaredMagnitude2d (arg . input)
  SquaredMagnitude3d arg . input = squaredMagnitude3d (arg . input)
  Magnitude2d arg . input = magnitude2d (arg . input)
  Magnitude3d arg . input = magnitude3d (arg . input)
  Dot2d lhs rhs . input = lhs . input `dot` rhs . input
  DotVariableConstant2d lhs rhs . input = lhs . input `dot` rhs
  Cross2d lhs rhs . input = lhs . input `cross` rhs . input
  CrossVariableConstant2d lhs rhs . input = lhs . input `cross` rhs
  Dot3d lhs rhs . input = lhs . input `dot` rhs . input
  DotVariableConstant3d lhs rhs . input = lhs . input `dot` rhs
  Desingularized1d parameter left middle right . input =
    desingularized1d (parameter . input) (left . input) (middle . input) (right . input)
  B00 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b00 paramVal)
    Variable1d paramVar -> Variable1d (B00 paramVar)
  B00d1 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b00d1 paramVal)
    Variable1d paramVar -> Variable1d (B00d1 paramVar)
  B00d2 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b00d2 paramVal)
    Variable1d paramVar -> Variable1d (B00d2 paramVar)
  B00d3 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b00d3 paramVal)
    Variable1d paramVar -> Variable1d (B00d3 paramVar)
  B01 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b01 paramVal)
    Variable1d paramVar -> Variable1d (B01 paramVar)
  B01d1 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b01d1 paramVal)
    Variable1d paramVar -> Variable1d (B01d1 paramVar)
  B01d2 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b01d2 paramVal)
    Variable1d paramVar -> Variable1d (B01d2 paramVar)
  B01d3 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b01d3 paramVal)
    Variable1d paramVar -> Variable1d (B01d3 paramVar)
  B02 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b02 paramVal)
    Variable1d paramVar -> Variable1d (B02 paramVar)
  B02d1 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b02d1 paramVal)
    Variable1d paramVar -> Variable1d (B02d1 paramVar)
  B02d2 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b02d2 paramVal)
    Variable1d paramVar -> Variable1d (B02d2 paramVar)
  B02d3 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b02d3 paramVal)
    Variable1d paramVar -> Variable1d (B02d3 paramVar)
  B10 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b10 paramVal)
    Variable1d paramVar -> Variable1d (B10 paramVar)
  B10d1 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b10d1 paramVal)
    Variable1d paramVar -> Variable1d (B10d1 paramVar)
  B10d2 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b10d2 paramVal)
    Variable1d paramVar -> Variable1d (B10d2 paramVar)
  B10d3 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b10d3 paramVal)
    Variable1d paramVar -> Variable1d (B10d3 paramVar)
  B11 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b11 paramVal)
    Variable1d paramVar -> Variable1d (B11 paramVar)
  B11d1 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b11d1 paramVal)
    Variable1d paramVar -> Variable1d (B11d1 paramVar)
  B11d2 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b11d2 paramVal)
    Variable1d paramVar -> Variable1d (B11d2 paramVar)
  B11d3 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b11d3 paramVal)
    Variable1d paramVar -> Variable1d (B11d3 paramVar)

instance Composition (Ast1d input) (Ast2d Number) (Ast2d input) where
  Constant2d outer . _ = Constant2d outer
  Variable2d outer . Variable1d inner = outer . inner
  outer . Constant1d inner = Constant2d (evaluateCurve2d outer inner)

instance Composition (Variable1d input) (Variable2d Number) (Ast2d input) where
  input . CurveParameter = Variable2d input
  XY x y . input = xy (x . input) (y . input)
  XC x y . input = xy (x . input) (Constant1d y)
  CY x y . input = xy (Constant1d x) (y . input)
  Negated2d arg . input = negative (arg . input)
  Sum2d lhs rhs . input = lhs . input + rhs . input
  SumVariableConstant2d lhs rhs . input = lhs . input + rhs
  Difference2d lhs rhs . input = lhs . input - rhs . input
  DifferenceConstantVariable2d lhs rhs . input = lhs - rhs . input
  Product2d lhs rhs . input = lhs . input * rhs . input
  ProductVariableConstant2d lhs rhs . input = lhs . input * rhs
  ProductConstantVariable2d lhs rhs . input = Constant2d lhs * rhs . input
  Quotient2d lhs rhs . input = lhs . input / rhs . input
  QuotientConstantVariable2d lhs rhs . input = lhs / rhs . input
  BezierCurve2d controlPoints param . input = case param . input of
    Constant1d paramVal -> Constant2d (evaluateCurve2d (bezierCurve2d controlPoints) paramVal)
    Variable1d paramVar -> Variable2d (BezierCurve2d controlPoints paramVar)
  TransformVector2d transform vector . input = transformVector2d transform (vector . input)
  TransformPoint2d transform point . input = transformPoint2d transform (point . input)
  ProjectVector3d plane vector . input = projectVector3dInto plane (vector . input)
  ProjectPoint3d plane point . input = projectPoint3dInto plane (point . input)
  Desingularized2d parameter left middle right . input =
    desingularized2d (parameter . input) (left . input) (middle . input) (right . input)

instance Composition (Ast1d input) (Ast3d Number) (Ast3d input) where
  Constant3d outer . _ = Constant3d outer
  Variable3d outer . Variable1d inner = outer . inner
  outer . Constant1d inner = Constant3d (evaluateCurve3d outer inner)

instance Composition (Variable1d input) (Variable3d Number) (Ast3d input) where
  input . CurveParameter = Variable3d input
  Negated3d arg . input = negative (arg . input)
  Sum3d lhs rhs . input = lhs . input + rhs . input
  SumVariableConstant3d lhs rhs . input = lhs . input + rhs
  Difference3d lhs rhs . input = lhs . input - rhs . input
  DifferenceConstantVariable3d lhs rhs . input = lhs - rhs . input
  Product3d lhs rhs . input = lhs . input * rhs . input
  ProductVariableConstant3d lhs rhs . input = lhs . input * rhs
  ProductConstantVariable3d lhs rhs . input = Constant3d lhs * rhs . input
  Quotient3d lhs rhs . input = lhs . input / rhs . input
  QuotientConstantVariable3d lhs rhs . input = Constant3d lhs / rhs . input
  BezierCurve3d controlPoints param . input = case param . input of
    Constant1d paramVal -> Constant3d (evaluateCurve3d (bezierCurve3d controlPoints) paramVal)
    Variable1d paramVar -> Variable3d (BezierCurve3d controlPoints paramVar)
  Cross3d lhs rhs . input = lhs . input `cross` rhs . input
  CrossVariableConstant3d lhs rhs . input = lhs . input `cross` rhs
  TransformVector3d transform vector . input = transformVector3d transform (vector . input)
  TransformPoint3d transform point . input = transformPoint3d transform (point . input)
  PlaceVector2d plane vector . input = placeVector2dOn plane (vector . input)
  PlacePoint2d plane point . input = placePoint2dOn plane (point . input)
  Desingularized3d parameter left middle right . input =
    desingularized3d (parameter . input) (left . input) (middle . input) (right . input)

instance Composition (Ast2d input) (Ast1d UvPoint) (Ast1d input) where
  Constant1d outer . _ = Constant1d outer
  Variable1d outer . Variable2d inner = outer . inner
  outer . Constant2d parameter = Constant1d (evaluateSurface1d outer (uvPoint parameter))

instance Composition (Variable2d input) (Variable1d UvPoint) (Ast1d input) where
  input . SurfaceParameters = Variable1d input
  SurfaceParameter U . input = xComponent (Variable2d input)
  SurfaceParameter V . input = yComponent (Variable2d input)
  XComponent arg . input = xComponent (arg . input)
  YComponent arg . input = yComponent (arg . input)
  Negated1d arg . input = negative (arg . input)
  Sum1d lhs rhs . input = lhs . input + rhs . input
  SumVariableConstant1d lhs rhs . input = lhs . input + rhs
  Difference1d lhs rhs . input = lhs . input - rhs . input
  DifferenceConstantVariable1d lhs rhs . input = lhs - rhs . input
  Product1d lhs rhs . input = lhs . input * rhs . input
  ProductVariableConstant1d lhs rhs . input = lhs . input * rhs
  Quotient1d lhs rhs . input = lhs . input / rhs . input
  QuotientConstantVariable1d lhs rhs . input = lhs / rhs . input
  Squared1d arg . input = squared (arg . input)
  Cubed1d arg . input = cubed (arg . input)
  Sqrt1d arg . input = sqrt (arg . input)
  Sin1d arg . input = sin (arg . input)
  Cos1d arg . input = cos (arg . input)
  BezierCurve1d controlPoints param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d (bezierCurve1d controlPoints) paramVal)
    Variable1d paramVar -> Variable1d (BezierCurve1d controlPoints paramVar)
  SquaredMagnitude2d arg . input = squaredMagnitude2d (arg . input)
  SquaredMagnitude3d arg . input = squaredMagnitude3d (arg . input)
  Magnitude2d arg . input = magnitude2d (arg . input)
  Magnitude3d arg . input = magnitude3d (arg . input)
  Dot2d lhs rhs . input = lhs . input `dot` rhs . input
  DotVariableConstant2d lhs rhs . input = lhs . input `dot` rhs
  Cross2d lhs rhs . input = lhs . input `cross` rhs . input
  CrossVariableConstant2d lhs rhs . input = lhs . input `cross` rhs
  Dot3d lhs rhs . input = lhs . input `dot` rhs . input
  DotVariableConstant3d lhs rhs . input = lhs . input `dot` rhs
  Desingularized1d parameter left middle right . input =
    desingularized1d (parameter . input) (left . input) (middle . input) (right . input)
  B00 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b00 paramVal)
    Variable1d paramVar -> Variable1d (B00 paramVar)
  B00d1 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b00d1 paramVal)
    Variable1d paramVar -> Variable1d (B00d1 paramVar)
  B00d2 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b00d2 paramVal)
    Variable1d paramVar -> Variable1d (B00d2 paramVar)
  B00d3 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b00d3 paramVal)
    Variable1d paramVar -> Variable1d (B00d3 paramVar)
  B01 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b01 paramVal)
    Variable1d paramVar -> Variable1d (B01 paramVar)
  B01d1 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b01d1 paramVal)
    Variable1d paramVar -> Variable1d (B01d1 paramVar)
  B01d2 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b01d2 paramVal)
    Variable1d paramVar -> Variable1d (B01d2 paramVar)
  B01d3 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b01d3 paramVal)
    Variable1d paramVar -> Variable1d (B01d3 paramVar)
  B02 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b02 paramVal)
    Variable1d paramVar -> Variable1d (B02 paramVar)
  B02d1 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b02d1 paramVal)
    Variable1d paramVar -> Variable1d (B02d1 paramVar)
  B02d2 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b02d2 paramVal)
    Variable1d paramVar -> Variable1d (B02d2 paramVar)
  B02d3 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b02d3 paramVal)
    Variable1d paramVar -> Variable1d (B02d3 paramVar)
  B10 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b10 paramVal)
    Variable1d paramVar -> Variable1d (B10 paramVar)
  B10d1 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b10d1 paramVal)
    Variable1d paramVar -> Variable1d (B10d1 paramVar)
  B10d2 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b10d2 paramVal)
    Variable1d paramVar -> Variable1d (B10d2 paramVar)
  B10d3 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b10d3 paramVal)
    Variable1d paramVar -> Variable1d (B10d3 paramVar)
  B11 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b11 paramVal)
    Variable1d paramVar -> Variable1d (B11 paramVar)
  B11d1 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b11d1 paramVal)
    Variable1d paramVar -> Variable1d (B11d1 paramVar)
  B11d2 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b11d2 paramVal)
    Variable1d paramVar -> Variable1d (B11d2 paramVar)
  B11d3 param . input = case param . input of
    Constant1d paramVal -> Constant1d (evaluateCurve1d b11d3 paramVal)
    Variable1d paramVar -> Variable1d (B11d3 paramVar)

instance Composition (Ast2d input) (Ast2d UvPoint) (Ast2d input) where
  Constant2d outer . _ = Constant2d outer
  Variable2d outer . Variable2d inner = outer . inner
  outer . Constant2d parameter = Constant2d (evaluateSurface2d outer (uvPoint parameter))

instance Composition (Variable2d input) (Variable2d UvPoint) (Ast2d input) where
  input . SurfaceParameters = Variable2d input
  SurfaceParameters . input = Variable2d input
  XY x y . input = xy (x . input) (y . input)
  XC x y . input = xy (x . input) (Constant1d y)
  CY x y . input = xy (Constant1d x) (y . input)
  Negated2d arg . input = negative (arg . input)
  Sum2d lhs rhs . input = lhs . input + rhs . input
  SumVariableConstant2d lhs rhs . input = lhs . input + rhs
  Difference2d lhs rhs . input = lhs . input - rhs . input
  DifferenceConstantVariable2d lhs rhs . input = lhs - rhs . input
  Product2d lhs rhs . input = lhs . input * rhs . input
  ProductVariableConstant2d lhs rhs . input = lhs . input * rhs
  ProductConstantVariable2d lhs rhs . input = Constant2d lhs * rhs . input
  Quotient2d lhs rhs . input = lhs . input / rhs . input
  QuotientConstantVariable2d lhs rhs . input = lhs / rhs . input
  BezierCurve2d controlPoints param . input = case param . input of
    Constant1d paramVal -> Constant2d (evaluateCurve2d (bezierCurve2d controlPoints) paramVal)
    Variable1d paramVar -> Variable2d (BezierCurve2d controlPoints paramVar)
  TransformVector2d transform vector . input = transformVector2d transform (vector . input)
  TransformPoint2d transform point . input = transformPoint2d transform (point . input)
  ProjectVector3d plane vector . input = projectVector3dInto plane (vector . input)
  ProjectPoint3d plane point . input = projectPoint3dInto plane (point . input)
  Desingularized2d parameter left middle right . input =
    desingularized2d (parameter . input) (left . input) (middle . input) (right . input)

instance Composition (Ast2d input) (Ast3d UvPoint) (Ast3d input) where
  Constant3d outer . _ = Constant3d outer
  Variable3d outer . Variable2d inner = outer . inner
  outer . Constant2d parameter = Constant3d (evaluateSurface3d outer (uvPoint parameter))

instance Composition (Variable2d input) (Variable3d UvPoint) (Ast3d input) where
  input . SurfaceParameters = Variable3d input
  Negated3d arg . input = negative (arg . input)
  Sum3d lhs rhs . input = lhs . input + rhs . input
  SumVariableConstant3d lhs rhs . input = lhs . input + rhs
  Difference3d lhs rhs . input = lhs . input - rhs . input
  DifferenceConstantVariable3d lhs rhs . input = lhs - rhs . input
  Product3d lhs rhs . input = lhs . input * rhs . input
  ProductVariableConstant3d lhs rhs . input = lhs . input * rhs
  ProductConstantVariable3d lhs rhs . input = Constant3d lhs * rhs . input
  Quotient3d lhs rhs . input = lhs . input / rhs . input
  QuotientConstantVariable3d lhs rhs . input = lhs / rhs . input
  BezierCurve3d controlPoints param . input = case param . input of
    Constant1d paramVal -> Constant3d (evaluateCurve3d (bezierCurve3d controlPoints) paramVal)
    Variable1d paramVar -> Variable3d (BezierCurve3d controlPoints paramVar)
  Cross3d lhs rhs . input = lhs . input `cross` rhs . input
  CrossVariableConstant3d lhs rhs . input = lhs . input `cross` rhs
  TransformVector3d transform vector . input = transformVector3d transform (vector . input)
  TransformPoint3d transform point . input = transformPoint3d transform (point . input)
  PlaceVector2d plane vector . input = placeVector2dOn plane (vector . input)
  PlacePoint2d plane point . input = placePoint2dOn plane (point . input)
  Desingularized3d parameter left middle right . input =
    desingularized3d (parameter . input) (left . input) (middle . input) (right . input)

constant1d :: Quantity units -> Ast1d input
constant1d value = Constant1d (Quantity.coerce value)

constant2d :: Vector2d (space @ units) -> Ast2d input
constant2d = Constant2d . Vector2d.coerce

constant3d :: Vector3d (space @ units) -> Ast3d input
constant3d = Constant3d . Vector3d.coerce

curveParameter :: Ast1d Number
curveParameter = Variable1d CurveParameter

surfaceParameter :: SurfaceParameter -> Ast1d UvPoint
surfaceParameter = Variable1d . SurfaceParameter

surfaceParameters :: Ast2d UvPoint
surfaceParameters = Variable2d SurfaceParameters

xComponent :: Ast2d input -> Ast1d input
xComponent (Constant2d val) = Constant1d (Vector2d.xComponent val)
xComponent (Variable2d (XY xVar _)) = Variable1d xVar
xComponent (Variable2d (XC xVar _)) = Variable1d xVar
xComponent (Variable2d (CY xVal _)) = Constant1d xVal
xComponent (Variable2d (BezierCurve2d controlPoints param)) =
  Variable1d (BezierCurve1d (NonEmpty.map Vector2d.xComponent controlPoints) param)
xComponent (Variable2d var) = Variable1d (XComponent var)

yComponent :: Ast2d input -> Ast1d input
yComponent (Constant2d val) = Constant1d (Vector2d.yComponent val)
yComponent (Variable2d (XY _ yVar)) = Variable1d yVar
yComponent (Variable2d (XC _ yVal)) = Constant1d yVal
yComponent (Variable2d (CY _ yVar)) = Variable1d yVar
yComponent (Variable2d (BezierCurve2d controlPoints param)) =
  Variable1d (BezierCurve1d (NonEmpty.map Vector2d.yComponent controlPoints) param)
yComponent (Variable2d var) = Variable1d (YComponent var)

instance Negation (Ast1d input) where
  negative (Constant1d val) = Constant1d (negative val)
  negative (Variable1d var) = Variable1d (negative var)

instance Negation (Variable1d input) where
  negative (Negated1d arg) = arg
  negative (Difference1d lhs rhs) = Difference1d rhs lhs
  negative (SumVariableConstant1d lhs rhs) = DifferenceConstantVariable1d (negative rhs) lhs
  negative (DifferenceConstantVariable1d lhs rhs) = SumVariableConstant1d rhs (negative lhs)
  negative (ProductVariableConstant1d lhs rhs) = ProductVariableConstant1d lhs (negative rhs)
  negative (QuotientConstantVariable1d lhs rhs) = QuotientConstantVariable1d (negative lhs) rhs
  negative (Cubed1d arg) = Cubed1d (negative arg)
  negative (Sin1d arg) = Sin1d (negative arg)
  negative (BezierCurve1d controlPoints param) =
    BezierCurve1d (NonEmpty.map negative controlPoints) param
  negative (DotVariableConstant2d lhs rhs) = DotVariableConstant2d lhs (negative rhs)
  negative (CrossVariableConstant2d lhs rhs) = CrossVariableConstant2d lhs (negative rhs)
  negative (DotVariableConstant3d lhs rhs) = DotVariableConstant3d lhs (negative rhs)
  negative var = Negated1d var

instance Multiplication Sign (Ast1d input) (Ast1d input) where
  Positive * ast = ast
  Negative * ast = negative ast

instance Multiplication (Ast1d input) Sign (Ast1d input) where
  ast * Positive = ast
  ast * Negative = negative ast

instance Multiplication Sign (Variable1d input) (Variable1d input) where
  Positive * var = var
  Negative * var = negative var

instance Multiplication (Variable1d input) Sign (Variable1d input) where
  var * Positive = var
  var * Negative = negative var

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

instance Addition (Quantity units) (Ast1d input) (Ast1d input) where
  lhs + rhs = constant1d lhs + rhs

instance Addition (Ast1d input1) (Quantity units) (Ast1d input1) where
  lhs + rhs = lhs + constant1d rhs

instance input1 ~ input2 => Subtraction (Ast1d input1) (Ast1d input2) (Ast1d input1) where
  lhs - Constant1d 0.0 = lhs
  Constant1d 0.0 - rhs = negative rhs
  Constant1d lhs - Constant1d rhs = Constant1d (lhs - rhs)
  Constant1d lhs - Variable1d rhs = Variable1d (DifferenceConstantVariable1d lhs rhs)
  Variable1d lhs - Constant1d rhs = Variable1d (SumVariableConstant1d lhs (negative rhs))
  Variable1d lhs - Variable1d rhs = Variable1d (lhs - rhs)

instance
  input1 ~ input2 =>
  Subtraction (Variable1d input1) (Variable1d input2) (Variable1d input1)
  where
  lhs - rhs = Difference1d lhs rhs

instance Subtraction (Quantity units) (Ast1d input) (Ast1d input) where
  lhs - rhs = constant1d lhs - rhs

instance Subtraction (Ast1d input1) (Quantity units) (Ast1d input1) where
  lhs - rhs = lhs - constant1d rhs

instance input1 ~ input2 => Multiplication (Ast1d input1) (Ast1d input2) (Ast1d input1) where
  Constant1d lhs * Constant1d rhs = Constant1d (lhs * rhs)
  _ * Constant1d 0.0 = Constant1d 0.0
  Constant1d 0.0 * _ = Constant1d 0.0
  lhs * Constant1d 1.0 = lhs
  Constant1d 1.0 * rhs = rhs
  lhs * Constant1d -1.0 = negative lhs
  Constant1d -1.0 * rhs = negative rhs
  Variable1d (ProductVariableConstant1d a b) * Constant1d c = Variable1d a * Constant1d (b * c)
  Constant1d a * Variable1d (ProductVariableConstant1d b c) = Constant1d (a * c) * Variable1d b
  Variable1d lhs * Constant1d rhs = Variable1d (ProductVariableConstant1d lhs rhs)
  Constant1d lhs * Variable1d rhs = Variable1d (ProductVariableConstant1d rhs lhs)
  Variable1d lhs * Variable1d rhs =
    Variable1d (if lhs <= rhs then Product1d lhs rhs else Product1d rhs lhs)

instance Multiplication (Quantity units) (Ast1d input) (Ast1d input) where
  lhs * rhs = constant1d lhs * rhs

instance Multiplication (Ast1d input1) (Quantity units) (Ast1d input1) where
  lhs * rhs = lhs * constant1d rhs

instance input1 ~ input2 => Division (Ast1d input1) (Ast1d input2) (Ast1d input1) where
  Constant1d lhs / Constant1d rhs = Constant1d (lhs / rhs)
  Constant1d 0.0 / _ = Constant1d 0.0
  lhs / Constant1d 1.0 = lhs
  lhs / Constant1d -1.0 = negative lhs
  Variable1d lhs / Constant1d rhs = Variable1d (ProductVariableConstant1d lhs (1.0 /. rhs))
  Constant1d lhs / Variable1d rhs = Variable1d (QuotientConstantVariable1d lhs rhs)
  Variable1d lhs / Variable1d rhs = Variable1d (lhs / rhs)

instance
  input1 ~ input2 =>
  Division (Variable1d input1) (Variable1d input2) (Variable1d input1)
  where
  lhs / rhs = Quotient1d lhs rhs

instance Division (Quantity units) (Ast1d input) (Ast1d input) where
  lhs / rhs = constant1d lhs / rhs

instance Division (Ast1d input) (Quantity units) (Ast1d input) where
  lhs / rhs = lhs / constant1d rhs

instance Negation (Ast2d input) where
  negative (Constant2d val) = Constant2d (negative val)
  negative (Variable2d var) = Variable2d (negative var)

instance Negation (Variable2d input) where
  negative (Negated2d arg) = arg
  negative (Difference2d lhs rhs) = Difference2d rhs lhs
  negative var = Negated2d var

instance Multiplication Sign (Ast2d input) (Ast2d input) where
  Positive * ast = ast
  Negative * ast = negative ast

instance Multiplication (Ast2d input) Sign (Ast2d input) where
  ast * Positive = ast
  ast * Negative = negative ast

instance Multiplication Sign (Variable2d input) (Variable2d input) where
  Positive * var = var
  Negative * var = negative var

instance Multiplication (Variable2d input) Sign (Variable2d input) where
  var * Positive = var
  var * Negative = negative var

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
  Constant2d lhs - rhs | lhs == Vector2d.zero = negative rhs
  Constant2d lhs - Constant2d rhs = Constant2d (lhs - rhs)
  Constant2d lhs - Variable2d rhs = Variable2d (DifferenceConstantVariable2d lhs rhs)
  Variable2d lhs - Constant2d rhs = Variable2d (SumVariableConstant2d lhs (negative rhs))
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
  lhs * Constant1d -1.0 = negative lhs
  Variable2d (ProductVariableConstant2d a b) * Constant1d c = Variable2d a * Constant1d (b * c)
  Constant2d a * Variable1d (ProductVariableConstant1d b c) = Constant2d (a * c) * Variable1d b
  Variable2d lhs * Constant1d rhs = Variable2d (ProductVariableConstant2d lhs rhs)
  Constant2d lhs * Variable1d rhs = Variable2d (ProductConstantVariable2d lhs rhs)
  Variable2d lhs * Variable1d rhs = Variable2d (Product2d lhs rhs)

instance input1 ~ input2 => Multiplication (Ast1d input1) (Ast2d input2) (Ast2d input1) where
  lhs * rhs = rhs * lhs

instance Multiplication (Ast2d input1) (Quantity units) (Ast2d input1) where
  lhs * rhs = lhs * constant1d rhs

instance Multiplication (Quantity units) (Ast2d input) (Ast2d input) where
  lhs * rhs = constant1d lhs * rhs

instance input1 ~ input2 => Division (Ast2d input1) (Ast1d input2) (Ast2d input1) where
  Constant2d lhs / Constant1d rhs = Constant2d (lhs / rhs)
  Constant2d lhs / _ | lhs == Vector2d.zero = Constant2d Vector2d.zero
  lhs / Constant1d 1.0 = lhs
  lhs / Constant1d -1.0 = negative lhs
  Variable2d lhs / Constant1d rhs = Variable2d (ProductVariableConstant2d lhs (1.0 /. rhs))
  Constant2d lhs / Variable1d rhs = Variable2d (QuotientConstantVariable2d lhs rhs)
  Variable2d lhs / Variable1d rhs = Variable2d (Quotient2d lhs rhs)

instance Division (Vector2d (space @ units)) (Ast1d input) (Ast2d input) where
  lhs / rhs = constant2d lhs / rhs

instance Division (Ast2d input) (Quantity units) (Ast2d input) where
  lhs / rhs = lhs / constant1d rhs

instance Negation (Ast3d input) where
  negative (Constant3d val) = Constant3d (negative val)
  negative (Variable3d var) = Variable3d (negative var)

instance Negation (Variable3d input) where
  negative (Negated3d arg) = arg
  negative (Difference3d lhs rhs) = Difference3d rhs lhs
  negative var = Negated3d var

instance Multiplication Sign (Ast3d input) (Ast3d input) where
  Positive * ast = ast
  Negative * ast = negative ast

instance Multiplication (Ast3d input) Sign (Ast3d input) where
  ast * Positive = ast
  ast * Negative = negative ast

instance Multiplication Sign (Variable3d input) (Variable3d input) where
  Positive * var = var
  Negative * var = negative var

instance Multiplication (Variable3d input) Sign (Variable3d input) where
  var * Positive = var
  var * Negative = negative var

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
  Constant3d lhs - rhs | lhs == Vector3d.zero = negative rhs
  Constant3d lhs - Constant3d rhs = Constant3d (lhs - rhs)
  Constant3d lhs - Variable3d rhs = Variable3d (DifferenceConstantVariable3d lhs rhs)
  Variable3d lhs - Constant3d rhs = Variable3d (SumVariableConstant3d lhs (negative rhs))
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
  lhs * Constant1d -1.0 = negative lhs
  Variable3d (ProductVariableConstant3d a b) * Constant1d c = Variable3d a * Constant1d (b * c)
  Constant3d a * Variable1d (ProductVariableConstant1d b c) = Constant3d (a * c) * Variable1d b
  Variable3d lhs * Constant1d rhs = Variable3d (ProductVariableConstant3d lhs rhs)
  Constant3d lhs * Variable1d rhs = Variable3d (ProductConstantVariable3d lhs rhs)
  Variable3d lhs * Variable1d rhs = Variable3d (Product3d lhs rhs)

instance input1 ~ input2 => Multiplication (Ast1d input1) (Ast3d input2) (Ast3d input1) where
  lhs * rhs = rhs * lhs

instance Multiplication (Ast3d input1) (Quantity units) (Ast3d input1) where
  lhs * rhs = lhs * constant1d rhs

instance Multiplication (Quantity units) (Ast3d input) (Ast3d input) where
  lhs * rhs = constant1d lhs * rhs

instance input1 ~ input2 => Division (Ast3d input1) (Ast1d input2) (Ast3d input1) where
  Constant3d lhs / Constant1d rhs = Constant3d (lhs / rhs)
  Constant3d lhs / _ | lhs == Vector3d.zero = Constant3d Vector3d.zero
  lhs / Constant1d 1.0 = lhs
  lhs / Constant1d -1.0 = negative lhs
  Variable3d lhs / Constant1d rhs = Variable3d (ProductVariableConstant3d lhs (1.0 /. rhs))
  Constant3d lhs / Variable1d rhs = Variable3d (QuotientConstantVariable3d lhs rhs)
  Variable3d lhs / Variable1d rhs = Variable3d (Quotient3d lhs rhs)

instance Division (Vector3d (space @ units)) (Ast1d input) (Ast3d input) where
  lhs / rhs = constant3d lhs / rhs

instance Division (Ast3d input) (Quantity units) (Ast3d input) where
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
  Constant2d lhs `cross` Variable2d rhs = Variable1d (CrossVariableConstant2d rhs (negative lhs))
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
  Constant3d lhs `cross` Variable3d rhs = Variable3d (CrossVariableConstant3d rhs (negative lhs))
  Variable3d lhs `cross` Variable3d rhs = Variable3d (Cross3d lhs rhs)

instance CrossMultiplication (Vector3d (space @ units)) (Ast3d input) (Ast3d input) where
  lhs `cross` rhs = constant3d lhs `cross` rhs

instance CrossMultiplication (Ast3d input) (Vector3d (space @ units)) (Ast3d input) where
  lhs `cross` rhs = lhs `cross` constant3d rhs

squared :: Ast1d input -> Ast1d input
squared ast = case ast of
  Constant1d val -> Constant1d (Number.squared val)
  Variable1d (Negated1d arg) -> Variable1d (Squared1d arg)
  Variable1d (Sqrt1d arg) -> Variable1d arg
  Variable1d var -> Variable1d (Squared1d var)

sqrt :: Ast1d input -> Ast1d input
sqrt (Constant1d value) = Constant1d (Number.sqrt value)
sqrt (Variable1d var) = Variable1d (Sqrt1d var)

cubed :: Ast1d input -> Ast1d input
cubed (Constant1d value) = Constant1d (Number.cubed value)
cubed (Variable1d var) = Variable1d (Cubed1d var)

sin :: Ast1d input -> Ast1d input
sin (Constant1d val) = Constant1d (Number.sin val)
sin (Variable1d var) = Variable1d (Sin1d var)

cos :: Ast1d input -> Ast1d input
cos (Constant1d value) = constant1d (Number.cos value)
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
    Variable2d (BezierCurve2d controlPoints param) -> do
      let transformedControlPoints =
            NonEmpty.map (Vector2d.transformBy erasedTransform) controlPoints
      Variable2d (BezierCurve2d transformedControlPoints param)
    Variable2d var -> Variable2d (TransformVector2d erasedTransform var)

transformVector3d :: Transform3d tag (space @ units) -> Ast3d input -> Ast3d input
transformVector3d transform ast = do
  let erasedTransform = Transform3d.coerce transform
  case ast of
    Constant3d val -> Constant3d (Vector3d.transformBy erasedTransform val)
    Variable3d (TransformVector3d existing var) ->
      Variable3d (TransformVector3d (erasedTransform . existing) var)
    Variable3d (BezierCurve3d controlPoints param) -> do
      let transformedControlPoints =
            NonEmpty.map (Vector3d.transformBy erasedTransform) controlPoints
      Variable3d (BezierCurve3d transformedControlPoints param)
    Variable3d var -> Variable3d (TransformVector3d erasedTransform var)

transformPoint2d :: Transform2d tag (space @ units) -> Ast2d input -> Ast2d input
transformPoint2d transform ast = do
  let erasedTransform = Transform2d.coerce transform
  case ast of
    Constant2d val -> do
      let Position2d transformed = Point2d.transformBy erasedTransform (Position2d val)
      Constant2d transformed
    Variable2d (TransformPoint2d existing var) ->
      Variable2d (TransformPoint2d (erasedTransform . existing) var)
    Variable2d (BezierCurve2d controlPoints param) -> do
      let transformedControlPoints =
            controlPoints
              |> Data.Coerce.coerce -- convert list of Vector2d to list of Point2d
              |> NonEmpty.map (Point2d.transformBy erasedTransform)
              |> Data.Coerce.coerce -- convert list of Point2d back to list of Vector2d
      Variable2d (BezierCurve2d transformedControlPoints param)
    Variable2d var -> Variable2d (TransformPoint2d erasedTransform var)

transformPoint3d :: Transform3d tag (space @ units) -> Ast3d input -> Ast3d input
transformPoint3d transform ast = do
  let erasedTransform = Transform3d.coerce transform
  case ast of
    Constant3d val -> do
      let Position3d transformed = Point3d.transformBy erasedTransform (Position3d val)
      Constant3d transformed
    Variable3d (TransformPoint3d existing var) ->
      Variable3d (TransformPoint3d (erasedTransform . existing) var)
    Variable3d (BezierCurve3d controlPoints param) -> do
      let transformedControlPoints =
            controlPoints
              |> Data.Coerce.coerce -- convert list of Vector3d to list of Point3d
              |> NonEmpty.map (Point3d.transformBy erasedTransform)
              |> Data.Coerce.coerce -- convert list of Point3d back to list of Vector3d
      Variable3d (BezierCurve3d transformedControlPoints param)
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
  Constant2d val -> Constant3d (Vector2d.placeOn (Plane3d.coerce plane) val)
  Variable2d var -> Variable3d (PlaceVector2d (Plane3d.coerce plane) var)

placePoint2dOn :: Plane3d (global @ units) (Defines local) -> Ast2d input -> Ast3d input
placePoint2dOn plane ast = case ast of
  Constant2d val -> do
    let Position3d placed = Point3d.on (Plane3d.coerce plane) (Position2d val)
    Constant3d placed
  Variable2d var -> Variable3d (PlacePoint2d (Plane3d.coerce plane) var)

projectVector3dInto :: Plane3d (global @ planeUnits) (Defines local) -> Ast3d input -> Ast2d input
projectVector3dInto plane ast = case ast of
  Constant3d val -> Constant2d (Vector3d.projectInto (Plane3d.coerce plane) val)
  Variable3d var -> Variable2d (ProjectVector3d (Plane3d.coerce plane) var)

projectPoint3dInto :: Plane3d (global @ units) (Defines local) -> Ast3d input -> Ast2d input
projectPoint3dInto plane ast = case ast of
  Constant3d val -> do
    let Position2d projected = Point3d.projectInto (Plane3d.coerce plane) (Position3d val)
    Constant2d projected
  Variable3d var -> Variable2d (ProjectPoint3d (Plane3d.coerce plane) var)

xy :: Ast1d input -> Ast1d input -> Ast2d input
xy (Constant1d x) (Constant1d y) = Constant2d (Vector2d x y)
xy (Constant1d x) (Variable1d y) = Variable2d (CY x y)
xy (Variable1d x) (Constant1d y) = Variable2d (XC x y)
xy (Variable1d x) (Variable1d y) = Variable2d (XY x y)

bezierCurve1d :: NonEmpty (Quantity units) -> Ast1d Number
bezierCurve1d (NonEmpty.One value) = constant1d value
bezierCurve1d controlPoints =
  Variable1d (BezierCurve1d (NonEmpty.map Quantity.coerce controlPoints) CurveParameter)

bezierCurve2d :: NonEmpty (Vector2d (space @ units)) -> Ast2d Number
bezierCurve2d (NonEmpty.One value) = constant2d value
bezierCurve2d controlPoints =
  Variable2d (BezierCurve2d (NonEmpty.map Vector2d.coerce controlPoints) CurveParameter)

bezierCurve3d :: NonEmpty (Vector3d (space @ units)) -> Ast3d Number
bezierCurve3d (NonEmpty.One value) = constant3d value
bezierCurve3d controlPoints =
  Variable3d (BezierCurve3d (NonEmpty.map Vector3d.coerce controlPoints) CurveParameter)

b00 :: Ast1d Number
b00 = Variable1d (B00 CurveParameter)

b00d1 :: Ast1d Number
b00d1 = Variable1d (B00d1 CurveParameter)

b00d2 :: Ast1d Number
b00d2 = Variable1d (B00d2 CurveParameter)

b00d3 :: Ast1d Number
b00d3 = Variable1d (B00d3 CurveParameter)

b01 :: Ast1d Number
b01 = Variable1d (B01 CurveParameter)

b01d1 :: Ast1d Number
b01d1 = Variable1d (B01d1 CurveParameter)

b01d2 :: Ast1d Number
b01d2 = Variable1d (B01d2 CurveParameter)

b01d3 :: Ast1d Number
b01d3 = Variable1d (B01d3 CurveParameter)

b02 :: Ast1d Number
b02 = Variable1d (B02 CurveParameter)

b02d1 :: Ast1d Number
b02d1 = Variable1d (B02d1 CurveParameter)

b02d2 :: Ast1d Number
b02d2 = Variable1d (B02d2 CurveParameter)

b02d3 :: Ast1d Number
b02d3 = Variable1d (B02d3 CurveParameter)

b10 :: Ast1d Number
b10 = Variable1d (B10 CurveParameter)

b10d1 :: Ast1d Number
b10d1 = Variable1d (B10d1 CurveParameter)

b10d2 :: Ast1d Number
b10d2 = Variable1d (B10d2 CurveParameter)

b10d3 :: Ast1d Number
b10d3 = Variable1d (B10d3 CurveParameter)

b11 :: Ast1d Number
b11 = Variable1d (B11 CurveParameter)

b11d1 :: Ast1d Number
b11d1 = Variable1d (B11d1 CurveParameter)

b11d2 :: Ast1d Number
b11d2 = Variable1d (B11d2 CurveParameter)

b11d3 :: Ast1d Number
b11d3 = Variable1d (B11d3 CurveParameter)

desingularized1d :: Ast1d input -> Ast1d input -> Ast1d input -> Ast1d input -> Ast1d input
desingularized1d (Constant1d parameter) left middle right =
  Desingularization.value parameter left middle right
desingularized1d _ (Constant1d left) _ _ = Constant1d left
desingularized1d _ _ (Constant1d middle) _ = Constant1d middle
desingularized1d _ _ _ (Constant1d right) = Constant1d right
desingularized1d (Variable1d parameter) (Variable1d left) (Variable1d middle) (Variable1d right) =
  Variable1d (Desingularized1d parameter left middle right)

desingularized2d :: Ast1d input -> Ast2d input -> Ast2d input -> Ast2d input -> Ast2d input
desingularized2d (Constant1d parameter) left middle right =
  Desingularization.value parameter left middle right
desingularized2d _ (Constant2d left) _ _ = Constant2d left
desingularized2d _ _ (Constant2d middle) _ = Constant2d middle
desingularized2d _ _ _ (Constant2d right) = Constant2d right
desingularized2d (Variable1d parameter) (Variable2d left) (Variable2d middle) (Variable2d right) =
  Variable2d (Desingularized2d parameter left middle right)

desingularized3d :: Ast1d input -> Ast3d input -> Ast3d input -> Ast3d input -> Ast3d input
desingularized3d (Constant1d parameter) left middle right =
  Desingularization.value parameter left middle right
desingularized3d _ (Constant3d left) _ _ = Constant3d left
desingularized3d _ _ (Constant3d middle) _ = Constant3d middle
desingularized3d _ _ _ (Constant3d right) = Constant3d right
desingularized3d (Variable1d parameter) (Variable3d left) (Variable3d middle) (Variable3d right) =
  Variable3d (Desingularized3d parameter left middle right)

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
  CurveParameter -> return (VariableIndex 0)
  SurfaceParameter U -> return (VariableIndex 0)
  SurfaceParameter V -> return (VariableIndex 1)
  XComponent arg -> do
    argIndex <- compileVariable2d arg
    Compile.addVariable1d (Instruction.Component0 argIndex)
  YComponent arg -> do
    argIndex <- compileVariable2d arg
    Compile.addVariable1d (Instruction.Component1 argIndex)
  Negated1d arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.Negate1d argIndex)
  Sum1d lhs rhs -> do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable1d (Instruction.Add1d lhsIndex rhsIndex)
  SumVariableConstant1d lhs rhs -> do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- Compile.addConstant1d rhs
    Compile.addVariable1d (Instruction.AddVariableConstant1d lhsIndex rhsIndex)
  Difference1d lhs rhs -> do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable1d (Instruction.Subtract1d lhsIndex rhsIndex)
  DifferenceConstantVariable1d lhs rhs -> do
    lhsIndex <- Compile.addConstant1d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable1d (Instruction.SubtractConstantVariable1d lhsIndex rhsIndex)
  Product1d lhs rhs -> do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable1d (Instruction.Multiply1d lhsIndex rhsIndex)
  ProductVariableConstant1d lhs rhs -> do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- Compile.addConstant1d rhs
    Compile.addVariable1d (Instruction.MultiplyVariableConstant1d lhsIndex rhsIndex)
  Quotient1d lhs rhs -> do
    lhsIndex <- compileVariable1d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable1d (Instruction.Divide1d lhsIndex rhsIndex)
  QuotientConstantVariable1d lhs rhs -> do
    lhsIndex <- Compile.addConstant1d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable1d (Instruction.DivideConstantVariable1d lhsIndex rhsIndex)
  Squared1d arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.Square1d argIndex)
  Cubed1d arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.Cube1d argIndex)
  Sqrt1d arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.Sqrt1d argIndex)
  Sin1d arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.Sin1d argIndex)
  Cos1d arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.Cos1d argIndex)
  BezierCurve1d controlPoints parameter -> do
    controlPointsIndex <- Compile.addConstant controlPoints
    parameterIndex <- compileVariable1d parameter
    let numControlPoints = NonEmpty.length controlPoints
    let instruction = Instruction.Bezier1d numControlPoints controlPointsIndex parameterIndex
    Compile.addVariable1d instruction
  SquaredMagnitude2d arg -> do
    argIndex <- compileVariable2d arg
    Compile.addVariable1d (Instruction.SquaredMagnitude2d argIndex)
  SquaredMagnitude3d arg -> do
    argIndex <- compileVariable3d arg
    Compile.addVariable1d (Instruction.SquaredMagnitude3d argIndex)
  Magnitude2d arg -> do
    argIndex <- compileVariable2d arg
    Compile.addVariable1d (Instruction.Magnitude2d argIndex)
  Magnitude3d arg -> do
    argIndex <- compileVariable3d arg
    Compile.addVariable1d (Instruction.Magnitude3d argIndex)
  Dot2d lhs rhs -> do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable2d rhs
    Compile.addVariable1d (Instruction.Dot2d lhsIndex rhsIndex)
  DotVariableConstant2d lhs rhs -> do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- Compile.addConstant2d rhs
    Compile.addVariable1d (Instruction.DotVariableConstant2d lhsIndex rhsIndex)
  Cross2d lhs rhs -> do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable2d rhs
    Compile.addVariable1d (Instruction.Cross2d lhsIndex rhsIndex)
  CrossVariableConstant2d lhs rhs -> do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- Compile.addConstant2d rhs
    Compile.addVariable1d (Instruction.CrossVariableConstant2d lhsIndex rhsIndex)
  Dot3d lhs rhs -> do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable3d rhs
    Compile.addVariable1d (Instruction.Dot3d lhsIndex rhsIndex)
  DotVariableConstant3d lhs rhs -> do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- Compile.addConstant3d rhs
    Compile.addVariable1d (Instruction.DotVariableConstant3d lhsIndex rhsIndex)
  Desingularized1d parameter left middle right -> do
    parameterIndex <- compileVariable1d parameter
    leftIndex <- compileVariable1d left
    middleIndex <- compileVariable1d middle
    rightIndex <- compileVariable1d right
    let instruction = Instruction.Desingularized1d parameterIndex leftIndex middleIndex rightIndex
    Compile.addVariable1d instruction
  B00 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B00 argIndex)
  B00d1 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B00d1 argIndex)
  B00d2 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B00d2 argIndex)
  B00d3 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B00d3 argIndex)
  B01 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B01 argIndex)
  B01d1 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B01d1 argIndex)
  B01d2 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B01d2 argIndex)
  B01d3 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B01d3 argIndex)
  B02 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B02 argIndex)
  B02d1 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B02d1 argIndex)
  B02d2 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B02d2 argIndex)
  B02d3 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B02d3 argIndex)
  B10 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B10 argIndex)
  B10d1 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B10d1 argIndex)
  B10d2 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B10d2 argIndex)
  B10d3 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B10d3 argIndex)
  B11 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B11 argIndex)
  B11d1 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B11d1 argIndex)
  B11d2 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B11d2 argIndex)
  B11d3 arg -> do
    argIndex <- compileVariable1d arg
    Compile.addVariable1d (Instruction.B11d3 argIndex)

coordinates2d :: Vector2d Coordinates -> NonEmpty Number
coordinates2d (Vector2d x y) = NonEmpty.two x y

coordinates3d :: Vector3d Coordinates -> NonEmpty Number
coordinates3d (Vector3d r f u) = NonEmpty.three r f u

compileVariable2d :: Variable2d input -> Compile.Step VariableIndex
compileVariable2d variable = case variable of
  SurfaceParameters -> return (VariableIndex 0)
  XY x y -> do
    xIndex <- compileVariable1d x
    yIndex <- compileVariable1d y
    Compile.addVariable2d (Instruction.XY xIndex yIndex)
  XC x y -> do
    xIndex <- compileVariable1d x
    yIndex <- Compile.addConstant1d y
    Compile.addVariable2d (Instruction.XC xIndex yIndex)
  CY x y -> do
    xIndex <- Compile.addConstant1d x
    yIndex <- compileVariable1d y
    Compile.addVariable2d (Instruction.CY xIndex yIndex)
  Negated2d arg -> do
    argIndex <- compileVariable2d arg
    Compile.addVariable2d (Instruction.Negate2d argIndex)
  Sum2d lhs rhs -> do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable2d rhs
    Compile.addVariable2d (Instruction.Add2d lhsIndex rhsIndex)
  SumVariableConstant2d lhs rhs -> do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- Compile.addConstant2d rhs
    Compile.addVariable2d (Instruction.AddVariableConstant2d lhsIndex rhsIndex)
  Difference2d lhs rhs -> do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable2d rhs
    Compile.addVariable2d (Instruction.Subtract2d lhsIndex rhsIndex)
  DifferenceConstantVariable2d lhs rhs -> do
    lhsIndex <- Compile.addConstant2d lhs
    rhsIndex <- compileVariable2d rhs
    Compile.addVariable2d (Instruction.SubtractConstantVariable2d lhsIndex rhsIndex)
  Product2d lhs rhs -> do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable2d (Instruction.Multiply2d lhsIndex rhsIndex)
  ProductVariableConstant2d lhs rhs -> do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- Compile.addConstant1d rhs
    Compile.addVariable2d (Instruction.MultiplyVariableConstant2d lhsIndex rhsIndex)
  ProductConstantVariable2d lhs rhs -> do
    lhsIndex <- Compile.addConstant2d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable2d (Instruction.MultiplyConstantVariable2d lhsIndex rhsIndex)
  Quotient2d lhs rhs -> do
    lhsIndex <- compileVariable2d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable2d (Instruction.Divide2d lhsIndex rhsIndex)
  QuotientConstantVariable2d lhs rhs -> do
    lhsIndex <- Compile.addConstant2d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable2d (Instruction.DivideConstantVariable2d lhsIndex rhsIndex)
  BezierCurve2d controlPoints parameter -> do
    let numControlPoints = NonEmpty.length controlPoints
    controlPointsIndex <- Compile.addConstant (NonEmpty.combine coordinates2d controlPoints)
    parameterIndex <- compileVariable1d parameter
    let instruction = Instruction.Bezier2d numControlPoints controlPointsIndex parameterIndex
    Compile.addVariable2d instruction
  TransformVector2d transform vector -> do
    matrixIndex <- addTransform2d transform
    vectorIndex <- compileVariable2d vector
    Compile.addVariable2d (Instruction.TransformVector2d matrixIndex vectorIndex)
  TransformPoint2d transform point -> do
    matrixIndex <- addTransform2d transform
    pointIndex <- compileVariable2d point
    Compile.addVariable2d (Instruction.TransformPoint2d matrixIndex pointIndex)
  ProjectVector3d plane vector -> do
    planeIndex <- addPlane plane
    vectorIndex <- compileVariable3d vector
    Compile.addVariable2d (Instruction.ProjectVector3d planeIndex vectorIndex)
  ProjectPoint3d plane point -> do
    planeIndex <- addPlane plane
    pointIndex <- compileVariable3d point
    Compile.addVariable2d (Instruction.ProjectPoint3d planeIndex pointIndex)
  Desingularized2d parameter left middle right -> do
    parameterIndex <- compileVariable1d parameter
    leftIndex <- compileVariable2d left
    middleIndex <- compileVariable2d middle
    rightIndex <- compileVariable2d right
    let instruction = Instruction.Desingularized2d parameterIndex leftIndex middleIndex rightIndex
    Compile.addVariable2d instruction

compileVariable3d :: Variable3d input -> Compile.Step VariableIndex
compileVariable3d variable = case variable of
  Negated3d arg -> do
    argIndex <- compileVariable3d arg
    Compile.addVariable3d (Instruction.Negate3d argIndex)
  Sum3d lhs rhs -> do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable3d rhs
    Compile.addVariable3d (Instruction.Add3d lhsIndex rhsIndex)
  SumVariableConstant3d lhs rhs -> do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- Compile.addConstant3d rhs
    Compile.addVariable3d (Instruction.AddVariableConstant3d lhsIndex rhsIndex)
  Difference3d lhs rhs -> do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable3d rhs
    Compile.addVariable3d (Instruction.Subtract3d lhsIndex rhsIndex)
  DifferenceConstantVariable3d lhs rhs -> do
    lhsIndex <- Compile.addConstant3d lhs
    rhsIndex <- compileVariable3d rhs
    Compile.addVariable3d (Instruction.SubtractConstantVariable3d lhsIndex rhsIndex)
  Product3d lhs rhs -> do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable3d (Instruction.Multiply3d lhsIndex rhsIndex)
  ProductVariableConstant3d lhs rhs -> do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- Compile.addConstant1d rhs
    Compile.addVariable3d (Instruction.MultiplyVariableConstant3d lhsIndex rhsIndex)
  ProductConstantVariable3d lhs rhs -> do
    lhsIndex <- Compile.addConstant3d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable3d (Instruction.MultiplyConstantVariable3d lhsIndex rhsIndex)
  Quotient3d lhs rhs -> do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable3d (Instruction.Divide3d lhsIndex rhsIndex)
  QuotientConstantVariable3d lhs rhs -> do
    lhsIndex <- Compile.addConstant3d lhs
    rhsIndex <- compileVariable1d rhs
    Compile.addVariable3d (Instruction.DivideConstantVariable3d lhsIndex rhsIndex)
  BezierCurve3d controlPoints parameter -> do
    let numControlPoints = NonEmpty.length controlPoints
    controlPointsIndex <- Compile.addConstant (NonEmpty.combine coordinates3d controlPoints)
    parameterIndex <- compileVariable1d parameter
    let instruction = Instruction.Bezier3d numControlPoints controlPointsIndex parameterIndex
    Compile.addVariable3d instruction
  Cross3d lhs rhs -> do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- compileVariable3d rhs
    Compile.addVariable3d (Instruction.Cross3d lhsIndex rhsIndex)
  CrossVariableConstant3d lhs rhs -> do
    lhsIndex <- compileVariable3d lhs
    rhsIndex <- Compile.addConstant3d rhs
    Compile.addVariable3d (Instruction.CrossVariableConstant3d lhsIndex rhsIndex)
  TransformVector3d transform vector -> do
    matrixIndex <- addTransform3d transform
    vectorIndex <- compileVariable3d vector
    Compile.addVariable3d (Instruction.TransformVector3d matrixIndex vectorIndex)
  TransformPoint3d transform point -> do
    matrixIndex <- addTransform3d transform
    pointIndex <- compileVariable3d point
    Compile.addVariable3d (Instruction.TransformPoint3d matrixIndex pointIndex)
  PlaceVector2d plane vector -> do
    planeIndex <- addPlane plane
    vectorIndex <- compileVariable2d vector
    Compile.addVariable3d (Instruction.PlaceVector2d planeIndex vectorIndex)
  PlacePoint2d plane point -> do
    planeIndex <- addPlane plane
    pointIndex <- compileVariable2d point
    Compile.addVariable3d (Instruction.PlacePoint2d planeIndex pointIndex)
  Desingularized3d parameter left middle right -> do
    parameterIndex <- compileVariable1d parameter
    leftIndex <- compileVariable3d left
    middleIndex <- compileVariable3d middle
    rightIndex <- compileVariable3d right
    let instruction = Instruction.Desingularized3d parameterIndex leftIndex middleIndex rightIndex
    Compile.addVariable3d instruction

compileCurve1d :: Ast1d Number -> Compiled Number Number
compileCurve1d (Constant1d val) = Evaluate.Constant val
compileCurve1d (Variable1d var) = Evaluate.Bytecode (Compile.curve1d (compileVariable1d var))

compileCurve2d :: Ast2d Number -> Compiled Number (Vector2d Coordinates)
compileCurve2d (Constant2d val) = Evaluate.Constant val
compileCurve2d (Variable2d var) = Evaluate.Bytecode (Compile.curve2d (compileVariable2d var))

compileCurve3d :: Ast3d Number -> Compiled Number (Vector3d Coordinates)
compileCurve3d (Constant3d val) = Evaluate.Constant val
compileCurve3d (Variable3d var) = Evaluate.Bytecode (Compile.curve3d (compileVariable3d var))

compileSurface1d :: Ast1d UvPoint -> Compiled UvPoint Number
compileSurface1d (Constant1d val) = Evaluate.Constant val
compileSurface1d (Variable1d var) = Evaluate.Bytecode (Compile.surface1d (compileVariable1d var))

compileSurface2d :: Ast2d UvPoint -> Compiled UvPoint (Vector2d Coordinates)
compileSurface2d (Constant2d val) = Evaluate.Constant val
compileSurface2d (Variable2d var) = Evaluate.Bytecode (Compile.surface2d (compileVariable2d var))

compileSurface3d :: Ast3d UvPoint -> Compiled UvPoint (Vector3d Coordinates)
compileSurface3d (Constant3d val) = Evaluate.Constant val
compileSurface3d (Variable3d var) = Evaluate.Bytecode (Compile.surface3d (compileVariable3d var))

evaluateCurve1d :: Ast1d Number -> Number -> Number
evaluateCurve1d ast input = Evaluate.curve1dValue (compileCurve1d ast) input

evaluateCurve2d :: Ast2d Number -> Number -> Vector2d Coordinates
evaluateCurve2d ast input = Evaluate.curve2dValue (compileCurve2d ast) input

evaluateCurve3d :: Ast3d Number -> Number -> Vector3d Coordinates
evaluateCurve3d ast input = Evaluate.curve3dValue (compileCurve3d ast) input

evaluateSurface1d :: Ast1d UvPoint -> UvPoint -> Number
evaluateSurface1d ast input = Evaluate.surface1dValue (compileSurface1d ast) input

evaluateSurface2d :: Ast2d UvPoint -> UvPoint -> Vector2d Coordinates
evaluateSurface2d ast input = Evaluate.surface2dValue (compileSurface2d ast) input

evaluateSurface3d :: Ast3d UvPoint -> UvPoint -> Vector3d Coordinates
evaluateSurface3d ast input = Evaluate.surface3dValue (compileSurface3d ast) input

debugCurve1d :: Ast1d Number -> Text
debugCurve1d (Constant1d value) = "Constant: " <> Text.number value
debugCurve1d (Variable1d variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugCurve (compileVariable1d variable))
    ]

debugCurve2d :: Ast2d Number -> Text
debugCurve2d (Constant2d value) = "Constant: " <> Text.show value
debugCurve2d (Variable2d variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugCurve (compileVariable2d variable))
    ]

debugCurve3d :: Ast3d Number -> Text
debugCurve3d (Constant3d value) = "Constant: " <> Text.show value
debugCurve3d (Variable3d variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugCurve (compileVariable3d variable))
    ]

debugSurface1d :: Ast1d UvPoint -> Text
debugSurface1d (Constant1d value) = "Constant: " <> Text.number value
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
