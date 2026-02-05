module OpenSolid.Bytecode.Ast
  ( Ast1D
  , Ast2D
  , Ast3D
  , Compiled (Constant, Bytecode)
  , Space
  , constant1D
  , constant2D
  , constant3D
  , curveParameter
  , surfaceParameter
  , xComponent
  , yComponent
  , squared
  , sqrt
  , cubed
  , sin
  , cos
  , squaredMagnitude2D
  , squaredMagnitude3D
  , magnitude2D
  , magnitude3D
  , transformVector2D
  , transformPoint2D
  , transformVector3D
  , transformPoint3D
  , placeVector2DIn
  , placePoint2DIn
  , placeVector3dIn
  , placePoint3dIn
  , placeVector2DOn
  , placePoint2DOn
  , projectVector3dInto
  , projectPoint3dInto
  , surfaceParameters
  , xy
  , bezierCurve1D
  , bezierCurve2D
  , bezierCurve3D
  , desingularized1D
  , desingularized2D
  , desingularized3D
  , compileCurve1D
  , compileCurve2D
  , compileCurve3D
  , compileSurface1D
  , compileSurface2D
  , compileSurface3D
  , debugCurve1D
  , debugCurve2D
  , debugCurve3D
  , debugSurface1D
  , debugSurface2D
  , debugSurface3D
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
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Length qualified as Length
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction3D (Direction3D)
  , Point2D (Point2D, Position2D)
  , Point3D (Point3D, Position3D)
  , Vector3D (Vector3D)
  )
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Text qualified as Text
import OpenSolid.Transform2D (Transform2D (Transform2D))
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Transform3D (Transform3D (Transform3D))
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D qualified as Vector3D

data Space

type Plane = Plane3D Space Space

data Ast1D input where
  Constant1D :: Number -> Ast1D input
  Variable1D :: Variable1D input -> Ast1D input

deriving instance Eq (Ast1D input)

deriving instance Ord (Ast1D input)

deriving instance Show (Ast1D input)

data Variable1D input where
  CurveParameter :: Variable1D Number
  SurfaceParameter :: SurfaceParameter -> Variable1D UvPoint
  XComponent :: Variable2D input -> Variable1D input
  YComponent :: Variable2D input -> Variable1D input
  Negated1D :: Variable1D input -> Variable1D input
  Sum1D :: Variable1D input -> Variable1D input -> Variable1D input
  SumVariableConstant1D :: Variable1D input -> Number -> Variable1D input
  Difference1D :: Variable1D input -> Variable1D input -> Variable1D input
  DifferenceConstantVariable1D :: Number -> Variable1D input -> Variable1D input
  Product1D :: Variable1D input -> Variable1D input -> Variable1D input
  ProductVariableConstant1D :: Variable1D input -> Number -> Variable1D input
  Quotient1D :: Variable1D input -> Variable1D input -> Variable1D input
  QuotientConstantVariable1D :: Number -> Variable1D input -> Variable1D input
  Squared1D :: Variable1D input -> Variable1D input
  Sqrt1D :: Variable1D input -> Variable1D input
  Cubed1D :: Variable1D input -> Variable1D input
  Sin1D :: Variable1D input -> Variable1D input
  Cos1D :: Variable1D input -> Variable1D input
  BezierCurve1D :: NonEmpty Number -> Variable1D input -> Variable1D input
  SquaredMagnitude2D :: Variable2D input -> Variable1D input
  SquaredMagnitude3D :: Variable3D input -> Variable1D input
  Magnitude2D :: Variable2D input -> Variable1D input
  Magnitude3D :: Variable3D input -> Variable1D input
  Dot2D :: Variable2D input -> Variable2D input -> Variable1D input
  DotVariableConstant2D :: Variable2D input -> Vector2D Unitless Space -> Variable1D input
  Cross2D :: Variable2D input -> Variable2D input -> Variable1D input
  CrossVariableConstant2D :: Variable2D input -> Vector2D Unitless Space -> Variable1D input
  Dot3D :: Variable3D input -> Variable3D input -> Variable1D input
  DotVariableConstant3D :: Variable3D input -> Vector3D Unitless Space -> Variable1D input
  Desingularized1D ::
    Variable1D input ->
    Variable1D input ->
    Variable1D input ->
    Variable1D input ->
    Variable1D input
  B00 :: Variable1D input -> Variable1D input
  B00d1 :: Variable1D input -> Variable1D input
  B00d2 :: Variable1D input -> Variable1D input
  B00d3 :: Variable1D input -> Variable1D input
  B01 :: Variable1D input -> Variable1D input
  B01d1 :: Variable1D input -> Variable1D input
  B01d2 :: Variable1D input -> Variable1D input
  B01d3 :: Variable1D input -> Variable1D input
  B02 :: Variable1D input -> Variable1D input
  B02d1 :: Variable1D input -> Variable1D input
  B02d2 :: Variable1D input -> Variable1D input
  B02d3 :: Variable1D input -> Variable1D input
  B10 :: Variable1D input -> Variable1D input
  B10d1 :: Variable1D input -> Variable1D input
  B10d2 :: Variable1D input -> Variable1D input
  B10d3 :: Variable1D input -> Variable1D input
  B11 :: Variable1D input -> Variable1D input
  B11d1 :: Variable1D input -> Variable1D input
  B11d2 :: Variable1D input -> Variable1D input
  B11d3 :: Variable1D input -> Variable1D input

deriving instance Eq (Variable1D input)

deriving instance Ord (Variable1D input)

deriving instance Show (Variable1D input)

data Ast2D input where
  Constant2D :: Vector2D Unitless Space -> Ast2D input
  Variable2D :: Variable2D input -> Ast2D input

deriving instance Eq (Ast2D input)

deriving instance Ord (Ast2D input)

deriving instance Show (Ast2D input)

data Variable2D input where
  SurfaceParameters :: Variable2D UvPoint
  XY :: Variable1D input -> Variable1D input -> Variable2D input
  XC :: Variable1D input -> Number -> Variable2D input
  CY :: Number -> Variable1D input -> Variable2D input
  Negated2D :: Variable2D input -> Variable2D input
  Sum2D :: Variable2D input -> Variable2D input -> Variable2D input
  SumVariableConstant2D :: Variable2D input -> Vector2D Unitless Space -> Variable2D input
  Difference2D :: Variable2D input -> Variable2D input -> Variable2D input
  DifferenceConstantVariable2D :: Vector2D Unitless Space -> Variable2D input -> Variable2D input
  Product2D :: Variable2D input -> Variable1D input -> Variable2D input
  ProductVariableConstant2D :: Variable2D input -> Number -> Variable2D input
  ProductConstantVariable2D :: Vector2D Unitless Space -> Variable1D input -> Variable2D input
  Quotient2D :: Variable2D input -> Variable1D input -> Variable2D input
  QuotientConstantVariable2D :: Vector2D Unitless Space -> Variable1D input -> Variable2D input
  BezierCurve2D :: NonEmpty (Vector2D Unitless Space) -> Variable1D input -> Variable2D input
  TransformVector2D :: Transform2D.Affine Unitless Space -> Variable2D input -> Variable2D input
  TransformPoint2D :: Transform2D.Affine Unitless Space -> Variable2D input -> Variable2D input
  ProjectVector3D :: Plane -> Variable3D input -> Variable2D input
  ProjectPoint3D :: Plane -> Variable3D input -> Variable2D input
  Desingularized2D ::
    Variable1D input ->
    Variable2D input ->
    Variable2D input ->
    Variable2D input ->
    Variable2D input

deriving instance Eq (Variable2D input)

deriving instance Ord (Variable2D input)

deriving instance Show (Variable2D input)

data Ast3D input where
  Constant3D :: Vector3D Unitless Space -> Ast3D input
  Variable3D :: Variable3D input -> Ast3D input

deriving instance Eq (Ast3D input)

deriving instance Ord (Ast3D input)

deriving instance Show (Ast3D input)

data Variable3D input where
  Negated3D :: Variable3D input -> Variable3D input
  Sum3D :: Variable3D input -> Variable3D input -> Variable3D input
  SumVariableConstant3D :: Variable3D input -> Vector3D Unitless Space -> Variable3D input
  Difference3D :: Variable3D input -> Variable3D input -> Variable3D input
  DifferenceConstantVariable3D :: Vector3D Unitless Space -> Variable3D input -> Variable3D input
  Product3D :: Variable3D input -> Variable1D input -> Variable3D input
  ProductVariableConstant3D :: Variable3D input -> Number -> Variable3D input
  ProductConstantVariable3D :: Vector3D Unitless Space -> Variable1D input -> Variable3D input
  Quotient3D :: Variable3D input -> Variable1D input -> Variable3D input
  QuotientConstantVariable3D :: Vector3D Unitless Space -> Variable1D input -> Variable3D input
  Cross3D :: Variable3D input -> Variable3D input -> Variable3D input
  CrossVariableConstant3D :: Variable3D input -> Vector3D Unitless Space -> Variable3D input
  BezierCurve3D :: NonEmpty (Vector3D Unitless Space) -> Variable1D input -> Variable3D input
  TransformVector3D :: Transform3D.Affine Space -> Variable3D input -> Variable3D input
  TransformPoint3D :: Transform3D.Affine Space -> Variable3D input -> Variable3D input
  PlaceVector2D :: Plane -> Variable2D input -> Variable3D input
  PlacePoint2D :: Plane -> Variable2D input -> Variable3D input
  Desingularized3D ::
    Variable1D input ->
    Variable3D input ->
    Variable3D input ->
    Variable3D input ->
    Variable3D input

deriving instance Eq (Variable3D input)

deriving instance Ord (Variable3D input)

deriving instance Show (Variable3D input)

uvPoint :: Vector2D Unitless Space -> UvPoint
uvPoint position = Point2D.coerce (Position2D position)

instance Composition (Ast1D input) (Ast1D Number) (Ast1D input) where
  Constant1D outer `compose` _ = Constant1D outer
  Variable1D outer `compose` Variable1D inner = outer `compose` inner
  outer `compose` Constant1D inner = Constant1D (evaluateCurve1D outer inner)

instance Composition (Variable1D input) (Variable1D Number) (Ast1D input) where
  input `compose` CurveParameter = Variable1D input
  CurveParameter `compose` input = Variable1D input
  XComponent arg `compose` input = xComponent (arg `compose` input)
  YComponent arg `compose` input = yComponent (arg `compose` input)
  Negated1D arg `compose` input = negate (arg `compose` input)
  Sum1D lhs rhs `compose` input = lhs `compose` input + rhs `compose` input
  SumVariableConstant1D lhs rhs `compose` input = lhs `compose` input + rhs
  Difference1D lhs rhs `compose` input = lhs `compose` input - rhs `compose` input
  DifferenceConstantVariable1D lhs rhs `compose` input = lhs - rhs `compose` input
  Product1D lhs rhs `compose` input = lhs `compose` input * rhs `compose` input
  ProductVariableConstant1D lhs rhs `compose` input = lhs `compose` input * rhs
  Quotient1D lhs rhs `compose` input = lhs `compose` input / rhs `compose` input
  QuotientConstantVariable1D lhs rhs `compose` input = lhs / rhs `compose` input
  Squared1D arg `compose` input = squared (arg `compose` input)
  Cubed1D arg `compose` input = cubed (arg `compose` input)
  Sqrt1D arg `compose` input = sqrt (arg `compose` input)
  Sin1D arg `compose` input = sin (arg `compose` input)
  Cos1D arg `compose` input = cos (arg `compose` input)
  BezierCurve1D controlPoints param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D (bezierCurve1D controlPoints) paramVal)
    Variable1D paramVar -> Variable1D (BezierCurve1D controlPoints paramVar)
  SquaredMagnitude2D arg `compose` input = squaredMagnitude2D (arg `compose` input)
  SquaredMagnitude3D arg `compose` input = squaredMagnitude3D (arg `compose` input)
  Magnitude2D arg `compose` input = magnitude2D (arg `compose` input)
  Magnitude3D arg `compose` input = magnitude3D (arg `compose` input)
  Dot2D lhs rhs `compose` input = lhs `compose` input `dot` rhs `compose` input
  DotVariableConstant2D lhs rhs `compose` input = lhs `compose` input `dot` rhs
  Cross2D lhs rhs `compose` input = lhs `compose` input `cross` rhs `compose` input
  CrossVariableConstant2D lhs rhs `compose` input = lhs `compose` input `cross` rhs
  Dot3D lhs rhs `compose` input = lhs `compose` input `dot` rhs `compose` input
  DotVariableConstant3D lhs rhs `compose` input = lhs `compose` input `dot` rhs
  Desingularized1D parameter left middle right `compose` input =
    desingularized1D
      (parameter `compose` input)
      (left `compose` input)
      (middle `compose` input)
      (right `compose` input)
  B00 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b00 paramVal)
    Variable1D paramVar -> Variable1D (B00 paramVar)
  B00d1 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b00d1 paramVal)
    Variable1D paramVar -> Variable1D (B00d1 paramVar)
  B00d2 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b00d2 paramVal)
    Variable1D paramVar -> Variable1D (B00d2 paramVar)
  B00d3 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b00d3 paramVal)
    Variable1D paramVar -> Variable1D (B00d3 paramVar)
  B01 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b01 paramVal)
    Variable1D paramVar -> Variable1D (B01 paramVar)
  B01d1 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b01d1 paramVal)
    Variable1D paramVar -> Variable1D (B01d1 paramVar)
  B01d2 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b01d2 paramVal)
    Variable1D paramVar -> Variable1D (B01d2 paramVar)
  B01d3 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b01d3 paramVal)
    Variable1D paramVar -> Variable1D (B01d3 paramVar)
  B02 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b02 paramVal)
    Variable1D paramVar -> Variable1D (B02 paramVar)
  B02d1 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b02d1 paramVal)
    Variable1D paramVar -> Variable1D (B02d1 paramVar)
  B02d2 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b02d2 paramVal)
    Variable1D paramVar -> Variable1D (B02d2 paramVar)
  B02d3 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b02d3 paramVal)
    Variable1D paramVar -> Variable1D (B02d3 paramVar)
  B10 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b10 paramVal)
    Variable1D paramVar -> Variable1D (B10 paramVar)
  B10d1 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b10d1 paramVal)
    Variable1D paramVar -> Variable1D (B10d1 paramVar)
  B10d2 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b10d2 paramVal)
    Variable1D paramVar -> Variable1D (B10d2 paramVar)
  B10d3 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b10d3 paramVal)
    Variable1D paramVar -> Variable1D (B10d3 paramVar)
  B11 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b11 paramVal)
    Variable1D paramVar -> Variable1D (B11 paramVar)
  B11d1 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b11d1 paramVal)
    Variable1D paramVar -> Variable1D (B11d1 paramVar)
  B11d2 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b11d2 paramVal)
    Variable1D paramVar -> Variable1D (B11d2 paramVar)
  B11d3 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b11d3 paramVal)
    Variable1D paramVar -> Variable1D (B11d3 paramVar)

instance Composition (Ast1D input) (Ast2D Number) (Ast2D input) where
  Constant2D outer `compose` _ = Constant2D outer
  Variable2D outer `compose` Variable1D inner = outer `compose` inner
  outer `compose` Constant1D inner = Constant2D (evaluateCurve2D outer inner)

instance Composition (Variable1D input) (Variable2D Number) (Ast2D input) where
  input `compose` CurveParameter = Variable2D input
  XY x y `compose` input = xy (x `compose` input) (y `compose` input)
  XC x y `compose` input = xy (x `compose` input) (Constant1D y)
  CY x y `compose` input = xy (Constant1D x) (y `compose` input)
  Negated2D arg `compose` input = negate (arg `compose` input)
  Sum2D lhs rhs `compose` input = lhs `compose` input + rhs `compose` input
  SumVariableConstant2D lhs rhs `compose` input = lhs `compose` input + rhs
  Difference2D lhs rhs `compose` input = lhs `compose` input - rhs `compose` input
  DifferenceConstantVariable2D lhs rhs `compose` input = lhs - rhs `compose` input
  Product2D lhs rhs `compose` input = lhs `compose` input * rhs `compose` input
  ProductVariableConstant2D lhs rhs `compose` input = lhs `compose` input * rhs
  ProductConstantVariable2D lhs rhs `compose` input = Constant2D lhs * rhs `compose` input
  Quotient2D lhs rhs `compose` input = lhs `compose` input / rhs `compose` input
  QuotientConstantVariable2D lhs rhs `compose` input = lhs / rhs `compose` input
  BezierCurve2D controlPoints param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant2D (evaluateCurve2D (bezierCurve2D controlPoints) paramVal)
    Variable1D paramVar -> Variable2D (BezierCurve2D controlPoints paramVar)
  TransformVector2D transform vector `compose` input =
    transformVector2D transform (vector `compose` input)
  TransformPoint2D transform point `compose` input =
    transformPoint2D transform (point `compose` input)
  ProjectVector3D plane vector `compose` input =
    projectVector3dInto plane (vector `compose` input)
  ProjectPoint3D plane point `compose` input =
    projectPoint3dInto plane (point `compose` input)
  Desingularized2D parameter left middle right `compose` input =
    desingularized2D
      (parameter `compose` input)
      (left `compose` input)
      (middle `compose` input)
      (right `compose` input)

instance Composition (Ast1D input) (Ast3D Number) (Ast3D input) where
  Constant3D outer `compose` _ = Constant3D outer
  Variable3D outer `compose` Variable1D inner = outer `compose` inner
  outer `compose` Constant1D inner = Constant3D (evaluateCurve3D outer inner)

instance Composition (Variable1D input) (Variable3D Number) (Ast3D input) where
  input `compose` CurveParameter = Variable3D input
  Negated3D arg `compose` input = negate (arg `compose` input)
  Sum3D lhs rhs `compose` input = lhs `compose` input + rhs `compose` input
  SumVariableConstant3D lhs rhs `compose` input = lhs `compose` input + rhs
  Difference3D lhs rhs `compose` input = lhs `compose` input - rhs `compose` input
  DifferenceConstantVariable3D lhs rhs `compose` input = lhs - rhs `compose` input
  Product3D lhs rhs `compose` input = lhs `compose` input * rhs `compose` input
  ProductVariableConstant3D lhs rhs `compose` input = lhs `compose` input * rhs
  ProductConstantVariable3D lhs rhs `compose` input = Constant3D lhs * rhs `compose` input
  Quotient3D lhs rhs `compose` input = lhs `compose` input / rhs `compose` input
  QuotientConstantVariable3D lhs rhs `compose` input = Constant3D lhs / rhs `compose` input
  BezierCurve3D controlPoints param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant3D (evaluateCurve3D (bezierCurve3D controlPoints) paramVal)
    Variable1D paramVar -> Variable3D (BezierCurve3D controlPoints paramVar)
  Cross3D lhs rhs `compose` input = lhs `compose` input `cross` rhs `compose` input
  CrossVariableConstant3D lhs rhs `compose` input = lhs `compose` input `cross` rhs
  TransformVector3D transform vector `compose` input =
    transformVector3D transform (vector `compose` input)
  TransformPoint3D transform point `compose` input =
    transformPoint3D transform (point `compose` input)
  PlaceVector2D plane vector `compose` input =
    placeVector2DOn plane (vector `compose` input)
  PlacePoint2D plane point `compose` input =
    placePoint2DOn plane (point `compose` input)
  Desingularized3D parameter left middle right `compose` input =
    desingularized3D
      (parameter `compose` input)
      (left `compose` input)
      (middle `compose` input)
      (right `compose` input)

instance Composition (Ast2D input) (Ast1D UvPoint) (Ast1D input) where
  Constant1D outer `compose` _ = Constant1D outer
  Variable1D outer `compose` Variable2D inner = outer `compose` inner
  outer `compose` Constant2D parameter = Constant1D (evaluateSurface1D outer (uvPoint parameter))

instance Composition (Variable2D input) (Variable1D UvPoint) (Ast1D input) where
  input `compose` SurfaceParameters = Variable1D input
  SurfaceParameter U `compose` input = xComponent (Variable2D input)
  SurfaceParameter V `compose` input = yComponent (Variable2D input)
  XComponent arg `compose` input = xComponent (arg `compose` input)
  YComponent arg `compose` input = yComponent (arg `compose` input)
  Negated1D arg `compose` input = negate (arg `compose` input)
  Sum1D lhs rhs `compose` input = lhs `compose` input + rhs `compose` input
  SumVariableConstant1D lhs rhs `compose` input = lhs `compose` input + rhs
  Difference1D lhs rhs `compose` input = lhs `compose` input - rhs `compose` input
  DifferenceConstantVariable1D lhs rhs `compose` input = lhs - rhs `compose` input
  Product1D lhs rhs `compose` input = lhs `compose` input * rhs `compose` input
  ProductVariableConstant1D lhs rhs `compose` input = lhs `compose` input * rhs
  Quotient1D lhs rhs `compose` input = lhs `compose` input / rhs `compose` input
  QuotientConstantVariable1D lhs rhs `compose` input = lhs / rhs `compose` input
  Squared1D arg `compose` input = squared (arg `compose` input)
  Cubed1D arg `compose` input = cubed (arg `compose` input)
  Sqrt1D arg `compose` input = sqrt (arg `compose` input)
  Sin1D arg `compose` input = sin (arg `compose` input)
  Cos1D arg `compose` input = cos (arg `compose` input)
  BezierCurve1D controlPoints param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D (bezierCurve1D controlPoints) paramVal)
    Variable1D paramVar -> Variable1D (BezierCurve1D controlPoints paramVar)
  SquaredMagnitude2D arg `compose` input = squaredMagnitude2D (arg `compose` input)
  SquaredMagnitude3D arg `compose` input = squaredMagnitude3D (arg `compose` input)
  Magnitude2D arg `compose` input = magnitude2D (arg `compose` input)
  Magnitude3D arg `compose` input = magnitude3D (arg `compose` input)
  Dot2D lhs rhs `compose` input = lhs `compose` input `dot` rhs `compose` input
  DotVariableConstant2D lhs rhs `compose` input = lhs `compose` input `dot` rhs
  Cross2D lhs rhs `compose` input = lhs `compose` input `cross` rhs `compose` input
  CrossVariableConstant2D lhs rhs `compose` input = lhs `compose` input `cross` rhs
  Dot3D lhs rhs `compose` input = lhs `compose` input `dot` rhs `compose` input
  DotVariableConstant3D lhs rhs `compose` input = lhs `compose` input `dot` rhs
  Desingularized1D parameter left middle right `compose` input =
    desingularized1D
      (parameter `compose` input)
      (left `compose` input)
      (middle `compose` input)
      (right `compose` input)
  B00 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b00 paramVal)
    Variable1D paramVar -> Variable1D (B00 paramVar)
  B00d1 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b00d1 paramVal)
    Variable1D paramVar -> Variable1D (B00d1 paramVar)
  B00d2 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b00d2 paramVal)
    Variable1D paramVar -> Variable1D (B00d2 paramVar)
  B00d3 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b00d3 paramVal)
    Variable1D paramVar -> Variable1D (B00d3 paramVar)
  B01 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b01 paramVal)
    Variable1D paramVar -> Variable1D (B01 paramVar)
  B01d1 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b01d1 paramVal)
    Variable1D paramVar -> Variable1D (B01d1 paramVar)
  B01d2 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b01d2 paramVal)
    Variable1D paramVar -> Variable1D (B01d2 paramVar)
  B01d3 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b01d3 paramVal)
    Variable1D paramVar -> Variable1D (B01d3 paramVar)
  B02 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b02 paramVal)
    Variable1D paramVar -> Variable1D (B02 paramVar)
  B02d1 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b02d1 paramVal)
    Variable1D paramVar -> Variable1D (B02d1 paramVar)
  B02d2 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b02d2 paramVal)
    Variable1D paramVar -> Variable1D (B02d2 paramVar)
  B02d3 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b02d3 paramVal)
    Variable1D paramVar -> Variable1D (B02d3 paramVar)
  B10 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b10 paramVal)
    Variable1D paramVar -> Variable1D (B10 paramVar)
  B10d1 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b10d1 paramVal)
    Variable1D paramVar -> Variable1D (B10d1 paramVar)
  B10d2 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b10d2 paramVal)
    Variable1D paramVar -> Variable1D (B10d2 paramVar)
  B10d3 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b10d3 paramVal)
    Variable1D paramVar -> Variable1D (B10d3 paramVar)
  B11 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b11 paramVal)
    Variable1D paramVar -> Variable1D (B11 paramVar)
  B11d1 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b11d1 paramVal)
    Variable1D paramVar -> Variable1D (B11d1 paramVar)
  B11d2 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b11d2 paramVal)
    Variable1D paramVar -> Variable1D (B11d2 paramVar)
  B11d3 param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant1D (evaluateCurve1D b11d3 paramVal)
    Variable1D paramVar -> Variable1D (B11d3 paramVar)

instance Composition (Ast2D input) (Ast2D UvPoint) (Ast2D input) where
  Constant2D outer `compose` _ = Constant2D outer
  Variable2D outer `compose` Variable2D inner = outer `compose` inner
  outer `compose` Constant2D parameter = Constant2D (evaluateSurface2D outer (uvPoint parameter))

instance Composition (Variable2D input) (Variable2D UvPoint) (Ast2D input) where
  input `compose` SurfaceParameters = Variable2D input
  SurfaceParameters `compose` input = Variable2D input
  XY x y `compose` input = xy (x `compose` input) (y `compose` input)
  XC x y `compose` input = xy (x `compose` input) (Constant1D y)
  CY x y `compose` input = xy (Constant1D x) (y `compose` input)
  Negated2D arg `compose` input = negate (arg `compose` input)
  Sum2D lhs rhs `compose` input = lhs `compose` input + rhs `compose` input
  SumVariableConstant2D lhs rhs `compose` input = lhs `compose` input + rhs
  Difference2D lhs rhs `compose` input = lhs `compose` input - rhs `compose` input
  DifferenceConstantVariable2D lhs rhs `compose` input = lhs - rhs `compose` input
  Product2D lhs rhs `compose` input = lhs `compose` input * rhs `compose` input
  ProductVariableConstant2D lhs rhs `compose` input = lhs `compose` input * rhs
  ProductConstantVariable2D lhs rhs `compose` input = Constant2D lhs * rhs `compose` input
  Quotient2D lhs rhs `compose` input = lhs `compose` input / rhs `compose` input
  QuotientConstantVariable2D lhs rhs `compose` input = lhs / rhs `compose` input
  BezierCurve2D controlPoints param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant2D (evaluateCurve2D (bezierCurve2D controlPoints) paramVal)
    Variable1D paramVar -> Variable2D (BezierCurve2D controlPoints paramVar)
  TransformVector2D transform vector `compose` input =
    transformVector2D transform (vector `compose` input)
  TransformPoint2D transform point `compose` input =
    transformPoint2D transform (point `compose` input)
  ProjectVector3D plane vector `compose` input =
    projectVector3dInto plane (vector `compose` input)
  ProjectPoint3D plane point `compose` input =
    projectPoint3dInto plane (point `compose` input)
  Desingularized2D parameter left middle right `compose` input =
    desingularized2D
      (parameter `compose` input)
      (left `compose` input)
      (middle `compose` input)
      (right `compose` input)

instance Composition (Ast2D input) (Ast3D UvPoint) (Ast3D input) where
  Constant3D outer `compose` _ = Constant3D outer
  Variable3D outer `compose` Variable2D inner = outer `compose` inner
  outer `compose` Constant2D parameter = Constant3D (evaluateSurface3D outer (uvPoint parameter))

instance Composition (Variable2D input) (Variable3D UvPoint) (Ast3D input) where
  input `compose` SurfaceParameters = Variable3D input
  Negated3D arg `compose` input = negate (arg `compose` input)
  Sum3D lhs rhs `compose` input = lhs `compose` input + rhs `compose` input
  SumVariableConstant3D lhs rhs `compose` input = lhs `compose` input + rhs
  Difference3D lhs rhs `compose` input = lhs `compose` input - rhs `compose` input
  DifferenceConstantVariable3D lhs rhs `compose` input = lhs - rhs `compose` input
  Product3D lhs rhs `compose` input = lhs `compose` input * rhs `compose` input
  ProductVariableConstant3D lhs rhs `compose` input = lhs `compose` input * rhs
  ProductConstantVariable3D lhs rhs `compose` input = Constant3D lhs * rhs `compose` input
  Quotient3D lhs rhs `compose` input = lhs `compose` input / rhs `compose` input
  QuotientConstantVariable3D lhs rhs `compose` input = lhs / rhs `compose` input
  BezierCurve3D controlPoints param `compose` input = case param `compose` input of
    Constant1D paramVal -> Constant3D (evaluateCurve3D (bezierCurve3D controlPoints) paramVal)
    Variable1D paramVar -> Variable3D (BezierCurve3D controlPoints paramVar)
  Cross3D lhs rhs `compose` input = lhs `compose` input `cross` rhs `compose` input
  CrossVariableConstant3D lhs rhs `compose` input = lhs `compose` input `cross` rhs
  TransformVector3D transform vector `compose` input = transformVector3D transform (vector `compose` input)
  TransformPoint3D transform point `compose` input = transformPoint3D transform (point `compose` input)
  PlaceVector2D plane vector `compose` input = placeVector2DOn plane (vector `compose` input)
  PlacePoint2D plane point `compose` input = placePoint2DOn plane (point `compose` input)
  Desingularized3D parameter left middle right `compose` input =
    desingularized3D
      (parameter `compose` input)
      (left `compose` input)
      (middle `compose` input)
      (right `compose` input)

constant1D :: Quantity units -> Ast1D input
constant1D value = Constant1D (Quantity.coerce value)

constant2D :: Vector2D units space -> Ast2D input
constant2D = Constant2D . Vector2D.coerce

constant3D :: Vector3D units space -> Ast3D input
constant3D = Constant3D . Vector3D.coerce

curveParameter :: Ast1D Number
curveParameter = Variable1D CurveParameter

surfaceParameter :: SurfaceParameter -> Ast1D UvPoint
surfaceParameter = Variable1D . SurfaceParameter

surfaceParameters :: Ast2D UvPoint
surfaceParameters = Variable2D SurfaceParameters

xComponent :: Ast2D input -> Ast1D input
xComponent (Constant2D val) = Constant1D (Vector2D.xComponent val)
xComponent (Variable2D (XY xVar _)) = Variable1D xVar
xComponent (Variable2D (XC xVar _)) = Variable1D xVar
xComponent (Variable2D (CY xVal _)) = Constant1D xVal
xComponent (Variable2D (BezierCurve2D controlPoints param)) =
  Variable1D (BezierCurve1D (NonEmpty.map Vector2D.xComponent controlPoints) param)
xComponent (Variable2D var) = Variable1D (XComponent var)

yComponent :: Ast2D input -> Ast1D input
yComponent (Constant2D val) = Constant1D (Vector2D.yComponent val)
yComponent (Variable2D (XY _ yVar)) = Variable1D yVar
yComponent (Variable2D (XC _ yVal)) = Constant1D yVal
yComponent (Variable2D (CY _ yVar)) = Variable1D yVar
yComponent (Variable2D (BezierCurve2D controlPoints param)) =
  Variable1D (BezierCurve1D (NonEmpty.map Vector2D.yComponent controlPoints) param)
yComponent (Variable2D var) = Variable1D (YComponent var)

instance Negation (Ast1D input) where
  negate (Constant1D val) = Constant1D (negate val)
  negate (Variable1D var) = Variable1D (negate var)

instance Negation (Variable1D input) where
  negate (Negated1D arg) = arg
  negate (Difference1D lhs rhs) = Difference1D rhs lhs
  negate (SumVariableConstant1D lhs rhs) = DifferenceConstantVariable1D (negate rhs) lhs
  negate (DifferenceConstantVariable1D lhs rhs) = SumVariableConstant1D rhs (negate lhs)
  negate (ProductVariableConstant1D lhs rhs) = ProductVariableConstant1D lhs (negate rhs)
  negate (QuotientConstantVariable1D lhs rhs) = QuotientConstantVariable1D (negate lhs) rhs
  negate (Cubed1D arg) = Cubed1D (negate arg)
  negate (Sin1D arg) = Sin1D (negate arg)
  negate (BezierCurve1D controlPoints param) =
    BezierCurve1D (NonEmpty.map negate controlPoints) param
  negate (DotVariableConstant2D lhs rhs) = DotVariableConstant2D lhs (negate rhs)
  negate (CrossVariableConstant2D lhs rhs) = CrossVariableConstant2D lhs (negate rhs)
  negate (DotVariableConstant3D lhs rhs) = DotVariableConstant3D lhs (negate rhs)
  negate var = Negated1D var

instance Multiplication Sign (Ast1D input) (Ast1D input) where
  Positive * ast = ast
  Negative * ast = -ast

instance Multiplication (Ast1D input) Sign (Ast1D input) where
  ast * Positive = ast
  ast * Negative = -ast

instance Multiplication Sign (Variable1D input) (Variable1D input) where
  Positive * var = var
  Negative * var = -var

instance Multiplication (Variable1D input) Sign (Variable1D input) where
  var * Positive = var
  var * Negative = -var

instance input1 ~ input2 => Addition (Ast1D input1) (Ast1D input2) (Ast1D input1) where
  Constant1D 0.0 + rhs = rhs
  lhs + Constant1D 0.0 = lhs
  Constant1D lhs + Constant1D rhs = Constant1D (lhs + rhs)
  Constant1D lhs + Variable1D rhs = Variable1D (SumVariableConstant1D rhs lhs)
  Variable1D lhs + Constant1D rhs = Variable1D (SumVariableConstant1D lhs rhs)
  Variable1D lhs + Variable1D rhs = Variable1D (lhs + rhs)

instance
  input1 ~ input2 =>
  Addition (Variable1D input1) (Variable1D input2) (Variable1D input1)
  where
  lhs + rhs = if lhs <= rhs then Sum1D lhs rhs else Sum1D rhs lhs

instance Addition (Quantity units) (Ast1D input) (Ast1D input) where
  lhs + rhs = constant1D lhs + rhs

instance Addition (Ast1D input1) (Quantity units) (Ast1D input1) where
  lhs + rhs = lhs + constant1D rhs

instance input1 ~ input2 => Subtraction (Ast1D input1) (Ast1D input2) (Ast1D input1) where
  lhs - Constant1D 0.0 = lhs
  Constant1D 0.0 - rhs = -rhs
  Constant1D lhs - Constant1D rhs = Constant1D (lhs - rhs)
  Constant1D lhs - Variable1D rhs = Variable1D (DifferenceConstantVariable1D lhs rhs)
  Variable1D lhs - Constant1D rhs = Variable1D (SumVariableConstant1D lhs -rhs)
  Variable1D lhs - Variable1D rhs = Variable1D (lhs - rhs)

instance
  input1 ~ input2 =>
  Subtraction (Variable1D input1) (Variable1D input2) (Variable1D input1)
  where
  lhs - rhs = Difference1D lhs rhs

instance Subtraction (Quantity units) (Ast1D input) (Ast1D input) where
  lhs - rhs = constant1D lhs - rhs

instance Subtraction (Ast1D input1) (Quantity units) (Ast1D input1) where
  lhs - rhs = lhs - constant1D rhs

instance input1 ~ input2 => Multiplication (Ast1D input1) (Ast1D input2) (Ast1D input1) where
  Constant1D lhs * Constant1D rhs = Constant1D (lhs * rhs)
  _ * Constant1D 0.0 = Constant1D 0.0
  Constant1D 0.0 * _ = Constant1D 0.0
  lhs * Constant1D 1.0 = lhs
  Constant1D 1.0 * rhs = rhs
  lhs * Constant1D -1.0 = -lhs
  Constant1D -1.0 * rhs = -rhs
  Variable1D (ProductVariableConstant1D a b) * Constant1D c =
    Variable1D a * Constant1D (b * c)
  Constant1D a * Variable1D (ProductVariableConstant1D b c) =
    Constant1D (a * c) * Variable1D b
  Variable1D lhs * Constant1D rhs = Variable1D (ProductVariableConstant1D lhs rhs)
  Constant1D lhs * Variable1D rhs = Variable1D (ProductVariableConstant1D rhs lhs)
  Variable1D lhs * Variable1D rhs =
    Variable1D (if lhs <= rhs then Product1D lhs rhs else Product1D rhs lhs)

instance Multiplication (Quantity units) (Ast1D input) (Ast1D input) where
  lhs * rhs = constant1D lhs * rhs

instance Multiplication (Ast1D input1) (Quantity units) (Ast1D input1) where
  lhs * rhs = lhs * constant1D rhs

instance input1 ~ input2 => Division (Ast1D input1) (Ast1D input2) (Ast1D input1) where
  Constant1D lhs / Constant1D rhs = Constant1D (lhs / rhs)
  Constant1D 0.0 / _ = Constant1D 0.0
  lhs / Constant1D 1.0 = lhs
  lhs / Constant1D -1.0 = -lhs
  Variable1D lhs / Constant1D rhs = Variable1D (ProductVariableConstant1D lhs (1.0 / rhs))
  Constant1D lhs / Variable1D rhs = Variable1D (QuotientConstantVariable1D lhs rhs)
  Variable1D lhs / Variable1D rhs = Variable1D (lhs / rhs)

instance
  input1 ~ input2 =>
  Division (Variable1D input1) (Variable1D input2) (Variable1D input1)
  where
  lhs / rhs = Quotient1D lhs rhs

instance Division (Quantity units) (Ast1D input) (Ast1D input) where
  lhs / rhs = constant1D lhs / rhs

instance Division (Ast1D input) (Quantity units) (Ast1D input) where
  lhs / rhs = lhs / constant1D rhs

instance Negation (Ast2D input) where
  negate (Constant2D val) = Constant2D (negate val)
  negate (Variable2D var) = Variable2D (negate var)

instance Negation (Variable2D input) where
  negate (Negated2D arg) = arg
  negate (Difference2D lhs rhs) = Difference2D rhs lhs
  negate var = Negated2D var

instance Multiplication Sign (Ast2D input) (Ast2D input) where
  Positive * ast = ast
  Negative * ast = -ast

instance Multiplication (Ast2D input) Sign (Ast2D input) where
  ast * Positive = ast
  ast * Negative = -ast

instance Multiplication Sign (Variable2D input) (Variable2D input) where
  Positive * var = var
  Negative * var = -var

instance Multiplication (Variable2D input) Sign (Variable2D input) where
  var * Positive = var
  var * Negative = -var

instance input1 ~ input2 => Addition (Ast2D input1) (Ast2D input2) (Ast2D input1) where
  Constant2D lhs + rhs | lhs == Vector2D.zero = rhs
  lhs + Constant2D rhs | rhs == Vector2D.zero = lhs
  Constant2D lhs + Constant2D rhs = Constant2D (lhs + rhs)
  Constant2D lhs + Variable2D rhs = Variable2D (SumVariableConstant2D rhs lhs)
  Variable2D lhs + Constant2D rhs = Variable2D (SumVariableConstant2D lhs rhs)
  Variable2D lhs + Variable2D rhs = Variable2D (lhs + rhs)

instance
  input1 ~ input2 =>
  Addition (Variable2D input1) (Variable2D input2) (Variable2D input1)
  where
  lhs + rhs = if lhs <= rhs then Sum2D lhs rhs else Sum2D rhs lhs

instance Addition (Vector2D units space) (Ast2D input) (Ast2D input) where
  lhs + rhs = constant2D lhs + rhs

instance Addition (Ast2D input1) (Vector2D units space) (Ast2D input1) where
  lhs + rhs = lhs + constant2D rhs

instance input1 ~ input2 => Subtraction (Ast2D input1) (Ast2D input2) (Ast2D input1) where
  lhs - Constant2D rhs | rhs == Vector2D.zero = lhs
  Constant2D lhs - rhs | lhs == Vector2D.zero = -rhs
  Constant2D lhs - Constant2D rhs = Constant2D (lhs - rhs)
  Constant2D lhs - Variable2D rhs = Variable2D (DifferenceConstantVariable2D lhs rhs)
  Variable2D lhs - Constant2D rhs = Variable2D (SumVariableConstant2D lhs -rhs)
  Variable2D lhs - Variable2D rhs = Variable2D (lhs - rhs)

instance
  input1 ~ input2 =>
  Subtraction (Variable2D input1) (Variable2D input2) (Variable2D input1)
  where
  lhs - rhs = Difference2D lhs rhs

instance Subtraction (Vector2D units space) (Ast2D input) (Ast2D input) where
  lhs - rhs = constant2D lhs - rhs

instance Subtraction (Ast2D input1) (Vector2D units space) (Ast2D input1) where
  lhs - rhs = lhs - constant2D rhs

instance input1 ~ input2 => Multiplication (Ast2D input1) (Ast1D input2) (Ast2D input1) where
  Constant2D lhs * Constant1D rhs = Constant2D (lhs * rhs)
  _ * Constant1D 0.0 = Constant2D Vector2D.zero
  Constant2D lhs * _ | lhs == Vector2D.zero = Constant2D Vector2D.zero
  lhs * Constant1D 1.0 = lhs
  lhs * Constant1D -1.0 = -lhs
  Variable2D (ProductVariableConstant2D a b) * Constant1D c =
    Variable2D a * Constant1D (b * c)
  Constant2D a * Variable1D (ProductVariableConstant1D b c) =
    Constant2D (a * c) * Variable1D b
  Variable2D lhs * Constant1D rhs = Variable2D (ProductVariableConstant2D lhs rhs)
  Constant2D lhs * Variable1D rhs = Variable2D (ProductConstantVariable2D lhs rhs)
  Variable2D lhs * Variable1D rhs = Variable2D (Product2D lhs rhs)

instance input1 ~ input2 => Multiplication (Ast1D input1) (Ast2D input2) (Ast2D input1) where
  lhs * rhs = rhs * lhs

instance Multiplication (Ast2D input1) (Quantity units) (Ast2D input1) where
  lhs * rhs = lhs * constant1D rhs

instance Multiplication (Quantity units) (Ast2D input) (Ast2D input) where
  lhs * rhs = constant1D lhs * rhs

instance input1 ~ input2 => Division (Ast2D input1) (Ast1D input2) (Ast2D input1) where
  Constant2D lhs / Constant1D rhs = Constant2D (lhs / rhs)
  Constant2D lhs / _ | lhs == Vector2D.zero = Constant2D Vector2D.zero
  lhs / Constant1D 1.0 = lhs
  lhs / Constant1D -1.0 = -lhs
  Variable2D lhs / Constant1D rhs = Variable2D (ProductVariableConstant2D lhs (1.0 / rhs))
  Constant2D lhs / Variable1D rhs = Variable2D (QuotientConstantVariable2D lhs rhs)
  Variable2D lhs / Variable1D rhs = Variable2D (Quotient2D lhs rhs)

instance Division (Vector2D units space) (Ast1D input) (Ast2D input) where
  lhs / rhs = constant2D lhs / rhs

instance Division (Ast2D input) (Quantity units) (Ast2D input) where
  lhs / rhs = lhs / constant1D rhs

instance Negation (Ast3D input) where
  negate (Constant3D val) = Constant3D (negate val)
  negate (Variable3D var) = Variable3D (negate var)

instance Negation (Variable3D input) where
  negate (Negated3D arg) = arg
  negate (Difference3D lhs rhs) = Difference3D rhs lhs
  negate var = Negated3D var

instance Multiplication Sign (Ast3D input) (Ast3D input) where
  Positive * ast = ast
  Negative * ast = -ast

instance Multiplication (Ast3D input) Sign (Ast3D input) where
  ast * Positive = ast
  ast * Negative = -ast

instance Multiplication Sign (Variable3D input) (Variable3D input) where
  Positive * var = var
  Negative * var = -var

instance Multiplication (Variable3D input) Sign (Variable3D input) where
  var * Positive = var
  var * Negative = -var

instance input1 ~ input2 => Addition (Ast3D input1) (Ast3D input2) (Ast3D input1) where
  Constant3D lhs + rhs | lhs == Vector3D.zero = rhs
  lhs + Constant3D rhs | rhs == Vector3D.zero = lhs
  Constant3D lhs + Constant3D rhs = Constant3D (lhs + rhs)
  Constant3D lhs + Variable3D rhs = Variable3D (SumVariableConstant3D rhs lhs)
  Variable3D lhs + Constant3D rhs = Variable3D (SumVariableConstant3D lhs rhs)
  Variable3D lhs + Variable3D rhs = Variable3D (lhs + rhs)

instance
  input1 ~ input2 =>
  Addition (Variable3D input1) (Variable3D input2) (Variable3D input1)
  where
  lhs + rhs = if lhs <= rhs then Sum3D lhs rhs else Sum3D rhs lhs

instance Addition (Vector3D units space) (Ast3D input) (Ast3D input) where
  lhs + rhs = constant3D lhs + rhs

instance Addition (Ast3D input1) (Vector3D units space) (Ast3D input1) where
  lhs + rhs = lhs + constant3D rhs

instance input1 ~ input2 => Subtraction (Ast3D input1) (Ast3D input2) (Ast3D input1) where
  lhs - Constant3D rhs | rhs == Vector3D.zero = lhs
  Constant3D lhs - rhs | lhs == Vector3D.zero = -rhs
  Constant3D lhs - Constant3D rhs = Constant3D (lhs - rhs)
  Constant3D lhs - Variable3D rhs = Variable3D (DifferenceConstantVariable3D lhs rhs)
  Variable3D lhs - Constant3D rhs = Variable3D (SumVariableConstant3D lhs -rhs)
  Variable3D lhs - Variable3D rhs = Variable3D (lhs - rhs)

instance
  input1 ~ input2 =>
  Subtraction (Variable3D input1) (Variable3D input2) (Variable3D input1)
  where
  lhs - rhs = Difference3D lhs rhs

instance Subtraction (Vector3D units space) (Ast3D input) (Ast3D input) where
  lhs - rhs = constant3D lhs - rhs

instance Subtraction (Ast3D input1) (Vector3D units space) (Ast3D input1) where
  lhs - rhs = lhs - constant3D rhs

instance input1 ~ input2 => Multiplication (Ast3D input1) (Ast1D input2) (Ast3D input1) where
  Constant3D lhs * Constant1D rhs = Constant3D (lhs * rhs)
  _ * Constant1D 0.0 = Constant3D Vector3D.zero
  Constant3D lhs * _ | lhs == Vector3D.zero = Constant3D Vector3D.zero
  lhs * Constant1D 1.0 = lhs
  lhs * Constant1D -1.0 = -lhs
  Variable3D (ProductVariableConstant3D a b) * Constant1D c = Variable3D a * Constant1D (b * c)
  Constant3D a * Variable1D (ProductVariableConstant1D b c) = Constant3D (a * c) * Variable1D b
  Variable3D lhs * Constant1D rhs = Variable3D (ProductVariableConstant3D lhs rhs)
  Constant3D lhs * Variable1D rhs = Variable3D (ProductConstantVariable3D lhs rhs)
  Variable3D lhs * Variable1D rhs = Variable3D (Product3D lhs rhs)

instance input1 ~ input2 => Multiplication (Ast1D input1) (Ast3D input2) (Ast3D input1) where
  lhs * rhs = rhs * lhs

instance Multiplication (Ast3D input1) (Quantity units) (Ast3D input1) where
  lhs * rhs = lhs * constant1D rhs

instance Multiplication (Quantity units) (Ast3D input) (Ast3D input) where
  lhs * rhs = constant1D lhs * rhs

instance input1 ~ input2 => Division (Ast3D input1) (Ast1D input2) (Ast3D input1) where
  Constant3D lhs / Constant1D rhs = Constant3D (lhs / rhs)
  Constant3D lhs / _ | lhs == Vector3D.zero = Constant3D Vector3D.zero
  lhs / Constant1D 1.0 = lhs
  lhs / Constant1D -1.0 = -lhs
  Variable3D lhs / Constant1D rhs = Variable3D (ProductVariableConstant3D lhs (1.0 / rhs))
  Constant3D lhs / Variable1D rhs = Variable3D (QuotientConstantVariable3D lhs rhs)
  Variable3D lhs / Variable1D rhs = Variable3D (Quotient3D lhs rhs)

instance Division (Vector3D units space) (Ast1D input) (Ast3D input) where
  lhs / rhs = constant3D lhs / rhs

instance Division (Ast3D input) (Quantity units) (Ast3D input) where
  lhs / rhs = lhs / constant1D rhs

instance input1 ~ input2 => DotMultiplication (Ast2D input1) (Ast2D input2) (Ast1D input1) where
  Constant2D lhs `dot` Constant2D rhs = Constant1D (lhs `dot` rhs)
  Constant2D lhs `dot` _ | lhs == Vector2D.zero = Constant1D 0.0
  _ `dot` Constant2D rhs | rhs == Vector2D.zero = Constant1D 0.0
  Variable2D lhs `dot` Constant2D rhs = Variable1D (DotVariableConstant2D lhs rhs)
  Constant2D lhs `dot` Variable2D rhs = Variable1D (DotVariableConstant2D rhs lhs)
  Variable2D lhs `dot` Variable2D rhs = Variable1D (lhs `dot` rhs)

instance
  input1 ~ input2 =>
  DotMultiplication (Variable2D input1) (Variable2D input2) (Variable1D input1)
  where
  lhs `dot` rhs = if lhs <= rhs then Dot2D lhs rhs else Dot2D rhs lhs

instance DotMultiplication (Vector2D units space) (Ast2D input) (Ast1D input) where
  lhs `dot` rhs = constant2D lhs `dot` rhs

instance DotMultiplication (Ast2D input) (Vector2D units space) (Ast1D input) where
  lhs `dot` rhs = lhs `dot` constant2D rhs

instance input1 ~ input2 => CrossMultiplication (Ast2D input1) (Ast2D input2) (Ast1D input1) where
  Constant2D lhs `cross` Constant2D rhs = Constant1D (lhs `cross` rhs)
  Constant2D lhs `cross` _ | lhs == Vector2D.zero = Constant1D 0.0
  _ `cross` Constant2D rhs | rhs == Vector2D.zero = Constant1D 0.0
  Variable2D lhs `cross` Constant2D rhs = Variable1D (CrossVariableConstant2D lhs rhs)
  Constant2D lhs `cross` Variable2D rhs = Variable1D (CrossVariableConstant2D rhs -lhs)
  Variable2D lhs `cross` Variable2D rhs = Variable1D (Cross2D lhs rhs)

instance CrossMultiplication (Vector2D units space) (Ast2D input) (Ast1D input) where
  lhs `cross` rhs = constant2D lhs `cross` rhs

instance CrossMultiplication (Ast2D input) (Vector2D units space) (Ast1D input) where
  lhs `cross` rhs = lhs `cross` constant2D rhs

instance input1 ~ input2 => DotMultiplication (Ast3D input1) (Ast3D input2) (Ast1D input1) where
  Constant3D lhs `dot` Constant3D rhs = Constant1D (lhs `dot` rhs)
  Constant3D lhs `dot` _ | lhs == Vector3D.zero = Constant1D 0.0
  _ `dot` Constant3D rhs | rhs == Vector3D.zero = Constant1D 0.0
  Variable3D lhs `dot` Constant3D rhs = Variable1D (DotVariableConstant3D lhs rhs)
  Constant3D lhs `dot` Variable3D rhs = Variable1D (DotVariableConstant3D rhs lhs)
  Variable3D lhs `dot` Variable3D rhs = Variable1D (lhs `dot` rhs)

instance
  input1 ~ input2 =>
  DotMultiplication (Variable3D input1) (Variable3D input2) (Variable1D input1)
  where
  lhs `dot` rhs = if lhs <= rhs then Dot3D lhs rhs else Dot3D rhs lhs

instance DotMultiplication (Vector3D units space) (Ast3D input) (Ast1D input) where
  lhs `dot` rhs = constant3D lhs `dot` rhs

instance DotMultiplication (Ast3D input) (Vector3D units space) (Ast1D input) where
  lhs `dot` rhs = lhs `dot` constant3D rhs

instance input1 ~ input2 => CrossMultiplication (Ast3D input1) (Ast3D input2) (Ast3D input1) where
  Constant3D lhs `cross` Constant3D rhs = Constant3D (lhs `cross` rhs)
  Constant3D lhs `cross` _ | lhs == Vector3D.zero = Constant3D Vector3D.zero
  _ `cross` Constant3D rhs | rhs == Vector3D.zero = Constant3D Vector3D.zero
  Variable3D lhs `cross` Constant3D rhs = Variable3D (CrossVariableConstant3D lhs rhs)
  Constant3D lhs `cross` Variable3D rhs = Variable3D (CrossVariableConstant3D rhs -lhs)
  Variable3D lhs `cross` Variable3D rhs = Variable3D (Cross3D lhs rhs)

instance CrossMultiplication (Vector3D units space) (Ast3D input) (Ast3D input) where
  lhs `cross` rhs = constant3D lhs `cross` rhs

instance CrossMultiplication (Ast3D input) (Vector3D units space) (Ast3D input) where
  lhs `cross` rhs = lhs `cross` constant3D rhs

squared :: Ast1D input -> Ast1D input
squared ast = case ast of
  Constant1D val -> Constant1D (Number.squared val)
  Variable1D (Negated1D arg) -> Variable1D (Squared1D arg)
  Variable1D (Sqrt1D arg) -> Variable1D arg
  Variable1D var -> Variable1D (Squared1D var)

sqrt :: Ast1D input -> Ast1D input
sqrt (Constant1D val) = Constant1D (Number.sqrt val)
sqrt (Variable1D var) = Variable1D (Sqrt1D var)

cubed :: Ast1D input -> Ast1D input
cubed (Constant1D val) = Constant1D (Number.cubed val)
cubed (Variable1D var) = Variable1D (Cubed1D var)

sin :: Ast1D input -> Ast1D input
sin (Constant1D val) = Constant1D (Number.sin val)
sin (Variable1D var) = Variable1D (Sin1D var)

cos :: Ast1D input -> Ast1D input
cos (Constant1D val) = constant1D (Number.cos val)
cos (Variable1D var) = Variable1D (Cos1D var)

squaredMagnitude2D :: Ast2D input -> Ast1D input
squaredMagnitude2D ast = case ast of
  Constant2D val -> Constant1D (Vector2D.squaredMagnitude val)
  Variable2D (Negated2D arg) -> Variable1D (SquaredMagnitude2D arg)
  Variable2D var -> Variable1D (SquaredMagnitude2D var)

squaredMagnitude3D :: Ast3D input -> Ast1D input
squaredMagnitude3D ast = case ast of
  Constant3D val -> Constant1D (Vector3D.squaredMagnitude val)
  Variable3D (Negated3D arg) -> Variable1D (SquaredMagnitude3D arg)
  Variable3D var -> Variable1D (SquaredMagnitude3D var)

magnitude2D :: Ast2D input -> Ast1D input
magnitude2D ast = case ast of
  Constant2D val -> Constant1D (Vector2D.magnitude val)
  Variable2D (Negated2D arg) -> Variable1D (Magnitude2D arg)
  Variable2D var -> Variable1D (Magnitude2D var)

magnitude3D :: Ast3D input -> Ast1D input
magnitude3D ast = case ast of
  Constant3D val -> Constant1D (Vector3D.magnitude val)
  Variable3D (Negated3D arg) -> Variable1D (Magnitude3D arg)
  Variable3D var -> Variable1D (Magnitude3D var)

transformVector2D :: Transform2D tag units space -> Ast2D input -> Ast2D input
transformVector2D transform ast = do
  let erasedTransform = Transform2D.coerce transform
  case ast of
    Constant2D val -> Constant2D (Vector2D.transformBy erasedTransform val)
    Variable2D (TransformVector2D existing var) ->
      Variable2D (TransformVector2D (erasedTransform `compose` existing) var)
    Variable2D (BezierCurve2D controlPoints param) -> do
      let transformedControlPoints =
            NonEmpty.map (Vector2D.transformBy erasedTransform) controlPoints
      Variable2D (BezierCurve2D transformedControlPoints param)
    Variable2D var -> Variable2D (TransformVector2D erasedTransform var)

transformVector3D :: Transform3D tag space -> Ast3D input -> Ast3D input
transformVector3D transform ast = do
  let erasedTransform = Transform3D.coerce transform
  case ast of
    Constant3D val -> Constant3D (Vector3D.transformBy erasedTransform val)
    Variable3D (TransformVector3D existing var) ->
      Variable3D (TransformVector3D (erasedTransform `compose` existing) var)
    Variable3D (BezierCurve3D controlPoints param) -> do
      let transformedControlPoints =
            NonEmpty.map (Vector3D.transformBy erasedTransform) controlPoints
      Variable3D (BezierCurve3D transformedControlPoints param)
    Variable3D var -> Variable3D (TransformVector3D erasedTransform var)

transformPoint2D :: Transform2D tag units space -> Ast2D input -> Ast2D input
transformPoint2D transform ast = do
  let erasedTransform = Transform2D.coerce transform
  case ast of
    Constant2D val -> do
      let Position2D transformed = Point2D.transformBy erasedTransform (Position2D val)
      Constant2D transformed
    Variable2D (TransformPoint2D existing var) ->
      Variable2D (TransformPoint2D (erasedTransform `compose` existing) var)
    Variable2D (BezierCurve2D controlPoints param) -> do
      let transformedControlPoints =
            controlPoints
              & Data.Coerce.coerce -- convert list of Vector2D to list of Point2D
              & NonEmpty.map (Point2D.transformBy erasedTransform)
              & Data.Coerce.coerce -- convert list of Point2D back to list of Vector2D
      Variable2D (BezierCurve2D transformedControlPoints param)
    Variable2D var -> Variable2D (TransformPoint2D erasedTransform var)

transformPoint3D :: Transform3D tag space -> Ast3D input -> Ast3D input
transformPoint3D transform ast = do
  let coercedTransform :: Transform3D.Affine Space = Transform3D.coerce transform
  case ast of
    Constant3D val -> do
      let point = Position3D (Vector3D.coerce val)
      let Position3D transformed = Point3D.transformBy coercedTransform point
      Constant3D (Vector3D.coerce transformed)
    Variable3D (TransformPoint3D existing var) ->
      Variable3D (TransformPoint3D (coercedTransform `compose` existing) var)
    Variable3D (BezierCurve3D controlPoints param) -> do
      let transformedControlPoints =
            controlPoints
              & Data.Coerce.coerce -- convert list of Vector3D to list of Point3D
              & NonEmpty.map (Point3D.transformBy coercedTransform)
              & Data.Coerce.coerce -- convert list of Point3D back to list of Vector3D
      Variable3D (BezierCurve3D transformedControlPoints param)
    Variable3D var -> Variable3D (TransformPoint3D coercedTransform var)

placementTransform2D :: Frame2D units global local -> Transform2D.Affine Unitless Space
placementTransform2D frame =
  Transform2D
    (Point2D.coerce (Frame2D.originPoint frame))
    (Vector2D.coerce (Vector2D.unit (Frame2D.xDirection frame)))
    (Vector2D.coerce (Vector2D.unit (Frame2D.yDirection frame)))

placementTransform3D :: Frame3D global local -> Transform3D.Affine Space
placementTransform3D frame =
  Transform3D
    (Point3D.coerce (Frame3D.originPoint frame))
    (Vector3D.coerce (Vector3D.unit (Frame3D.rightwardDirection frame)))
    (Vector3D.coerce (Vector3D.unit (Frame3D.forwardDirection frame)))
    (Vector3D.coerce (Vector3D.unit (Frame3D.upwardDirection frame)))

placeVector2DIn :: Frame2D frameUnits global local -> Ast2D input -> Ast2D input
placeVector2DIn frame ast = transformVector2D (placementTransform2D frame) ast

placePoint2DIn :: Frame2D units global local -> Ast2D input -> Ast2D input
placePoint2DIn frame ast = transformPoint2D (placementTransform2D frame) ast

placeVector3dIn :: Frame3D global local -> Ast3D input -> Ast3D input
placeVector3dIn frame ast = transformVector3D (placementTransform3D frame) ast

placePoint3dIn :: Frame3D global local -> Ast3D input -> Ast3D input
placePoint3dIn frame ast = transformPoint3D (placementTransform3D frame) ast

placeVector2DOn :: Plane3D global local -> Ast2D input -> Ast3D input
placeVector2DOn plane ast = case ast of
  Constant2D val -> Constant3D (Vector2D.placeOn (Plane3D.coerce plane) val)
  Variable2D var -> Variable3D (PlaceVector2D (Plane3D.coerce plane) var)

placePoint2DOn :: Plane3D global local -> Ast2D input -> Ast3D input
placePoint2DOn plane ast = case ast of
  Constant2D val -> do
    let point = Position2D (Vector2D.coerce val)
    let Position3D placed = Point3D.on (Plane3D.coerce plane) point
    Constant3D (Vector3D.coerce placed)
  Variable2D var -> Variable3D (PlacePoint2D (Plane3D.coerce plane) var)

projectVector3dInto :: Plane3D global local -> Ast3D input -> Ast2D input
projectVector3dInto plane ast = case ast of
  Constant3D val -> Constant2D (Vector3D.projectInto (Plane3D.coerce plane) val)
  Variable3D var -> Variable2D (ProjectVector3D (Plane3D.coerce plane) var)

projectPoint3dInto :: Plane3D global local -> Ast3D input -> Ast2D input
projectPoint3dInto plane ast = case ast of
  Constant3D val -> do
    let point = Position3D (Vector3D.coerce val)
    let Position2D projected = Point3D.projectInto (Plane3D.coerce plane) point
    Constant2D (Vector2D.coerce projected)
  Variable3D var -> Variable2D (ProjectPoint3D (Plane3D.coerce plane) var)

xy :: Ast1D input -> Ast1D input -> Ast2D input
xy (Constant1D x) (Constant1D y) = Constant2D (Vector2D x y)
xy (Constant1D x) (Variable1D y) = Variable2D (CY x y)
xy (Variable1D x) (Constant1D y) = Variable2D (XC x y)
xy (Variable1D x) (Variable1D y) = Variable2D (XY x y)

bezierCurve1D :: NonEmpty (Quantity units) -> Ast1D Number
bezierCurve1D (NonEmpty.One value) = constant1D value
bezierCurve1D controlPoints =
  Variable1D (BezierCurve1D (NonEmpty.map Quantity.coerce controlPoints) CurveParameter)

bezierCurve2D :: NonEmpty (Vector2D units space) -> Ast2D Number
bezierCurve2D (NonEmpty.One value) = constant2D value
bezierCurve2D controlPoints =
  Variable2D (BezierCurve2D (NonEmpty.map Vector2D.coerce controlPoints) CurveParameter)

bezierCurve3D :: NonEmpty (Vector3D units space) -> Ast3D Number
bezierCurve3D (NonEmpty.One value) = constant3D value
bezierCurve3D controlPoints =
  Variable3D (BezierCurve3D (NonEmpty.map Vector3D.coerce controlPoints) CurveParameter)

b00 :: Ast1D Number
b00 = Variable1D (B00 CurveParameter)

b00d1 :: Ast1D Number
b00d1 = Variable1D (B00d1 CurveParameter)

b00d2 :: Ast1D Number
b00d2 = Variable1D (B00d2 CurveParameter)

b00d3 :: Ast1D Number
b00d3 = Variable1D (B00d3 CurveParameter)

b01 :: Ast1D Number
b01 = Variable1D (B01 CurveParameter)

b01d1 :: Ast1D Number
b01d1 = Variable1D (B01d1 CurveParameter)

b01d2 :: Ast1D Number
b01d2 = Variable1D (B01d2 CurveParameter)

b01d3 :: Ast1D Number
b01d3 = Variable1D (B01d3 CurveParameter)

b02 :: Ast1D Number
b02 = Variable1D (B02 CurveParameter)

b02d1 :: Ast1D Number
b02d1 = Variable1D (B02d1 CurveParameter)

b02d2 :: Ast1D Number
b02d2 = Variable1D (B02d2 CurveParameter)

b02d3 :: Ast1D Number
b02d3 = Variable1D (B02d3 CurveParameter)

b10 :: Ast1D Number
b10 = Variable1D (B10 CurveParameter)

b10d1 :: Ast1D Number
b10d1 = Variable1D (B10d1 CurveParameter)

b10d2 :: Ast1D Number
b10d2 = Variable1D (B10d2 CurveParameter)

b10d3 :: Ast1D Number
b10d3 = Variable1D (B10d3 CurveParameter)

b11 :: Ast1D Number
b11 = Variable1D (B11 CurveParameter)

b11d1 :: Ast1D Number
b11d1 = Variable1D (B11d1 CurveParameter)

b11d2 :: Ast1D Number
b11d2 = Variable1D (B11d2 CurveParameter)

b11d3 :: Ast1D Number
b11d3 = Variable1D (B11d3 CurveParameter)

desingularized1D :: Ast1D input -> Ast1D input -> Ast1D input -> Ast1D input -> Ast1D input
desingularized1D (Constant1D parameter) left middle right =
  Desingularization.value parameter left middle right
desingularized1D _ (Constant1D left) _ _ = Constant1D left
desingularized1D _ _ (Constant1D middle) _ = Constant1D middle
desingularized1D _ _ _ (Constant1D right) = Constant1D right
desingularized1D (Variable1D parameter) (Variable1D left) (Variable1D middle) (Variable1D right) =
  Variable1D (Desingularized1D parameter left middle right)

desingularized2D :: Ast1D input -> Ast2D input -> Ast2D input -> Ast2D input -> Ast2D input
desingularized2D (Constant1D parameter) left middle right =
  Desingularization.value parameter left middle right
desingularized2D _ (Constant2D left) _ _ = Constant2D left
desingularized2D _ _ (Constant2D middle) _ = Constant2D middle
desingularized2D _ _ _ (Constant2D right) = Constant2D right
desingularized2D (Variable1D parameter) (Variable2D left) (Variable2D middle) (Variable2D right) =
  Variable2D (Desingularized2D parameter left middle right)

desingularized3D :: Ast1D input -> Ast3D input -> Ast3D input -> Ast3D input -> Ast3D input
desingularized3D (Constant1D parameter) left middle right =
  Desingularization.value parameter left middle right
desingularized3D _ (Constant3D left) _ _ = Constant3D left
desingularized3D _ _ (Constant3D middle) _ = Constant3D middle
desingularized3D _ _ _ (Constant3D right) = Constant3D right
desingularized3D (Variable1D parameter) (Variable3D left) (Variable3D middle) (Variable3D right) =
  Variable3D (Desingularized3D parameter left middle right)

addTransform2D :: Transform2D.Affine Unitless Space -> Compile.Step ConstantIndex
addTransform2D (Transform2D origin i j) = do
  let Vector2D iX iY = i
  let Vector2D jX jY = j
  let Point2D oX oY = origin
  Compile.addConstant (iX :| [iY, jX, jY, oX, oY])

addTransform3D :: Transform3D.Affine Space -> Compile.Step ConstantIndex
addTransform3D (Transform3D origin i j k) = do
  let Vector3D iR iF iU = i
  let Vector3D jR jF jU = j
  let Vector3D kR kF kU = k
  let Point3D oR oF oU = origin
  Compile.addConstant (iR :| [iF, iU, jR, jF, jU, kR, kF, kU, Length.inMeters oR, Length.inMeters oF, Length.inMeters oU])

addPlane :: Plane -> Compile.Step ConstantIndex
addPlane plane = do
  let Direction3D iR iF iU = Plane3D.xDirection plane
  let Direction3D jR jF jU = Plane3D.yDirection plane
  let Point3D oR oF oU = Plane3D.originPoint plane
  Compile.addConstant (iR :| [iF, iU, jR, jF, jU, Length.inMeters oR, Length.inMeters oF, Length.inMeters oU])

compileVariable1D :: Variable1D input -> Compile.Step VariableIndex
compileVariable1D variable = case variable of
  CurveParameter -> Compile.return (VariableIndex 0)
  SurfaceParameter U -> Compile.return (VariableIndex 0)
  SurfaceParameter V -> Compile.return (VariableIndex 1)
  XComponent arg -> do
    argIndex <- compileVariable2D arg
    Compile.addVariable1D (Instruction.Component0 argIndex)
  YComponent arg -> do
    argIndex <- compileVariable2D arg
    Compile.addVariable1D (Instruction.Component1 argIndex)
  Negated1D arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.Negate1D argIndex)
  Sum1D lhs rhs -> do
    lhsIndex <- compileVariable1D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable1D (Instruction.Add1D lhsIndex rhsIndex)
  SumVariableConstant1D lhs rhs -> do
    lhsIndex <- compileVariable1D lhs
    rhsIndex <- Compile.addConstant1D rhs
    Compile.addVariable1D (Instruction.AddVariableConstant1D lhsIndex rhsIndex)
  Difference1D lhs rhs -> do
    lhsIndex <- compileVariable1D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable1D (Instruction.Subtract1D lhsIndex rhsIndex)
  DifferenceConstantVariable1D lhs rhs -> do
    lhsIndex <- Compile.addConstant1D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable1D (Instruction.SubtractConstantVariable1D lhsIndex rhsIndex)
  Product1D lhs rhs -> do
    lhsIndex <- compileVariable1D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable1D (Instruction.Multiply1D lhsIndex rhsIndex)
  ProductVariableConstant1D lhs rhs -> do
    lhsIndex <- compileVariable1D lhs
    rhsIndex <- Compile.addConstant1D rhs
    Compile.addVariable1D (Instruction.MultiplyVariableConstant1D lhsIndex rhsIndex)
  Quotient1D lhs rhs -> do
    lhsIndex <- compileVariable1D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable1D (Instruction.Divide1D lhsIndex rhsIndex)
  QuotientConstantVariable1D lhs rhs -> do
    lhsIndex <- Compile.addConstant1D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable1D (Instruction.DivideConstantVariable1D lhsIndex rhsIndex)
  Squared1D arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.Square1D argIndex)
  Cubed1D arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.Cube1D argIndex)
  Sqrt1D arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.Sqrt1D argIndex)
  Sin1D arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.Sin1D argIndex)
  Cos1D arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.Cos1D argIndex)
  BezierCurve1D controlPoints parameter -> do
    controlPointsIndex <- Compile.addConstant controlPoints
    parameterIndex <- compileVariable1D parameter
    let numControlPoints = NonEmpty.length controlPoints
    let instruction = Instruction.Bezier1D numControlPoints controlPointsIndex parameterIndex
    Compile.addVariable1D instruction
  SquaredMagnitude2D arg -> do
    argIndex <- compileVariable2D arg
    Compile.addVariable1D (Instruction.SquaredMagnitude2D argIndex)
  SquaredMagnitude3D arg -> do
    argIndex <- compileVariable3D arg
    Compile.addVariable1D (Instruction.SquaredMagnitude3D argIndex)
  Magnitude2D arg -> do
    argIndex <- compileVariable2D arg
    Compile.addVariable1D (Instruction.Magnitude2D argIndex)
  Magnitude3D arg -> do
    argIndex <- compileVariable3D arg
    Compile.addVariable1D (Instruction.Magnitude3D argIndex)
  Dot2D lhs rhs -> do
    lhsIndex <- compileVariable2D lhs
    rhsIndex <- compileVariable2D rhs
    Compile.addVariable1D (Instruction.Dot2D lhsIndex rhsIndex)
  DotVariableConstant2D lhs rhs -> do
    lhsIndex <- compileVariable2D lhs
    rhsIndex <- Compile.addConstant2D rhs
    Compile.addVariable1D (Instruction.DotVariableConstant2D lhsIndex rhsIndex)
  Cross2D lhs rhs -> do
    lhsIndex <- compileVariable2D lhs
    rhsIndex <- compileVariable2D rhs
    Compile.addVariable1D (Instruction.Cross2D lhsIndex rhsIndex)
  CrossVariableConstant2D lhs rhs -> do
    lhsIndex <- compileVariable2D lhs
    rhsIndex <- Compile.addConstant2D rhs
    Compile.addVariable1D (Instruction.CrossVariableConstant2D lhsIndex rhsIndex)
  Dot3D lhs rhs -> do
    lhsIndex <- compileVariable3D lhs
    rhsIndex <- compileVariable3D rhs
    Compile.addVariable1D (Instruction.Dot3D lhsIndex rhsIndex)
  DotVariableConstant3D lhs rhs -> do
    lhsIndex <- compileVariable3D lhs
    rhsIndex <- Compile.addConstant3D rhs
    Compile.addVariable1D (Instruction.DotVariableConstant3D lhsIndex rhsIndex)
  Desingularized1D parameter left middle right -> do
    parameterIndex <- compileVariable1D parameter
    leftIndex <- compileVariable1D left
    middleIndex <- compileVariable1D middle
    rightIndex <- compileVariable1D right
    let instruction = Instruction.Desingularized1D parameterIndex leftIndex middleIndex rightIndex
    Compile.addVariable1D instruction
  B00 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B00 argIndex)
  B00d1 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B00d1 argIndex)
  B00d2 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B00d2 argIndex)
  B00d3 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B00d3 argIndex)
  B01 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B01 argIndex)
  B01d1 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B01d1 argIndex)
  B01d2 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B01d2 argIndex)
  B01d3 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B01d3 argIndex)
  B02 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B02 argIndex)
  B02d1 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B02d1 argIndex)
  B02d2 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B02d2 argIndex)
  B02d3 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B02d3 argIndex)
  B10 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B10 argIndex)
  B10d1 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B10d1 argIndex)
  B10d2 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B10d2 argIndex)
  B10d3 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B10d3 argIndex)
  B11 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B11 argIndex)
  B11d1 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B11d1 argIndex)
  B11d2 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B11d2 argIndex)
  B11d3 arg -> do
    argIndex <- compileVariable1D arg
    Compile.addVariable1D (Instruction.B11d3 argIndex)

coordinates2D :: Vector2D Unitless Space -> NonEmpty Number
coordinates2D (Vector2D x y) = NonEmpty.two x y

coordinates3D :: Vector3D Unitless Space -> NonEmpty Number
coordinates3D (Vector3D r f u) = NonEmpty.three r f u

compileVariable2D :: Variable2D input -> Compile.Step VariableIndex
compileVariable2D variable = case variable of
  SurfaceParameters -> Compile.return (VariableIndex 0)
  XY x y -> do
    xIndex <- compileVariable1D x
    yIndex <- compileVariable1D y
    Compile.addVariable2D (Instruction.XY xIndex yIndex)
  XC x y -> do
    xIndex <- compileVariable1D x
    yIndex <- Compile.addConstant1D y
    Compile.addVariable2D (Instruction.XC xIndex yIndex)
  CY x y -> do
    xIndex <- Compile.addConstant1D x
    yIndex <- compileVariable1D y
    Compile.addVariable2D (Instruction.CY xIndex yIndex)
  Negated2D arg -> do
    argIndex <- compileVariable2D arg
    Compile.addVariable2D (Instruction.Negate2D argIndex)
  Sum2D lhs rhs -> do
    lhsIndex <- compileVariable2D lhs
    rhsIndex <- compileVariable2D rhs
    Compile.addVariable2D (Instruction.Add2D lhsIndex rhsIndex)
  SumVariableConstant2D lhs rhs -> do
    lhsIndex <- compileVariable2D lhs
    rhsIndex <- Compile.addConstant2D rhs
    Compile.addVariable2D (Instruction.AddVariableConstant2D lhsIndex rhsIndex)
  Difference2D lhs rhs -> do
    lhsIndex <- compileVariable2D lhs
    rhsIndex <- compileVariable2D rhs
    Compile.addVariable2D (Instruction.Subtract2D lhsIndex rhsIndex)
  DifferenceConstantVariable2D lhs rhs -> do
    lhsIndex <- Compile.addConstant2D lhs
    rhsIndex <- compileVariable2D rhs
    Compile.addVariable2D (Instruction.SubtractConstantVariable2D lhsIndex rhsIndex)
  Product2D lhs rhs -> do
    lhsIndex <- compileVariable2D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable2D (Instruction.Multiply2D lhsIndex rhsIndex)
  ProductVariableConstant2D lhs rhs -> do
    lhsIndex <- compileVariable2D lhs
    rhsIndex <- Compile.addConstant1D rhs
    Compile.addVariable2D (Instruction.MultiplyVariableConstant2D lhsIndex rhsIndex)
  ProductConstantVariable2D lhs rhs -> do
    lhsIndex <- Compile.addConstant2D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable2D (Instruction.MultiplyConstantVariable2D lhsIndex rhsIndex)
  Quotient2D lhs rhs -> do
    lhsIndex <- compileVariable2D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable2D (Instruction.Divide2D lhsIndex rhsIndex)
  QuotientConstantVariable2D lhs rhs -> do
    lhsIndex <- Compile.addConstant2D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable2D (Instruction.DivideConstantVariable2D lhsIndex rhsIndex)
  BezierCurve2D controlPoints parameter -> do
    let numControlPoints = NonEmpty.length controlPoints
    controlPointsIndex <- Compile.addConstant (NonEmpty.combine coordinates2D controlPoints)
    parameterIndex <- compileVariable1D parameter
    let instruction = Instruction.Bezier2D numControlPoints controlPointsIndex parameterIndex
    Compile.addVariable2D instruction
  TransformVector2D transform vector -> do
    matrixIndex <- addTransform2D transform
    vectorIndex <- compileVariable2D vector
    Compile.addVariable2D (Instruction.TransformVector2D matrixIndex vectorIndex)
  TransformPoint2D transform point -> do
    matrixIndex <- addTransform2D transform
    pointIndex <- compileVariable2D point
    Compile.addVariable2D (Instruction.TransformPoint2D matrixIndex pointIndex)
  ProjectVector3D plane vector -> do
    planeIndex <- addPlane plane
    vectorIndex <- compileVariable3D vector
    Compile.addVariable2D (Instruction.ProjectVector3D planeIndex vectorIndex)
  ProjectPoint3D plane point -> do
    planeIndex <- addPlane plane
    pointIndex <- compileVariable3D point
    Compile.addVariable2D (Instruction.ProjectPoint3D planeIndex pointIndex)
  Desingularized2D parameter left middle right -> do
    parameterIndex <- compileVariable1D parameter
    leftIndex <- compileVariable2D left
    middleIndex <- compileVariable2D middle
    rightIndex <- compileVariable2D right
    let instruction = Instruction.Desingularized2D parameterIndex leftIndex middleIndex rightIndex
    Compile.addVariable2D instruction

compileVariable3D :: Variable3D input -> Compile.Step VariableIndex
compileVariable3D variable = case variable of
  Negated3D arg -> do
    argIndex <- compileVariable3D arg
    Compile.addVariable3D (Instruction.Negate3D argIndex)
  Sum3D lhs rhs -> do
    lhsIndex <- compileVariable3D lhs
    rhsIndex <- compileVariable3D rhs
    Compile.addVariable3D (Instruction.Add3D lhsIndex rhsIndex)
  SumVariableConstant3D lhs rhs -> do
    lhsIndex <- compileVariable3D lhs
    rhsIndex <- Compile.addConstant3D rhs
    Compile.addVariable3D (Instruction.AddVariableConstant3D lhsIndex rhsIndex)
  Difference3D lhs rhs -> do
    lhsIndex <- compileVariable3D lhs
    rhsIndex <- compileVariable3D rhs
    Compile.addVariable3D (Instruction.Subtract3D lhsIndex rhsIndex)
  DifferenceConstantVariable3D lhs rhs -> do
    lhsIndex <- Compile.addConstant3D lhs
    rhsIndex <- compileVariable3D rhs
    Compile.addVariable3D (Instruction.SubtractConstantVariable3D lhsIndex rhsIndex)
  Product3D lhs rhs -> do
    lhsIndex <- compileVariable3D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable3D (Instruction.Multiply3D lhsIndex rhsIndex)
  ProductVariableConstant3D lhs rhs -> do
    lhsIndex <- compileVariable3D lhs
    rhsIndex <- Compile.addConstant1D rhs
    Compile.addVariable3D (Instruction.MultiplyVariableConstant3D lhsIndex rhsIndex)
  ProductConstantVariable3D lhs rhs -> do
    lhsIndex <- Compile.addConstant3D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable3D (Instruction.MultiplyConstantVariable3D lhsIndex rhsIndex)
  Quotient3D lhs rhs -> do
    lhsIndex <- compileVariable3D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable3D (Instruction.Divide3D lhsIndex rhsIndex)
  QuotientConstantVariable3D lhs rhs -> do
    lhsIndex <- Compile.addConstant3D lhs
    rhsIndex <- compileVariable1D rhs
    Compile.addVariable3D (Instruction.DivideConstantVariable3D lhsIndex rhsIndex)
  BezierCurve3D controlPoints parameter -> do
    let numControlPoints = NonEmpty.length controlPoints
    controlPointsIndex <- Compile.addConstant (NonEmpty.combine coordinates3D controlPoints)
    parameterIndex <- compileVariable1D parameter
    let instruction = Instruction.Bezier3D numControlPoints controlPointsIndex parameterIndex
    Compile.addVariable3D instruction
  Cross3D lhs rhs -> do
    lhsIndex <- compileVariable3D lhs
    rhsIndex <- compileVariable3D rhs
    Compile.addVariable3D (Instruction.Cross3D lhsIndex rhsIndex)
  CrossVariableConstant3D lhs rhs -> do
    lhsIndex <- compileVariable3D lhs
    rhsIndex <- Compile.addConstant3D rhs
    Compile.addVariable3D (Instruction.CrossVariableConstant3D lhsIndex rhsIndex)
  TransformVector3D transform vector -> do
    matrixIndex <- addTransform3D transform
    vectorIndex <- compileVariable3D vector
    Compile.addVariable3D (Instruction.TransformVector3D matrixIndex vectorIndex)
  TransformPoint3D transform point -> do
    matrixIndex <- addTransform3D transform
    pointIndex <- compileVariable3D point
    Compile.addVariable3D (Instruction.TransformPoint3D matrixIndex pointIndex)
  PlaceVector2D plane vector -> do
    planeIndex <- addPlane plane
    vectorIndex <- compileVariable2D vector
    Compile.addVariable3D (Instruction.PlaceVector2D planeIndex vectorIndex)
  PlacePoint2D plane point -> do
    planeIndex <- addPlane plane
    pointIndex <- compileVariable2D point
    Compile.addVariable3D (Instruction.PlacePoint2D planeIndex pointIndex)
  Desingularized3D parameter left middle right -> do
    parameterIndex <- compileVariable1D parameter
    leftIndex <- compileVariable3D left
    middleIndex <- compileVariable3D middle
    rightIndex <- compileVariable3D right
    let instruction = Instruction.Desingularized3D parameterIndex leftIndex middleIndex rightIndex
    Compile.addVariable3D instruction

compileCurve1D :: Ast1D Number -> Compiled Number Number
compileCurve1D (Constant1D val) = Evaluate.Constant val
compileCurve1D (Variable1D var) = Evaluate.Bytecode (Compile.curve1D (compileVariable1D var))

compileCurve2D :: Ast2D Number -> Compiled Number (Vector2D Unitless Space)
compileCurve2D (Constant2D val) = Evaluate.Constant val
compileCurve2D (Variable2D var) = Evaluate.Bytecode (Compile.curve2D (compileVariable2D var))

compileCurve3D :: Ast3D Number -> Compiled Number (Vector3D Unitless Space)
compileCurve3D (Constant3D val) = Evaluate.Constant val
compileCurve3D (Variable3D var) = Evaluate.Bytecode (Compile.curve3D (compileVariable3D var))

compileSurface1D :: Ast1D UvPoint -> Compiled UvPoint Number
compileSurface1D (Constant1D val) = Evaluate.Constant val
compileSurface1D (Variable1D var) = Evaluate.Bytecode (Compile.surface1D (compileVariable1D var))

compileSurface2D :: Ast2D UvPoint -> Compiled UvPoint (Vector2D Unitless Space)
compileSurface2D (Constant2D val) = Evaluate.Constant val
compileSurface2D (Variable2D var) = Evaluate.Bytecode (Compile.surface2D (compileVariable2D var))

compileSurface3D :: Ast3D UvPoint -> Compiled UvPoint (Vector3D Unitless Space)
compileSurface3D (Constant3D val) = Evaluate.Constant val
compileSurface3D (Variable3D var) = Evaluate.Bytecode (Compile.surface3D (compileVariable3D var))

evaluateCurve1D :: Ast1D Number -> Number -> Number
evaluateCurve1D ast input = Evaluate.curve1dValue (compileCurve1D ast) input

evaluateCurve2D :: Ast2D Number -> Number -> Vector2D Unitless Space
evaluateCurve2D ast input = Evaluate.curve2dValue (compileCurve2D ast) input

evaluateCurve3D :: Ast3D Number -> Number -> Vector3D Unitless Space
evaluateCurve3D ast input = Evaluate.curve3dValue (compileCurve3D ast) input

evaluateSurface1D :: Ast1D UvPoint -> UvPoint -> Number
evaluateSurface1D ast input = Evaluate.surface1dValue (compileSurface1D ast) input

evaluateSurface2D :: Ast2D UvPoint -> UvPoint -> Vector2D Unitless Space
evaluateSurface2D ast input = Evaluate.surface2dValue (compileSurface2D ast) input

evaluateSurface3D :: Ast3D UvPoint -> UvPoint -> Vector3D Unitless Space
evaluateSurface3D ast input = Evaluate.surface3dValue (compileSurface3D ast) input

debugCurve1D :: Ast1D Number -> Text
debugCurve1D (Constant1D value) = "Constant: " <> Text.number value
debugCurve1D (Variable1D variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugCurve (compileVariable1D variable))
    ]

debugCurve2D :: Ast2D Number -> Text
debugCurve2D (Constant2D value) = "Constant: " <> Text.show value
debugCurve2D (Variable2D variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugCurve (compileVariable2D variable))
    ]

debugCurve3D :: Ast3D Number -> Text
debugCurve3D (Constant3D value) = "Constant: " <> Text.show value
debugCurve3D (Variable3D variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugCurve (compileVariable3D variable))
    ]

debugSurface1D :: Ast1D UvPoint -> Text
debugSurface1D (Constant1D value) = "Constant: " <> Text.number value
debugSurface1D (Variable1D variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugSurface (compileVariable1D variable))
    ]

debugSurface2D :: Ast2D UvPoint -> Text
debugSurface2D (Constant2D value) = "Constant: " <> Text.show value
debugSurface2D (Variable2D variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugSurface (compileVariable2D variable))
    ]

debugSurface3D :: Ast3D UvPoint -> Text
debugSurface3D (Constant3D value) = "Constant: " <> Text.show value
debugSurface3D (Variable3D variable) =
  Text.multiline
    [ "Bytecode:"
    , Text.indent " " (Compile.debugSurface (compileVariable3D variable))
    ]
