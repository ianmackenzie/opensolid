module OpenSolid.Ast
  ( Ast1d
  -- , zero2d
  , constant1d
  -- , constant2d
  , curveParameter
  , surfaceParameter
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
  -- , uComponent
  -- , vComponent
  , compileCurve1d
  )
where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Unsafe qualified
import Data.Word (Word8)
import Foreign (Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified
import Foreign.Ptr qualified
import GHC.ByteOrder qualified
import GHC.Foreign (CString)
import OpenSolid.Binary (Builder)
import OpenSolid.Binary qualified as Binary
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvPoint)
import OpenSolid.Units qualified as Units
import System.IO.Unsafe (unsafeDupablePerformIO)
import Prelude (Double)

data Ast1d input where
  Constant1d :: Float -> Ast1d input
  Variable1d :: Variable1d input -> Ast1d input

deriving instance Eq (Ast1d input)

deriving instance Ord (Ast1d input)

deriving instance Show (Ast1d input)

data Variable1d input where
  CurveParameter :: Variable1d Float
  SurfaceParameter :: SurfaceParameter -> Variable1d UvPoint
  Negated1d :: Variable1d input -> Variable1d input
  Sum1d :: Variable1d input -> Variable1d input -> Variable1d input
  SumVariableConstant1d :: Variable1d input -> Float -> Variable1d input
  Difference1d :: Variable1d input -> Variable1d input -> Variable1d input
  DifferenceConstantVariable1d :: Float -> Variable1d input -> Variable1d input
  Squared1d :: Variable1d input -> Variable1d input
  Product1d :: Variable1d input -> Variable1d input -> Variable1d input
  ProductVariableConstant1d :: Variable1d input -> Float -> Variable1d input
  Quotient1d :: Variable1d input -> Variable1d input -> Variable1d input
  QuotientConstantVariable1d :: Float -> Variable1d input -> Variable1d input
  SquareRoot1d :: Variable1d input -> Variable1d input
  Sine1d :: Variable1d input -> Variable1d input
  Cosine1d :: Variable1d input -> Variable1d input
  BezierCurve1d :: NonEmpty Float -> Variable1d input -> Variable1d input

-- UComponent :: Ast2d input -> Ast1d input
-- VComponent :: Ast2d input -> Ast1d input

deriving instance Eq (Variable1d input)

deriving instance Ord (Variable1d input)

deriving instance Show (Variable1d input)

-- data Ast2d kind input where
--   Constant2d :: Float -> Float -> Ast2d Constant input
--   UV :: Variable1d input -> Variable1d input -> Ast2d Variable input

-- deriving instance Eq (Ast2d input)

-- deriving instance Ord (Ast2d input)

-- deriving instance Show (Ast2d input)

instance Composition (Ast1d input) (Ast1d Float) (Ast1d input) where
  Constant1d outer . _ = Constant1d outer
  outer . Constant1d inner = Constant1d (compileCurve1d outer inner)
  Variable1d outer . Variable1d inner = Variable1d (outer . inner)

instance Composition (Variable1d input) (Variable1d Float) (Variable1d input) where
  CurveParameter . input = input
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
  SquareRoot1d arg . input = SquareRoot1d (arg . input)
  Sine1d arg . input = Sine1d (arg . input)
  Cosine1d arg . input = Cosine1d (arg . input)
  BezierCurve1d controlPoints param . input = BezierCurve1d controlPoints (param . input)

-- zero2d :: Ast2d input
-- zero2d = constant2d 0.0 0.0

constant1d :: Qty units -> Ast1d input
constant1d = Constant1d . Units.coerce

-- constant2d :: Qty units -> Qty units -> Ast2d input
-- constant2d u v = Constant2d (Units.coerce u) (Units.coerce v)

curveParameter :: Ast1d Float
curveParameter = Variable1d CurveParameter

surfaceParameter :: SurfaceParameter -> Ast1d UvPoint
surfaceParameter = Variable1d . SurfaceParameter

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
  Variable1d (SquareRoot1d arg) -> Variable1d arg
  Variable1d var -> Variable1d (Squared1d var)

sqrt :: Ast1d input -> Ast1d input
sqrt (Constant1d value) = Constant1d (Float.sqrt value)
sqrt (Variable1d var) = Variable1d (SquareRoot1d var)

sin :: Ast1d input -> Ast1d input
sin (Constant1d val) = Constant1d (Float.sin val)
sin (Variable1d var) = Variable1d (Sine1d var)

cos :: Ast1d input -> Ast1d input
cos (Constant1d value) = constant1d (Float.cos value)
cos (Variable1d var) = Variable1d (Cosine1d var)

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
bezierCurve1d controlPoints param
  | NonEmpty.length controlPoints < 256 =
      Variable1d (BezierCurve1d (NonEmpty.map Units.coerce controlPoints) CurveParameter) . param
  | otherwise = exception "More than 255 control points in Bezier curve"

-- uComponent :: Ast2d input -> Ast1d input
-- uComponent (Constant2d u _) = constant1d u
-- uComponent (UV u _) = u

-- vComponent :: Ast2d input -> Ast1d input
-- vComponent (Constant2d _ v) = constant1d v
-- vComponent (UV _ v) = v

newtype ConstantIndex = ConstantIndex Int deriving (Eq, Ord)

newtype VariableIndex = VariableIndex Int deriving (Eq, Ord)

data Compilation = Compilation
  { constantsBuilder :: Builder
  , constants :: Map (NonEmpty Float) ConstantIndex
  , numConstants :: Int
  , bytecodeBuilder :: Builder
  , variables :: Map Instruction VariableIndex
  , numVariables :: Int
  }

data Instruction
  = Negate1d VariableIndex
  | Add1d VariableIndex VariableIndex
  | AddVariableConstant1d VariableIndex ConstantIndex
  | Subtract1d VariableIndex VariableIndex
  | SubtractConstantVariable1d ConstantIndex VariableIndex
  | Square1d VariableIndex
  | Multiply1d VariableIndex VariableIndex
  | MultiplyVariableConstant1d VariableIndex ConstantIndex
  | Divide1d VariableIndex VariableIndex
  | DivideConstantVariable1d ConstantIndex VariableIndex
  | Sqrt1d VariableIndex
  | Sin1d VariableIndex
  | Cos1d VariableIndex
  | Bezier1d Int ConstantIndex VariableIndex
  deriving (Eq, Ord)

encodeOpcodeAndArguments :: Instruction -> Builder
encodeOpcodeAndArguments instruction = case instruction of
  Negate1d arg ->
    Builder.word8 negate1dOpcode
      <> encodeVariableIndex arg
  Add1d lhs rhs ->
    Builder.word8 add1dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  AddVariableConstant1d lhs rhs ->
    Builder.word8 addVariableConstant1dOpcode
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Subtract1d lhs rhs ->
    Builder.word8 subtract1dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  SubtractConstantVariable1d lhs rhs ->
    Builder.word8 subtractConstantVariable1dOpcode
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Square1d arg ->
    Builder.word8 square1dOpcode
      <> encodeVariableIndex arg
  Multiply1d lhs rhs ->
    Builder.word8 multiply1dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  MultiplyVariableConstant1d lhs rhs ->
    Builder.word8 multiplyVariableConstant1dOpcode
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Divide1d lhs rhs ->
    Builder.word8 divide1dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DivideConstantVariable1d lhs rhs ->
    Builder.word8 divideConstantVariable1dOpcode
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Sqrt1d arg ->
    Builder.word8 sqrt1dOpcode
      <> encodeVariableIndex arg
  Sin1d arg ->
    Builder.word8 sin1dOpcode
      <> encodeVariableIndex arg
  Cos1d arg ->
    Builder.word8 cos1dOpcode
      <> encodeVariableIndex arg
  Bezier1d 2 controlPoints parameter ->
    Builder.word8 linear1dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d 3 controlPoints parameter ->
    Builder.word8 quadratic1dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d 4 controlPoints parameter ->
    Builder.word8 cubic1dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d 5 controlPoints parameter ->
    Builder.word8 quartic1dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d 6 controlPoints parameter ->
    Builder.word8 quintic1dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d n controlPoints parameter ->
    Builder.word8 bezier1dOpcode
      <> encodeByte n
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter

encodeByte :: Int -> Builder
encodeByte value = Builder.word8 (fromIntegral value)

encodeInt :: Int -> Builder
encodeInt value
  | value < 65536 = encodeByte (value % 256) <> encodeByte (value // 256)
  | otherwise = exception "More than 65536 locals or constants in compiled function"

encodeVariableIndex :: VariableIndex -> Builder
encodeVariableIndex (VariableIndex index) = encodeInt index

encodeConstantIndex :: ConstantIndex -> Builder
encodeConstantIndex (ConstantIndex index) = encodeInt index

encodeDouble :: Double -> Builder
encodeDouble = case GHC.ByteOrder.targetByteOrder of
  GHC.ByteOrder.LittleEndian -> Builder.doubleLE
  GHC.ByteOrder.BigEndian -> Builder.doubleBE

encodeFloat :: Float -> Builder
encodeFloat = encodeDouble . Float.toDouble

addConstant :: NonEmpty Float -> Compilation -> (Compilation, ConstantIndex)
addConstant values initialCompilation =
  case Map.get values (constants initialCompilation) of
    Just constantIndex -> (initialCompilation, constantIndex)
    Nothing -> do
      let constantIndex = ConstantIndex (numConstants initialCompilation)
      let updatedCompilation =
            initialCompilation
              { constantsBuilder =
                  constantsBuilder initialCompilation
                    <> Binary.collect encodeFloat (NonEmpty.toList values)
              , constants = Map.set values constantIndex (constants initialCompilation)
              , numConstants = numConstants initialCompilation + NonEmpty.length values
              }
      (updatedCompilation, constantIndex)

addInstruction :: Instruction -> Int -> Compilation -> (Compilation, VariableIndex)
addInstruction instruction resultSize initialCompilation =
  case Map.get instruction (variables initialCompilation) of
    Just resultIndex -> (initialCompilation, resultIndex)
    Nothing -> do
      let resultIndex = VariableIndex (numVariables initialCompilation)
      let updatedCompilation =
            initialCompilation
              { bytecodeBuilder =
                  bytecodeBuilder initialCompilation
                    <> encodeOpcodeAndArguments instruction
                    <> encodeVariableIndex resultIndex
              , variables = Map.set instruction resultIndex (variables initialCompilation)
              , numVariables = numVariables initialCompilation + resultSize
              }
      (updatedCompilation, resultIndex)

compileUnary1d ::
  (VariableIndex -> Instruction) ->
  Variable1d input ->
  Compilation ->
  (Compilation, VariableIndex)
compileUnary1d instructionConstructor argument compilation0 = do
  let (compilation1, argumentIndex) = compileVariable1d argument compilation0
  let instruction = instructionConstructor argumentIndex
  addInstruction instruction 1 compilation1

compileBinary1d ::
  (VariableIndex -> VariableIndex -> Instruction) ->
  Variable1d input ->
  Variable1d input ->
  Compilation ->
  (Compilation, VariableIndex)
compileBinary1d instructionConstructor lhs rhs compilation0 = do
  let (compilation1, lhsIndex) = compileVariable1d lhs compilation0
  let (compilation2, rhsIndex) = compileVariable1d rhs compilation1
  let instruction = instructionConstructor lhsIndex rhsIndex
  addInstruction instruction 1 compilation2

compileConstantVariableBinary1d ::
  (ConstantIndex -> VariableIndex -> Instruction) ->
  Float ->
  Variable1d input ->
  Compilation ->
  (Compilation, VariableIndex)
compileConstantVariableBinary1d instructionConstructor lhs rhs compilation0 = do
  let (compilation1, lhsIndex) = addConstant (NonEmpty.one lhs) compilation0
  let (compilation2, rhsIndex) = compileVariable1d rhs compilation1
  let instruction = instructionConstructor lhsIndex rhsIndex
  addInstruction instruction 1 compilation2

compileVariableConstantBinary1d ::
  (VariableIndex -> ConstantIndex -> Instruction) ->
  Variable1d input ->
  Float ->
  Compilation ->
  (Compilation, VariableIndex)
compileVariableConstantBinary1d instructionConstructor lhs rhs compilation0 = do
  let (compilation1, lhsIndex) = compileVariable1d lhs compilation0
  let (compilation2, rhsIndex) = addConstant (NonEmpty.one rhs) compilation1
  let instruction = instructionConstructor lhsIndex rhsIndex
  addInstruction instruction 1 compilation2

compileBezier1d ::
  NonEmpty Float ->
  Variable1d input ->
  Compilation ->
  (Compilation, VariableIndex)
compileBezier1d controlPoints parameter compilation0 = do
  let (compilation1, controlPointsIndex) = addConstant controlPoints compilation0
  let (compilation2, parameterIndex) = compileVariable1d parameter compilation1
  let numControlPoints = NonEmpty.length controlPoints
  let instruction = Bezier1d numControlPoints controlPointsIndex parameterIndex
  addInstruction instruction (numControlPoints - 1) compilation2

compileVariable1d :: Variable1d input -> Compilation -> (Compilation, VariableIndex)
compileVariable1d variable compilation = case variable of
  CurveParameter -> (compilation, VariableIndex 0)
  SurfaceParameter U -> (compilation, VariableIndex 0)
  SurfaceParameter V -> (compilation, VariableIndex 1)
  Negated1d argument -> compileUnary1d Negate1d argument compilation
  Sum1d lhs rhs -> compileBinary1d Add1d lhs rhs compilation
  SumVariableConstant1d lhs rhs ->
    compileVariableConstantBinary1d AddVariableConstant1d lhs rhs compilation
  Difference1d lhs rhs -> compileBinary1d Subtract1d lhs rhs compilation
  DifferenceConstantVariable1d lhs rhs ->
    compileConstantVariableBinary1d SubtractConstantVariable1d lhs rhs compilation
  Squared1d argument -> compileUnary1d Square1d argument compilation
  Product1d lhs rhs -> compileBinary1d Multiply1d lhs rhs compilation
  ProductVariableConstant1d lhs rhs ->
    compileVariableConstantBinary1d MultiplyVariableConstant1d lhs rhs compilation
  Quotient1d lhs rhs -> compileBinary1d Divide1d lhs rhs compilation
  QuotientConstantVariable1d lhs rhs ->
    compileConstantVariableBinary1d DivideConstantVariable1d lhs rhs compilation
  SquareRoot1d argument -> compileUnary1d Sqrt1d argument compilation
  Sine1d argument -> compileUnary1d Sin1d argument compilation
  Cosine1d argument -> compileUnary1d Cos1d argument compilation
  BezierCurve1d controlPoints t -> compileBezier1d controlPoints t compilation

initCompilation :: Int -> Compilation
initCompilation numArguments =
  Compilation
    { constantsBuilder = Binary.empty
    , constants = Map.empty
    , numConstants = 0
    , bytecodeBuilder = Binary.empty
    , variables = Map.empty
    , numVariables = numArguments
    }

returnInstruction :: Int -> VariableIndex -> Builder
returnInstruction dimension variableIndex =
  Builder.word8 returnOpcode <> encodeByte dimension <> encodeVariableIndex variableIndex

compileCurve1d :: Ast1d Float -> (Float -> Float)
compileCurve1d (Constant1d value) = always value
compileCurve1d (Variable1d variable) = do
  let (compilation, resultIndex) = compileVariable1d variable (initCompilation 1)
  let Compilation{constantsBuilder, bytecodeBuilder, numVariables} = compilation
  let constantBytes = Binary.bytes constantsBuilder
  let bytecode = Binary.bytes (bytecodeBuilder <> returnInstruction 1 resultIndex)
  \tValue ->
    unsafeDupablePerformIO $
      Data.ByteString.Unsafe.unsafeUseAsCString constantBytes \constantBytesPtr ->
        Data.ByteString.Unsafe.unsafeUseAsCString bytecode \bytecodePtr ->
          Foreign.Marshal.Alloc.allocaBytes 8 \outputPtr -> IO.do
            opensolid_curve1d_value
              bytecodePtr
              (Float.toDouble tValue)
              (Foreign.Ptr.castPtr constantBytesPtr)
              numVariables
              outputPtr
            IO.map Float.fromDouble (Foreign.peek outputPtr)

foreign import capi "expression.h value Return"
  returnOpcode :: Word8

foreign import capi "expression.h value Negate1d"
  negate1dOpcode :: Word8

foreign import capi "expression.h value Add1d"
  add1dOpcode :: Word8

foreign import capi "expression.h value AddVariableConstant1d"
  addVariableConstant1dOpcode :: Word8

foreign import capi "expression.h value Subtract1d"
  subtract1dOpcode :: Word8

foreign import capi "expression.h value SubtractConstantVariable1d"
  subtractConstantVariable1dOpcode :: Word8

foreign import capi "expression.h value Square1d"
  square1dOpcode :: Word8

foreign import capi "expression.h value Multiply1d"
  multiply1dOpcode :: Word8

foreign import capi "expression.h value MultiplyVariableConstant1d"
  multiplyVariableConstant1dOpcode :: Word8

foreign import capi "expression.h value Divide1d"
  divide1dOpcode :: Word8

foreign import capi "expression.h value DivideConstantVariable1d"
  divideConstantVariable1dOpcode :: Word8

foreign import capi "expression.h value Sqrt1d"
  sqrt1dOpcode :: Word8

foreign import capi "expression.h value Sin1d"
  sin1dOpcode :: Word8

foreign import capi "expression.h value Cos1d"
  cos1dOpcode :: Word8

foreign import capi "expression.h value Linear1d"
  linear1dOpcode :: Word8

foreign import capi "expression.h value Quadratic1d"
  quadratic1dOpcode :: Word8

foreign import capi "expression.h value Cubic1d"
  cubic1dOpcode :: Word8

foreign import capi "expression.h value Quartic1d"
  quartic1dOpcode :: Word8

foreign import capi "expression.h value Quintic1d"
  quintic1dOpcode :: Word8

foreign import capi "expression.h value Bezier1d"
  bezier1dOpcode :: Word8

foreign import capi "expression.h opensolid_curve1d_value"
  opensolid_curve1d_value :: CString -> Double -> Ptr Double -> Int -> Ptr Double -> IO ()
