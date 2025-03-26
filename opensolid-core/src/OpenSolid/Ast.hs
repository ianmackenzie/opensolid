{-# LANGUAGE CPP #-}

module OpenSolid.Ast
  ( Ast1d
  , zero1d
  , zero2d
  , constant1d
  , constant2d
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
  , uComponent
  , vComponent
  , compile1d
  , expression1d
  , Evaluation (evaluate)
  )
where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Unsafe qualified
import Data.Word (Word8)
import Foreign (Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified
import Foreign.Ptr qualified
import GHC.Foreign (CString)
import OpenSolid.Binary (Builder, ByteString)
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

#include "MachDeps.h"

data Ast1d input where
  CurveParameter :: Ast1d Float
  SurfaceParameter :: SurfaceParameter -> Ast1d UvPoint
  Constant1d :: Float -> Ast1d input
  Negated1d :: Ast1d input -> Ast1d input
  Sum1d :: Ast1d input -> Ast1d input -> Ast1d input
  Difference1d :: Ast1d input -> Ast1d input -> Ast1d input
  Squared1d :: Ast1d input -> Ast1d input
  Product1d :: Ast1d input -> Ast1d input -> Ast1d input
  Quotient1d :: Ast1d input -> Ast1d input -> Ast1d input
  SquareRoot1d :: Ast1d input -> Ast1d input
  Sine1d :: Ast1d input -> Ast1d input
  Cosine1d :: Ast1d input -> Ast1d input
  BezierCurve1d :: NonEmpty Float -> Ast1d input -> Ast1d input

-- UComponent :: Ast2d input -> Ast1d input
-- VComponent :: Ast2d input -> Ast1d input

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
  Squared1d arg . input = squared (arg . input)
  SquareRoot1d arg . input = sqrt (arg . input)
  Sine1d arg . input = sin (arg . input)
  Cosine1d arg . input = cos (arg . input)
  BezierCurve1d controlPoints param . input = BezierCurve1d controlPoints (param . input)

-- UComponent ast2d . input = uComponent (ast2d . input)
-- VComponent ast2d . input = vComponent (ast2d . input)

instance Composition (Ast1d input) (Ast2d Float) (Ast2d input) where
  Constant2d u v . _ = Constant2d u v
  UV u v . input = UV (u . input) (v . input)

toFloat :: Qty units -> Float
toFloat = Units.coerce

zero1d :: Ast1d input
zero1d = constant1d 0.0

zero2d :: Ast2d input
zero2d = constant2d 0.0 0.0

constant1d :: Qty units -> Ast1d input
constant1d value = Constant1d (toFloat value)

constant2d :: Qty units -> Qty units -> Ast2d input
constant2d u v = Constant2d (toFloat u) (toFloat v)

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
squared (SquareRoot1d expression) = expression
squared expression = Squared1d expression

sqrt :: Ast1d input -> Ast1d input
sqrt (Constant1d value) = constant1d (Float.sqrt value)
sqrt expression = SquareRoot1d expression

sin :: Ast1d input -> Ast1d input
sin (Constant1d value) = constant1d (Float.sin value)
sin expression = Sine1d expression

cos :: Ast1d input -> Ast1d input
cos (Constant1d value) = constant1d (Float.cos value)
cos (Negated1d expression) = cos expression
cos expression = Cosine1d expression

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
bezierCurve1d controlPoints param = BezierCurve1d (NonEmpty.map toFloat controlPoints) param

uComponent :: Ast2d input -> Ast1d input
uComponent (Constant2d u _) = constant1d u
uComponent (UV u _) = u

vComponent :: Ast2d input -> Ast1d input
vComponent (Constant2d _ v) = constant1d v
vComponent (UV _ v) = v

data Compilation = Compilation
  { constantsBuilder :: Builder
  , numConstants :: Int
  , bytecodeBuilder :: Builder
  , variables :: Map Instruction Int
  , numVariables :: Int
  }

data Input
  = Argument Int
  | Constant Int
  | Variable Int
  deriving (Eq, Ord)

data Instruction
  = Negate1d Input
  | Add1d Input Input
  | Subtract1d Input Input
  | Square1d Input
  | Multiply1d Input Input
  | Divide1d Input Input
  | Sqrt1d Input
  | Sin1d Input
  | Cos1d Input
  | Bezier1d (NonEmpty Float) Input
  deriving (Eq, Ord)

instructionOpcode :: Instruction -> Word8
instructionOpcode instruction = case instruction of
  Negate1d{} -> negate1dOpcode
  Add1d{} -> add1dOpcode
  Subtract1d{} -> subtract1dOpcode
  Square1d{} -> square1dOpcode
  Multiply1d{} -> multiply1dOpcode
  Divide1d{} -> divide1dOpcode
  Sqrt1d{} -> sqrt1dOpcode
  Sin1d{} -> sin1dOpcode
  Cos1d{} -> cos1dOpcode
  Bezier1d NonEmpty.Two{} _ -> linear1dOpcode
  Bezier1d NonEmpty.Three{} _ -> quadratic1dOpcode
  Bezier1d NonEmpty.Four{} _ -> cubic1dOpcode
  Bezier1d NonEmpty.Five{} _ -> quartic1dOpcode
  Bezier1d NonEmpty.Six{} _ -> quintic1dOpcode
  Bezier1d{} -> bezier1dOpcode

encodeByte :: Int -> Builder
encodeByte value = Builder.word8 (fromIntegral value)

encodeInput :: Input -> Builder
encodeInput (Argument index) = encodeByte index
encodeInput (Constant index) = encodeByte 2 <> encodeInt index
encodeInput (Variable index) = encodeByte 3 <> encodeInt index

encodeInt :: Int -> Builder
encodeInt index
  | index < 65536 = encodeByte (index % 256) <> encodeByte (index // 256)
  | otherwise = internalError "More than 65536 locals or constants in compiled function"

-- Encode a (64-bit) Float value using the current system endianness
-- (will pretty much always be little-endian unless we're deploying to IBM mainframes...)
encodeFloat :: Float -> Builder
#ifdef WORDS_BIGENDIAN
encodeFloat value = Builder.doubleBE (Float.toDouble value)
#else
encodeFloat value = Builder.doubleLE (Float.toDouble value)
#endif

compileConstant1d :: Float -> Compilation -> (Compilation, Input)
compileConstant1d value compilation0 = do
  let constantIndex = numConstants compilation0
  let compilation1 =
        compilation0
          { constantsBuilder = constantsBuilder compilation0 <> encodeFloat value
          , numConstants = numConstants compilation0 + 1
          }
  (compilation1, Constant constantIndex)

compileUnary1d :: (Input -> Instruction) -> Ast1d input -> Compilation -> (Compilation, Input)
compileUnary1d instructionConstructor argument compilation0 = do
  let (compilation1, argumentInput) = compile1d argument compilation0
  let existingVariables = variables compilation1
  let instruction = instructionConstructor argumentInput
  let opcode = instructionOpcode instruction
  case Map.get instruction existingVariables of
    Just resultIndex -> (compilation1, Variable resultIndex)
    Nothing -> do
      let resultIndex = numVariables compilation1
      let compilation2 =
            compilation1
              { bytecodeBuilder =
                  bytecodeBuilder compilation1
                    <> Builder.word8 opcode
                    <> encodeInput argumentInput
                    <> encodeInt resultIndex
              , variables = Map.set instruction resultIndex existingVariables
              , numVariables = numVariables compilation1 + 1
              }
      (compilation2, Variable resultIndex)

compileBinary1d ::
  (Input -> Input -> Instruction) ->
  Ast1d input ->
  Ast1d input ->
  Compilation ->
  (Compilation, Input)
compileBinary1d instructionConstructor lhs rhs compilation0 = do
  let (compilation1, lhsInput) = compile1d lhs compilation0
  let (compilation2, rhsInput) = compile1d rhs compilation1
  let existingVariables = variables compilation2
  let instruction = instructionConstructor lhsInput rhsInput
  let opcode = instructionOpcode instruction
  case Map.get instruction existingVariables of
    Just resultIndex -> (compilation2, Variable resultIndex)
    Nothing -> do
      let resultIndex = numVariables compilation2
      let compilation3 =
            compilation2
              { bytecodeBuilder =
                  bytecodeBuilder compilation2
                    <> Builder.word8 opcode
                    <> encodeInput lhsInput
                    <> encodeInput rhsInput
                    <> encodeInt resultIndex
              , variables = Map.set instruction resultIndex existingVariables
              , numVariables = numVariables compilation2 + 1
              }
      (compilation3, Variable resultIndex)

compileBezier1d ::
  NonEmpty Float ->
  Ast1d input ->
  Compilation ->
  (Compilation, Input)
compileBezier1d controlPoints parameter compilation0 = do
  let (compilation1, parameterInput) = compile1d parameter compilation0
  let existingVariables = variables compilation1
  let instruction = Bezier1d controlPoints parameterInput
  let opcode = instructionOpcode instruction
  case Map.get instruction existingVariables of
    Just resultIndex -> (compilation1, Variable resultIndex)
    Nothing -> do
      let controlPointsIndex = numConstants compilation1
      let resultIndex = numVariables compilation1
      let numControlPoints = NonEmpty.length controlPoints
      let pointCount =
            if opcode == bezier1dOpcode
              then encodeInt (NonEmpty.length controlPoints)
              else Binary.empty -- Fixed-size Bezier curves don't require control point count
      let compilation2 =
            compilation1
              { bytecodeBuilder =
                  bytecodeBuilder compilation1
                    <> Builder.word8 opcode
                    <> pointCount
                    <> encodeInt controlPointsIndex
                    <> encodeInput parameterInput
                    <> encodeInt resultIndex
              , constantsBuilder =
                  constantsBuilder compilation1
                    <> Binary.collect encodeFloat (NonEmpty.toList controlPoints)
              , numConstants = numConstants compilation1 + numControlPoints
              , variables = Map.set instruction resultIndex existingVariables
              , -- Allocate enough extra local variables for spline interpolation
                numVariables = numVariables compilation1 + (numControlPoints - 1)
              }
      (compilation2, Variable resultIndex)

compile1d :: Ast1d input -> Compilation -> (Compilation, Input)
compile1d ast compilation = case ast of
  CurveParameter -> (compilation, Argument 0)
  SurfaceParameter U -> (compilation, Argument 0)
  SurfaceParameter V -> (compilation, Argument 1)
  Constant1d value -> compileConstant1d value compilation
  Negated1d argument -> compileUnary1d Negate1d argument compilation
  Sum1d lhs rhs -> compileBinary1d Add1d lhs rhs compilation
  Difference1d lhs rhs -> compileBinary1d Subtract1d lhs rhs compilation
  Squared1d argument -> compileUnary1d Square1d argument compilation
  Product1d lhs rhs -> compileBinary1d Multiply1d lhs rhs compilation
  Quotient1d lhs rhs -> compileBinary1d Divide1d lhs rhs compilation
  SquareRoot1d argument -> compileUnary1d Sqrt1d argument compilation
  Sine1d argument -> compileUnary1d Sin1d argument compilation
  Cosine1d argument -> compileUnary1d Cos1d argument compilation
  BezierCurve1d controlPoints t -> compileBezier1d controlPoints t compilation

emptyCompilation :: Compilation
emptyCompilation =
  Compilation
    { constantsBuilder = Binary.empty
    , numConstants = 0
    , bytecodeBuilder = Binary.empty
    , variables = Map.empty
    , numVariables = 0
    }

expression1d :: Ast1d input -> Expression input Float
expression1d ast = do
  let (compilation, resultValue) = compile1d ast emptyCompilation
  let Compilation{constantsBuilder, bytecodeBuilder, numVariables} = compilation
  let constants = Binary.bytes constantsBuilder
  let returnInstruction = Builder.word8 return1dOpcode <> encodeInput resultValue
  let bytecode = Binary.bytes (bytecodeBuilder <> returnInstruction)
  Expression constants bytecode numVariables

data Expression input output = Expression ByteString ByteString Int

foreign import capi "expression.h value Return1d" return1dOpcode :: Word8

foreign import capi "expression.h value Negate1d" negate1dOpcode :: Word8

foreign import capi "expression.h value Add1d" add1dOpcode :: Word8

foreign import capi "expression.h value Subtract1d" subtract1dOpcode :: Word8

foreign import capi "expression.h value Square1d" square1dOpcode :: Word8

foreign import capi "expression.h value Multiply1d" multiply1dOpcode :: Word8

foreign import capi "expression.h value Divide1d" divide1dOpcode :: Word8

foreign import capi "expression.h value Sqrt1d" sqrt1dOpcode :: Word8

foreign import capi "expression.h value Sin1d" sin1dOpcode :: Word8

foreign import capi "expression.h value Cos1d" cos1dOpcode :: Word8

foreign import capi "expression.h value Linear1d" linear1dOpcode :: Word8

foreign import capi "expression.h value Quadratic1d" quadratic1dOpcode :: Word8

foreign import capi "expression.h value Cubic1d" cubic1dOpcode :: Word8

foreign import capi "expression.h value Quartic1d" quartic1dOpcode :: Word8

foreign import capi "expression.h value Quintic1d" quintic1dOpcode :: Word8

foreign import capi "expression.h value Bezier1d" bezier1dOpcode :: Word8

foreign import capi "expression.h opensolid_curve1d_value"
  opensolid_curve1d_value :: CString -> Double -> Ptr Double -> Int -> Ptr Double -> IO ()

class Evaluation input output where
  evaluate :: Expression input output -> input -> output

instance Evaluation Float Float where
  evaluate (Expression constants bytecode numVariables) t =
    unsafeDupablePerformIO $
      Data.ByteString.Unsafe.unsafeUseAsCString constants \constantsPtr ->
        Data.ByteString.Unsafe.unsafeUseAsCString bytecode \bytecodePtr ->
          Foreign.Marshal.Alloc.allocaBytes 8 \outputPtr -> IO.do
            opensolid_curve1d_value
              bytecodePtr
              (Float.toDouble t)
              (Foreign.Ptr.castPtr constantsPtr)
              numVariables
              outputPtr
            IO.map Float.fromDouble (Foreign.peek outputPtr)
