module OpenSolid.Bytecode.Instruction
  ( Instruction (..)
  , VariableIndex (VariableIndex)
  , ConstantIndex (ConstantIndex)
  , encode
  , return
  )
where

import Data.ByteString.Builder qualified as Builder
import Data.Word (Word16)
import GHC.ByteOrder qualified
import OpenSolid.Binary (Builder)
import OpenSolid.Prelude

newtype ConstantIndex = ConstantIndex Int deriving (Eq, Ord)

newtype VariableIndex = VariableIndex Int deriving (Eq, Ord)

data Instruction
  = XComponent VariableIndex
  | YComponent VariableIndex
  | ZComponent VariableIndex
  | Negate1d VariableIndex
  | Add1d VariableIndex VariableIndex
  | AddVariableConstant1d VariableIndex ConstantIndex
  | Subtract1d VariableIndex VariableIndex
  | SubtractConstantVariable1d ConstantIndex VariableIndex
  | Multiply1d VariableIndex VariableIndex
  | MultiplyVariableConstant1d VariableIndex ConstantIndex
  | Divide1d VariableIndex VariableIndex
  | DivideConstantVariable1d ConstantIndex VariableIndex
  | Square1d VariableIndex
  | Sqrt1d VariableIndex
  | Sin1d VariableIndex
  | Cos1d VariableIndex
  | Bezier1d Int ConstantIndex VariableIndex
  | XY2d VariableIndex VariableIndex
  | XC2d VariableIndex ConstantIndex
  | CY2d ConstantIndex VariableIndex
  | Negate2d VariableIndex
  | Add2d VariableIndex VariableIndex
  | AddVariableConstant2d VariableIndex ConstantIndex
  | Subtract2d VariableIndex VariableIndex
  | SubtractConstantVariable2d ConstantIndex VariableIndex
  | Multiply2d VariableIndex VariableIndex
  | MultiplyVariableConstant2d VariableIndex ConstantIndex
  | MultiplyConstantVariable2d ConstantIndex VariableIndex
  | Divide2d VariableIndex VariableIndex
  | DivideConstantVariable2d ConstantIndex VariableIndex
  | SquaredNorm2d VariableIndex
  | Norm2d VariableIndex
  | Dot2d VariableIndex VariableIndex
  | DotVariableConstant2d VariableIndex ConstantIndex
  | Cross2d VariableIndex VariableIndex
  | CrossVariableConstant2d VariableIndex ConstantIndex
  | Bezier2d Int ConstantIndex VariableIndex
  | XYZ3d VariableIndex VariableIndex VariableIndex
  | XYC3d VariableIndex VariableIndex ConstantIndex
  | XCZ3d VariableIndex ConstantIndex VariableIndex
  | CYZ3d ConstantIndex VariableIndex VariableIndex
  | XCC3d VariableIndex ConstantIndex ConstantIndex
  | CYC3d ConstantIndex VariableIndex ConstantIndex
  | CCZ3d ConstantIndex ConstantIndex VariableIndex
  | Negate3d VariableIndex
  | Add3d VariableIndex VariableIndex
  | AddVariableConstant3d VariableIndex ConstantIndex
  | Subtract3d VariableIndex VariableIndex
  | SubtractConstantVariable3d ConstantIndex VariableIndex
  | Multiply3d VariableIndex VariableIndex
  | MultiplyVariableConstant3d VariableIndex ConstantIndex
  | MultiplyConstantVariable3d ConstantIndex VariableIndex
  | Divide3d VariableIndex VariableIndex
  | DivideConstantVariable3d ConstantIndex VariableIndex
  | SquaredNorm3d VariableIndex
  | Norm3d VariableIndex
  | Dot3d VariableIndex VariableIndex
  | DotVariableConstant3d VariableIndex ConstantIndex
  | Cross3d VariableIndex VariableIndex
  | CrossVariableConstant3d VariableIndex ConstantIndex
  | Bezier3d Int ConstantIndex VariableIndex
  deriving (Eq, Ord)

encodeWord :: Word16 -> Builder
encodeWord = case GHC.ByteOrder.targetByteOrder of
  GHC.ByteOrder.LittleEndian -> Builder.word16LE
  GHC.ByteOrder.BigEndian -> Builder.word16BE

encodeInt :: Int -> Builder
encodeInt value
  | value < 65536 = encodeWord (fromIntegral value)
  | otherwise = exception "More than 65536 locals or constants in compiled function"

encodeVariableIndex :: VariableIndex -> Builder
encodeVariableIndex (VariableIndex index) = encodeInt index

encodeConstantIndex :: ConstantIndex -> Builder
encodeConstantIndex (ConstantIndex index) = encodeInt index

encode :: Instruction -> VariableIndex -> Builder
encode instruction outputIndex =
  encodeOpcodeAndArguments instruction <> encodeVariableIndex outputIndex

encodeOpcodeAndArguments :: Instruction -> Builder
encodeOpcodeAndArguments instruction = case instruction of
  XComponent arg ->
    encodeInt xComponentOpcode
      <> encodeVariableIndex arg
  YComponent arg ->
    encodeInt yComponentOpcode
      <> encodeVariableIndex arg
  ZComponent arg ->
    encodeInt zComponentOpcode
      <> encodeVariableIndex arg
  Negate1d arg ->
    encodeInt negate1dOpcode
      <> encodeVariableIndex arg
  Add1d lhs rhs ->
    encodeInt add1dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  AddVariableConstant1d lhs rhs ->
    encodeInt addVariableConstant1dOpcode
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Subtract1d lhs rhs ->
    encodeInt subtract1dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  SubtractConstantVariable1d lhs rhs ->
    encodeInt subtractConstantVariable1dOpcode
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Multiply1d lhs rhs ->
    encodeInt multiply1dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  MultiplyVariableConstant1d lhs rhs ->
    encodeInt multiplyVariableConstant1dOpcode
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Divide1d lhs rhs ->
    encodeInt divide1dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DivideConstantVariable1d lhs rhs ->
    encodeInt divideConstantVariable1dOpcode
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Square1d arg ->
    encodeInt square1dOpcode
      <> encodeVariableIndex arg
  Sqrt1d arg ->
    encodeInt sqrt1dOpcode
      <> encodeVariableIndex arg
  Sin1d arg ->
    encodeInt sin1dOpcode
      <> encodeVariableIndex arg
  Cos1d arg ->
    encodeInt cos1dOpcode
      <> encodeVariableIndex arg
  Bezier1d 2 controlPoints parameter ->
    encodeInt linear1dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d 3 controlPoints parameter ->
    encodeInt quadratic1dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d 4 controlPoints parameter ->
    encodeInt cubic1dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d 5 controlPoints parameter ->
    encodeInt quartic1dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d 6 controlPoints parameter ->
    encodeInt quintic1dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d n controlPoints parameter ->
    encodeInt bezier1dOpcode
      <> encodeInt n
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  XY2d x y ->
    encodeInt xy2dOpcode
      <> encodeVariableIndex x
      <> encodeVariableIndex y
  XC2d x y ->
    encodeInt xc2dOpcode
      <> encodeVariableIndex x
      <> encodeConstantIndex y
  CY2d x y ->
    encodeInt cy2dOpcode
      <> encodeConstantIndex x
      <> encodeVariableIndex y
  Negate2d arg ->
    encodeInt negate2dOpcode
      <> encodeVariableIndex arg
  Add2d lhs rhs ->
    encodeInt add2dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  AddVariableConstant2d lhs rhs ->
    encodeInt addVariableConstant2dOpcode
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Subtract2d lhs rhs ->
    encodeInt subtract2dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  SubtractConstantVariable2d lhs rhs ->
    encodeInt subtractConstantVariable2dOpcode
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Multiply2d lhs rhs ->
    encodeInt multiply2dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  MultiplyVariableConstant2d lhs rhs ->
    encodeInt multiplyVariableConstant2dOpcode
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  MultiplyConstantVariable2d lhs rhs ->
    encodeInt multiplyConstantVariable2dOpcode
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Divide2d lhs rhs ->
    encodeInt divide2dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DivideConstantVariable2d lhs rhs ->
    encodeInt divideConstantVariable2dOpcode
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  SquaredNorm2d arg ->
    encodeInt squaredNorm2dOpcode
      <> encodeVariableIndex arg
  Norm2d arg ->
    encodeInt norm2dOpcode
      <> encodeVariableIndex arg
  Dot2d lhs rhs ->
    encodeInt dot2dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DotVariableConstant2d lhs rhs ->
    encodeInt dotVariableConstant2dOpcode
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Cross2d lhs rhs ->
    encodeInt cross2dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  CrossVariableConstant2d lhs rhs ->
    encodeInt crossVariableConstant2dOpcode
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Bezier2d 2 controlPoints parameter ->
    encodeInt linear2dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2d 3 controlPoints parameter ->
    encodeInt quadratic2dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2d 4 controlPoints parameter ->
    encodeInt cubic2dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2d 5 controlPoints parameter ->
    encodeInt quartic2dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2d 6 controlPoints parameter ->
    encodeInt quintic2dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2d n controlPoints parameter ->
    encodeInt bezier2dOpcode
      <> encodeInt n
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  XYZ3d x y z ->
    encodeInt xyz3dOpcode
      <> encodeVariableIndex x
      <> encodeVariableIndex y
      <> encodeVariableIndex z
  XYC3d x y z ->
    encodeInt xyc3dOpcode
      <> encodeVariableIndex x
      <> encodeVariableIndex y
      <> encodeConstantIndex z
  XCZ3d x y z ->
    encodeInt xcz3dOpcode
      <> encodeVariableIndex x
      <> encodeConstantIndex y
      <> encodeVariableIndex z
  CYZ3d x y z ->
    encodeInt cyz3dOpcode
      <> encodeConstantIndex x
      <> encodeVariableIndex y
      <> encodeVariableIndex z
  XCC3d x y z ->
    encodeInt xcc3dOpcode
      <> encodeVariableIndex x
      <> encodeConstantIndex y
      <> encodeConstantIndex z
  CYC3d x y z ->
    encodeInt cyc3dOpcode
      <> encodeConstantIndex x
      <> encodeVariableIndex y
      <> encodeConstantIndex z
  CCZ3d x y z ->
    encodeInt ccz3dOpcode
      <> encodeConstantIndex x
      <> encodeConstantIndex y
      <> encodeVariableIndex z
  Negate3d arg ->
    encodeInt negate3dOpcode
      <> encodeVariableIndex arg
  Add3d lhs rhs ->
    encodeInt add3dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  AddVariableConstant3d lhs rhs ->
    encodeInt addVariableConstant3dOpcode
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Subtract3d lhs rhs ->
    encodeInt subtract3dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  SubtractConstantVariable3d lhs rhs ->
    encodeInt subtractConstantVariable3dOpcode
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Multiply3d lhs rhs ->
    encodeInt multiply3dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  MultiplyVariableConstant3d lhs rhs ->
    encodeInt multiplyVariableConstant3dOpcode
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  MultiplyConstantVariable3d lhs rhs ->
    encodeInt multiplyConstantVariable3dOpcode
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Divide3d lhs rhs ->
    encodeInt divide3dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DivideConstantVariable3d lhs rhs ->
    encodeInt divideConstantVariable3dOpcode
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  SquaredNorm3d arg ->
    encodeInt squaredNorm3dOpcode
      <> encodeVariableIndex arg
  Norm3d arg ->
    encodeInt norm3dOpcode
      <> encodeVariableIndex arg
  Dot3d lhs rhs ->
    encodeInt dot3dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DotVariableConstant3d lhs rhs ->
    encodeInt dotVariableConstant3dOpcode
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Cross3d lhs rhs ->
    encodeInt cross3dOpcode
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  CrossVariableConstant3d lhs rhs ->
    encodeInt crossVariableConstant3dOpcode
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Bezier3d 2 controlPoints parameter ->
    encodeInt linear3dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3d 3 controlPoints parameter ->
    encodeInt quadratic3dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3d 4 controlPoints parameter ->
    encodeInt cubic3dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3d 5 controlPoints parameter ->
    encodeInt quartic3dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3d 6 controlPoints parameter ->
    encodeInt quintic3dOpcode
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3d n controlPoints parameter ->
    encodeInt bezier3dOpcode
      <> encodeInt n
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter

return :: Int -> VariableIndex -> Builder
return dimension variableIndex =
  encodeInt returnOpcode <> encodeInt dimension <> encodeVariableIndex variableIndex

foreign import capi "bytecode.h value Return"
  returnOpcode :: Int

foreign import capi "bytecode.h value XComponent"
  xComponentOpcode :: Int

foreign import capi "bytecode.h value YComponent"
  yComponentOpcode :: Int

foreign import capi "bytecode.h value ZComponent"
  zComponentOpcode :: Int

foreign import capi "bytecode.h value Negate1d"
  negate1dOpcode :: Int

foreign import capi "bytecode.h value Add1d"
  add1dOpcode :: Int

foreign import capi "bytecode.h value AddVariableConstant1d"
  addVariableConstant1dOpcode :: Int

foreign import capi "bytecode.h value Subtract1d"
  subtract1dOpcode :: Int

foreign import capi "bytecode.h value SubtractConstantVariable1d"
  subtractConstantVariable1dOpcode :: Int

foreign import capi "bytecode.h value Multiply1d"
  multiply1dOpcode :: Int

foreign import capi "bytecode.h value MultiplyVariableConstant1d"
  multiplyVariableConstant1dOpcode :: Int

foreign import capi "bytecode.h value Divide1d"
  divide1dOpcode :: Int

foreign import capi "bytecode.h value DivideConstantVariable1d"
  divideConstantVariable1dOpcode :: Int

foreign import capi "bytecode.h value Square1d"
  square1dOpcode :: Int

foreign import capi "bytecode.h value Sqrt1d"
  sqrt1dOpcode :: Int

foreign import capi "bytecode.h value Sin1d"
  sin1dOpcode :: Int

foreign import capi "bytecode.h value Cos1d"
  cos1dOpcode :: Int

foreign import capi "bytecode.h value Linear1d"
  linear1dOpcode :: Int

foreign import capi "bytecode.h value Quadratic1d"
  quadratic1dOpcode :: Int

foreign import capi "bytecode.h value Cubic1d"
  cubic1dOpcode :: Int

foreign import capi "bytecode.h value Quartic1d"
  quartic1dOpcode :: Int

foreign import capi "bytecode.h value Quintic1d"
  quintic1dOpcode :: Int

foreign import capi "bytecode.h value Bezier1d"
  bezier1dOpcode :: Int

foreign import capi "bytecode.h value XY2d"
  xy2dOpcode :: Int

foreign import capi "bytecode.h value XC2d"
  xc2dOpcode :: Int

foreign import capi "bytecode.h value CY2d"
  cy2dOpcode :: Int

foreign import capi "bytecode.h value Negate2d"
  negate2dOpcode :: Int

foreign import capi "bytecode.h value Add2d"
  add2dOpcode :: Int

foreign import capi "bytecode.h value AddVariableConstant2d"
  addVariableConstant2dOpcode :: Int

foreign import capi "bytecode.h value Subtract2d"
  subtract2dOpcode :: Int

foreign import capi "bytecode.h value SubtractConstantVariable2d"
  subtractConstantVariable2dOpcode :: Int

foreign import capi "bytecode.h value Multiply2d"
  multiply2dOpcode :: Int

foreign import capi "bytecode.h value MultiplyVariableConstant2d"
  multiplyVariableConstant2dOpcode :: Int

foreign import capi "bytecode.h value MultiplyConstantVariable2d"
  multiplyConstantVariable2dOpcode :: Int

foreign import capi "bytecode.h value Divide2d"
  divide2dOpcode :: Int

foreign import capi "bytecode.h value DivideConstantVariable2d"
  divideConstantVariable2dOpcode :: Int

foreign import capi "bytecode.h value SquaredNorm2d"
  squaredNorm2dOpcode :: Int

foreign import capi "bytecode.h value Norm2d"
  norm2dOpcode :: Int

foreign import capi "bytecode.h value Dot2d"
  dot2dOpcode :: Int

foreign import capi "bytecode.h value DotVariableConstant2d"
  dotVariableConstant2dOpcode :: Int

foreign import capi "bytecode.h value Cross2d"
  cross2dOpcode :: Int

foreign import capi "bytecode.h value CrossVariableConstant2d"
  crossVariableConstant2dOpcode :: Int

foreign import capi "bytecode.h value Linear2d"
  linear2dOpcode :: Int

foreign import capi "bytecode.h value Quadratic2d"
  quadratic2dOpcode :: Int

foreign import capi "bytecode.h value Cubic2d"
  cubic2dOpcode :: Int

foreign import capi "bytecode.h value Quartic2d"
  quartic2dOpcode :: Int

foreign import capi "bytecode.h value Quintic2d"
  quintic2dOpcode :: Int

foreign import capi "bytecode.h value Bezier2d"
  bezier2dOpcode :: Int

foreign import capi "bytecode.h value XYZ3d"
  xyz3dOpcode :: Int

foreign import capi "bytecode.h value XYC3d"
  xyc3dOpcode :: Int

foreign import capi "bytecode.h value XCZ3d"
  xcz3dOpcode :: Int

foreign import capi "bytecode.h value CYZ3d"
  cyz3dOpcode :: Int

foreign import capi "bytecode.h value XCC3d"
  xcc3dOpcode :: Int

foreign import capi "bytecode.h value CYC3d"
  cyc3dOpcode :: Int

foreign import capi "bytecode.h value CCZ3d"
  ccz3dOpcode :: Int

foreign import capi "bytecode.h value Negate3d"
  negate3dOpcode :: Int

foreign import capi "bytecode.h value Add3d"
  add3dOpcode :: Int

foreign import capi "bytecode.h value AddVariableConstant3d"
  addVariableConstant3dOpcode :: Int

foreign import capi "bytecode.h value Subtract3d"
  subtract3dOpcode :: Int

foreign import capi "bytecode.h value SubtractConstantVariable3d"
  subtractConstantVariable3dOpcode :: Int

foreign import capi "bytecode.h value Multiply3d"
  multiply3dOpcode :: Int

foreign import capi "bytecode.h value MultiplyVariableConstant3d"
  multiplyVariableConstant3dOpcode :: Int

foreign import capi "bytecode.h value MultiplyConstantVariable3d"
  multiplyConstantVariable3dOpcode :: Int

foreign import capi "bytecode.h value Divide3d"
  divide3dOpcode :: Int

foreign import capi "bytecode.h value DivideConstantVariable3d"
  divideConstantVariable3dOpcode :: Int

foreign import capi "bytecode.h value SquaredNorm3d"
  squaredNorm3dOpcode :: Int

foreign import capi "bytecode.h value Norm3d"
  norm3dOpcode :: Int

foreign import capi "bytecode.h value Dot3d"
  dot3dOpcode :: Int

foreign import capi "bytecode.h value DotVariableConstant3d"
  dotVariableConstant3dOpcode :: Int

foreign import capi "bytecode.h value Cross3d"
  cross3dOpcode :: Int

foreign import capi "bytecode.h value CrossVariableConstant3d"
  crossVariableConstant3dOpcode :: Int

foreign import capi "bytecode.h value Linear3d"
  linear3dOpcode :: Int

foreign import capi "bytecode.h value Quadratic3d"
  quadratic3dOpcode :: Int

foreign import capi "bytecode.h value Cubic3d"
  cubic3dOpcode :: Int

foreign import capi "bytecode.h value Quartic3d"
  quartic3dOpcode :: Int

foreign import capi "bytecode.h value Quintic3d"
  quintic3dOpcode :: Int

foreign import capi "bytecode.h value Bezier3d"
  bezier3dOpcode :: Int
