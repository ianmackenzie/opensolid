module OpenSolid.Bytecode.Instruction
  ( Instruction (..)
  , VariableIndex (VariableIndex)
  , ConstantIndex (ConstantIndex)
  , ValueIndex (ConstantValue, VariableValue)
  , encode
  , return
  )
where

import OpenSolid.Binary (Builder)
import OpenSolid.Bytecode.Encode qualified as Encode
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Prelude qualified

newtype ConstantIndex = ConstantIndex Int deriving (Eq, Ord)

instance Show ConstantIndex where
  show (ConstantIndex index) = Text.unpack ("C" <> Text.int index)

newtype VariableIndex = VariableIndex Int deriving (Eq, Ord)

instance Show VariableIndex where
  show (VariableIndex index) = Text.unpack ("V" <> Text.int index)

data ValueIndex = ConstantValue ConstantIndex | VariableValue VariableIndex deriving (Eq, Ord)

instance Show ValueIndex where
  show (ConstantValue constantIndex) = Prelude.show constantIndex
  show (VariableValue variableIndex) = Prelude.show variableIndex

data Instruction
  = Component0 VariableIndex
  | Component1 VariableIndex
  | Component2 VariableIndex
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
  | XY VariableIndex VariableIndex
  | XC VariableIndex ConstantIndex
  | CY ConstantIndex VariableIndex
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
  | SquaredMagnitude2d VariableIndex
  | Magnitude2d VariableIndex
  | Dot2d VariableIndex VariableIndex
  | DotVariableConstant2d VariableIndex ConstantIndex
  | Cross2d VariableIndex VariableIndex
  | CrossVariableConstant2d VariableIndex ConstantIndex
  | Bezier2d Int ConstantIndex VariableIndex
  | TransformVector2d ConstantIndex VariableIndex
  | TransformPoint2d ConstantIndex VariableIndex
  | ProjectVector3d ConstantIndex VariableIndex
  | ProjectPoint3d ConstantIndex VariableIndex
  | RFU VariableIndex VariableIndex VariableIndex
  | RFC VariableIndex VariableIndex ConstantIndex
  | RCU VariableIndex ConstantIndex VariableIndex
  | CFU ConstantIndex VariableIndex VariableIndex
  | RCC VariableIndex ConstantIndex ConstantIndex
  | CFC ConstantIndex VariableIndex ConstantIndex
  | CCU ConstantIndex ConstantIndex VariableIndex
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
  | SquaredMagnitude3d VariableIndex
  | Magnitude3d VariableIndex
  | Dot3d VariableIndex VariableIndex
  | DotVariableConstant3d VariableIndex ConstantIndex
  | Cross3d VariableIndex VariableIndex
  | CrossVariableConstant3d VariableIndex ConstantIndex
  | Bezier3d Int ConstantIndex VariableIndex
  | TransformVector3d ConstantIndex VariableIndex
  | TransformPoint3d ConstantIndex VariableIndex
  | PlaceVector2d ConstantIndex VariableIndex
  | PlacePoint2d ConstantIndex VariableIndex
  | Desingularized1d VariableIndex VariableIndex VariableIndex VariableIndex
  | Desingularized2d VariableIndex VariableIndex VariableIndex VariableIndex
  | Desingularized3d VariableIndex VariableIndex VariableIndex VariableIndex
  | Blend1d ValueIndex (List ValueIndex) ValueIndex (List ValueIndex) ValueIndex
  | Blend2d ValueIndex (List ValueIndex) ValueIndex (List ValueIndex) ValueIndex
  | Blend3d ValueIndex (List ValueIndex) ValueIndex (List ValueIndex) ValueIndex
  | Cube1d VariableIndex
  | B00 VariableIndex
  | B00d1 VariableIndex
  | B00d2 VariableIndex
  | B00d3 VariableIndex
  | B01 VariableIndex
  | B01d1 VariableIndex
  | B01d2 VariableIndex
  | B01d3 VariableIndex
  | B02 VariableIndex
  | B02d1 VariableIndex
  | B02d2 VariableIndex
  | B02d3 VariableIndex
  | B10 VariableIndex
  | B10d1 VariableIndex
  | B10d2 VariableIndex
  | B10d3 VariableIndex
  | B11 VariableIndex
  | B11d1 VariableIndex
  | B11d2 VariableIndex
  | B11d3 VariableIndex
  deriving (Eq, Ord, Show)

maxValues :: Int
maxValues = 32768

tooManyVariables :: Text
tooManyVariables = "More than " <> Text.int maxValues <> " variables in compiled bytecode"

tooManyConstants :: Text
tooManyConstants = "More than " <> Text.int maxValues <> " constants in compiled bytecode"

encodeVariableIndex :: VariableIndex -> Builder
encodeVariableIndex (VariableIndex index)
  | index < maxValues = Encode.int index
  | otherwise = exception tooManyVariables

encodeConstantIndex :: ConstantIndex -> Builder
encodeConstantIndex (ConstantIndex index)
  | index < maxValues = Encode.int index
  | otherwise = exception tooManyConstants

encodeValueIndex :: ValueIndex -> Builder
encodeValueIndex (VariableValue (VariableIndex index))
  | index < maxValues = Encode.int index
  | otherwise = exception tooManyVariables
encodeValueIndex (ConstantValue (ConstantIndex index))
  | index < maxValues = Encode.int (maxValues + index)
  | otherwise = exception tooManyConstants

encode :: Instruction -> VariableIndex -> Builder
encode instruction outputIndex =
  encodeOpcodeAndArguments instruction <> encodeVariableIndex outputIndex

encodeOpcodeAndArguments :: Instruction -> Builder
encodeOpcodeAndArguments instruction = case instruction of
  Component0 arg ->
    Encode.int 1
      <> encodeVariableIndex arg
  Component1 arg ->
    Encode.int 2
      <> encodeVariableIndex arg
  Component2 arg ->
    Encode.int 3
      <> encodeVariableIndex arg
  Negate1d arg ->
    Encode.int 4
      <> encodeVariableIndex arg
  Add1d lhs rhs ->
    Encode.int 5
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  AddVariableConstant1d lhs rhs ->
    Encode.int 6
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Subtract1d lhs rhs ->
    Encode.int 7
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  SubtractConstantVariable1d lhs rhs ->
    Encode.int 8
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Multiply1d lhs rhs ->
    Encode.int 9
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  MultiplyVariableConstant1d lhs rhs ->
    Encode.int 10
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Divide1d lhs rhs ->
    Encode.int 11
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DivideConstantVariable1d lhs rhs ->
    Encode.int 12
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Square1d arg ->
    Encode.int 13
      <> encodeVariableIndex arg
  Sqrt1d arg ->
    Encode.int 14
      <> encodeVariableIndex arg
  Sin1d arg ->
    Encode.int 15
      <> encodeVariableIndex arg
  Cos1d arg ->
    Encode.int 16
      <> encodeVariableIndex arg
  Bezier1d 2 controlPoints parameter ->
    Encode.int 17
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d 3 controlPoints parameter ->
    Encode.int 18
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d 4 controlPoints parameter ->
    Encode.int 19
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d 5 controlPoints parameter ->
    Encode.int 20
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d 6 controlPoints parameter ->
    Encode.int 21
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1d n controlPoints parameter ->
    Encode.int 22
      <> Encode.int n
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  XY x y ->
    Encode.int 23
      <> encodeVariableIndex x
      <> encodeVariableIndex y
  XC x y ->
    Encode.int 24
      <> encodeVariableIndex x
      <> encodeConstantIndex y
  CY x y ->
    Encode.int 25
      <> encodeConstantIndex x
      <> encodeVariableIndex y
  Negate2d arg ->
    Encode.int 26
      <> encodeVariableIndex arg
  Add2d lhs rhs ->
    Encode.int 27
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  AddVariableConstant2d lhs rhs ->
    Encode.int 28
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Subtract2d lhs rhs ->
    Encode.int 29
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  SubtractConstantVariable2d lhs rhs ->
    Encode.int 30
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Multiply2d lhs rhs ->
    Encode.int 31
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  MultiplyVariableConstant2d lhs rhs ->
    Encode.int 32
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  MultiplyConstantVariable2d lhs rhs ->
    Encode.int 33
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Divide2d lhs rhs ->
    Encode.int 34
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DivideConstantVariable2d lhs rhs ->
    Encode.int 35
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  SquaredMagnitude2d arg ->
    Encode.int 36
      <> encodeVariableIndex arg
  Magnitude2d arg ->
    Encode.int 37
      <> encodeVariableIndex arg
  Dot2d lhs rhs ->
    Encode.int 38
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DotVariableConstant2d lhs rhs ->
    Encode.int 39
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Cross2d lhs rhs ->
    Encode.int 40
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  CrossVariableConstant2d lhs rhs ->
    Encode.int 41
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Bezier2d 2 controlPoints parameter ->
    Encode.int 42
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2d 3 controlPoints parameter ->
    Encode.int 43
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2d 4 controlPoints parameter ->
    Encode.int 44
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2d 5 controlPoints parameter ->
    Encode.int 45
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2d 6 controlPoints parameter ->
    Encode.int 46
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2d n controlPoints parameter ->
    Encode.int 47
      <> Encode.int n
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  TransformVector2d matrix vector ->
    Encode.int 48
      <> encodeConstantIndex matrix
      <> encodeVariableIndex vector
  TransformPoint2d matrix point ->
    Encode.int 49
      <> encodeConstantIndex matrix
      <> encodeVariableIndex point
  ProjectVector3d matrix vector ->
    Encode.int 50
      <> encodeConstantIndex matrix
      <> encodeVariableIndex vector
  ProjectPoint3d matrix point ->
    Encode.int 51
      <> encodeConstantIndex matrix
      <> encodeVariableIndex point
  RFU r f u ->
    Encode.int 52
      <> encodeVariableIndex r
      <> encodeVariableIndex f
      <> encodeVariableIndex u
  RFC r f u ->
    Encode.int 53
      <> encodeVariableIndex r
      <> encodeVariableIndex f
      <> encodeConstantIndex u
  RCU r f u ->
    Encode.int 54
      <> encodeVariableIndex r
      <> encodeConstantIndex f
      <> encodeVariableIndex u
  CFU r f u ->
    Encode.int 55
      <> encodeConstantIndex r
      <> encodeVariableIndex f
      <> encodeVariableIndex u
  RCC r f u ->
    Encode.int 56
      <> encodeVariableIndex r
      <> encodeConstantIndex f
      <> encodeConstantIndex u
  CFC r f u ->
    Encode.int 57
      <> encodeConstantIndex r
      <> encodeVariableIndex f
      <> encodeConstantIndex u
  CCU r f u ->
    Encode.int 58
      <> encodeConstantIndex r
      <> encodeConstantIndex f
      <> encodeVariableIndex u
  Negate3d arg ->
    Encode.int 59
      <> encodeVariableIndex arg
  Add3d lhs rhs ->
    Encode.int 60
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  AddVariableConstant3d lhs rhs ->
    Encode.int 61
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Subtract3d lhs rhs ->
    Encode.int 62
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  SubtractConstantVariable3d lhs rhs ->
    Encode.int 63
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Multiply3d lhs rhs ->
    Encode.int 64
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  MultiplyVariableConstant3d lhs rhs ->
    Encode.int 65
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  MultiplyConstantVariable3d lhs rhs ->
    Encode.int 66
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Divide3d lhs rhs ->
    Encode.int 67
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DivideConstantVariable3d lhs rhs ->
    Encode.int 68
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  SquaredMagnitude3d arg ->
    Encode.int 69
      <> encodeVariableIndex arg
  Magnitude3d arg ->
    Encode.int 70
      <> encodeVariableIndex arg
  Dot3d lhs rhs ->
    Encode.int 71
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DotVariableConstant3d lhs rhs ->
    Encode.int 72
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Cross3d lhs rhs ->
    Encode.int 73
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  CrossVariableConstant3d lhs rhs ->
    Encode.int 74
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Bezier3d 2 controlPoints parameter ->
    Encode.int 75
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3d 3 controlPoints parameter ->
    Encode.int 76
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3d 4 controlPoints parameter ->
    Encode.int 77
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3d 5 controlPoints parameter ->
    Encode.int 78
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3d 6 controlPoints parameter ->
    Encode.int 79
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3d n controlPoints parameter ->
    Encode.int 80
      <> Encode.int n
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  TransformVector3d matrix vector ->
    Encode.int 81
      <> encodeConstantIndex matrix
      <> encodeVariableIndex vector
  TransformPoint3d matrix point ->
    Encode.int 82
      <> encodeConstantIndex matrix
      <> encodeVariableIndex point
  PlaceVector2d matrix vector ->
    Encode.int 83
      <> encodeConstantIndex matrix
      <> encodeVariableIndex vector
  PlacePoint2d matrix point ->
    Encode.int 84
      <> encodeConstantIndex matrix
      <> encodeVariableIndex point
  Desingularized1d parameterValue left middle right ->
    Encode.int 85
      <> encodeVariableIndex parameterValue
      <> encodeVariableIndex left
      <> encodeVariableIndex middle
      <> encodeVariableIndex right
  Desingularized2d parameterValue left middle right ->
    Encode.int 86
      <> encodeVariableIndex parameterValue
      <> encodeVariableIndex left
      <> encodeVariableIndex middle
      <> encodeVariableIndex right
  Desingularized3d parameterValue left middle right ->
    Encode.int 87
      <> encodeVariableIndex parameterValue
      <> encodeVariableIndex left
      <> encodeVariableIndex middle
      <> encodeVariableIndex right
  Blend1d startValue startDerivatives endValue endDerivatives parameterValue ->
    Encode.int 88
      <> encodeValueIndex startValue
      <> Encode.list encodeValueIndex startDerivatives
      <> encodeValueIndex endValue
      <> Encode.list encodeValueIndex endDerivatives
      <> encodeValueIndex parameterValue
  Blend2d startValue startDerivatives endValue endDerivatives parameterValue ->
    Encode.int 89
      <> encodeValueIndex startValue
      <> Encode.list encodeValueIndex startDerivatives
      <> encodeValueIndex endValue
      <> Encode.list encodeValueIndex endDerivatives
      <> encodeValueIndex parameterValue
  Blend3d startValue startDerivatives endValue endDerivatives parameterValue ->
    Encode.int 90
      <> encodeValueIndex startValue
      <> Encode.list encodeValueIndex startDerivatives
      <> encodeValueIndex endValue
      <> Encode.list encodeValueIndex endDerivatives
      <> encodeValueIndex parameterValue
  Cube1d arg ->
    Encode.int 91
      <> encodeVariableIndex arg
  B00 arg ->
    Encode.int 92
      <> encodeVariableIndex arg
  B00d1 arg ->
    Encode.int 93
      <> encodeVariableIndex arg
  B00d2 arg ->
    Encode.int 94
      <> encodeVariableIndex arg
  B00d3 arg ->
    Encode.int 95
      <> encodeVariableIndex arg
  B01 arg ->
    Encode.int 96
      <> encodeVariableIndex arg
  B01d1 arg ->
    Encode.int 97
      <> encodeVariableIndex arg
  B01d2 arg ->
    Encode.int 98
      <> encodeVariableIndex arg
  B01d3 arg ->
    Encode.int 99
      <> encodeVariableIndex arg
  B02 arg ->
    Encode.int 100
      <> encodeVariableIndex arg
  B02d1 arg ->
    Encode.int 101
      <> encodeVariableIndex arg
  B02d2 arg ->
    Encode.int 102
      <> encodeVariableIndex arg
  B02d3 arg ->
    Encode.int 103
      <> encodeVariableIndex arg
  B10 arg ->
    Encode.int 104
      <> encodeVariableIndex arg
  B10d1 arg ->
    Encode.int 105
      <> encodeVariableIndex arg
  B10d2 arg ->
    Encode.int 106
      <> encodeVariableIndex arg
  B10d3 arg ->
    Encode.int 107
      <> encodeVariableIndex arg
  B11 arg ->
    Encode.int 108
      <> encodeVariableIndex arg
  B11d1 arg ->
    Encode.int 109
      <> encodeVariableIndex arg
  B11d2 arg ->
    Encode.int 110
      <> encodeVariableIndex arg
  B11d3 arg ->
    Encode.int 111
      <> encodeVariableIndex arg

return :: Int -> VariableIndex -> Builder
return dimension variableIndex =
  Encode.int 0 <> Encode.int dimension <> encodeVariableIndex variableIndex
