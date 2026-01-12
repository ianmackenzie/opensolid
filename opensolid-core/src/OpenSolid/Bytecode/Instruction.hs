module OpenSolid.Bytecode.Instruction
  ( Instruction (..)
  , VariableIndex (VariableIndex)
  , ConstantIndex (ConstantIndex)
  , encode
  , return
  )
where

import OpenSolid.Binary (Builder)
import OpenSolid.Bytecode.Encode qualified as Encode
import OpenSolid.Prelude

newtype ConstantIndex = ConstantIndex Int deriving (Eq, Ord, Show)

newtype VariableIndex = VariableIndex Int deriving (Eq, Ord, Show)

data Instruction
  = Component0 VariableIndex
  | Component1 VariableIndex
  | Component2 VariableIndex
  | Negate1D VariableIndex
  | Add1D VariableIndex VariableIndex
  | AddVariableConstant1D VariableIndex ConstantIndex
  | Subtract1D VariableIndex VariableIndex
  | SubtractConstantVariable1D ConstantIndex VariableIndex
  | Multiply1D VariableIndex VariableIndex
  | MultiplyVariableConstant1D VariableIndex ConstantIndex
  | Divide1D VariableIndex VariableIndex
  | DivideConstantVariable1D ConstantIndex VariableIndex
  | Square1D VariableIndex
  | Sqrt1D VariableIndex
  | Sin1D VariableIndex
  | Cos1D VariableIndex
  | Bezier1D Int ConstantIndex VariableIndex
  | XY VariableIndex VariableIndex
  | XC VariableIndex ConstantIndex
  | CY ConstantIndex VariableIndex
  | Negate2D VariableIndex
  | Add2D VariableIndex VariableIndex
  | AddVariableConstant2D VariableIndex ConstantIndex
  | Subtract2D VariableIndex VariableIndex
  | SubtractConstantVariable2D ConstantIndex VariableIndex
  | Multiply2D VariableIndex VariableIndex
  | MultiplyVariableConstant2D VariableIndex ConstantIndex
  | MultiplyConstantVariable2D ConstantIndex VariableIndex
  | Divide2D VariableIndex VariableIndex
  | DivideConstantVariable2D ConstantIndex VariableIndex
  | SquaredMagnitude2D VariableIndex
  | Magnitude2D VariableIndex
  | Dot2D VariableIndex VariableIndex
  | DotVariableConstant2D VariableIndex ConstantIndex
  | Cross2D VariableIndex VariableIndex
  | CrossVariableConstant2D VariableIndex ConstantIndex
  | Bezier2D Int ConstantIndex VariableIndex
  | TransformVector2D ConstantIndex VariableIndex
  | TransformPoint2D ConstantIndex VariableIndex
  | ProjectVector3D ConstantIndex VariableIndex
  | ProjectPoint3D ConstantIndex VariableIndex
  | RFU VariableIndex VariableIndex VariableIndex
  | RFC VariableIndex VariableIndex ConstantIndex
  | RCU VariableIndex ConstantIndex VariableIndex
  | CFU ConstantIndex VariableIndex VariableIndex
  | RCC VariableIndex ConstantIndex ConstantIndex
  | CFC ConstantIndex VariableIndex ConstantIndex
  | CCU ConstantIndex ConstantIndex VariableIndex
  | Negate3D VariableIndex
  | Add3D VariableIndex VariableIndex
  | AddVariableConstant3D VariableIndex ConstantIndex
  | Subtract3D VariableIndex VariableIndex
  | SubtractConstantVariable3D ConstantIndex VariableIndex
  | Multiply3D VariableIndex VariableIndex
  | MultiplyVariableConstant3D VariableIndex ConstantIndex
  | MultiplyConstantVariable3D ConstantIndex VariableIndex
  | Divide3D VariableIndex VariableIndex
  | DivideConstantVariable3D ConstantIndex VariableIndex
  | SquaredMagnitude3D VariableIndex
  | Magnitude3D VariableIndex
  | Dot3D VariableIndex VariableIndex
  | DotVariableConstant3D VariableIndex ConstantIndex
  | Cross3D VariableIndex VariableIndex
  | CrossVariableConstant3D VariableIndex ConstantIndex
  | Bezier3D Int ConstantIndex VariableIndex
  | TransformVector3D ConstantIndex VariableIndex
  | TransformPoint3D ConstantIndex VariableIndex
  | PlaceVector2D ConstantIndex VariableIndex
  | PlacePoint2D ConstantIndex VariableIndex
  | Desingularized1D VariableIndex VariableIndex VariableIndex VariableIndex
  | Desingularized2D VariableIndex VariableIndex VariableIndex VariableIndex
  | Desingularized3D VariableIndex VariableIndex VariableIndex VariableIndex
  | Cube1D VariableIndex
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

newtype TooManyVariables = TooManyVariables Int deriving (Show)

deriving anyclass instance Exception TooManyVariables

newtype TooManyConstants = TooManyConstants Int deriving (Show)

deriving anyclass instance Exception TooManyConstants

encodeVariableIndex :: VariableIndex -> Builder
encodeVariableIndex (VariableIndex index)
  | index < maxValues = Encode.int index
  | otherwise = throw (TooManyVariables (index + 1))

encodeConstantIndex :: ConstantIndex -> Builder
encodeConstantIndex (ConstantIndex index)
  | index < maxValues = Encode.int index
  | otherwise = throw (TooManyConstants (index + 1))

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
  Negate1D arg ->
    Encode.int 4
      <> encodeVariableIndex arg
  Add1D lhs rhs ->
    Encode.int 5
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  AddVariableConstant1D lhs rhs ->
    Encode.int 6
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Subtract1D lhs rhs ->
    Encode.int 7
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  SubtractConstantVariable1D lhs rhs ->
    Encode.int 8
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Multiply1D lhs rhs ->
    Encode.int 9
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  MultiplyVariableConstant1D lhs rhs ->
    Encode.int 10
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Divide1D lhs rhs ->
    Encode.int 11
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DivideConstantVariable1D lhs rhs ->
    Encode.int 12
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Square1D arg ->
    Encode.int 13
      <> encodeVariableIndex arg
  Sqrt1D arg ->
    Encode.int 14
      <> encodeVariableIndex arg
  Sin1D arg ->
    Encode.int 15
      <> encodeVariableIndex arg
  Cos1D arg ->
    Encode.int 16
      <> encodeVariableIndex arg
  Bezier1D 2 controlPoints parameter ->
    Encode.int 17
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1D 3 controlPoints parameter ->
    Encode.int 18
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1D 4 controlPoints parameter ->
    Encode.int 19
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1D 5 controlPoints parameter ->
    Encode.int 20
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1D 6 controlPoints parameter ->
    Encode.int 21
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier1D n controlPoints parameter ->
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
  Negate2D arg ->
    Encode.int 26
      <> encodeVariableIndex arg
  Add2D lhs rhs ->
    Encode.int 27
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  AddVariableConstant2D lhs rhs ->
    Encode.int 28
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Subtract2D lhs rhs ->
    Encode.int 29
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  SubtractConstantVariable2D lhs rhs ->
    Encode.int 30
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Multiply2D lhs rhs ->
    Encode.int 31
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  MultiplyVariableConstant2D lhs rhs ->
    Encode.int 32
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  MultiplyConstantVariable2D lhs rhs ->
    Encode.int 33
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Divide2D lhs rhs ->
    Encode.int 34
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DivideConstantVariable2D lhs rhs ->
    Encode.int 35
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  SquaredMagnitude2D arg ->
    Encode.int 36
      <> encodeVariableIndex arg
  Magnitude2D arg ->
    Encode.int 37
      <> encodeVariableIndex arg
  Dot2D lhs rhs ->
    Encode.int 38
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DotVariableConstant2D lhs rhs ->
    Encode.int 39
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Cross2D lhs rhs ->
    Encode.int 40
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  CrossVariableConstant2D lhs rhs ->
    Encode.int 41
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Bezier2D 2 controlPoints parameter ->
    Encode.int 42
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2D 3 controlPoints parameter ->
    Encode.int 43
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2D 4 controlPoints parameter ->
    Encode.int 44
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2D 5 controlPoints parameter ->
    Encode.int 45
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2D 6 controlPoints parameter ->
    Encode.int 46
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier2D n controlPoints parameter ->
    Encode.int 47
      <> Encode.int n
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  TransformVector2D matrix vector ->
    Encode.int 48
      <> encodeConstantIndex matrix
      <> encodeVariableIndex vector
  TransformPoint2D matrix point ->
    Encode.int 49
      <> encodeConstantIndex matrix
      <> encodeVariableIndex point
  ProjectVector3D matrix vector ->
    Encode.int 50
      <> encodeConstantIndex matrix
      <> encodeVariableIndex vector
  ProjectPoint3D matrix point ->
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
  Negate3D arg ->
    Encode.int 59
      <> encodeVariableIndex arg
  Add3D lhs rhs ->
    Encode.int 60
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  AddVariableConstant3D lhs rhs ->
    Encode.int 61
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Subtract3D lhs rhs ->
    Encode.int 62
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  SubtractConstantVariable3D lhs rhs ->
    Encode.int 63
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Multiply3D lhs rhs ->
    Encode.int 64
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  MultiplyVariableConstant3D lhs rhs ->
    Encode.int 65
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  MultiplyConstantVariable3D lhs rhs ->
    Encode.int 66
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  Divide3D lhs rhs ->
    Encode.int 67
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DivideConstantVariable3D lhs rhs ->
    Encode.int 68
      <> encodeConstantIndex lhs
      <> encodeVariableIndex rhs
  SquaredMagnitude3D arg ->
    Encode.int 69
      <> encodeVariableIndex arg
  Magnitude3D arg ->
    Encode.int 70
      <> encodeVariableIndex arg
  Dot3D lhs rhs ->
    Encode.int 71
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  DotVariableConstant3D lhs rhs ->
    Encode.int 72
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Cross3D lhs rhs ->
    Encode.int 73
      <> encodeVariableIndex lhs
      <> encodeVariableIndex rhs
  CrossVariableConstant3D lhs rhs ->
    Encode.int 74
      <> encodeVariableIndex lhs
      <> encodeConstantIndex rhs
  Bezier3D 2 controlPoints parameter ->
    Encode.int 75
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3D 3 controlPoints parameter ->
    Encode.int 76
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3D 4 controlPoints parameter ->
    Encode.int 77
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3D 5 controlPoints parameter ->
    Encode.int 78
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3D 6 controlPoints parameter ->
    Encode.int 79
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  Bezier3D n controlPoints parameter ->
    Encode.int 80
      <> Encode.int n
      <> encodeConstantIndex controlPoints
      <> encodeVariableIndex parameter
  TransformVector3D matrix vector ->
    Encode.int 81
      <> encodeConstantIndex matrix
      <> encodeVariableIndex vector
  TransformPoint3D matrix point ->
    Encode.int 82
      <> encodeConstantIndex matrix
      <> encodeVariableIndex point
  PlaceVector2D matrix vector ->
    Encode.int 83
      <> encodeConstantIndex matrix
      <> encodeVariableIndex vector
  PlacePoint2D matrix point ->
    Encode.int 84
      <> encodeConstantIndex matrix
      <> encodeVariableIndex point
  Desingularized1D parameterValue left middle right ->
    Encode.int 85
      <> encodeVariableIndex parameterValue
      <> encodeVariableIndex left
      <> encodeVariableIndex middle
      <> encodeVariableIndex right
  Desingularized2D parameterValue left middle right ->
    Encode.int 86
      <> encodeVariableIndex parameterValue
      <> encodeVariableIndex left
      <> encodeVariableIndex middle
      <> encodeVariableIndex right
  Desingularized3D parameterValue left middle right ->
    Encode.int 87
      <> encodeVariableIndex parameterValue
      <> encodeVariableIndex left
      <> encodeVariableIndex middle
      <> encodeVariableIndex right
  Cube1D arg ->
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
