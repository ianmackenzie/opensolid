module OpenSolid.Bytecode.Compilation
  ( Step
  , NumComponents (NumComponents)
  , OutputComponents (OutputComponents)
  , InputComponents (InputComponents)
  , (>>=)
  , return
  , addConstant
  , addConstant1d
  , addConstant2d
  , addVariable
  , addVariable1d
  , addVariable2d
  , compile
  , curve1d
  , curve2d
  )
where

import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Unsafe qualified
import Data.Word (Word16)
import Foreign (Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified
import Foreign.Ptr qualified
import GHC.ByteOrder qualified
import OpenSolid.Binary (Builder, ByteString)
import OpenSolid.Binary qualified as Binary
import OpenSolid.Bytecode.Instruction
  ( ConstantIndex (ConstantIndex)
  , Instruction
  , VariableIndex (VariableIndex)
  )
import OpenSolid.Bytecode.Instruction qualified as Instruction
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Prelude
import OpenSolid.Range (Range (Range))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Prelude (Double)

newtype NumComponents = NumComponents Int deriving (Eq, Ord, Show)

instance Addition NumComponents NumComponents NumComponents where
  NumComponents a + NumComponents b = NumComponents (a + b)

instance Addition NumComponents Int NumComponents where
  NumComponents a + b = NumComponents (a + b)

newtype OutputComponents = OutputComponents Int deriving (Eq, Ord, Show)

newtype InputComponents = InputComponents Int deriving (Eq, Ord, Show)

data State = State
  { constantsBuilder :: Builder
  , constants :: Map (NonEmpty Float) ConstantIndex
  , constantComponents :: NumComponents
  , wordsBuilder :: Builder
  , variables :: Map Instruction VariableIndex
  , variableComponents :: NumComponents
  }

newtype Step a = Step (State -> (State, a))

(>>=) :: Step a -> (a -> Step b) -> Step b
Step step1 >>= f = Step $ \compilation0 -> do
  let (compilation1, result1) = step1 compilation0
  let Step step2 = f result1
  step2 compilation1

nextConstantIndex :: State -> ConstantIndex
nextConstantIndex State{constantComponents = NumComponents n} = ConstantIndex n

encodeDouble :: Double -> Builder
encodeDouble = case GHC.ByteOrder.targetByteOrder of
  GHC.ByteOrder.LittleEndian -> Builder.doubleLE
  GHC.ByteOrder.BigEndian -> Builder.doubleBE

encodeFloat :: Float -> Builder
encodeFloat = encodeDouble . Float.toDouble

return :: a -> Step a
return value = Step (\compilation -> (compilation, value))

addConstant :: NonEmpty Float -> Step ConstantIndex
addConstant components = Step \initialCompilation ->
  case Map.get components (constants initialCompilation) of
    Just constantIndex -> (initialCompilation, constantIndex)
    Nothing -> do
      let constantIndex = nextConstantIndex initialCompilation
      let updatedCompilation =
            initialCompilation
              { constantsBuilder =
                  constantsBuilder initialCompilation
                    <> Binary.collect encodeFloat components
              , constants =
                  constants initialCompilation
                    |> Map.set components constantIndex
              , constantComponents =
                  constantComponents initialCompilation
                    + NonEmpty.length components
              }
      (updatedCompilation, constantIndex)

addConstant1d :: Float -> Step ConstantIndex
addConstant1d value = addConstant (NonEmpty.one value)

addConstant2d :: (Float, Float) -> Step ConstantIndex
addConstant2d (x, y) = addConstant (NonEmpty.two x y)

nextVariableIndex :: State -> VariableIndex
nextVariableIndex State{variableComponents = NumComponents n} = VariableIndex n

addVariable :: Instruction -> OutputComponents -> Step VariableIndex
addVariable instruction (OutputComponents outputComponents) = Step \initialCompilation ->
  case Map.get instruction (variables initialCompilation) of
    Just resultIndex -> (initialCompilation, resultIndex)
    Nothing -> do
      let resultIndex = nextVariableIndex initialCompilation
      let updatedCompilation =
            initialCompilation
              { wordsBuilder =
                  wordsBuilder initialCompilation
                    <> Instruction.encode instruction resultIndex
              , variables =
                  variables initialCompilation
                    |> Map.set instruction resultIndex
              , variableComponents =
                  variableComponents initialCompilation
                    + outputComponents
              }
      (updatedCompilation, resultIndex)

addVariable1d :: Instruction -> Step VariableIndex
addVariable1d instruction = addVariable instruction (OutputComponents 1)

addVariable2d :: Instruction -> Step VariableIndex
addVariable2d instruction = addVariable instruction (OutputComponents 2)

init :: InputComponents -> State
init (InputComponents inputComponents) =
  State
    { constantsBuilder = Binary.empty
    , constants = Map.empty
    , constantComponents = NumComponents 0
    , wordsBuilder = Binary.empty
    , variables = Map.empty
    , variableComponents = NumComponents inputComponents
    }

data Output = Output
  { constantBytes :: ByteString
  , wordBytes :: ByteString
  , numVariableComponents :: Int
  }

compile :: InputComponents -> OutputComponents -> Step VariableIndex -> Output
compile (InputComponents inputComponents) (OutputComponents outputComponents) (Step step) = do
  let (finalState, resultIndex) = step (init (InputComponents inputComponents))
  let constantBytes = Binary.bytes (constantsBuilder finalState)
  let returnInstruction = Instruction.return outputComponents resultIndex
  let wordBytes = Binary.bytes (wordsBuilder finalState <> returnInstruction)
  let NumComponents numVariableComponents = variableComponents finalState
  Output{constantBytes, wordBytes, numVariableComponents}

callWith :: ByteString -> ByteString -> Int -> (Ptr Word16 -> Ptr Double -> Ptr Double -> IO a) -> a
callWith wordBytes constantBytes numReturnValues callback =
  unsafeDupablePerformIO $
    Data.ByteString.Unsafe.unsafeUseAsCString wordBytes \wordBytesPointer ->
      Data.ByteString.Unsafe.unsafeUseAsCString constantBytes \constantBytesPointer ->
        Foreign.Marshal.Alloc.allocaBytes (8 * numReturnValues) \returnValuesPointer -> do
          let wordsPointer = Foreign.Ptr.castPtr wordBytesPointer
          let constantsPointer = Foreign.Ptr.castPtr constantBytesPointer
          callback wordsPointer constantsPointer returnValuesPointer

getReturnValue :: Int -> Ptr Double -> IO Float
getReturnValue index returnValuesPointer =
  IO.map Float.fromDouble (Foreign.peekElemOff returnValuesPointer index)

curve1d :: Step VariableIndex -> (Float -> Float, Range Unitless -> Range Unitless)
curve1d step = do
  let output = compile (InputComponents 1) (OutputComponents 1) step
  let Output{constantBytes, wordBytes, numVariableComponents} = output
  let value tValue =
        callWith wordBytes constantBytes 1 $
          \wordsPointer constantsPointer returnValuePointer -> IO.do
            opensolid_curve1d_value
              wordsPointer
              (Float.toDouble tValue)
              constantsPointer
              numVariableComponents
              returnValuePointer
            getReturnValue 0 returnValuePointer
  let bounds (Range tLower tUpper) =
        callWith wordBytes constantBytes 2 $
          \wordsPointer constantsPointer returnValuesPointer -> IO.do
            opensolid_curve1d_bounds
              wordsPointer
              (Float.toDouble tLower)
              (Float.toDouble tUpper)
              constantsPointer
              numVariableComponents
              returnValuesPointer
            lower <- getReturnValue 0 returnValuesPointer
            upper <- getReturnValue 1 returnValuesPointer
            IO.succeed (Range lower upper)
  (value, bounds)

curve2d ::
  Step VariableIndex ->
  (Float -> (Float, Float), Range Unitless -> (Range Unitless, Range Unitless))
curve2d step = do
  let output = compile (InputComponents 1) (OutputComponents 2) step
  let Output{constantBytes, wordBytes, numVariableComponents} = output
  let value tValue =
        callWith wordBytes constantBytes 2 $
          \wordsPointer constantsPointer returnValuesPointer -> IO.do
            opensolid_curve2d_value
              wordsPointer
              (Float.toDouble tValue)
              constantsPointer
              numVariableComponents
              returnValuesPointer
            x <- getReturnValue 0 returnValuesPointer
            y <- getReturnValue 1 returnValuesPointer
            IO.succeed (x, y)
  let bounds (Range tLower tUpper) =
        callWith wordBytes constantBytes 4 $
          \wordsPointer constantsPointer returnValuesPointer -> IO.do
            opensolid_curve2d_bounds
              wordsPointer
              (Float.toDouble tLower)
              (Float.toDouble tUpper)
              constantsPointer
              numVariableComponents
              returnValuesPointer
            xLower <- getReturnValue 0 returnValuesPointer
            xUpper <- getReturnValue 1 returnValuesPointer
            yLower <- getReturnValue 2 returnValuesPointer
            yUpper <- getReturnValue 3 returnValuesPointer
            IO.succeed (Range xLower xUpper, Range yLower yUpper)
  (value, bounds)

foreign import capi "bytecode.h opensolid_curve1d_value"
  opensolid_curve1d_value ::
    Ptr Word16 -> Double -> Ptr Double -> Int -> Ptr Double -> IO ()

foreign import capi "bytecode.h opensolid_curve1d_bounds"
  opensolid_curve1d_bounds ::
    Ptr Word16 -> Double -> Double -> Ptr Double -> Int -> Ptr Double -> IO ()

foreign import capi "bytecode.h opensolid_curve2d_value"
  opensolid_curve2d_value ::
    Ptr Word16 -> Double -> Ptr Double -> Int -> Ptr Double -> IO ()

foreign import capi "bytecode.h opensolid_curve2d_bounds"
  opensolid_curve2d_bounds ::
    Ptr Word16 -> Double -> Double -> Ptr Double -> Int -> Ptr Double -> IO ()
