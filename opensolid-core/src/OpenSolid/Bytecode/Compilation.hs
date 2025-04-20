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
  , addConstant3d
  , addVariable
  , addVariable1d
  , addVariable2d
  , addVariable3d
  , compile
  , curve1d
  , curve2d
  , curve3d
  , surface1d
  , surface2d
  , surface3d
  , debugCurve
  , debugSurface
  )
where

import Data.ByteString.Unsafe qualified
import Foreign (Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified
import GHC.Foreign (CString)
import OpenSolid.Binary (Builder, ByteString)
import OpenSolid.Binary qualified as Binary
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bytecode.Encode qualified as Encode
import OpenSolid.Bytecode.Instruction
  ( ConstantIndex (ConstantIndex)
  , Instruction
  , VariableIndex (VariableIndex)
  )
import OpenSolid.Bytecode.Instruction qualified as Instruction
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.List qualified as List
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude
import OpenSolid.Range (Range (Range))
import OpenSolid.SurfaceParameter (UvBounds, UvPoint)
import OpenSolid.Text qualified as Text
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector3d (Vector3d (Vector3d))
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds3d (VectorBounds3d (VectorBounds3d))
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
  , variablesBuilder :: Builder
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
                    <> Binary.collect Encode.float components
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

addConstant2d :: Vector2d (space @ Unitless) -> Step ConstantIndex
addConstant2d (Vector2d x y) = addConstant (NonEmpty.two x y)

addConstant3d :: Vector3d (space @ Unitless) -> Step ConstantIndex
addConstant3d (Vector3d x y z) = addConstant (NonEmpty.three x y z)

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
              { variablesBuilder =
                  variablesBuilder initialCompilation
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

addVariable3d :: Instruction -> Step VariableIndex
addVariable3d instruction = addVariable instruction (OutputComponents 3)

init :: InputComponents -> State
init (InputComponents inputComponents) =
  State
    { constantsBuilder = Binary.empty
    , constants = Map.empty
    , constantComponents = NumComponents 0
    , variablesBuilder = Binary.empty
    , variables = Map.empty
    , variableComponents = NumComponents inputComponents
    }

compile :: InputComponents -> OutputComponents -> Step VariableIndex -> ByteString
compile (InputComponents inputComponents) (OutputComponents outputComponents) (Step step) = do
  let (finalState, resultIndex) = step (init (InputComponents inputComponents))
  let NumComponents numConstantComponents = constantComponents finalState
  let NumComponents numVariableComponents = variableComponents finalState
  Binary.bytes $
    Binary.concat
      [ Encode.int numConstantComponents
      , Encode.int numVariableComponents
      , Encode.int 0
      , Encode.int 0
      , constantsBuilder finalState
      , variablesBuilder finalState
      , Instruction.return outputComponents resultIndex
      ]

debug :: InputComponents -> Step VariableIndex -> Text
debug (InputComponents inputComponents) (Step step) = do
  let (finalState, _) = step (init (InputComponents inputComponents))
  debugText finalState

debugText :: State -> Text
debugText State{variables} =
  Map.toList variables
    |> List.sortBy Pair.second
    |> List.map showInstruction
    |> Text.multiline

showInstruction :: (Instruction, VariableIndex) -> Text
showInstruction (instruction, variableIndex) =
  Text.show variableIndex <> " = " <> Text.show instruction

debugCurve :: Step VariableIndex -> Text
debugCurve = debug (InputComponents 1)

debugSurface :: Step VariableIndex -> Text
debugSurface = debug (InputComponents 2)

callWith :: ByteString -> Int -> (CString -> Ptr Double -> IO a) -> a
callWith functionBytes numReturnValues callback =
  unsafeDupablePerformIO $
    Data.ByteString.Unsafe.unsafeUseAsCString functionBytes \functionPointer ->
      Foreign.Marshal.Alloc.allocaBytes (8 * numReturnValues) \returnValuesPointer -> do
        callback functionPointer returnValuesPointer

getReturnValue :: Int -> Ptr Double -> IO Float
getReturnValue index returnValuesPointer =
  IO.map Float.fromDouble (Foreign.peekElemOff returnValuesPointer index)

curve1d ::
  Step VariableIndex ->
  ( Float -> Float
  , Range Unitless -> Range Unitless
  )
curve1d step = do
  let output = compile (InputComponents 1) (OutputComponents 1) step
  (curve1dValue output, curve1dBounds output)

curve1dValue :: ByteString -> Float -> Float
curve1dValue functionBytes tValue =
  callWith functionBytes 1 $
    \functionPointer returnValuePointer -> IO.do
      opensolid_curve_value
        functionPointer
        (Float.toDouble tValue)
        returnValuePointer
      getReturnValue 0 returnValuePointer

curve1dBounds :: ByteString -> Range Unitless -> Range Unitless
curve1dBounds functionBytes (Range tLower tUpper) =
  callWith functionBytes 2 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_curve_bounds
        functionPointer
        (Float.toDouble tLower)
        (Float.toDouble tUpper)
        returnValuesPointer
      lower <- getReturnValue 0 returnValuesPointer
      upper <- getReturnValue 1 returnValuesPointer
      IO.succeed (Range lower upper)

curve2d ::
  Step VariableIndex ->
  ( Float -> Vector2d (space @ Unitless)
  , Range Unitless -> VectorBounds2d (space @ Unitless)
  )
curve2d step = do
  let output = compile (InputComponents 1) (OutputComponents 2) step
  (curve2dValue output, curve2dBounds output)

curve2dValue :: ByteString -> Float -> Vector2d (space @ Unitless)
curve2dValue functionBytes tValue =
  callWith functionBytes 2 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_curve_value
        functionPointer
        (Float.toDouble tValue)
        returnValuesPointer
      x <- getReturnValue 0 returnValuesPointer
      y <- getReturnValue 1 returnValuesPointer
      IO.succeed (Vector2d x y)

curve2dBounds :: ByteString -> Range Unitless -> VectorBounds2d (space @ Unitless)
curve2dBounds functionBytes (Range tLower tUpper) =
  callWith functionBytes 4 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_curve_bounds
        functionPointer
        (Float.toDouble tLower)
        (Float.toDouble tUpper)
        returnValuesPointer
      xLower <- getReturnValue 0 returnValuesPointer
      xUpper <- getReturnValue 1 returnValuesPointer
      yLower <- getReturnValue 2 returnValuesPointer
      yUpper <- getReturnValue 3 returnValuesPointer
      IO.succeed (VectorBounds2d (Range xLower xUpper) (Range yLower yUpper))

curve3d ::
  Step VariableIndex ->
  ( Float -> Vector3d (space @ Unitless)
  , Range Unitless -> VectorBounds3d (space @ Unitless)
  )
curve3d step = do
  let output = compile (InputComponents 1) (OutputComponents 3) step
  (curve3dValue output, curve3dBounds output)

curve3dValue :: ByteString -> Float -> Vector3d (space @ Unitless)
curve3dValue functionBytes tValue =
  callWith functionBytes 3 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_curve_value
        functionPointer
        (Float.toDouble tValue)
        returnValuesPointer
      x <- getReturnValue 0 returnValuesPointer
      y <- getReturnValue 1 returnValuesPointer
      z <- getReturnValue 2 returnValuesPointer
      IO.succeed (Vector3d x y z)

curve3dBounds :: ByteString -> Range Unitless -> VectorBounds3d (space @ Unitless)
curve3dBounds functionBytes (Range tLower tUpper) =
  callWith functionBytes 6 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_curve_bounds
        functionPointer
        (Float.toDouble tLower)
        (Float.toDouble tUpper)
        returnValuesPointer
      xLower <- getReturnValue 0 returnValuesPointer
      xUpper <- getReturnValue 1 returnValuesPointer
      yLower <- getReturnValue 2 returnValuesPointer
      yUpper <- getReturnValue 3 returnValuesPointer
      zLower <- getReturnValue 4 returnValuesPointer
      zUpper <- getReturnValue 5 returnValuesPointer
      IO.succeed (VectorBounds3d (Range xLower xUpper) (Range yLower yUpper) (Range zLower zUpper))

surface1d ::
  Step VariableIndex ->
  ( UvPoint -> Float
  , UvBounds -> Range Unitless
  )
surface1d step = do
  let output = compile (InputComponents 2) (OutputComponents 1) step
  (surface1dValue output, surface1dBounds output)

surface1dValue :: ByteString -> UvPoint -> Float
surface1dValue functionBytes (Point2d uValue vValue) =
  callWith functionBytes 1 $
    \functionPointer returnValuePointer -> IO.do
      opensolid_surface_value
        functionPointer
        (Float.toDouble uValue)
        (Float.toDouble vValue)
        returnValuePointer
      getReturnValue 0 returnValuePointer

surface1dBounds :: ByteString -> UvBounds -> Range Unitless
surface1dBounds functionBytes (Bounds2d (Range uLower uUpper) (Range vLower vUpper)) =
  callWith functionBytes 2 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_surface_bounds
        functionPointer
        (Float.toDouble uLower)
        (Float.toDouble uUpper)
        (Float.toDouble vLower)
        (Float.toDouble vUpper)
        returnValuesPointer
      lower <- getReturnValue 0 returnValuesPointer
      upper <- getReturnValue 1 returnValuesPointer
      IO.succeed (Range lower upper)

surface2d ::
  Step VariableIndex ->
  ( UvPoint -> Vector2d (space @ Unitless)
  , UvBounds -> VectorBounds2d (space @ Unitless)
  )
surface2d step = do
  let output = compile (InputComponents 2) (OutputComponents 2) step
  (surface2dValue output, surface2dBounds output)

surface2dValue :: ByteString -> UvPoint -> Vector2d (space @ Unitless)
surface2dValue functionBytes (Point2d uValue vValue) =
  callWith functionBytes 2 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_surface_value
        functionPointer
        (Float.toDouble uValue)
        (Float.toDouble vValue)
        returnValuesPointer
      x <- getReturnValue 0 returnValuesPointer
      y <- getReturnValue 1 returnValuesPointer
      IO.succeed (Vector2d x y)

surface2dBounds :: ByteString -> UvBounds -> VectorBounds2d (space @ Unitless)
surface2dBounds functionBytes (Bounds2d (Range uLower uUpper) (Range vLower vUpper)) =
  callWith functionBytes 4 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_surface_bounds
        functionPointer
        (Float.toDouble uLower)
        (Float.toDouble uUpper)
        (Float.toDouble vLower)
        (Float.toDouble vUpper)
        returnValuesPointer
      xLower <- getReturnValue 0 returnValuesPointer
      xUpper <- getReturnValue 1 returnValuesPointer
      yLower <- getReturnValue 2 returnValuesPointer
      yUpper <- getReturnValue 3 returnValuesPointer
      IO.succeed (VectorBounds2d (Range xLower xUpper) (Range yLower yUpper))

surface3d ::
  Step VariableIndex ->
  ( UvPoint -> Vector3d (space @ Unitless)
  , UvBounds -> VectorBounds3d (space @ Unitless)
  )
surface3d step = do
  let output = compile (InputComponents 2) (OutputComponents 3) step
  (surface3dValue output, surface3dBounds output)

surface3dValue :: ByteString -> UvPoint -> Vector3d (space @ Unitless)
surface3dValue functionBytes (Point2d uValue vValue) =
  callWith functionBytes 3 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_surface_value
        functionPointer
        (Float.toDouble uValue)
        (Float.toDouble vValue)
        returnValuesPointer
      x <- getReturnValue 0 returnValuesPointer
      y <- getReturnValue 1 returnValuesPointer
      z <- getReturnValue 2 returnValuesPointer
      IO.succeed (Vector3d x y z)

surface3dBounds :: ByteString -> UvBounds -> VectorBounds3d (space @ Unitless)
surface3dBounds functionBytes (Bounds2d (Range uLower uUpper) (Range vLower vUpper)) =
  callWith functionBytes 6 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_surface_bounds
        functionPointer
        (Float.toDouble uLower)
        (Float.toDouble uUpper)
        (Float.toDouble vLower)
        (Float.toDouble vUpper)
        returnValuesPointer
      xLower <- getReturnValue 0 returnValuesPointer
      xUpper <- getReturnValue 1 returnValuesPointer
      yLower <- getReturnValue 2 returnValuesPointer
      yUpper <- getReturnValue 3 returnValuesPointer
      zLower <- getReturnValue 4 returnValuesPointer
      zUpper <- getReturnValue 5 returnValuesPointer
      IO.succeed (VectorBounds3d (Range xLower xUpper) (Range yLower yUpper) (Range zLower zUpper))

foreign import capi "bytecode.h opensolid_curve_value"
  opensolid_curve_value ::
    CString -> Double -> Ptr Double -> IO ()

foreign import capi "bytecode.h opensolid_curve_bounds"
  opensolid_curve_bounds ::
    CString -> Double -> Double -> Ptr Double -> IO ()

foreign import capi "bytecode.h opensolid_surface_value"
  opensolid_surface_value ::
    CString -> Double -> Double -> Ptr Double -> IO ()

foreign import capi "bytecode.h opensolid_surface_bounds"
  opensolid_surface_bounds ::
    CString -> Double -> Double -> Double -> Double -> Ptr Double -> IO ()
