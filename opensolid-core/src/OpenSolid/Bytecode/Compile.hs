module OpenSolid.Bytecode.Compile
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
  , surface1d
  , curve1d
  , curve2d
  , curve3d
  , surface2d
  , surface3d
  , debugCurve
  , debugSurface
  )
where

import OpenSolid.Binary (Builder, ByteString)
import OpenSolid.Binary qualified as Binary
import OpenSolid.Bytecode.Encode qualified as Encode
import OpenSolid.Bytecode.Instruction
  ( ConstantIndex (ConstantIndex)
  , Instruction
  , VariableIndex (VariableIndex)
  )
import OpenSolid.Bytecode.Instruction qualified as Instruction
import OpenSolid.List qualified as List
import OpenSolid.Map (Map)
import OpenSolid.Map qualified as Map
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude hiding (return)
import OpenSolid.Primitives (Vector3d (Vector3d))
import OpenSolid.Text qualified as Text
import OpenSolid.Vector2d (Vector2d (Vector2d))

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

newtype Step a = Step (State -> ("state" ::: State, "output" ::: a))

apply :: Step a -> State -> ("state" ::: State, "output" ::: a)
apply (Step step) state = step state

(>>=) :: Step a -> (a -> Step b) -> Step b
step1 >>= f = Step $ \state0 -> do
  let stepped1 = apply step1 state0
  let step2 = f stepped1.output
  apply step2 stepped1.state

nextConstantIndex :: State -> ConstantIndex
nextConstantIndex State{constantComponents = NumComponents n} = ConstantIndex n

return :: a -> Step a
return value = Step (\compilation -> (#state compilation, #output value))

addConstant :: NonEmpty Float -> Step ConstantIndex
addConstant components = Step \initialState ->
  case Map.get components initialState.constants of
    Just constantIndex -> (#state initialState, #output constantIndex)
    Nothing -> do
      let constantIndex = nextConstantIndex initialState
      let updatedCompilation =
            initialState
              { constantsBuilder =
                  initialState.constantsBuilder
                    <> Binary.collect Encode.float components
              , constants =
                  initialState.constants
                    |> Map.set components constantIndex
              , constantComponents =
                  initialState.constantComponents
                    + NonEmpty.length components
              }
      (#state updatedCompilation, #output constantIndex)

addConstant1d :: Float -> Step ConstantIndex
addConstant1d value = addConstant (NonEmpty.one value)

addConstant2d :: Vector2d (space @ Unitless) -> Step ConstantIndex
addConstant2d (Vector2d x y) = addConstant (NonEmpty.two x y)

addConstant3d :: Vector3d (space @ Unitless) -> Step ConstantIndex
addConstant3d (Vector3d x y z) = addConstant (NonEmpty.three x y z)

nextVariableIndex :: State -> VariableIndex
nextVariableIndex State{variableComponents = NumComponents n} = VariableIndex n

addVariable :: Instruction -> OutputComponents -> Step VariableIndex
addVariable instruction (OutputComponents outputComponents) = Step \initialState ->
  case Map.get instruction initialState.variables of
    Just resultIndex -> (#state initialState, #output resultIndex)
    Nothing -> do
      let resultIndex = nextVariableIndex initialState
      let updatedState =
            initialState
              { variablesBuilder =
                  initialState.variablesBuilder
                    <> Instruction.encode instruction resultIndex
              , variables =
                  initialState.variables
                    |> Map.set instruction resultIndex
              , variableComponents =
                  initialState.variableComponents
                    + outputComponents
              }
      (#state updatedState, #output resultIndex)

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
  let final = step (init (InputComponents inputComponents))
  let NumComponents numConstantComponents = final.state.constantComponents
  let NumComponents numVariableComponents = final.state.variableComponents
  Binary.bytes $
    Binary.concat
      [ Encode.int numConstantComponents
      , Encode.int numVariableComponents
      , Encode.int 0
      , Encode.int 0
      , final.state.constantsBuilder
      , final.state.variablesBuilder
      , Instruction.return outputComponents final.output
      ]

curve1d :: Step VariableIndex -> ByteString
curve1d = compile (InputComponents 1) (OutputComponents 1)

curve2d :: Step VariableIndex -> ByteString
curve2d = compile (InputComponents 1) (OutputComponents 2)

curve3d :: Step VariableIndex -> ByteString
curve3d = compile (InputComponents 1) (OutputComponents 3)

surface1d :: Step VariableIndex -> ByteString
surface1d = compile (InputComponents 2) (OutputComponents 1)

surface2d :: Step VariableIndex -> ByteString
surface2d = compile (InputComponents 2) (OutputComponents 2)

surface3d :: Step VariableIndex -> ByteString
surface3d = compile (InputComponents 2) (OutputComponents 3)

debug :: InputComponents -> Step VariableIndex -> Text
debug (InputComponents inputComponents) step = do
  let initialState = init (InputComponents inputComponents)
  let finalState = (apply step initialState).state
  Map.toList finalState.variables
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
