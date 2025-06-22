module Python.Constructor (definition) where

import OpenSolid.API.Constructor (Constructor (..))
import OpenSolid.API.Constructor qualified as Constructor
import OpenSolid.API.Upcast (Upcast)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Python qualified
import Python.Class qualified
import Python.FFI qualified
import Python.Function qualified
import Python.Upcast qualified

definition :: FFI.ClassName -> Maybe Constructor -> Maybe Upcast -> Text
definition className maybeConstructor maybeUpcast = case maybeConstructor of
  Nothing -> ""
  Just constructor -> do
    let ffiFunctionName = Constructor.ffiName className constructor
    let selfType = FFI.Class className
    let arguments = Constructor.signature constructor
    let functionArguments = Text.join "," (List.map Python.Function.argument arguments)
    let ffiArguments = List.map (Pair.mapFirst FFI.snakeCase) arguments
    let pointerFieldName = Python.Class.pointerFieldName className
    Python.lines
      [ "def __init__(self, " <> functionArguments <> ") -> None:"
      , Python.indent
          [ Python.docstring (Constructor.documentation constructor)
          , "inputs = " <> Python.FFI.argumentValue ffiArguments
          , "self." <> pointerFieldName <> " = " <> Python.FFI.dummyValue selfType
          , Python.FFI.invoke ffiFunctionName
              @ "ctypes.byref(inputs)"
              @ "ctypes.byref(self." <> pointerFieldName <> ")"
          , Python.Upcast.lines className "self" maybeUpcast
          ]
      ]
