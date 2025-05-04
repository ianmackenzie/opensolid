module Python.Constructor (definition) where

import API.Constructor (Constructor (..))
import API.Constructor qualified as Constructor
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Python qualified
import Python.Class qualified
import Python.FFI qualified
import Python.Function qualified

definition :: FFI.Class -> Maybe Constructor -> Text
definition ffiClass maybeConstructor = case maybeConstructor of
  Nothing -> ""
  Just constructor -> do
    let ffiFunctionName = Constructor.ffiName ffiClass constructor
    let selfType = FFI.Class ffiClass
    let arguments = Constructor.signature constructor
    let functionArguments = Text.join "," (List.map Python.Function.argument arguments)
    let ffiArguments = List.map (Pair.mapFirst FFI.snakeCase) arguments
    let pointerFieldName = Python.Class.pointerFieldName ffiClass
    Python.lines
      [ "def __init__(self, " <> functionArguments <> ") -> None:"
      , Python.indent
          [ Python.docstring (Constructor.documentation constructor)
          , "inputs = " <> Python.FFI.argumentValue ffiArguments
          , "self." <> pointerFieldName <> " = " <> Python.FFI.dummyValue selfType
          , Python.FFI.invoke ffiFunctionName
              # "ctypes.byref(inputs)"
              # "ctypes.byref(self." <> pointerFieldName <> ")"
          ]
      ]
