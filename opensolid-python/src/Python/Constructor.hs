module Python.Constructor (definition) where

import OpenSolid.API.Constructor (Constructor (..))
import OpenSolid.API.Constructor qualified as Constructor
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Python qualified
import Python.Class qualified
import Python.FFI qualified
import Python.Function qualified

definition :: FFI.ClassName -> Maybe Constructor -> Text
definition className maybeConstructor = case maybeConstructor of
  Nothing -> ""
  Just constructor -> do
    let ffiFunctionName = Constructor.ffiName className constructor
    let selfType = FFI.Class className
    let arguments = Constructor.signature constructor
    let functionArguments = Text.join "," (List.map Python.Function.argument arguments)
    let ffiArguments = List.map (Pair.mapFirst FFI.snakeCase) arguments
    let argumentsName = "arguments"
    let pointerFieldName = Python.Class.pointerFieldName className
    let statusName = "status"
    let errorMessageName = "error_message"
    Python.lines
      [ "def __init__(self, " <> functionArguments <> ") -> None:"
      , Python.indent
          [ Python.docstring (Constructor.documentation constructor)
          , argumentsName <> " = " <> Python.FFI.argumentValue ffiArguments
          , "self." <> pointerFieldName <> " = " <> Python.FFI.dummyValue selfType
          , errorMessageName <> " = " <> Python.FFI.dummyValue FFI.Text
          , Text.sentence
              [ statusName
              , "="
              , Python.FFI.invoke
                  ffiFunctionName
                  ("ctypes.byref(" <> argumentsName <> ")")
                  ("ctypes.byref(self." <> pointerFieldName <> ")")
                  ("ctypes.byref(" <> errorMessageName <> ")")
              ]
          , Python.lines
              [ "if " <> statusName <> " != 0:"
              , "    _error(_text_to_str(" <> errorMessageName <> "))"
              ]
          ]
      ]
