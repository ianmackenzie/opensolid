module Python.Constant (declaration, definition) where

import OpenSolid.API.Constant (Constant (..))
import OpenSolid.API.Constant qualified as Constant
import OpenSolid.FFI (Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Python qualified
import Python.Class qualified
import Python.FFI qualified
import Python.Type qualified

declaration :: (Name, Constant) -> Text
declaration (name, (Constant @t _ documentation)) = do
  let typeName = Python.Type.qualifiedName (FFI.typeOf t)
  Python.lines
    [ FFI.snakeCase name <> ": " <> typeName
    , Python.docstring documentation
    , ""
    ]

definition :: FFI.ClassName -> (Name, Constant) -> Text
definition className (name, (Constant @t _ _)) = do
  let valueType = FFI.typeOf t
  let ffiFunctionName = Constant.ffiName className name
  let constantName = FFI.snakeCase name
  let pythonClassName = Python.Class.qualifiedName className
  let helperFunctionName = "_" <> Text.toLower pythonClassName <> "_" <> constantName
  let outputName = "output"
  let errorMessageName = "error_message"
  let statusName = "status"
  Python.lines
    [ "def " <> helperFunctionName <> "() -> " <> Python.Type.qualifiedName valueType <> ":"
    , Python.indent
        [ outputName <> " = " <> Python.FFI.dummyValue valueType
        , errorMessageName <> " = " <> Python.FFI.dummyValue FFI.Text
        , Text.sentence
            [ statusName
            , "="
            , Python.FFI.invoke
                ffiFunctionName
                "c_void_p()"
                ("ctypes.byref(" <> outputName <> ")")
                ("ctypes.byref(" <> errorMessageName <> ")")
            ]
        , "if " <> statusName <> " == 0:"
        , "    return " <> Python.FFI.outputValue valueType outputName
        , "else:"
        , "    _error(_text_to_str(" <> errorMessageName <> "))"
        ]
    , pythonClassName <> "." <> constantName <> " = " <> helperFunctionName <> "()"
    ]
