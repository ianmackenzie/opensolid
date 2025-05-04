module Python.Constant (declaration, definition) where

import API.Constant (Constant (..))
import API.Constant qualified as Constant
import OpenSolid.FFI (Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Python qualified
import Python.Class qualified
import Python.FFI qualified
import Python.Type qualified

declaration :: (Name, Constant) -> Text
declaration (name, (Constant value documentation)) = do
  let typeName = Python.Type.qualifiedName (Constant.valueType value)
  Python.lines
    [ FFI.snakeCase name <> ": " <> typeName <> " = None # type: ignore[assignment]"
    , Python.docstring documentation
    , ""
    ]

definition :: FFI.ClassName -> (Name, Constant) -> Text
definition className (name, (Constant value _)) = do
  let valueType = Constant.valueType value
  let ffiFunctionName = Constant.ffiName className name
  let constantName = FFI.snakeCase name
  let pythonClassName = Python.Class.qualifiedName className
  let helperFunctionName = "_" <> Text.toLower pythonClassName <> "_" <> constantName
  Python.lines
    [ "def " <> helperFunctionName <> "() -> " <> Python.Type.qualifiedName valueType <> ":"
    , Python.indent
        [ "output = " <> Python.FFI.dummyValue valueType
        , Python.FFI.invoke ffiFunctionName "c_void_p()" "ctypes.byref(output)"
        , "return " <> Python.FFI.outputValue valueType "output"
        ]
    , pythonClassName <> "." <> constantName <> " = " <> helperFunctionName <> "()"
    ]
