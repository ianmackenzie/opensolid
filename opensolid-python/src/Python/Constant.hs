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

definition :: FFI.Id a -> (Name, Constant) -> Text
definition classId (name, (Constant value _)) = do
  let valueType = Constant.valueType value
  let ffiFunctionName = Constant.ffiName classId name
  let constantName = FFI.snakeCase name
  let className = Python.Class.qualifiedName classId
  let helperFunctionName = "_" <> Text.toLower className <> "_" <> constantName
  Python.lines
    [ "def " <> helperFunctionName <> "() -> " <> Python.Type.qualifiedName valueType <> ":"
    , Python.indent
        [ "output = " <> Python.FFI.dummyValue valueType
        , Python.FFI.invoke ffiFunctionName "c_void_p()" "ctypes.byref(output)"
        , "return " <> Python.FFI.outputValue valueType "output"
        ]
    , className <> "." <> constantName <> " = " <> helperFunctionName <> "()"
    ]
