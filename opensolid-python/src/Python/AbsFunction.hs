module Python.AbsFunction (definition) where

import API.AbsFunction qualified as AbsFunction
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import Python qualified
import Python.Function qualified
import Python.Type qualified

definition :: forall value. FFI value => FFI.Id value -> Maybe (value -> value) -> Text
definition classId maybeFunction = case maybeFunction of
  Nothing -> ""
  Just function -> do
    let valueType = AbsFunction.valueType function
    let valueTypeName = Python.Type.qualifiedName valueType
    let ffiFunctionName = AbsFunction.ffiName classId
    Python.lines
      [ "def __abs__(self) -> " + valueTypeName + ":"
      , Python.indent
          [ Python.docstring "Return ``abs(self)``."
          , Python.Function.body ffiFunctionName [("self", valueType)] valueType
          ]
      ]
