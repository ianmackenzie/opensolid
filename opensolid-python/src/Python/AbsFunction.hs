module Python.AbsFunction (definition) where

import API.AbsFunction qualified as AbsFunction
import OpenSolid
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Python qualified
import Python.FFI qualified
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
          , "output = " + Python.FFI.dummyValue valueType
          , Python.FFI.invoke ffiFunctionName "ctypes.byref(self._ptr)" "ctypes.byref(output)"
          , "return " + Python.FFI.outputValue valueType "output"
          ]
      ]
