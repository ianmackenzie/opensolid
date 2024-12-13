module Python.NegationFunction (definition) where

import API.NegationFunction qualified as NegationFunction
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
    let valueType = NegationFunction.valueType function
    let valueTypeName = Python.Type.qualifiedName valueType
    let ffiFunctionName = NegationFunction.ffiName classId
    Python.lines
      [ "def __neg__(self) -> " + valueTypeName + ":"
      , Python.indent
          [ "output = " + Python.FFI.dummyValue valueType
          , Python.FFI.invoke ffiFunctionName "ctypes.byref(self._ptr)" "ctypes.byref(output)"
          , "return " + Python.FFI.outputValue valueType "output"
          ]
      ]
