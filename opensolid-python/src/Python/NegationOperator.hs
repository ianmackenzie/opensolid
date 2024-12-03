module Python.NegationOperator (definition) where

import OpenSolid
import OpenSolid.API.NegationOperator qualified as NegationOperator
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Python qualified
import Python.Class qualified
import Python.FFI qualified

definition :: forall value. FFI value => FFI.Id value -> Maybe (value -> value) -> Text
definition classId maybeFunction = case maybeFunction of
  Nothing -> ""
  Just function -> do
    let valueType = NegationOperator.valueType function
    let ffiFunctionName = NegationOperator.ffiName classId
    Python.lines
      [ "def __neg__(self) -> " + Python.Class.unqualifiedName classId + ":"
      , Python.indent
          [ "output = " + Python.FFI.dummyValue valueType
          , Python.FFI.invoke ffiFunctionName "ctypes.byref(self.__ptr__)" "ctypes.byref(output)"
          , "return " + Python.FFI.outputValue valueType "output"
          ]
      ]
