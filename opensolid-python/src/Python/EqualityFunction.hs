module Python.EqualityFunction (definition) where

import OpenSolid
import OpenSolid.API.EqualityFunction qualified as EqualityFunction
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Python qualified
import Python.FFI qualified
import Python.Type qualified

definition :: forall value. FFI value => FFI.Id value -> Maybe (value -> value -> Bool) -> Text
definition classId maybeFunction = case maybeFunction of
  Nothing -> ""
  Just function -> do
    let valueType = EqualityFunction.valueType function
    let valueTypeName = Python.Type.qualifiedName valueType
    let ffiFunctionName = EqualityFunction.ffiName classId
    Python.lines
      [ "def __eq__(self, other: object) -> bool:"
      , Python.indent
          [ "if isinstance(other, " + valueTypeName + "):"
          , Python.indent
              [ "inputs = " + Python.FFI.argumentValue [("self", valueType), ("other", valueType)]
              , "output = " + Python.FFI.dummyValue EqualityFunction.returnType
              , Python.FFI.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
              , "return " + Python.FFI.outputValue EqualityFunction.returnType "output"
              ]
          , "return False"
          ]
      ]
