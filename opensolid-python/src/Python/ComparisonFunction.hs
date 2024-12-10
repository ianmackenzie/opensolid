module Python.ComparisonFunction (definitions) where

import API.ComparisonFunction qualified as ComparisonFunction
import OpenSolid
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Python qualified
import Python.FFI qualified
import Python.Type qualified

definitions :: forall value. FFI value => FFI.Id value -> Maybe (value -> value -> Int) -> Text
definitions classId maybeFunction = case maybeFunction of
  Nothing -> ""
  Just function -> do
    let valueType = ComparisonFunction.valueType function
    let valueTypeName = Python.Type.qualifiedName valueType
    let ffiFunctionName = ComparisonFunction.ffiName classId
    let helperDefinition =
          Python.lines
            [ "def _compare(self, other: " + valueTypeName + ") -> int:"
            , Python.indent
                [ "inputs = " + Python.FFI.argumentValue [("self", valueType), ("other", valueType)]
                , "output = " + Python.FFI.dummyValue ComparisonFunction.returnType
                , Python.FFI.invoke ffiFunctionName "ctypes.byref(inputs)" "ctypes.byref(output)"
                , "return " + Python.FFI.outputValue ComparisonFunction.returnType "output"
                ]
            ]
    let operatorDefinition name condition =
          Python.lines
            [ "def " + name + "(self, other: " + valueTypeName + ") -> bool:"
            , "    return self._compare(other) " + condition
            ]
    Python.lines
      [ helperDefinition
      , operatorDefinition "__lt__" "< 0"
      , operatorDefinition "__le__" "<= 0"
      , operatorDefinition "__ge__" ">= 0"
      , operatorDefinition "__gt__" "> 0"
      ]
