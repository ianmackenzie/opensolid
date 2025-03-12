module Python.ComparisonFunction (definitions) where

import API.ComparisonFunction qualified as ComparisonFunction
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import Python qualified
import Python.Function qualified
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
            [ "def _compare(self, other: " <> valueTypeName <> ") -> int:"
            , Python.indent
                [ Python.Function.body
                    ffiFunctionName
                    [("self", valueType), ("other", valueType)]
                    ComparisonFunction.returnType
                ]
            ]
    let operatorDefinition name symbol =
          Python.lines
            [ "def " <> name <> "(self, other: " <> valueTypeName <> ") -> bool:"
            , "    \"\"\"Return ``self " <> symbol <> " other``.\"\"\""
            , "    return self._compare(other) " <> symbol <> " 0"
            ]
    Python.lines
      [ helperDefinition
      , operatorDefinition "__lt__" "<"
      , operatorDefinition "__le__" "<="
      , operatorDefinition "__ge__" ">="
      , operatorDefinition "__gt__" ">"
      ]
