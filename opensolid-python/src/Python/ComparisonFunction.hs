module Python.ComparisonFunction (definitions) where

import OpenSolid.API.ComparisonFunction qualified as ComparisonFunction
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import Python qualified
import Python.Class qualified
import Python.Function qualified

definitions :: FFI.ClassName -> Text
definitions className = do
  let selfType = FFI.Class className
  let selfTypeName = Python.Class.qualifiedName className
  let ffiFunctionName = ComparisonFunction.ffiName className
  let helperDefinition =
        Python.lines
          [ "def _compare(self, other: " <> selfTypeName <> ") -> int:"
          , Python.indent
              [ Python.Function.body
                  ffiFunctionName
                  [("self", selfType), ("other", selfType)]
                  FFI.Int
              ]
          ]
  let operatorDefinition name symbol =
        Python.lines
          [ "def " <> name <> "(self, other: " <> selfTypeName <> ") -> bool:"
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
