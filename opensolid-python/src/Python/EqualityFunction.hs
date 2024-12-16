module Python.EqualityFunction (definition) where

import API.EqualityFunction qualified as EqualityFunction
import OpenSolid
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Python qualified
import Python.Function qualified
import Python.Type qualified

definition :: forall value. FFI value => FFI.Id value -> Maybe (value -> value -> Bool) -> Text
definition classId maybeFunction = case maybeFunction of
  Nothing -> ""
  Just function -> do
    let valueType = EqualityFunction.valueType function
    let ffiFunctionName = EqualityFunction.ffiName classId
    Python.lines
      [ "def __eq__(self, other: object) -> bool:"
      , Python.indent
          [ "\"\"\"Return ``self == other``."
          , ""
          , "Note that this is an *exact* comparison; for a tolerant comparison"
          , "(one which will return true if two values are *almost* equal)"
          , "you'll likely want to use an ``is_zero()`` method instead."
          , "\"\"\""
          , "if not isinstance(other, " + Python.Type.qualifiedName valueType + "):"
          , "    return False"
          , Python.Function.body
              ffiFunctionName
              [("self", valueType), ("other", valueType)]
              EqualityFunction.returnType
          ]
      ]
