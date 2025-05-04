module Python.EqualityFunction (definition) where

import API.EqualityFunction qualified as EqualityFunction
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import Python qualified
import Python.Class qualified
import Python.Function qualified

definition :: FFI.Class -> Text
definition ffiClass = do
  let selfType = FFI.Class ffiClass
  let ffiFunctionName = EqualityFunction.ffiName ffiClass
  Python.lines
    [ "def __eq__(self, other: object) -> bool:"
    , Python.indent
        [ "\"\"\"Return ``self == other``."
        , ""
        , "Note that this is an *exact* comparison; for a tolerant comparison"
        , "(one which will return true if two values are *almost* equal)"
        , "you'll likely want to use an ``is_zero()`` method instead."
        , "\"\"\""
        , "if not isinstance(other, " <> Python.Class.qualifiedName ffiClass <> "):"
        , "    return False"
        , Python.Function.body
            ffiFunctionName
            [("self", selfType), ("other", selfType)]
            FFI.Bool
        ]
    ]
