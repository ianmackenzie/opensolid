module Python.HashFunction (definition) where

import OpenSolid.API.HashFunction qualified as HashFunction
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import Python qualified
import Python.Function qualified

definition :: FFI.ClassName -> Text
definition className = do
  let selfType = FFI.Class className
  let ffiFunctionName = HashFunction.ffiName className
  Python.lines
    [ "def __hash__(self) -> int:"
    , Python.indent
        [ "\"\"\"Return a hash code for ``self``.\"\"\""
        , Python.Function.body ffiFunctionName [("self", selfType)] FFI.Int
        ]
    ]
