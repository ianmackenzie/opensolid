module Python.NegationFunction (definition) where

import API.NegationFunction qualified as NegationFunction
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import Python qualified
import Python.Class qualified
import Python.Function qualified

definition :: FFI.Class -> Text
definition ffiClass = do
  let ffiFunctionName = NegationFunction.ffiName ffiClass
  let selfType = FFI.Class ffiClass
  Python.lines
    [ "def __neg__(self) -> " <> Python.Class.qualifiedName ffiClass <> ":"
    , Python.indent
        [ Python.docstring "Return ``-self``."
        , Python.Function.body ffiFunctionName [("self", selfType)] selfType
        ]
    ]
