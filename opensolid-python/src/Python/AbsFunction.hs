module Python.AbsFunction (definition) where

import API.AbsFunction qualified as AbsFunction
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import Python qualified
import Python.Class qualified
import Python.Function qualified

definition :: FFI.Class -> Text
definition ffiClass = do
  let ffiFunctionName = AbsFunction.ffiName ffiClass
  let selfType = FFI.Class ffiClass
  Python.lines
    [ "def __abs__(self) -> " <> Python.Class.qualifiedName ffiClass <> ":"
    , Python.indent
        [ Python.docstring "Return ``abs(self)``."
        , Python.Function.body ffiFunctionName [("self", selfType)] selfType
        ]
    ]
