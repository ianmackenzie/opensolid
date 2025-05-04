module Python.AbsFunction (definition) where

import API.AbsFunction qualified as AbsFunction
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import Python qualified
import Python.Class qualified
import Python.Function qualified

definition :: FFI.ClassName -> Text
definition className = do
  let ffiFunctionName = AbsFunction.ffiName className
  let selfType = FFI.Class className
  Python.lines
    [ "def __abs__(self) -> " <> Python.Class.qualifiedName className <> ":"
    , Python.indent
        [ Python.docstring "Return ``abs(self)``."
        , Python.Function.body ffiFunctionName [("self", selfType)] selfType
        ]
    ]
