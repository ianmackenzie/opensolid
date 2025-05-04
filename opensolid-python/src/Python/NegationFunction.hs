module Python.NegationFunction (definition) where

import API.NegationFunction qualified as NegationFunction
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import Python qualified
import Python.Class qualified
import Python.Function qualified

definition :: FFI.ClassName -> Text
definition className = do
  let ffiFunctionName = NegationFunction.ffiName className
  let selfType = FFI.Class className
  Python.lines
    [ "def __neg__(self) -> " <> Python.Class.qualifiedName className <> ":"
    , Python.indent
        [ Python.docstring "Return ``-self``."
        , Python.Function.body ffiFunctionName [("self", selfType)] selfType
        ]
    ]
