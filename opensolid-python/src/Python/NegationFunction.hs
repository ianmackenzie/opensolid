module Python.NegationFunction (definition) where

import API.NegationFunction qualified as NegationFunction
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import Python qualified
import Python.Function qualified
import Python.Type qualified

definition :: forall value. FFI value => FFI.Class -> Maybe (value -> value) -> Text
definition ffiClass maybeFunction = case maybeFunction of
  Nothing -> ""
  Just function -> do
    let valueType = NegationFunction.valueType function
    let valueTypeName = Python.Type.qualifiedName valueType
    let ffiFunctionName = NegationFunction.ffiName ffiClass
    Python.lines
      [ "def __neg__(self) -> " <> valueTypeName <> ":"
      , Python.indent
          [ Python.docstring "Return ``-self``."
          , Python.Function.body ffiFunctionName [("self", valueType)] valueType
          ]
      ]
