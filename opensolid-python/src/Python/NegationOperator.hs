module Python.NegationOperator (definition) where

import Data.Proxy (Proxy (Proxy))
import OpenSolid
import OpenSolid.API.NegationOperator qualified as NegationOperator
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Python qualified
import Python.Class qualified
import Python.FFI qualified

definition :: forall value. FFI value => FFI.Id value -> Maybe (value -> value) -> Text
definition classId maybeFunction = case maybeFunction of
  Nothing -> ""
  Just _ -> do
    let selfType = FFI.typeOf @value Proxy
    let ffiFunctionName = NegationOperator.ffiName classId
    Python.lines
      [ "def __neg__(self) -> " + Python.Class.unqualifiedName classId + ":"
      , Python.indent
          [ "output = " + Python.FFI.dummyValue selfType
          , Python.FFI.invoke ffiFunctionName "ctypes.byref(self.__ptr__)" "ctypes.byref(output)"
          , "return " + Python.FFI.outputValue selfType "output"
          ]
      ]
