module Python.Upcast (lines) where

import API.Upcast (Upcast)
import API.Upcast qualified as Upcast
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import Python qualified
import Python.Class qualified
import Python.FFI qualified

lines :: FFI.Class -> Text -> Maybe Upcast -> Text
lines _ _ Nothing = ""
lines ffiClass target (Just upcast) = do
  let childPointerFieldName = Python.Class.pointerFieldName ffiClass
  let parentPointerFieldName = Python.Class.pointerFieldName (Upcast.parentClass upcast)
  Python.lines
    [ target <> "." <> parentPointerFieldName <> " = c_void_p()"
    , Python.FFI.invoke (Upcast.ffiName ffiClass)
        # "ctypes.byref(" <> target <> "." <> childPointerFieldName <> ")"
        # "ctypes.byref(" <> target <> "." <> parentPointerFieldName <> ")"
    ]
