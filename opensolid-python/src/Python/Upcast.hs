module Python.Upcast (lines) where

import API.Upcast (Upcast)
import API.Upcast qualified as Upcast
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import Python qualified
import Python.Class qualified
import Python.FFI qualified

lines :: FFI.ClassName -> Text -> Maybe Upcast -> Text
lines _ _ Nothing = ""
lines className target (Just upcast) = do
  let childPointerFieldName = Python.Class.pointerFieldName className
  let parentPointerFieldName = Python.Class.pointerFieldName (Upcast.parentClassName upcast)
  Python.lines
    [ target <> "." <> parentPointerFieldName <> " = c_void_p()"
    , Python.FFI.invoke (Upcast.ffiName className)
        @ "ctypes.byref(" <> target <> "." <> childPointerFieldName <> ")"
        @ "ctypes.byref(" <> target <> "." <> parentPointerFieldName <> ")"
    ]
