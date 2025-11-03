module OpenSolid.API.Upcast (Upcast (Upcast), ffiName, invoke, parentClassName) where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data Upcast where
  Upcast :: (FFI parent, FFI child) => (child -> parent) -> Upcast

ffiName :: FFI.ClassName -> Text
ffiName className =
  Text.join "_" ["opensolid", FFI.concatenatedName className, "upcast"]

invoke :: Upcast -> Ptr () -> Ptr () -> IO ()
invoke (Upcast f) inputPtr outputPtr = do
  value <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f value)

parentClassName :: Upcast -> FFI.ClassName
parentClassName (Upcast f) = parentClassNameImpl f

parentClassNameImpl :: forall parent child. FFI parent => (child -> parent) -> FFI.ClassName
parentClassNameImpl _ = FFI.className @parent Proxy
