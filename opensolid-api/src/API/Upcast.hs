module API.Upcast (Upcast (Upcast), ffiName, invoke, parentClass) where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data Upcast where
  Upcast :: (FFI parent, FFI child) => (child -> parent) -> Upcast

ffiName :: FFI.Class -> Text
ffiName ffiClass =
  Text.join "_" ["opensolid", FFI.concatenatedName ffiClass, "upcast"]

invoke :: Upcast -> Ptr () -> Ptr () -> IO ()
invoke (Upcast f) inputPtr outputPtr = IO.do
  value <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f value)

parentClass :: Upcast -> FFI.Class
parentClass (Upcast f) = parentClassImpl f

parentClassImpl :: forall parent child. FFI parent => (child -> parent) -> FFI.Class
parentClassImpl _ = FFI.classOf @parent Proxy
