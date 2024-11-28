module OpenSolid.API.Argument (Argument (Argument)) where

import Data.Proxy (Proxy)
import OpenSolid.API.Name (Name)
import OpenSolid.FFI (FFI)

data Argument where
  Argument :: FFI a => Proxy a -> Name -> Argument
