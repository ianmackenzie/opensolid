module OpenSolid.Scene3d.Labels where

import Data.Proxy (Proxy (Proxy))
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude

newtype Metallic a = Metallic a

instance FFI a => FFI (Metallic a) where
  representation _ = FFI.namedArgumentRepresentation @a "Metallic" Proxy

newtype Roughness a = Roughness a

instance FFI a => FFI (Roughness a) where
  representation _ = FFI.namedArgumentRepresentation @a "Roughness" Proxy
