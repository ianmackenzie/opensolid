module OpenSolid.Labels where

import Data.Proxy (Proxy (Proxy))
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude

newtype Radius a = Radius a

instance FFI a => FFI (Radius a) where
  representation _ = FFI.namedArgumentRepresentation @a "Radius" Proxy

newtype Diameter a = Diameter a

instance FFI a => FFI (Diameter a) where
  representation _ = FFI.namedArgumentRepresentation @a "Diameter" Proxy

newtype StartAngle a = StartAngle a

instance FFI a => FFI (StartAngle a) where
  representation _ = FFI.namedArgumentRepresentation @a "Start Angle" Proxy

newtype EndAngle a = EndAngle a

instance FFI a => FFI (EndAngle a) where
  representation _ = FFI.namedArgumentRepresentation @a "End Angle" Proxy

newtype CenterPoint a = CenterPoint a

instance FFI a => FFI (CenterPoint a) where
  representation _ = FFI.namedArgumentRepresentation @a "Center Point" Proxy
