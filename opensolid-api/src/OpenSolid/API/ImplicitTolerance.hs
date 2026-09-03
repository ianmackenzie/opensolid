module OpenSolid.API.ImplicitTolerance
  ( ImplicitTolerance (..)
  , ffiType
  )
where

import OpenSolid.FFI qualified as FFI
import OpenSolid.Length (Length)
import OpenSolid.Prelude hiding (data Number)

data ImplicitTolerance = ImplicitTolerance deriving (Show)

ffiType :: FFI.Type
ffiType = FFI.typeOf Length
