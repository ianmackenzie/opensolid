module OpenSolid.API.ImplicitArgument
  ( ImplicitArgument (..)
  , ffiType
  )
where

import OpenSolid.FFI qualified as FFI
import OpenSolid.Length (Length)
import OpenSolid.Prelude hiding (data Number)

data ImplicitArgument
  = ToleranceMeters
  deriving (Show)

ffiType :: ImplicitArgument -> FFI.Type
ffiType constraint = case constraint of
  ToleranceMeters -> FFI.typeOf Length
