module OpenSolid.API.ImplicitArgument
  ( ImplicitArgument (..)
  , ffiType
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Area (Area)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Length (Length)
import OpenSolid.Prelude

data ImplicitArgument
  = ToleranceUnitless
  | ToleranceRadians
  | ToleranceMeters
  | ToleranceSquareMeters
  deriving (Show)

ffiType :: ImplicitArgument -> FFI.Type
ffiType constraint = case constraint of
  ToleranceUnitless -> FFI.typeOf Number
  ToleranceRadians -> FFI.typeOf Angle
  ToleranceMeters -> FFI.typeOf Length
  ToleranceSquareMeters -> FFI.typeOf Area
