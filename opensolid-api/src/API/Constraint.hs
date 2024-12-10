module API.Constraint
  ( Constraint (..)
  , toleranceType
  )
where

import Data.Proxy (Proxy (Proxy))
import Length (Length)
import OpenSolid
import OpenSolid.FFI qualified as FFI

data Constraint
  = ToleranceUnitless
  | ToleranceMeters
  | ToleranceRadians

toleranceType :: Constraint -> FFI.Type
toleranceType constraint = case constraint of
  ToleranceUnitless -> FFI.typeOf @Float Proxy
  ToleranceMeters -> FFI.typeOf @Length Proxy
  ToleranceRadians -> FFI.typeOf @Angle Proxy
