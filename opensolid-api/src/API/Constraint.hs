module API.Constraint
  ( Constraint (..)
  , toleranceType
  )
where

import Data.Proxy (Proxy (Proxy))
import OpenSolid.Angle (Angle)
import OpenSolid.Area (Area)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Length (Length)
import OpenSolid.Prelude

data Constraint
  = ToleranceUnitless
  | ToleranceRadians
  | ToleranceMeters
  | ToleranceSquareMeters

toleranceType :: Constraint -> FFI.Type
toleranceType constraint = case constraint of
  ToleranceUnitless -> FFI.typeOf @Float Proxy
  ToleranceRadians -> FFI.typeOf @Angle Proxy
  ToleranceMeters -> FFI.typeOf @Length Proxy
  ToleranceSquareMeters -> FFI.typeOf @Area Proxy
