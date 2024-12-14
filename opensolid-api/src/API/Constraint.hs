module API.Constraint
  ( Constraint (..)
  , toleranceType
  )
where

import Area (Area)
import Data.Proxy (Proxy (Proxy))
import Length (Length)
import OpenSolid
import OpenSolid.FFI qualified as FFI

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
