module OpenSolid.Tolerance
  ( Tolerance
  , using
  , unitless
  , angle
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Prelude

using :: Quantity units -> (Tolerance units => a) -> a
using tolerance expression = let ?tolerance = tolerance in expression

unitless :: Number
unitless = 1e-9

angle :: Angle
angle = Angle.radians unitless
