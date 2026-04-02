module OpenSolid.Tolerance
  ( Tolerance
  , using
  , unitless
  , angle
  , length
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Prelude

using :: Quantity units -> (Tolerance units => a) -> a
using tolerance expression = let ?tolerance = tolerance in expression

unitless :: Number
unitless = 1e-9

angle :: Angle
angle = Angle.radians unitless

length :: Length
length = Length.meters unitless
