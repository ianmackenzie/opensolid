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

{-| A default tolerance of 1e-9 used internally when comparing unitless quantities.

In general, this tolerance should only be used for "normalized" values
(ones which typically range from 0 to 1 or -1 to 1).
For example, it is used in some places to assess whether two directions are parallel
(since direction components are always between -1 and )
or if two curve parameter values are equal
(since curve parameters are always in the range 0 to 1).
-}
unitless :: Number
unitless = 1e-9

-- | A default tolerance of 1e-9 radians used internally when comparing angles.
angle :: Angle
angle = Angle.radians unitless
