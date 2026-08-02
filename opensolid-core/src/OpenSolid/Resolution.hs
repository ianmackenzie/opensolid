module OpenSolid.Resolution
  ( Resolution (maxError, maxSize)
  , maxError
  , maxSize
  , custom
  , acceptable
  )
where

import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

-- | Specify the desired accuracy of a linear approximation such as a polyline or triangle mesh.
data Resolution units = Resolution
  { maxError :: Quantity units
  , maxSize :: Quantity units
  }
  deriving (Show)

instance FFI (Resolution Unitless) where
  representation = FFI.classRepresentation "UnitlessResolution"

instance FFI (Resolution Meters) where
  representation = FFI.classRepresentation "Resolution"

-- | Specify the maximum error/deviation of the approximation from the actual shape.
maxError :: Quantity units -> Resolution units
maxError givenError = Resolution{maxError = givenError, maxSize = Quantity.infinity}

-- | Specify the maximum size of any element (line, triangle) in the approximation.
maxSize :: Quantity units -> Resolution units
maxSize givenSize = Resolution{maxError = Quantity.infinity, maxSize = givenSize}

-- | Specify both the maximum error and maximum element size in the approximation.
custom :: "maxError" ::: Quantity units -> "maxSize" ::: Quantity units -> Resolution units
custom ("maxError" ::: givenError) ("maxSize" ::: givenSize) =
  Resolution{maxError = givenError, maxSize = givenSize}

acceptable :: "size" ::: Quantity units -> "error" ::: Quantity units -> Resolution units -> Bool
acceptable ("size" ::: givenSize) ("error" ::: givenError) resolution =
  (Quantity.isInfinite resolution.maxSize || givenSize <= resolution.maxSize)
    && (Quantity.isInfinite resolution.maxError || givenError <= resolution.maxError)
