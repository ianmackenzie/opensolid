module OpenSolid.Resolution
  ( Resolution (maxError, maxSize)
  , maxError
  , maxSize
  , custom
  , predicate
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

instance FFI (Resolution Meters) where
  representation = FFI.classRepresentation "Resolution"

-- | Specify the maximum error/deviation of the approximation from the actual shape.
maxError :: Quantity units -> Resolution units
maxError error = Resolution{maxError = error, maxSize = Quantity.infinity}

-- | Specify the maximum size of any element (line segment, triangle) in the approximation.
maxSize :: Quantity units -> Resolution units
maxSize size = Resolution{maxError = Quantity.infinity, maxSize = size}

-- | Specify both the maximum error and maximum element size in the approximation.
custom :: "maxError" ::: Quantity units -> "maxSize" ::: Quantity units -> Resolution units
custom (Named givenMaxError) (Named givenMaxSize) =
  Resolution{maxError = givenMaxError, maxSize = givenMaxSize}

predicate ::
  "size" ::: (a -> Quantity units) ->
  "error" ::: (a -> Quantity units) ->
  Resolution units ->
  a ->
  Bool
predicate (Named size) (Named error) resolution value = do
  let acceptableSize = Quantity.isInfinite resolution.maxSize || size value <= resolution.maxSize
  let acceptableError = Quantity.isInfinite resolution.maxError || error value <= resolution.maxError
  acceptableSize && acceptableError
