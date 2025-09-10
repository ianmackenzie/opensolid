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
import OpenSolid.Qty qualified as Qty

-- | Specify the desired accuracy of a linear approximation such as a polyline or triangle mesh.
data Resolution units = Resolution
  { maxError :: Qty units
  , maxSize :: Qty units
  }

instance FFI (Resolution Meters) where
  representation = FFI.classRepresentation "Resolution"

-- | Specify the maximum error/deviation of the approximation from the actual shape.
maxError :: Qty units -> Resolution units
maxError error = Resolution{maxError = error, maxSize = Qty.infinity}

-- | Specify the maximum size of any element (line segment, triangle) in the approximation.
maxSize :: Qty units -> Resolution units
maxSize size = Resolution{maxError = Qty.infinity, maxSize = size}

-- | Specify both the maximum error and maximum element size in the approximation.
custom :: ("maxError" ::: Qty units, "maxSize" ::: Qty units) -> Resolution units
custom args = Resolution{maxError = args.maxError, maxSize = args.maxSize}

predicate ::
  ("size" ::: (a -> Qty units), "error" ::: (a -> Qty units)) ->
  Resolution units ->
  a ->
  Bool
predicate args resolution value = do
  let acceptableSize =
        Qty.isInfinite resolution.maxSize || args.size value <= resolution.maxSize
  let acceptableError =
        Qty.isInfinite resolution.maxError || args.error value <= resolution.maxError
  acceptableSize && acceptableError
