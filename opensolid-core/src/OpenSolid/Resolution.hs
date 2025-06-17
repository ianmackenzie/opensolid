module OpenSolid.Resolution (Resolution (Resolution), maxError, maxSize) where

import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty

-- | Specify the desired accuracy of a linear approximation such as a polyline or triangle mesh.
data Resolution units
  = -- | Construct a resolution from both a max error and a max element size.
    Resolution
      ( "maxError" ::: Qty units
      , "maxSize" ::: Qty units
      )

instance HasField "maxError" (Resolution units) (Qty units) where
  getField (Resolution fields) = fields.maxError

instance HasField "maxSize" (Resolution units) (Qty units) where
  getField (Resolution fields) = fields.maxSize

instance FFI (Resolution Meters) where
  representation = FFI.classRepresentation "Resolution"

-- | Specify the maximum error/deviation of the approximation from the actual shape.
maxError :: Qty units -> Resolution units
maxError error = Resolution (#maxError error, #maxSize Qty.infinity)

-- | Specify the maximum size of any element (line segment, triangle) in the approximation.
maxSize :: Qty units -> Resolution units
maxSize size = Resolution (#maxError Qty.infinity, #maxSize size)
