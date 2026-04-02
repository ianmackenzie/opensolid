module OpenSolid.UvRegion
  ( UvRegion
  , unitSquare
  )
where

import OpenSolid.InternalError qualified as InternalError
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2D (Region2D)
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvBounds qualified as UvBounds

type UvRegion = Region2D Unitless

-- | The unit square in UV space.
unitSquare :: UvRegion
unitSquare = Tolerance.using Quantity.zero do
  case Region2D.rectangle UvBounds.unitSquare of
    Ok region -> region
    Error Region2D.EmptyRegion ->
      InternalError.throw "Constructing UV unit square region should not fail"
