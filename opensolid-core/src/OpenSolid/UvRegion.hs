module OpenSolid.UvRegion
  ( UvRegion
  , unitSquare
  , flip
  , classify
  , classifyBounds
  )
where

import OpenSolid.Axis2D (Axis2D (Axis2D))
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2D (Region2D)
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvBounds qualified as UvBounds
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvPoint qualified as UvPoint

type UvRegion = Region2D Unitless

-- | The unit square in UV space.
unitSquare :: UvRegion
unitSquare = Tolerance.using Quantity.zero do
  case Region2D.rectangle UvBounds.unitSquare of
    Ok region -> region
    Err Region2D.EmptyRegion -> error "Constructing UV unit square region should not fail"

flip :: UvRegion -> UvRegion
flip = Region2D.mirrorAcross (Axis2D (UvPoint.bottom 0.5) Direction2D.y)

classify :: UvPoint -> UvRegion -> Region2D.Classification
classify = unitless Region2D.classify

classifyBounds :: UvBounds -> UvRegion -> Fuzzy Region2D.Classification
classifyBounds = unitless Region2D.classifyBounds
