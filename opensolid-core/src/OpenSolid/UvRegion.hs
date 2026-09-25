module OpenSolid.UvRegion
  ( UvRegion
  , unitSquare
  , boundedBy
  , rectangle
  , circle
  , flip
  , flipAxis
  , classify
  , classifyBounds
  )
where

import OpenSolid.Axis2D (Axis2D (Axis2D))
import OpenSolid.Circle2D (Circle2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2D (Region2D)
import OpenSolid.Region2D qualified as Region2D
import OpenSolid.Region2D.BoundedBy qualified as Region2D.BoundedBy
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvBounds qualified as UvBounds
import OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvPoint qualified as UvPoint

type UvRegion = Region2D Unitless

-- | The unit square in UV space.
unitSquare :: UvRegion
unitSquare = Tolerance.using Quantity.zero do
  case Region2D.rectangle UvBounds.unitSquare of
    Ok region -> region
    Err Region2D.EmptyRegion -> error "Constructing UV unit square region should not fail"

{-| Create a region bounded by the given curves.

The curves may be given in any order,
do not need to have consistent directions
and can form multiple separate loops if the region has holes.
However, the curves must not overlap or intersect (other than at endpoints)
and there must not be any gaps between them.
-}
boundedBy :: List UvCurve -> Result Region2D.BoundedBy.Error UvRegion
boundedBy = unitless Region2D.boundedBy

{-| Create a rectangular region.

Fails if the given bounds are empty
(zero area, i.e. zero width in either direction).
-}
rectangle :: UvBounds -> Result Region2D.EmptyRegion UvRegion
rectangle = unitless Region2D.rectangle

-- | Create a region from the given circle.
circle :: Circle2D Unitless -> Result Region2D.EmptyRegion UvRegion
circle = unitless Region2D.circle

flip :: UvRegion -> UvRegion
flip = Region2D.mirrorAcross flipAxis

flipAxis :: Axis2D Unitless
flipAxis = Axis2D (UvPoint.bottom 0.5) Direction2D.y

classify :: UvPoint -> UvRegion -> Region2D.Classification
classify = unitless Region2D.classify

classifyBounds :: UvBounds -> UvRegion -> Fuzzy Region2D.Classification
classifyBounds = unitless Region2D.classifyBounds
