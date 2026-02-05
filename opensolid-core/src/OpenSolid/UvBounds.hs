module OpenSolid.UvBounds
  ( UvBounds
  , pattern UvBounds
  , unitSquare
  )
where

import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Prelude
import OpenSolid.UvSpace (UvSpace)

type UvBounds = Bounds2D Unitless UvSpace

{-# COMPLETE UvBounds #-}

-- | Construct a UV bounding box from its U and V coordinate intervals.
{-# INLINE UvBounds #-}
pattern UvBounds :: Interval Unitless -> Interval Unitless -> UvBounds
pattern UvBounds u v = Bounds2D u v

unitSquare :: UvBounds
unitSquare = Bounds2D Interval.unit Interval.unit
