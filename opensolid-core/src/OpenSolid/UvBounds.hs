module OpenSolid.UvBounds
  ( UvBounds
  , data UvBounds
  , unitSquare
  , leftEdge
  , rightEdge
  , bottomEdge
  , topEdge
  , constant
  )
where

import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)

type UvBounds = Bounds2D Unitless

{-# COMPLETE UvBounds #-}

-- | Construct a UV bounding box from its U and V coordinate intervals.
{-# INLINE UvBounds #-}
pattern UvBounds :: Interval Unitless -> Interval Unitless -> UvBounds
pattern UvBounds u v = Bounds2D u v

unitSquare :: UvBounds
unitSquare = Bounds2D Interval.unit Interval.unit

leftEdge :: UvBounds
leftEdge = UvBounds (Interval.constant 0.0) Interval.unit

rightEdge :: UvBounds
rightEdge = UvBounds (Interval.constant 1.0) Interval.unit

bottomEdge :: UvBounds
bottomEdge = UvBounds Interval.unit (Interval.constant 0.0)

topEdge :: UvBounds
topEdge = UvBounds Interval.unit (Interval.constant 1.0)

constant :: UvPoint -> UvBounds
constant = Bounds2D.constant
