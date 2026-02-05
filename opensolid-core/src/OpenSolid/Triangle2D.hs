module OpenSolid.Triangle2D
  ( Triangle2D (Triangle2D)
  , vertices
  , signedArea_
  , signedArea
  )
where

import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units

-- | A triangle in 2D.
data Triangle2D units space
  = -- | Construct a triangle from its three vertices.
    Triangle2D (Point2D units space) (Point2D units space) (Point2D units space)

instance FFI (Triangle2D Meters FFI.Space) where
  representation = FFI.classRepresentation "Triangle2D"

-- | Get the vertices of a triangle as a tuple.
vertices :: Triangle2D units space -> (Point2D units space, Point2D units space, Point2D units space)
vertices (Triangle2D p1 p2 p3) = (p1, p2, p3)

signedArea_ :: Triangle2D units space -> Quantity (units ?*? units)
signedArea_ (Triangle2D p1 p2 p3) = 0.5 * (p2 - p1) `cross_` (p3 - p1)

{-| Compute the signed area of a triangle.

This will be positive if the triangle's vertices are in counterclockwise order,
and negative otherwise.
-}
signedArea :: Units.Product units1 units1 units2 => Triangle2D units1 space -> Quantity units2
signedArea = Units.specialize . signedArea_
