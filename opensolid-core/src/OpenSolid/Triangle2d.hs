module OpenSolid.Triangle2d
  ( Triangle2d (Triangle2d)
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
data Triangle2d units space
  = -- | Construct a triangle from its three vertices.
    Triangle2d (Point2D units space) (Point2D units space) (Point2D units space)

instance FFI (Triangle2d Meters FFI.Space) where
  representation = FFI.classRepresentation "Triangle2d"

-- | Get the vertices of a triangle as a tuple.
vertices :: Triangle2d units space -> (Point2D units space, Point2D units space, Point2D units space)
vertices (Triangle2d p1 p2 p3) = (p1, p2, p3)

signedArea_ :: Triangle2d units space -> Quantity (units ?*? units)
signedArea_ (Triangle2d p1 p2 p3) = 0.5 *. (p2 .-. p1) `cross_` (p3 .-. p1)

{-| Compute the signed area of a triangle.

This will be positive if the triangle's vertices are in counterclockwise order,
and negative otherwise.
-}
signedArea :: Units.Product units1 units1 units2 => Triangle2d units1 space -> Quantity units2
signedArea = Units.specialize . signedArea_
