module OpenSolid.Triangle2d
  ( Triangle2d (Triangle2d)
  , vertices
  , signedArea#
  , signedArea
  )
where

import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vertex2d (Vertex2d)
import OpenSolid.Vertex2d qualified as Vertex2d

data Triangle2d vertex = Triangle2d vertex vertex vertex

vertices :: Triangle2d vertex -> (vertex, vertex, vertex)
vertices (Triangle2d v1 v2 v3) = (v1, v2, v3)

signedArea# ::
  Vertex2d vertex space units =>
  Triangle2d vertex ->
  Quantity (units #*# units)
signedArea# (Triangle2d v1 v2 v3) = do
  let p1 = Vertex2d.position v1
  let p2 = Vertex2d.position v2
  let p3 = Vertex2d.position v3
  0.5 *. (p2 .-. p1) `cross#` (p3 .-. p1)

signedArea ::
  ( Vertex2d vertex space units1
  , Units.Product units1 units1 units2
  ) =>
  Triangle2d vertex ->
  Quantity units2
signedArea = Units.specialize . signedArea#
