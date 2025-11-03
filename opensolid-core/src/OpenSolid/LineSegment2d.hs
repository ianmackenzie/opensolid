{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.LineSegment2d
  ( LineSegment2d (LineSegment2d)
  , startVertex
  , endVertex
  , startPoint
  , endPoint
  , length
  , length#
  , bounds
  , distanceTo
  , distanceTo#
  )
where

import OpenSolid.Bounded2d (Bounded2d)
import OpenSolid.Bounded2d qualified as Bounded2d
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Unboxed.Math
import OpenSolid.Vertex2d (Vertex2d)
import OpenSolid.Vertex2d qualified as Vertex2d

data LineSegment2d vertex = LineSegment2d vertex vertex

instance HasField "startVertex" (LineSegment2d vertex) vertex where
  getField = startVertex

instance HasField "endVertex" (LineSegment2d vertex) vertex where
  getField = endVertex

instance
  Vertex2d vertex (space @ units) =>
  HasField "startPoint" (LineSegment2d vertex) (Point2d (space @ units))
  where
  getField = startPoint

instance
  Vertex2d vertex (space @ units) =>
  HasField "endPoint" (LineSegment2d vertex) (Point2d (space @ units))
  where
  getField = endPoint

instance
  Vertex2d vertex (space @ units) =>
  HasField "length" (LineSegment2d vertex) (Quantity units)
  where
  getField = length

instance
  Vertex2d vertex (space @ units) =>
  HasField "bounds" (LineSegment2d vertex) (Bounds2d (space @ units))
  where
  getField = bounds

instance
  Vertex2d vertex (space @ units) =>
  Bounded2d (LineSegment2d vertex) (space @ units)
  where
  bounds = bounds

{-# INLINE startVertex #-}
startVertex :: LineSegment2d vertex -> vertex
startVertex (LineSegment2d v1 _) = v1

{-# INLINE endVertex #-}
endVertex :: LineSegment2d vertex -> vertex
endVertex (LineSegment2d _ v2) = v2

{-# INLINE startPoint #-}
startPoint :: Vertex2d vertex (space @ units) => LineSegment2d vertex -> Point2d (space @ units)
startPoint (LineSegment2d v1 _) = Vertex2d.position v1

{-# INLINE endPoint #-}
endPoint :: Vertex2d vertex (space @ units) => LineSegment2d vertex -> Point2d (space @ units)
endPoint (LineSegment2d _ v2) = Vertex2d.position v2

length :: Vertex2d vertex (space @ units) => LineSegment2d vertex -> Quantity units
length segment = Quantity# (length# segment)

{-# INLINE length# #-}
length# :: Vertex2d vertex (space @ units) => LineSegment2d vertex -> Double#
length# segment = Point2d.distanceFrom# (startPoint segment) (endPoint segment)

bounds :: Vertex2d vertex (space @ units) => LineSegment2d vertex -> Bounds2d (space @ units)
bounds segment = Bounds2d.hull2 (startPoint segment) (endPoint segment)

distanceTo ::
  Vertex2d vertex (space @ units) =>
  Point2d (space @ units) ->
  LineSegment2d vertex ->
  Quantity units
distanceTo p0 segment = Quantity# (distanceTo# p0 segment)

{-# INLINEABLE distanceTo# #-}
distanceTo# ::
  Vertex2d vertex (space @ units) =>
  Point2d (space @ units) ->
  LineSegment2d vertex ->
  Double#
distanceTo# p0 (LineSegment2d p1 p2) = do
  let !(Point2d (Quantity# x0#) (Quantity# y0#)) = p0
  let !(Point2d (Quantity# x1#) (Quantity# y1#)) = Vertex2d.position p1
  let !(Point2d (Quantity# x2#) (Quantity# y2#)) = Vertex2d.position p2
  let ux# = x0# -# x1#
  let uy# = y0# -# y1#
  let vx# = x2# -# x1#
  let vy# = y2# -# y1#
  let lengthSquared# = vx# *# vx# +# vy# *# vy#
  let dotProduct# = ux# *# vx# +# uy# *# vy#
  case (# dotProduct# <=# 0.0##, dotProduct# >=# lengthSquared# #) of
    (# 1#, _ #) -> hypot2# ux# uy#
    (# _, 1# #) -> hypot2# (x0# -# x2#) (y0# -# y2#)
    (# _, _ #) -> abs# (vx# *# uy# -# vy# *# ux#) /# sqrt# lengthSquared#
