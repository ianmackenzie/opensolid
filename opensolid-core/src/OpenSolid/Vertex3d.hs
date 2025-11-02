module OpenSolid.Vertex3d
  ( Vertex3d (position)
  , pattern Vertex3d
  , HasNormal (normal)
  , HasUv (uv)
  )
where

import OpenSolid.CoordinateSystem (Space)
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d

class
  Eq vertex =>
  Vertex3d vertex (coordinateSystem :: CoordinateSystem)
    | vertex -> coordinateSystem
  where
  position :: vertex -> Point3d coordinateSystem

instance Vertex3d (Point3d (space @ units)) (space @ units) where
  position = id

instance
  (space1 ~ space2, units2 ~ Unitless) =>
  Vertex3d (Point3d (space1 @ units1), Vector3d (space2 @ units2)) (space1 @ units1)
  where
  position (p, _) = p

instance
  space1 ~ space2 =>
  Vertex3d (Point3d (space1 @ units1), Direction3d space2) (space1 @ units1)
  where
  position (p, _) = p

instance
  (space2 ~ UvSpace, units2 ~ Unitless) =>
  Vertex3d (Point3d (space1 @ units1), Point2d (space2 @ units2)) (space1 @ units1)
  where
  position (p, _) = p

instance
  (space1 ~ space2, units2 ~ Unitless, space3 ~ UvSpace, units3 ~ Unitless) =>
  Vertex3d
    (Point3d (space1 @ units1), Vector3d (space2 @ units2), Point2d (space3 @ units3))
    (space1 @ units1)
  where
  position (p, _, _) = p

instance
  space1 ~ space2 =>
  Vertex3d
    (Point3d (space1 @ units1), Direction3d space2, Point2d (space3 @ units3))
    (space1 @ units1)
  where
  position (p, _, _) = p

{-# COMPLETE Vertex3d #-}

pattern Vertex3d :: Vertex3d vertex (space @ units) => Point3d (space @ units) -> vertex
pattern Vertex3d point <- (position -> point)

class
  Vertex3d vertex coordinateSystem =>
  HasNormal vertex (coordinateSystem :: CoordinateSystem)
    | vertex -> coordinateSystem
  where
  normal :: vertex -> Vector3d (Space coordinateSystem @ Unitless)

instance
  (space1 ~ space2, units2 ~ Unitless) =>
  HasNormal (Point3d (space1 @ units1), Vector3d (space2 @ units2)) (space1 @ units1)
  where
  normal (_, n) = n

instance
  space1 ~ space2 =>
  HasNormal (Point3d (space1 @ units1), Direction3d space2) (space1 @ units1)
  where
  normal (_, d) = Vector3d.unit d

instance
  (space1 ~ space2, units2 ~ Unitless, space3 ~ UvSpace, units3 ~ Unitless) =>
  HasNormal
    (Point3d (space1 @ units1), Vector3d (space2 @ units2), Point2d (space3 @ units3))
    (space1 @ units1)
  where
  normal (_, n, _) = n

instance
  (space1 ~ space2, space3 ~ UvSpace, units3 ~ Unitless) =>
  HasNormal
    (Point3d (space1 @ units1), Direction3d space2, Point2d (space3 @ units3))
    (space1 @ units1)
  where
  normal (_, d, _) = Vector3d.unit d

class
  Vertex3d vertex coordinateSystem =>
  HasUv vertex coordinateSystem
    | vertex -> coordinateSystem
  where
  uv :: vertex -> UvPoint

instance
  (space2 ~ UvSpace, units2 ~ Unitless) =>
  HasUv (Point3d (space1 @ units1), Point2d (space2 @ units2)) (space1 @ units1)
  where
  uv (_, t) = t

instance
  (space1 ~ space2, units2 ~ Unitless, space3 ~ UvSpace, units3 ~ Unitless) =>
  HasUv
    (Point3d (space1 @ units1), Vector3d (space2 @ units2), Point2d (space3 @ units3))
    (space1 @ units1)
  where
  uv (_, _, t) = t

instance
  (space1 ~ space2, space3 ~ UvSpace, units3 ~ Unitless) =>
  HasUv
    (Point3d (space1 @ units1), Direction3d space2, Point2d (space3 @ units3))
    (space1 @ units1)
  where
  uv (_, _, t) = t
