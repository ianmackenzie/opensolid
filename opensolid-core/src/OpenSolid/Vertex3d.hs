module OpenSolid.Vertex3d
  ( Vertex3d (position)
  , pattern Vertex3d
  , HasNormal (normal)
  , HasUv (uv)
  )
where

import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d

class Eq vertex => Vertex3d vertex space | vertex -> space where
  position :: vertex -> Point3d space Meters

instance
  meters ~ Meters =>
  Vertex3d (Point3d space meters) space
  where
  position = id

instance
  (space1 ~ space2, meters ~ Meters, unitless ~ Unitless) =>
  Vertex3d (Point3d space1 meters, Vector3d space2 unitless) space1
  where
  position (p, _) = p

instance
  (space1 ~ space2, meters ~ Meters) =>
  Vertex3d (Point3d space1 meters, Direction3d space2) space1
  where
  position (p, _) = p

instance
  (meters ~ Meters, uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Vertex3d (Point3d space meters, Point2d uvSpace unitless) space
  where
  position (p, _) = p

instance
  ( space1 ~ space2
  , meters ~ Meters
  , unitless1 ~ Unitless
  , uvSpace ~ UvSpace
  , unitless2 ~ Unitless
  ) =>
  Vertex3d (Point3d space1 meters, Vector3d space2 unitless1, Point2d uvSpace unitless2) space1
  where
  position (p, _, _) = p

instance
  (space1 ~ space2, meters ~ Meters, uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Vertex3d (Point3d space1 meters, Direction3d space2, Point2d uvSpace unitless) space1
  where
  position (p, _, _) = p

{-# COMPLETE Vertex3d #-}

pattern Vertex3d :: Vertex3d vertex space => Point3d space Meters -> vertex
pattern Vertex3d point <- (position -> point)

class Vertex3d vertex space => HasNormal vertex space | vertex -> space where
  normal :: vertex -> Vector3d space Unitless

instance
  (space1 ~ space2, meters ~ Meters, unitless ~ Unitless) =>
  HasNormal (Point3d space1 meters, Vector3d space2 unitless) space1
  where
  normal (_, n) = n

instance
  (space1 ~ space2, meters ~ Meters) =>
  HasNormal (Point3d space1 meters, Direction3d space2) space1
  where
  normal (_, d) = Vector3d.unit d

instance
  ( space1 ~ space2
  , meters ~ Meters
  , unitless1 ~ Unitless
  , uvSpace ~ UvSpace
  , unitless2 ~ Unitless
  ) =>
  HasNormal (Point3d space1 meters, Vector3d space2 unitless1, Point2d uvSpace unitless2) space1
  where
  normal (_, n, _) = n

instance
  ( space1 ~ space2
  , meters ~ Meters
  , uvSpace ~ UvSpace
  , unitless ~ Unitless
  ) =>
  HasNormal (Point3d space1 meters, Direction3d space2, Point2d uvSpace unitless) space1
  where
  normal (_, d, _) = Vector3d.unit d

class Vertex3d vertex space => HasUv vertex space | vertex -> space where
  uv :: vertex -> UvPoint

instance
  (meters ~ Meters, uvSpace ~ UvSpace, unitless ~ Unitless) =>
  HasUv (Point3d space meters, Point2d uvSpace unitless) space
  where
  uv (_, t) = t

instance
  ( space1 ~ space2
  , meters ~ Meters
  , unitless1 ~ Unitless
  , uvSpace ~ UvSpace
  , unitless2 ~ Unitless
  ) =>
  HasUv (Point3d space1 meters, Vector3d space2 unitless1, Point2d uvSpace unitless2) space1
  where
  uv (_, _, t) = t

instance
  (space1 ~ space2, meters ~ Meters, uvSpace ~ UvSpace, unitless ~ Unitless) =>
  HasUv (Point3d space1 meters, Direction3d space2, Point2d uvSpace unitless) space1
  where
  uv (_, _, t) = t
