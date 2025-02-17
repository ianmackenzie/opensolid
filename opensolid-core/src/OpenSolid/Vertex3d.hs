module OpenSolid.Vertex3d
  ( Vertex3d (..)
  , pattern Vertex3d
  )
where

import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude

class Eq vertex => Vertex3d vertex (coordinateSystem :: CoordinateSystem) | vertex -> coordinateSystem where
  position :: vertex -> Point3d coordinateSystem

instance Vertex3d (Point3d (space @ units)) (space @ units) where
  position = identity

{-# COMPLETE Vertex3d #-}

pattern Vertex3d :: Vertex3d vertex (space @ units) => Point3d (space @ units) -> vertex
pattern Vertex3d point <- (position -> point)
