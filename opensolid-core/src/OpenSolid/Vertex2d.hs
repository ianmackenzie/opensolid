module OpenSolid.Vertex2d
  ( Vertex2d (position)
  , pattern Vertex2d
  )
where

import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude

class
  Eq vertex =>
  Vertex2d vertex (coordinateSystem :: CoordinateSystem)
    | vertex -> coordinateSystem
  where
  position :: vertex -> Point2d coordinateSystem

instance Vertex2d (Point2d (space @ units)) (space @ units) where
  position = id

{-# COMPLETE Vertex2d #-}

pattern Vertex2d :: Vertex2d vertex (space @ units) => Point2d (space @ units) -> vertex
pattern Vertex2d point <- (position -> point)
