module OpenSolid.Vertex2d
  ( Vertex2d (position)
  , pattern Vertex2d
  )
where

import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude

class
  Eq vertex =>
  Vertex2d vertex units space
    | vertex -> space
    , vertex -> units
  where
  position :: vertex -> Point2d units space

instance Vertex2d (Point2d units space) units space where
  position = id

{-# COMPLETE Vertex2d #-}

pattern Vertex2d :: Vertex2d vertex units space => Point2d units space -> vertex
pattern Vertex2d point <- (position -> point)
