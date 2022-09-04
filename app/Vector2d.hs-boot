module Vector2d (Vector2d, xy, components) where

import OpenSolid

newtype Vector2d units coordinates = Vector2d (Quantity units, Quantity units)

xy :: Quantity units -> Quantity units -> Vector2d units coordinates
components :: Vector2d units coordinates -> (Quantity units, Quantity units)
