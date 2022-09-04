module Vector3d (Vector3d, xyz, components) where

import OpenSolid

newtype Vector3d units coordinates = Vector3d (Quantity units, Quantity units, Quantity units)

xyz :: Quantity units -> Quantity units -> Quantity units -> Vector3d units coordinates
components :: Vector3d units coordinates -> (Quantity units, Quantity units, Quantity units)
