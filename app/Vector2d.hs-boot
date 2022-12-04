module Vector2d (Vector2d) where

import OpenSolid

data Vector2d units coordinates = Vector2d !(Qty units) !(Qty units)
