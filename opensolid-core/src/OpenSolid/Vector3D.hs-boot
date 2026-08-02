module OpenSolid.Vector3D
  ( Vector3D
  , magnitude
  )
where

import OpenSolid (Quantity)
import OpenSolid.Primitives (Vector3D)

magnitude :: Vector3D units space -> Quantity units
