module OpenSolid.NewtonRaphson3D (curve) where

import OpenSolid.Prelude
import OpenSolid.Vector3D (Vector3D)

curve :: (Number -> Vector3D units space) -> (Number -> Vector3D units space) -> Number -> Number
