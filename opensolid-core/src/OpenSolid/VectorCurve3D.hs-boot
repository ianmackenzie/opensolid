module OpenSolid.VectorCurve3D (VectorCurve3D, constant, on) where

import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Vector3D (Vector3D)
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve3D)
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

constant :: Vector3D units space -> VectorCurve3D units space
on :: Plane3D space -> VectorCurve2D units -> VectorCurve3D units space
