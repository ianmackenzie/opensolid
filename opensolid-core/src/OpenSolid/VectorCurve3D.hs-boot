module OpenSolid.VectorCurve3D (VectorCurve3D, constant) where

import OpenSolid.Vector3D (Vector3D)
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve3D)

constant :: Vector3D units space -> VectorCurve3D units space
