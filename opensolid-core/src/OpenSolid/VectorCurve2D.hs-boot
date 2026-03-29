module OpenSolid.VectorCurve2D (VectorCurve2D, constant) where

import OpenSolid.Vector2D (Vector2D)
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)

type VectorCurve2D units space = VectorCurve 2 units space

constant :: Vector2D units space -> VectorCurve2D units space
