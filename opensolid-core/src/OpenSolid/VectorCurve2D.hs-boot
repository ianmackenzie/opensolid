module OpenSolid.VectorCurve2D (VectorCurve2D, constant) where

import Data.Void (Void)
import OpenSolid.Vector2D (Vector2D)
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)

type VectorCurve2D units = VectorCurve 2 units Void

constant :: Vector2D units -> VectorCurve2D units
