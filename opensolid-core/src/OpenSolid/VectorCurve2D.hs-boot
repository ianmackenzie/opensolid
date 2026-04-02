module OpenSolid.VectorCurve2D (VectorCurve2D, constant) where

import OpenSolid.Vector2D (Vector2D)
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve2D)

constant :: Vector2D units -> VectorCurve2D units
