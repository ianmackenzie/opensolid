{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson3D (curve, surface) where

import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3D (Vector3D)

curve :: (Number -> (# Vector3D units space, Vector3D units space #)) -> Number -> Number
surface ::
  (UvPoint -> (# Vector3D units space, Vector3D units space, Vector3D units space #)) ->
  UvPoint ->
  UvPoint
