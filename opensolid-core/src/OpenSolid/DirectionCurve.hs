module OpenSolid.DirectionCurve
  ( DirectionCurve
  , Exists
  , unsafe
  , unwrap
  )
where

import {-# SOURCE #-} OpenSolid.DirectionCurve2D (DirectionCurve2D)
import {-# SOURCE #-} OpenSolid.DirectionCurve2D qualified as DirectionCurve2D
import {-# SOURCE #-} OpenSolid.DirectionCurve3D (DirectionCurve3D)
import {-# SOURCE #-} OpenSolid.DirectionCurve3D qualified as DirectionCurve3D
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

type family
  DirectionCurve dimension space =
    directionCurve | directionCurve -> dimension space
  where
  DirectionCurve 2 space = DirectionCurve2D space
  DirectionCurve 3 space = DirectionCurve3D space

class
  VectorCurve.Exists dimension Unitless space =>
  Exists dimension space
  where
  unsafeImpl :: VectorCurve dimension Unitless space -> DirectionCurve dimension space
  unwrapImpl :: DirectionCurve dimension space -> VectorCurve dimension Unitless space

instance Exists 2 space where
  unsafeImpl = DirectionCurve2D.unsafe
  unwrapImpl = DirectionCurve2D.unwrap

instance Exists 3 space where
  unsafeImpl = DirectionCurve3D.unsafe
  unwrapImpl = DirectionCurve3D.unwrap

unsafe ::
  Exists dimension space =>
  VectorCurve dimension Unitless space ->
  DirectionCurve dimension space
unsafe = unsafeImpl

unwrap ::
  Exists dimension space =>
  DirectionCurve dimension space ->
  VectorCurve dimension Unitless space
unwrap = unwrapImpl
