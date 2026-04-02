module OpenSolid.DirectionCurve
  ( DirectionCurve
  , Exists
  , unsafe
  , unwrap
  , value
  , bounds
  , derivative
  )
where

import Data.Void (Void)
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import {-# SOURCE #-} OpenSolid.DirectionCurve2D (DirectionCurve2D)
import {-# SOURCE #-} OpenSolid.DirectionCurve2D qualified as DirectionCurve2D
import {-# SOURCE #-} OpenSolid.DirectionCurve3D (DirectionCurve3D)
import {-# SOURCE #-} OpenSolid.DirectionCurve3D qualified as DirectionCurve3D
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

type family
  DirectionCurve dimension space =
    directionCurve | directionCurve -> dimension space
  where
  DirectionCurve 2 Void = DirectionCurve2D
  DirectionCurve 3 space = DirectionCurve3D space

class
  ( Direction.Exists dimension space
  , DirectionBounds.Exists dimension space
  , VectorCurve.Exists dimension Unitless space
  ) =>
  Exists dimension space
  where
  unsafeImpl :: VectorCurve dimension Unitless space -> DirectionCurve dimension space
  unwrapImpl :: DirectionCurve dimension space -> VectorCurve dimension Unitless space
  value :: DirectionCurve dimension space -> Number -> Direction dimension space
  bounds :: DirectionCurve dimension space -> Interval Unitless -> DirectionBounds dimension space
  derivative :: DirectionCurve dimension space -> VectorCurve dimension Unitless space

instance Exists 2 Void where
  unsafeImpl = DirectionCurve2D.unsafe
  unwrapImpl = DirectionCurve2D.unwrap
  value = DirectionCurve2D.value
  bounds = DirectionCurve2D.bounds
  derivative = DirectionCurve2D.derivative

instance Exists 3 space where
  unsafeImpl = DirectionCurve3D.unsafe
  unwrapImpl = DirectionCurve3D.unwrap
  value = DirectionCurve3D.value
  bounds = DirectionCurve3D.bounds
  derivative = DirectionCurve3D.derivative

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
