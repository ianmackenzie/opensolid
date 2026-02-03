module OpenSolid.DirectionCurve
  ( DirectionCurve
  , Exists
  , unsafe
  , unwrap
  )
where

import GHC.TypeLits (Natural)
import {-# SOURCE #-} OpenSolid.DirectionCurve2D (DirectionCurve2D)
import {-# SOURCE #-} OpenSolid.DirectionCurve3D (DirectionCurve3D)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)

type family
  DirectionCurve dimension space =
    directionCurve | directionCurve -> dimension space
  where
  DirectionCurve 2 space = DirectionCurve2D space
  DirectionCurve 3 space = DirectionCurve3D space

class Exists (dimension :: Natural) (space :: Type)

instance Exists 2 space

instance Exists 3 space

unsafe ::
  Exists dimension space =>
  VectorCurve dimension Unitless space ->
  DirectionCurve dimension space
unwrap ::
  Exists dimension space =>
  DirectionCurve dimension space ->
  VectorCurve dimension Unitless space
