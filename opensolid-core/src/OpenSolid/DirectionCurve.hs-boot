module OpenSolid.DirectionCurve
  ( DirectionCurve
  , Exists
  , unsafe
  , unwrap
  , value
  , bounds
  )
where

import Data.Void (Void)
import GHC.TypeLits (Natural)
import OpenSolid.Direction (Direction)
import OpenSolid.DirectionBounds (DirectionBounds)
import {-# SOURCE #-} OpenSolid.DirectionCurve2D (DirectionCurve2D)
import {-# SOURCE #-} OpenSolid.DirectionCurve3D (DirectionCurve3D)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)

type family
  DirectionCurve dimension space =
    directionCurve | directionCurve -> dimension space
  where
  DirectionCurve 2 Void = DirectionCurve2D
  DirectionCurve 3 space = DirectionCurve3D space

class Exists (dimension :: Natural) (space :: Type)

instance Exists 2 Void

instance Exists 3 space

unsafe ::
  Exists dimension space =>
  VectorCurve dimension Unitless space ->
  DirectionCurve dimension space
unwrap ::
  Exists dimension space =>
  DirectionCurve dimension space ->
  VectorCurve dimension Unitless space
value ::
  Exists dimension space =>
  DirectionCurve dimension space ->
  Number ->
  Direction dimension space
bounds ::
  Exists dimension space =>
  DirectionCurve dimension space ->
  Interval Unitless ->
  DirectionBounds dimension space
