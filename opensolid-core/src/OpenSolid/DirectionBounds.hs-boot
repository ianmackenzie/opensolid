module OpenSolid.DirectionBounds
  ( DirectionBounds
  , Exists
  , unsafe
  )
where

import Data.Void (Void)
import GHC.TypeLits (Natural)
import OpenSolid.DirectionBounds2D (DirectionBounds2D)
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorBounds (VectorBounds)

type family
  DirectionBounds dimension space =
    directionBounds | directionBounds -> dimension space
  where
  DirectionBounds 1 Void = Interval Unitless
  DirectionBounds 2 Void = DirectionBounds2D
  DirectionBounds 3 space = DirectionBounds3D space

class Exists (dimension :: Natural) (space :: Type)

instance Exists 1 Void

instance Exists 2 Void

instance Exists 3 space

unsafe ::
  Exists dimension space =>
  VectorBounds dimension Unitless space ->
  DirectionBounds dimension space
