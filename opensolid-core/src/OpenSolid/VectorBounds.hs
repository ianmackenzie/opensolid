module OpenSolid.VectorBounds
  ( VectorBounds
  , Exists
  , includes
  , center
  , squaredMagnitude_
  , magnitude
  )
where

import Data.Void (Void)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector
import OpenSolid.VectorBounds2D (VectorBounds2D)
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorBounds3D (VectorBounds3D)
import OpenSolid.VectorBounds3D qualified as VectorBounds3D

type family
  VectorBounds dimension units space =
    vectorBounds | vectorBounds -> dimension units space
  where
  VectorBounds 1 units Void = Interval units
  VectorBounds 2 units space = VectorBounds2D units space
  VectorBounds 3 units space = VectorBounds3D units space

class
  ( Vector.Exists dimension units space
  , Exists dimension Unitless space
  , HasUnits (VectorBounds dimension units space) units
  , Units.Coercion (VectorBounds dimension units space) (VectorBounds dimension Unitless space)
  , Units.Coercion (VectorBounds dimension Unitless space) (VectorBounds dimension units space)
  , Negation (VectorBounds dimension units space)
  , Addition
      (VectorBounds dimension units space)
      (VectorBounds dimension units space)
      (VectorBounds dimension units space)
  , Subtraction
      (VectorBounds dimension units space)
      (VectorBounds dimension units space)
      (VectorBounds dimension units space)
  , DotMultiplication
      (VectorBounds dimension units space)
      (Vector dimension Unitless space)
      (Interval units)
  , DotMultiplication
      (Vector dimension Unitless space)
      (VectorBounds dimension units space)
      (Interval units)
  , DotMultiplication
      (VectorBounds dimension Unitless space)
      (Vector dimension units space)
      (Interval units)
  , DotMultiplication
      (Vector dimension units space)
      (VectorBounds dimension Unitless space)
      (Interval units)
  , DotMultiplication
      (VectorBounds dimension units space)
      (VectorBounds dimension Unitless space)
      (Interval units)
  , DotMultiplication
      (VectorBounds dimension Unitless space)
      (VectorBounds dimension units space)
      (Interval units)
  ) =>
  Exists dimension units space
  where
  includes :: Vector dimension units space -> VectorBounds dimension units space -> Bool
  center :: VectorBounds dimension units space -> Vector dimension units space
  squaredMagnitude_ :: VectorBounds dimension units space -> Interval (units ?*? units)
  magnitude :: VectorBounds dimension units space -> Interval units

instance Exists 2 units space where
  includes = VectorBounds2D.includes
  center = VectorBounds2D.center
  squaredMagnitude_ = VectorBounds2D.squaredMagnitude_
  magnitude = VectorBounds2D.magnitude

instance Exists 3 units space where
  includes = VectorBounds3D.includes
  center = VectorBounds3D.center
  squaredMagnitude_ = VectorBounds3D.squaredMagnitude_
  magnitude = VectorBounds3D.magnitude
