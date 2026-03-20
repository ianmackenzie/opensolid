module OpenSolid.VectorBounds
  ( VectorBounds
  , Exists
  , includes
  , center
  , squaredMagnitude_
  , magnitude
  , normalize
  , diameter
  , isResolved
  , areDistinct
  , areIndependent
  , erase
  , unerase
  , coerce
  )
where

import Data.Void (Void)
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
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
  , Multiplication Number (VectorBounds dimension units space) (VectorBounds dimension units space)
  , Multiplication (VectorBounds dimension units space) Number (VectorBounds dimension units space)
  , Multiplication (Quantity units) (VectorBounds dimension Unitless space) (VectorBounds dimension units space)
  , Multiplication (VectorBounds dimension Unitless space) (Quantity units) (VectorBounds dimension units space)
  , Division (VectorBounds dimension units space) Number (VectorBounds dimension units space)
  , Division (VectorBounds dimension units space) (Quantity units) (VectorBounds dimension Unitless space)
  , Multiplication (Interval Unitless) (VectorBounds dimension units space) (VectorBounds dimension units space)
  , Multiplication (VectorBounds dimension units space) (Interval Unitless) (VectorBounds dimension units space)
  , Multiplication (Interval units) (VectorBounds dimension Unitless space) (VectorBounds dimension units space)
  , Multiplication (VectorBounds dimension Unitless space) (Interval units) (VectorBounds dimension units space)
  , Division (VectorBounds dimension units space) (Interval Unitless) (VectorBounds dimension units space)
  , Division (VectorBounds dimension units space) (Interval units) (VectorBounds dimension Unitless space)
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
  , DotMultiplication_
      (VectorBounds dimension units space)
      (VectorBounds dimension units space)
      (Interval (units ?*? units))
  ) =>
  Exists dimension units space
  where
  includes :: Vector dimension units space -> VectorBounds dimension units space -> Bool
  center :: VectorBounds dimension units space -> Vector dimension units space
  squaredMagnitude_ :: VectorBounds dimension units space -> Interval (units ?*? units)
  magnitude :: VectorBounds dimension units space -> Interval units
  normalize :: VectorBounds dimension units space -> VectorBounds dimension Unitless space
  diameter :: VectorBounds dimension units space -> Quantity units
  isResolved :: VectorBounds dimension units space -> Bool
  areDistinct :: VectorBounds dimension units space -> VectorBounds dimension units space -> Bool
  areIndependent :: VectorBounds dimension units space -> VectorBounds dimension units space -> Bool

instance Exists 1 units Void where
  includes = Interval.includes
  center = Interval.midpoint
  squaredMagnitude_ = Interval.squared_
  magnitude = Interval.abs
  normalize (Interval low high)
    | low > Quantity.zero = Interval.constant 1.0
    | high < Quantity.zero = Interval.constant -1.0
    | otherwise = Interval -1.0 1.0
  diameter = Interval.width
  isResolved = Interval.isResolved
  areDistinct = Interval.areDistinct
  areIndependent _ _ = False

instance Exists 2 units space where
  includes = VectorBounds2D.includes
  center = VectorBounds2D.center
  squaredMagnitude_ = VectorBounds2D.squaredMagnitude_
  magnitude = VectorBounds2D.magnitude
  normalize = VectorBounds2D.normalize
  diameter = VectorBounds2D.diameter
  isResolved = VectorBounds2D.isResolved
  areDistinct = VectorBounds2D.areDistinct
  areIndependent = VectorBounds2D.areIndependent

instance Exists 3 units space where
  includes = VectorBounds3D.includes
  center = VectorBounds3D.center
  squaredMagnitude_ = VectorBounds3D.squaredMagnitude_
  magnitude = VectorBounds3D.magnitude
  normalize = VectorBounds3D.normalize
  diameter = VectorBounds3D.diameter
  isResolved = VectorBounds3D.isResolved
  areDistinct = VectorBounds3D.areDistinct
  areIndependent = VectorBounds3D.areIndependent

{-# INLINE erase #-}
erase ::
  Exists dimension units space =>
  VectorBounds dimension units space ->
  VectorBounds dimension Unitless space
erase = Units.erase

{-# INLINE unerase #-}
unerase ::
  Exists dimension units space =>
  VectorBounds dimension Unitless space ->
  VectorBounds dimension units space
unerase = Units.unerase

{-# INLINE coerce #-}
coerce ::
  (Exists dimension units1 space, Exists dimension units2 space) =>
  VectorBounds dimension units1 space ->
  VectorBounds dimension units2 space
coerce = unerase . erase
