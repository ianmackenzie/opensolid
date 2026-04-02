module OpenSolid.DirectionBounds
  ( DirectionBounds
  , Exists
  , unsafe
  , unwrap
  , areDistinct
  , areIndependent
  )
where

import Data.Void (Void)
import OpenSolid.DirectionBounds2D (DirectionBounds2D)
import OpenSolid.DirectionBounds2D qualified as DirectionBounds2D
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.DirectionBounds3D qualified as DirectionBounds3D
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds

type family
  DirectionBounds dimension space =
    directionBounds | directionBounds -> dimension space
  where
  DirectionBounds 1 Void = Interval Unitless
  DirectionBounds 2 Void = DirectionBounds2D
  DirectionBounds 3 space = DirectionBounds3D space

class
  ( VectorBounds.Exists dimension Unitless space
  , Show (DirectionBounds dimension space)
  , Negation (DirectionBounds dimension space)
  , DotMultiplication
      (DirectionBounds dimension space)
      (DirectionBounds dimension space)
      (Interval Unitless)
  ) =>
  Exists dimension (space :: Type)
  where
  unsafeImpl :: VectorBounds dimension Unitless space -> DirectionBounds dimension space
  unwrapImpl :: DirectionBounds dimension space -> VectorBounds dimension Unitless space

instance Exists 1 Void where
  unsafeImpl = id
  unwrapImpl = id

instance Exists 2 Void where
  {-# INLINEABLE unsafeImpl #-}
  unsafeImpl = DirectionBounds2D.unsafe
  {-# INLINEABLE unwrapImpl #-}
  unwrapImpl = DirectionBounds2D.unwrap

instance Exists 3 space where
  {-# INLINEABLE unsafeImpl #-}
  unsafeImpl = DirectionBounds3D.unsafe
  {-# INLINEABLE unwrapImpl #-}
  unwrapImpl = DirectionBounds3D.unwrap

{-# INLINE unsafe #-}
unsafe ::
  Exists dimension space =>
  VectorBounds dimension Unitless space ->
  DirectionBounds dimension space
unsafe = unsafeImpl

{-# INLINE unwrap #-}
unwrap ::
  Exists dimension space =>
  DirectionBounds dimension space ->
  VectorBounds dimension Unitless space
unwrap = unwrapImpl

areDistinct ::
  Exists dimension space =>
  DirectionBounds dimension space ->
  DirectionBounds dimension space ->
  Bool
areDistinct directionBounds1 directionBounds2 =
  VectorBounds.areDistinct (unwrap directionBounds1) (unwrap directionBounds2)

areIndependent ::
  Exists dimension space =>
  DirectionBounds dimension space ->
  DirectionBounds dimension space ->
  Bool
areIndependent directionBounds1 directionBounds2 =
  areDistinct directionBounds1 directionBounds2
    && areDistinct directionBounds1 -directionBounds2
