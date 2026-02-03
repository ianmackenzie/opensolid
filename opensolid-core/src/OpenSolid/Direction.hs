module OpenSolid.Direction
  ( Direction
  , Exists
  , unsafe
  , unwrap
  )
where

import GHC.TypeLits (Natural)
import {-# SOURCE #-} OpenSolid.Direction2D qualified as Direction2D
import {-# SOURCE #-} OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.Prelude
import OpenSolid.Primitives (Direction2D, Direction3D)
import {-# SOURCE #-} OpenSolid.Vector (Vector)
import {-# SOURCE #-} OpenSolid.Vector qualified as Vector

type family
  Direction (dimension :: Natural) space =
    direction | direction -> dimension space
  where
  Direction 2 space = Direction2D space
  Direction 3 space = Direction3D space

class
  ( Vector.Exists dimension Unitless space
  , DotMultiplication (Direction dimension space) (Direction dimension space) Number
  ) =>
  Exists (dimension :: Natural) (space :: Type)
  where
  unsafeImpl :: Vector dimension Unitless space -> Direction dimension space
  unwrapImpl :: Direction dimension space -> Vector dimension Unitless space

instance Exists 2 space where
  {-# INLINEABLE unsafeImpl #-}
  unsafeImpl = Direction2D.unsafe
  {-# INLINEABLE unwrapImpl #-}
  unwrapImpl = Direction2D.unwrap

instance Exists 3 space where
  {-# INLINEABLE unsafeImpl #-}
  unsafeImpl = Direction3D.unsafe
  {-# INLINEABLE unwrapImpl #-}
  unwrapImpl = Direction3D.unwrap

{-# INLINE unsafe #-}
unsafe :: Exists dimension space => Vector dimension Unitless space -> Direction dimension space
unsafe = unsafeImpl

{-# INLINE unwrap #-}
unwrap :: Exists dimension space => Direction dimension space -> Vector dimension Unitless space
unwrap = unwrapImpl
