module OpenSolid.Direction
  ( Direction
  , Exists
  , unsafe
  , unwrap
  , parallel
  , perpendicular
  )
where

import Data.Void (Void)
import GHC.TypeLits (Natural)
import {-# SOURCE #-} OpenSolid.Direction2D qualified as Direction2D
import {-# SOURCE #-} OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.Prelude
import OpenSolid.Primitives (Direction2D, Direction3D)
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Sign qualified as Sign
import {-# SOURCE #-} OpenSolid.Vector (Vector)
import {-# SOURCE #-} OpenSolid.Vector qualified as Vector

type family
  Direction (dimension :: Natural) space =
    direction | direction -> dimension space
  where
  Direction 1 Void = Sign
  Direction 2 Void = Direction2D
  Direction 3 space = Direction3D space

class
  ( Vector.Exists dimension Unitless space
  , Negation (Direction dimension space)
  , ApproximateEquality (Direction dimension space) ()
  , DotMultiplication (Direction dimension space) (Direction dimension space) Number
  ) =>
  Exists (dimension :: Natural) (space :: Type)
  where
  unsafeImpl :: Vector dimension Unitless space -> Direction dimension space
  unwrapImpl :: Direction dimension space -> Vector dimension Unitless space
  parallel :: Direction dimension space -> Direction dimension space -> Bool
  perpendicular :: Direction dimension space -> Direction dimension space -> Bool

instance Exists 1 Void where
  unsafeImpl = Quantity.sign
  unwrapImpl = Sign.value
  parallel _ _ = True
  perpendicular _ _ = False

instance Exists 2 Void where
  {-# INLINEABLE unsafeImpl #-}
  unsafeImpl = Direction2D.unsafe
  {-# INLINEABLE unwrapImpl #-}
  unwrapImpl = Direction2D.unwrap
  {-# INLINEABLE parallel #-}
  parallel = Direction2D.parallel
  {-# INLINEABLE perpendicular #-}
  perpendicular = Direction2D.perpendicular

instance Exists 3 space where
  {-# INLINEABLE unsafeImpl #-}
  unsafeImpl = Direction3D.unsafe
  {-# INLINEABLE unwrapImpl #-}
  unwrapImpl = Direction3D.unwrap
  {-# INLINEABLE parallel #-}
  parallel = Direction3D.parallel
  {-# INLINEABLE perpendicular #-}
  perpendicular = Direction3D.perpendicular

{-# INLINE unsafe #-}
unsafe :: Exists dimension space => Vector dimension Unitless space -> Direction dimension space
unsafe = unsafeImpl

{-# INLINE unwrap #-}
unwrap :: Exists dimension space => Direction dimension space -> Vector dimension Unitless space
unwrap = unwrapImpl
