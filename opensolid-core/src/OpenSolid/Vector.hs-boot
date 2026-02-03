module OpenSolid.Vector (Vector, Exists) where

import Data.Void (Void)
import GHC.TypeLits (Natural)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Vector2D (Vector2D)
import {-# SOURCE #-} OpenSolid.Vector3D (Vector3D)

type family
  Vector (dimension :: Natural) (units :: Type) (space :: Type) =
    (vector :: Type) | vector -> dimension units space
  where
  Vector 1 units Void = Quantity units
  Vector 2 units space = Vector2D units space
  Vector 3 units space = Vector3D units space

class Exists (dimension :: Natural) (units :: Type) (space :: Type)

instance Exists 1 units Void

instance Exists 2 units space

instance Exists 3 units space
