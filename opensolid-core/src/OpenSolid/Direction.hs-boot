module OpenSolid.Direction
  ( Direction
  , Exists
  , unsafe
  , unwrap
  )
where

import GHC.TypeLits (Natural)
import {-# SOURCE #-} OpenSolid.Direction2D (Direction2D)
import {-# SOURCE #-} OpenSolid.Direction3D (Direction3D)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Vector (Vector)

type family
  Direction (dimension :: Natural) space =
    direction | direction -> dimension space
  where
  Direction 2 space = Direction2D space
  Direction 3 space = Direction3D space

class Exists (dimension :: Natural) (space :: Type)

instance Exists 2 space

instance Exists 3 space

unsafe :: Exists dimension space => Vector dimension Unitless space -> Direction dimension space
unwrap :: Exists dimension space => Direction dimension space -> Vector dimension Unitless space
