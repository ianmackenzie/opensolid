module OpenSolid.Bounds (Bounds, Exists) where

import GHC.TypeLits (Natural)
import {-# SOURCE #-} OpenSolid.Bounds2D (Bounds2D)
import {-# SOURCE #-} OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Prelude

type family Bounds dimension units space = bounds | bounds -> dimension units space where
  Bounds 2 units Void = Bounds2D units
  Bounds 3 Meters space = Bounds3D space

class Exists (dimension :: Natural) (units :: Type) (space :: Type)

instance Exists 2 units Void

instance Exists 3 Meters space
