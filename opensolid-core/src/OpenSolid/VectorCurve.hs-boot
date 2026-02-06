module OpenSolid.VectorCurve
  ( VectorCurve
  , Exists
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import Data.Void (Void)
import GHC.TypeLits (Natural)
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Vector (Vector)
import OpenSolid.VectorBounds (VectorBounds)
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)

type family
  VectorCurve (dimension :: Natural) (units :: Type) (space :: Type) =
    vectorCurve | vectorCurve -> dimension units space
  where
  VectorCurve 1 units Void = Curve1D units
  VectorCurve 2 units space = VectorCurve2D units space
  VectorCurve 3 units space = VectorCurve3D units space

class Exists (dimension :: Natural) (units :: Type) (space :: Type)

instance Exists 1 units Void

instance Exists 2 units space

instance Exists 3 units space

evaluate ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Number ->
  Vector dimension units space
evaluateBounds ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
derivative ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  VectorCurve dimension units space
