module OpenSolid.VectorCurve.Nondegenerate
  ( Nondegenerate
  , Exists
  , curve
  , magnitude
  , normalize
  , direction
  , squaredMagnitude_
  , squaredMagnitude
  )
where

import Data.Void (Void)
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.Curve1D qualified as Curve1D
import {-# SOURCE #-} OpenSolid.Curve1D.Nondegenerate qualified as Curve1D.Nondegenerate
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Sign qualified as Sign
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve
import OpenSolid.VectorCurve2D.Nondegenerate qualified as VectorCurve2D.Nondegenerate
import OpenSolid.VectorCurve3D.Nondegenerate qualified as VectorCurve3D.Nondegenerate

type family
  Nondegenerate dimension units space =
    nondegenerate | nondegenerate -> dimension units space
  where
  Nondegenerate 1 units Void = Curve1D.Nondegenerate units
  Nondegenerate 2 units space = VectorCurve2D.Nondegenerate.Nondegenerate units space
  Nondegenerate 3 units space = VectorCurve3D.Nondegenerate.Nondegenerate units space

class
  ( HasUnits (Nondegenerate dimension units space) units
  , Units.Coercion (Nondegenerate dimension units space) (Nondegenerate dimension Unitless space)
  , Units.Coercion (Nondegenerate dimension Unitless space) (Nondegenerate dimension units space)
  ) =>
  Exists dimension units space
  where
  curve :: Nondegenerate dimension units space -> VectorCurve dimension units space
  squaredMagnitude_ :: Nondegenerate dimension units space -> Curve1D.Nondegenerate (units ?*? units)
  magnitude :: Nondegenerate dimension units space -> Curve1D.Nondegenerate units
  normalize :: Nondegenerate dimension units space -> VectorCurve dimension Unitless space

nondegenerateSign1D :: Curve1D units -> Sign
nondegenerateSign1D givenCurve = Quantity.sign (Curve1D.evaluate givenCurve 0.5)

instance Exists 1 units Void where
  curve = Curve1D.Nondegenerate.curve
  squaredMagnitude_ = Curve1D.Nondegenerate.squared_
  magnitude (Curve1D.Nondegenerate givenCurve) =
    Curve1D.Nondegenerate (nondegenerateSign1D givenCurve * givenCurve)
  normalize (Curve1D.Nondegenerate givenCurve) =
    Curve1D.constant (Sign.value (nondegenerateSign1D givenCurve))

instance Exists 2 units space where
  curve = VectorCurve2D.Nondegenerate.curve
  squaredMagnitude_ = VectorCurve2D.Nondegenerate.squaredMagnitude_
  magnitude = VectorCurve2D.Nondegenerate.magnitude
  normalize = VectorCurve2D.Nondegenerate.normalize

instance Exists 3 units space where
  curve = VectorCurve3D.Nondegenerate.curve
  squaredMagnitude_ = VectorCurve3D.Nondegenerate.squaredMagnitude_
  magnitude = VectorCurve3D.Nondegenerate.magnitude
  normalize = VectorCurve3D.Nondegenerate.normalize

squaredMagnitude ::
  ( Exists dimension units1 space
  , VectorCurve.Exists dimension units1 space
  , Units.Squared units1 units2
  ) =>
  Nondegenerate dimension units1 space ->
  Curve1D.Nondegenerate units2
squaredMagnitude = Units.specialize . squaredMagnitude_

direction ::
  ( Exists dimension units space
  , DirectionCurve.Exists dimension space
  ) =>
  Nondegenerate dimension units space ->
  DirectionCurve dimension space
direction = DirectionCurve.unsafe . normalize
