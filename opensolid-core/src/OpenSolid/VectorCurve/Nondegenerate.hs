module OpenSolid.VectorCurve.Nondegenerate
  ( Nondegenerate
  , Exists
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
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Sign qualified as Sign
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve
import OpenSolid.VectorCurve2D.Nondegenerate qualified as VectorCurve2D.Nondegenerate
import OpenSolid.VectorCurve3D.Nondegenerate qualified as VectorCurve3D.Nondegenerate

class
  ( HasUnits (Nondegenerate (VectorCurve dimension units space)) units
  , Units.Coercion
      (Nondegenerate (VectorCurve dimension units space))
      (Nondegenerate (VectorCurve dimension Unitless space))
  , Units.Coercion
      (Nondegenerate (VectorCurve dimension Unitless space))
      (Nondegenerate (VectorCurve dimension units space))
  ) =>
  Exists dimension units space
  where
  squaredMagnitude_ ::
    Nondegenerate (VectorCurve dimension units space) ->
    Nondegenerate (Curve1D (units ?*? units))
  magnitude ::
    Nondegenerate (VectorCurve dimension units space) ->
    Nondegenerate (Curve1D units)
  normalize ::
    Nondegenerate (VectorCurve dimension units space) ->
    VectorCurve dimension Unitless space

nondegenerateSign1D :: Curve1D units -> Sign
nondegenerateSign1D curve = Quantity.sign (Curve1D.evaluate curve 0.5)

instance Exists 1 units Void where
  squaredMagnitude_ = Curve1D.Nondegenerate.squared_
  magnitude (Nondegenerate curve) = Nondegenerate (nondegenerateSign1D curve * curve)
  normalize (Nondegenerate curve) = Curve1D.constant (Sign.value (nondegenerateSign1D curve))

instance Exists 2 units space where
  squaredMagnitude_ = VectorCurve2D.Nondegenerate.squaredMagnitude_
  magnitude = VectorCurve2D.Nondegenerate.magnitude
  normalize = VectorCurve2D.Nondegenerate.normalize

instance Exists 3 units space where
  squaredMagnitude_ = VectorCurve3D.Nondegenerate.squaredMagnitude_
  magnitude = VectorCurve3D.Nondegenerate.magnitude
  normalize = VectorCurve3D.Nondegenerate.normalize

squaredMagnitude ::
  ( Exists dimension units1 space
  , VectorCurve.Exists dimension units1 space
  , Units.Squared units1 units2
  ) =>
  Nondegenerate (VectorCurve dimension units1 space) ->
  Nondegenerate (Curve1D units2)
squaredMagnitude = Units.specialize . squaredMagnitude_

direction ::
  ( Exists dimension units space
  , DirectionCurve.Exists dimension space
  ) =>
  Nondegenerate (VectorCurve dimension units space) ->
  DirectionCurve dimension space
direction = DirectionCurve.unsafe . normalize
