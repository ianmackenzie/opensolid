module OpenSolid.CoordinateSystem
  ( Unitless (..)
  , Linear (..)
  , CoordinateSystem (..)
  )
where

import GHC.Num (Natural)
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import {-# SOURCE #-} OpenSolid.Curve3D (Curve3D)
import {-# SOURCE #-} OpenSolid.Curve3D qualified as Curve3D
import {-# SOURCE #-} OpenSolid.Direction2D qualified as Direction2D
import {-# SOURCE #-} OpenSolid.Direction3D qualified as Direction3D
import {-# SOURCE #-} OpenSolid.DirectionBounds2D (DirectionBounds2D)
import {-# SOURCE #-} OpenSolid.DirectionBounds2D qualified as DirectionBounds2D
import {-# SOURCE #-} OpenSolid.DirectionBounds3D (DirectionBounds3D)
import {-# SOURCE #-} OpenSolid.DirectionBounds3D qualified as DirectionBounds3D
import {-# SOURCE #-} OpenSolid.DirectionCurve2D (DirectionCurve2D)
import {-# SOURCE #-} OpenSolid.DirectionCurve2D qualified as DirectionCurve2D
import {-# SOURCE #-} OpenSolid.DirectionCurve3D (DirectionCurve3D)
import {-# SOURCE #-} OpenSolid.DirectionCurve3D qualified as DirectionCurve3D
import OpenSolid.HasZero (HasZero)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude hiding (Unitless)
import OpenSolid.Primitives
import OpenSolid.Units qualified as Units
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)
import {-# SOURCE #-} OpenSolid.VectorCurve2D qualified as VectorCurve2D
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D qualified as VectorCurve3D

class
  ( DotMultiplication (Direction dimension space) (Direction dimension space) Number
  , Linear dimension Units.Unitless space
  ) =>
  Unitless (dimension :: Natural) (space :: Type)
  where
  type Direction dimension space = direction | direction -> dimension space
  type DirectionBounds dimension space = directionBounds | directionBounds -> dimension space
  type DirectionCurve dimension space = directionCurve | directionCurve -> dimension space

  unsafeDirection :: Vector dimension Units.Unitless space -> Direction dimension space
  unsafeDirectionBounds :: VectorBounds dimension Units.Unitless space -> DirectionBounds dimension space
  unsafeDirectionCurve :: VectorCurve dimension Units.Unitless space -> DirectionCurve dimension space

class
  ( HasZero (Vector dimension units space)
  , Eq (Vector dimension units space)
  , Negation (Vector dimension units space)
  , Addition
      (Vector dimension units space)
      (Vector dimension units space)
      (Vector dimension units space)
  , Subtraction
      (Vector dimension units space)
      (Vector dimension units space)
      (Vector dimension units space)
  , Multiplication Number (Vector dimension units space) (Vector dimension units space)
  , Multiplication (Vector dimension units space) Number (Vector dimension units space)
  , Multiplication (Quantity units) (Direction dimension space) (Vector dimension units space)
  , Multiplication (Direction dimension space) (Quantity units) (Vector dimension units space)
  , DotMultiplication_
      (Vector dimension units space)
      (Vector dimension units space)
      (Quantity (units ?*? units))
  , DotMultiplication (Vector dimension units space) (Direction dimension space) (Quantity units)
  , DotMultiplication (Direction dimension space) (Vector dimension units space) (Quantity units)
  , Division (Vector dimension units space) Number (Vector dimension units space)
  , Division (Vector dimension units space) (Quantity units) (Vector dimension Units.Unitless space)
  , Unitless dimension space
  , Linear dimension Units.Unitless space
  ) =>
  Linear (dimension :: Natural) (units :: Type) (space :: Type)
  where
  type Vector dimension units space = vector | vector -> dimension units space
  type VectorBounds dimension units space = vectorBounds | vectorBounds -> dimension units space
  type VectorCurve dimension units space = vectorCurve | vectorCurve -> dimension units space

  vectorCurveDerivative :: VectorCurve dimension units space -> VectorCurve dimension units space
  vectorCurveIsZero :: Tolerance units => VectorCurve dimension units space -> Bool
  evaluateVectorCurve ::
    VectorCurve dimension units space ->
    Number ->
    Vector dimension units space
  evaluateVectorCurveBounds ::
    VectorCurve dimension units space ->
    Interval Units.Unitless ->
    VectorBounds dimension units space
  vectorCurveSquaredMagnitude_ ::
    VectorCurve dimension units space ->
    Curve1D (units ?*? units)
  normalizeVectorCurve ::
    Tolerance units =>
    VectorCurve dimension units space ->
    VectorCurve dimension Units.Unitless space

class
  ( Addition
      (Point dimension units space)
      (Vector dimension units space)
      (Point dimension units space)
  , Subtraction
      (Point dimension units space)
      (Point dimension units space)
      (Vector dimension units space)
  , Addition
      (Curve dimension units space)
      (Vector dimension units space)
      (Curve dimension units space)
  , Subtraction
      (Curve dimension units space)
      (Vector dimension units space)
      (Curve dimension units space)
  , Addition
      (Curve dimension units space)
      (VectorCurve dimension units space)
      (Curve dimension units space)
  , Subtraction
      (Curve dimension units space)
      (VectorCurve dimension units space)
      (Curve dimension units space)
  , Subtraction
      (Curve dimension units space)
      (Curve dimension units space)
      (VectorCurve dimension units space)
  , Subtraction
      (Curve dimension units space)
      (Point dimension units space)
      (VectorCurve dimension units space)
  , Subtraction
      (Point dimension units space)
      (Curve dimension units space)
      (VectorCurve dimension units space)
  , Linear dimension units space
  ) =>
  CoordinateSystem (dimension :: Natural) (units :: Type) (space :: Type)
  where
  type Point dimension units space = point | point -> dimension units space
  type Bounds dimension units space = bounds | bounds -> dimension units space
  type Curve dimension units space = curve | curve -> dimension units space

  curveDerivative :: Curve dimension units space -> VectorCurve dimension units space

instance Unitless 2 space where
  type Direction 2 space = Direction2D space
  type DirectionBounds 2 space = DirectionBounds2D space
  type DirectionCurve 2 space = DirectionCurve2D space

  {-# INLINE unsafeDirection #-}
  unsafeDirection = Direction2D.unsafe

  {-# INLINE unsafeDirectionBounds #-}
  unsafeDirectionBounds = DirectionBounds2D.unsafe

  {-# INLINE unsafeDirectionCurve #-}
  unsafeDirectionCurve = DirectionCurve2D.unsafe

instance Linear 2 units space where
  type Vector 2 units space = Vector2D units space
  type VectorBounds 2 units space = VectorBounds2D units space
  type VectorCurve 2 units space = VectorCurve2D units space

  vectorCurveDerivative = VectorCurve2D.derivative
  vectorCurveIsZero = VectorCurve2D.isZero
  evaluateVectorCurve = VectorCurve2D.evaluate
  evaluateVectorCurveBounds = VectorCurve2D.evaluateBounds
  vectorCurveSquaredMagnitude_ = VectorCurve2D.squaredMagnitude_
  normalizeVectorCurve = VectorCurve2D.normalize

instance CoordinateSystem 2 units space where
  type Point 2 units space = Point2D units space
  type Bounds 2 units space = Bounds2D units space
  type Curve 2 units space = Curve2D units space

  curveDerivative = Curve2D.derivative

instance Unitless 3 space where
  type Direction 3 space = Direction3D space
  type DirectionBounds 3 space = DirectionBounds3D space
  type DirectionCurve 3 space = DirectionCurve3D space

  {-# INLINE unsafeDirection #-}
  unsafeDirection = Direction3D.unsafe

  {-# INLINE unsafeDirectionBounds #-}
  unsafeDirectionBounds = DirectionBounds3D.unsafe

  {-# INLINE unsafeDirectionCurve #-}
  unsafeDirectionCurve = DirectionCurve3D.unsafe

instance Linear 3 units space where
  type Vector 3 units space = Vector3D units space
  type VectorBounds 3 units space = VectorBounds3D units space
  type VectorCurve 3 units space = VectorCurve3D units space

  vectorCurveDerivative = VectorCurve3D.derivative
  vectorCurveIsZero = VectorCurve3D.isZero
  evaluateVectorCurve = VectorCurve3D.evaluate
  evaluateVectorCurveBounds = VectorCurve3D.evaluateBounds
  vectorCurveSquaredMagnitude_ = VectorCurve3D.squaredMagnitude_
  normalizeVectorCurve = VectorCurve3D.normalize

instance CoordinateSystem 3 Meters space where
  type Point 3 Meters space = Point3D space
  type Bounds 3 Meters space = Bounds3D space
  type Curve 3 Meters space = Curve3D space

  curveDerivative = Curve3D.derivative
