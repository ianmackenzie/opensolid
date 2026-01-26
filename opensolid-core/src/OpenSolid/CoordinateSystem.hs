module OpenSolid.CoordinateSystem
  ( Directional (..)
  , Generic (..)
  , Vectorial (..)
  , CoordinateSystem (..)
  )
where

import GHC.Num (Natural)
import {-# SOURCE #-} OpenSolid.Bounds2D qualified as Bounds2D
import {-# SOURCE #-} OpenSolid.Bounds3D qualified as Bounds3D
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.Curve1D qualified as Curve1D
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
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction2D (DirectionSurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction2D qualified as DirectionSurfaceFunction2D
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction3D (DirectionSurfaceFunction3D)
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction3D qualified as DirectionSurfaceFunction3D
import OpenSolid.HasZero (HasZero)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Bounds2D (Bounds2D)
  , Bounds3D (Bounds3D)
  , Direction2D
  , Direction3D
  , Point2D
  , Point3D
  , Vector2D
  , Vector3D
  , VectorBounds2D
  , VectorBounds3D
  )
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction3D (SurfaceFunction3D)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)
import {-# SOURCE #-} OpenSolid.VectorCurve2D qualified as VectorCurve2D
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D qualified as VectorCurve3D
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

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
  , Addition
      (VectorCurve dimension units space)
      (VectorCurve dimension units space)
      (VectorCurve dimension units space)
  , Addition
      (VectorSurfaceFunction dimension units space)
      (VectorSurfaceFunction dimension units space)
      (VectorSurfaceFunction dimension units space)
  , Multiplication Number (Vector dimension units space) (Vector dimension units space)
  , Multiplication (Vector dimension units space) Number (Vector dimension units space)
  , Division (Vector dimension units space) Number (Vector dimension units space)
  , Division (Vector dimension units space) (Quantity units) (Vector dimension Unitless space)
  , Multiplication Number (VectorCurve dimension units space) (VectorCurve dimension units space)
  , Multiplication (VectorCurve dimension units space) Number (VectorCurve dimension units space)
  , Multiplication
      (Curve1D Unitless)
      (VectorCurve dimension units space)
      (VectorCurve dimension units space)
  , Multiplication
      (VectorCurve dimension units space)
      (Curve1D Unitless)
      (VectorCurve dimension units space)
  , Multiplication
      Number
      (VectorSurfaceFunction dimension units space)
      (VectorSurfaceFunction dimension units space)
  , Multiplication
      (VectorSurfaceFunction dimension units space)
      Number
      (VectorSurfaceFunction dimension units space)
  , Multiplication
      (SurfaceFunction1D Unitless)
      (VectorSurfaceFunction dimension units space)
      (VectorSurfaceFunction dimension units space)
  , Multiplication
      (VectorSurfaceFunction dimension units space)
      (SurfaceFunction1D Unitless)
      (VectorSurfaceFunction dimension units space)
  , Composition
      (Curve1D Unitless)
      (VectorCurve dimension units space)
      (VectorCurve dimension units space)
  , Composition
      (SurfaceFunction2D Unitless UvSpace)
      (VectorSurfaceFunction dimension units space)
      (VectorSurfaceFunction dimension units space)
  ) =>
  Generic (dimension :: Natural) (units :: Type) (space :: Type)
  where
  type Vector dimension units space = vector | vector -> dimension units space
  type VectorBounds dimension units space = vectorBounds | vectorBounds -> dimension units space
  type VectorCurve dimension units space = vectorCurve | vectorCurve -> dimension units space
  type VectorSurfaceFunction dimension units space = vectorSurfaceFunction | vectorSurfaceFunction -> dimension units space

  vectorCurveDerivative :: VectorCurve dimension units space -> VectorCurve dimension units space
  evaluateVectorCurve ::
    VectorCurve dimension units space ->
    Number ->
    Vector dimension units space
  evaluateVectorCurveBounds ::
    VectorCurve dimension units space ->
    Interval Unitless ->
    VectorBounds dimension units space

  vectorSurfaceFunctionDerivative ::
    SurfaceParameter ->
    VectorSurfaceFunction dimension units space ->
    VectorSurfaceFunction dimension units space

class
  ( DotMultiplication (Direction dimension space) (Direction dimension space) Number
  , Vectorial dimension Unitless space
  ) =>
  Directional (dimension :: Natural) (space :: Type)
  where
  type Direction dimension space = direction | direction -> dimension space
  type DirectionBounds dimension space = directionBounds | directionBounds -> dimension space
  type DirectionCurve dimension space = directionCurve | directionCurve -> dimension space
  type DirectionSurfaceFunction dimension space = directionSurfaceFunction | directionSurfaceFunction -> dimension space

  unsafeDirection :: Vector dimension Unitless space -> Direction dimension space
  unsafeDirectionBounds :: VectorBounds dimension Unitless space -> DirectionBounds dimension space
  unsafeDirectionCurve :: VectorCurve dimension Unitless space -> DirectionCurve dimension space
  unsafeDirectionSurfaceFunction :: VectorSurfaceFunction dimension Unitless space -> DirectionSurfaceFunction dimension space

class
  ( Multiplication (Quantity units) (Direction dimension space) (Vector dimension units space)
  , Multiplication (Direction dimension space) (Quantity units) (Vector dimension units space)
  , DotMultiplication_
      (Vector dimension units space)
      (Vector dimension units space)
      (Quantity (units ?*? units))
  , DotMultiplication
      (Vector dimension units space)
      (Direction dimension space)
      (Quantity units)
  , DotMultiplication
      (Direction dimension space)
      (Vector dimension units space)
      (Quantity units)
  , Generic dimension units space
  , Directional dimension space
  ) =>
  Vectorial (dimension :: Natural) (units :: Type) (space :: Type)
  where
  vectorCurveSquaredMagnitude_ ::
    VectorCurve dimension units space ->
    Curve1D (units ?*? units)
  vectorCurveIsZero :: Tolerance units => VectorCurve dimension units space -> Bool
  normalizeVectorCurve ::
    Tolerance units =>
    VectorCurve dimension units space ->
    VectorCurve dimension Unitless space

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
  , Intersects (Point dimension units space) (Bounds dimension units space) units
  , Intersects (Bounds dimension units space) (Point dimension units space) units
  , Intersects (Bounds dimension units space) (Bounds dimension units space) units
  , Vectorial dimension units space
  ) =>
  CoordinateSystem (dimension :: Natural) (units :: Type) (space :: Type)
  where
  type Point dimension units space = point | point -> dimension units space
  type Bounds dimension units space = bounds | bounds -> dimension units space
  type Curve dimension units space = curve | curve -> dimension units space
  type SurfaceFunction dimension units space = surfaceFunction | surfaceFunction -> dimension units space

  cyclicBoundsCoordinate :: Int -> Bounds dimension units space -> Interval units
  boundsContains ::
    Bounds dimension units space ->
    Bounds dimension units space ->
    Bool
  boundsAggregate2 ::
    Bounds dimension units space ->
    Bounds dimension units space ->
    Bounds dimension units space

  curveDerivative :: Curve dimension units space -> VectorCurve dimension units space

instance Generic 1 units () where
  type Vector 1 units () = Quantity units
  type VectorBounds 1 units () = Interval units
  type VectorCurve 1 units () = Curve1D units
  type VectorSurfaceFunction 1 units () = SurfaceFunction1D units

  vectorCurveDerivative = Curve1D.derivative
  evaluateVectorCurve = Curve1D.evaluate
  evaluateVectorCurveBounds = Curve1D.evaluateBounds

  vectorSurfaceFunctionDerivative = SurfaceFunction1D.derivative

instance Generic 2 units space where
  type Vector 2 units space = Vector2D units space
  type VectorBounds 2 units space = VectorBounds2D units space
  type VectorCurve 2 units space = VectorCurve2D units space
  type VectorSurfaceFunction 2 units space = VectorSurfaceFunction2D units space

  vectorCurveDerivative = VectorCurve2D.derivative
  evaluateVectorCurve = VectorCurve2D.evaluate
  evaluateVectorCurveBounds = VectorCurve2D.evaluateBounds

  vectorSurfaceFunctionDerivative = VectorSurfaceFunction2D.derivative

instance Directional 2 space where
  type Direction 2 space = Direction2D space
  type DirectionBounds 2 space = DirectionBounds2D space
  type DirectionCurve 2 space = DirectionCurve2D space
  type DirectionSurfaceFunction 2 space = DirectionSurfaceFunction2D space

  {-# INLINE unsafeDirection #-}
  unsafeDirection = Direction2D.unsafe

  {-# INLINE unsafeDirectionBounds #-}
  unsafeDirectionBounds = DirectionBounds2D.unsafe

  unsafeDirectionCurve = DirectionCurve2D.unsafe
  unsafeDirectionSurfaceFunction = DirectionSurfaceFunction2D.unsafe

instance Vectorial 2 units space where
  vectorCurveSquaredMagnitude_ = VectorCurve2D.squaredMagnitude_
  vectorCurveIsZero = VectorCurve2D.isZero
  normalizeVectorCurve = VectorCurve2D.normalize

instance CoordinateSystem 2 units space where
  type Point 2 units space = Point2D units space
  type Bounds 2 units space = Bounds2D units space
  type Curve 2 units space = Curve2D units space
  type SurfaceFunction 2 units space = SurfaceFunction2D units space

  cyclicBoundsCoordinate i (Bounds2D x y) = case i `mod` 2 of 0 -> x; _ -> y
  boundsContains = Bounds2D.contains
  boundsAggregate2 = Bounds2D.aggregate2

  curveDerivative = Curve2D.derivative

instance Generic 3 units space where
  type Vector 3 units space = Vector3D units space
  type VectorBounds 3 units space = VectorBounds3D units space
  type VectorCurve 3 units space = VectorCurve3D units space
  type VectorSurfaceFunction 3 units space = VectorSurfaceFunction3D units space

  vectorCurveDerivative = VectorCurve3D.derivative
  evaluateVectorCurve = VectorCurve3D.evaluate
  evaluateVectorCurveBounds = VectorCurve3D.evaluateBounds

  vectorSurfaceFunctionDerivative = VectorSurfaceFunction3D.derivative

instance Directional 3 space where
  type Direction 3 space = Direction3D space
  type DirectionBounds 3 space = DirectionBounds3D space
  type DirectionCurve 3 space = DirectionCurve3D space
  type DirectionSurfaceFunction 3 space = DirectionSurfaceFunction3D space

  {-# INLINE unsafeDirection #-}
  unsafeDirection = Direction3D.unsafe

  {-# INLINE unsafeDirectionBounds #-}
  unsafeDirectionBounds = DirectionBounds3D.unsafe

  unsafeDirectionCurve = DirectionCurve3D.unsafe
  unsafeDirectionSurfaceFunction = DirectionSurfaceFunction3D.unsafe

instance Vectorial 3 units space where
  vectorCurveSquaredMagnitude_ = VectorCurve3D.squaredMagnitude_
  vectorCurveIsZero = VectorCurve3D.isZero
  normalizeVectorCurve = VectorCurve3D.normalize

instance CoordinateSystem 3 Meters space where
  type Point 3 Meters space = Point3D space
  type Bounds 3 Meters space = Bounds3D space
  type Curve 3 Meters space = Curve3D space
  type SurfaceFunction 3 Meters space = SurfaceFunction3D space

  cyclicBoundsCoordinate i (Bounds3D x y z) = case i `mod` 3 of 0 -> x; 1 -> y; _ -> z
  boundsContains = Bounds3D.contains
  boundsAggregate2 = Bounds3D.aggregate2

  curveDerivative = Curve3D.derivative
