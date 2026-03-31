module OpenSolid.Curve
  ( Curve
  , Exists
  , Compiled
  , constant
  , point
  , bounds
  , startPoint
  , endPoint
  , derivative
  , derivativeValue
  , derivativeBounds
  , secondDerivative
  , secondDerivativeValue
  , secondDerivativeBounds
  , tangentDirection
  , curvatureVector_
  , singular0
  , singular1
  )
where

import GHC.TypeLits (Natural)
import {-# SOURCE #-} OpenSolid.Bounds (Bounds)
import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import {-# SOURCE #-} OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.Interval (Interval)
import OpenSolid.Nondegenerate (IsDegenerate)
import OpenSolid.Point (Point)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.UvSpace (UvSpace)
import OpenSolid.Vector (Vector)
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorBounds (VectorBounds)
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)
import {-# SOURCE #-} OpenSolid.VectorCurve3D (VectorCurve3D)

type role Curve nominal nominal nominal

data Curve (dimension :: Natural) (units :: Type) (space :: Type)

class Exists (dimension :: Natural) (units :: Type) (space :: Type)

instance Exists 2 units space

instance Exists 3 Meters space

type Compiled dimension units space =
  CompiledFunction
    Number
    (Point dimension units space)
    (Interval Unitless)
    (Bounds dimension units space)

data HasSingularity

instance
  (Exists dimension1 units1 space1, dimension1 ~ dimension2, space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve dimension1 units1 space1)
    (VectorCurve dimension2 units2 space2)
    (Curve dimension1 units1 space1)

instance
  (Exists dimension1 units1 space1, dimension1 ~ dimension2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve dimension1 units1 space1)
    (VectorCurve dimension2 units2 space2)
    (Curve dimension1 units1 space1)

instance
  (Exists dimension1 units1 space1, dimension1 ~ dimension2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve dimension1 units1 space1)
    (Curve dimension2 units2 space2)
    (VectorCurve dimension1 units1 space1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve 2 units1 space1)
    (Vector2D units2 space2)
    (Curve 2 units1 space1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve 2 units1 space1)
    (Vector2D units2 space2)
    (Curve 2 units1 space1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2D units1 space1)
    (Curve 2 units2 space2)
    (VectorCurve2D units1 space1)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction1D units)
    (Curve 2 unitless uvSpace)
    (Curve1D units)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (Curve 3 Meters space1)
    (Vector3D meters space2)
    (Curve 3 Meters space1)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (Curve 3 Meters space1)
    (Vector3D meters space2)
    (Curve 3 Meters space1)

instance
  space1 ~ space2 =>
  Subtraction
    (Curve 3 Meters space1)
    (Point3D space2)
    (VectorCurve3D Meters space1)

instance
  space1 ~ space2 =>
  Subtraction
    (Point3D space1)
    (Curve 3 Meters space2)
    (VectorCurve3D Meters space1)

constant ::
  Exists dimension units space =>
  Point dimension units space ->
  Curve dimension units space
point ::
  Curve dimension units space ->
  Number ->
  Point dimension units space
bounds ::
  Curve dimension units space ->
  Interval Unitless ->
  Bounds dimension units space
startPoint ::
  Curve dimension units space ->
  Point dimension units space
endPoint ::
  Curve dimension units space ->
  Point dimension units space
derivative ::
  Curve dimension units space ->
  VectorCurve dimension units space
derivativeValue ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Vector dimension units space
derivativeBounds ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
secondDerivative ::
  Exists dimension units space =>
  Curve dimension units space ->
  VectorCurve dimension units space
secondDerivativeValue ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Vector dimension units space
secondDerivativeBounds ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
tangentDirection ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result IsDegenerate (DirectionCurve dimension space)
curvatureVector_ ::
  ( Exists dimension units space
  , VectorCurve.Exists dimension (Unitless ?/? units) space
  , Tolerance units
  ) =>
  Curve dimension units space ->
  Result HasSingularity (VectorCurve dimension (Unitless ?/? units) space)
singular0 :: Exists dimension units space => Curve dimension units space -> Bool
singular1 :: Exists dimension units space => Curve dimension units space -> Bool
