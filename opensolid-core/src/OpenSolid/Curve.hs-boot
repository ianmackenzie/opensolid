module OpenSolid.Curve
  ( Curve
  , Curve2D
  , Curve3D
  , CurveExists
  , Solver
  , Compiled
  , constant
  , pointAt
  , pointOn
  , range
  , startPoint
  , endPoint
  , derivative
  , derivativeAt
  , derivativeRange
  , secondDerivative
  , secondDerivativeAt
  , secondDerivativeRange
  , hasDegenerateStart
  , hasDegenerateEnd
  )
where

import GHC.TypeLits (Natural)
import OpenSolid.Bounds (Bounds)
import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.Interval (Interval)
import OpenSolid.Point (Point)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.Vector (Vector)
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorBounds (VectorBounds)
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve, VectorCurve2D, VectorCurve3D)

type role Curve nominal nominal nominal

data Curve (dimension :: Natural) (units :: Type) (space :: Type)

type Curve2D units = Curve 2 units Void

type Curve3D space = Curve 3 Meters space

class CurveExists (dimension :: Natural) (units :: Type) (space :: Type)

type role Solver nominal nominal nominal

data Solver (dimension :: Natural) (units :: Type) (space :: Type)

instance CurveExists 2 units Void

instance CurveExists 3 Meters space

type Compiled dimension units space =
  CompiledFunction
    Number
    (Point dimension units space)
    (Interval Unitless)
    (Bounds dimension units space)

instance
  (CurveExists dimension1 units1 space1, dimension1 ~ dimension2, space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve dimension1 units1 space1)
    (VectorCurve dimension2 units2 space2)
    (Curve dimension1 units1 space1)

instance
  (CurveExists dimension1 units1 space1, dimension1 ~ dimension2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve dimension1 units1 space1)
    (VectorCurve dimension2 units2 space2)
    (Curve dimension1 units1 space1)

instance
  (CurveExists dimension1 units1 space1, dimension1 ~ dimension2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve dimension1 units1 space1)
    (Curve dimension2 units2 space2)
    (VectorCurve dimension1 units1 space1)

instance
  units1 ~ units2 =>
  Addition (Curve2D units1) (Vector2D units2) (Curve2D units1)

instance
  units1 ~ units2 =>
  Subtraction (Curve2D units1) (Vector2D units2) (Curve2D units1)

instance
  units1 ~ units2 =>
  Subtraction (Point2D units1) (Curve2D units2) (VectorCurve2D units1)

instance Composition (SurfaceFunction1D units) (Curve2D Unitless) (Curve1D units)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition (Curve3D space1) (Vector3D meters space2) (Curve3D space1)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction (Curve3D space1) (Vector3D meters space2) (Curve3D space1)

instance
  space1 ~ space2 =>
  Subtraction (Curve3D space1) (Point3D space2) (VectorCurve3D Meters space1)

instance
  space1 ~ space2 =>
  Subtraction (Point3D space1) (Curve3D space2) (VectorCurve3D Meters space1)

constant ::
  CurveExists dimension units space =>
  Point dimension units space ->
  Curve dimension units space
pointAt ::
  Number ->
  Curve dimension units space ->
  Point dimension units space
pointOn ::
  Curve dimension units space ->
  Number ->
  Point dimension units space
range ::
  Interval Unitless ->
  Curve dimension units space ->
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
derivativeAt ::
  CurveExists dimension units space =>
  Number ->
  Curve dimension units space ->
  Vector dimension units space
derivativeRange ::
  CurveExists dimension units space =>
  Interval Unitless ->
  Curve dimension units space ->
  VectorBounds dimension units space
secondDerivative ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  VectorCurve dimension units space
secondDerivativeAt ::
  CurveExists dimension units space =>
  Number ->
  Curve dimension units space ->
  Vector dimension units space
secondDerivativeRange ::
  CurveExists dimension units space =>
  Interval Unitless ->
  Curve dimension units space ->
  VectorBounds dimension units space
hasDegenerateStart :: CurveExists dimension units space => Curve dimension units space -> Bool
hasDegenerateEnd :: CurveExists dimension units space => Curve dimension units space -> Bool
