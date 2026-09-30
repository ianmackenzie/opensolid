module OpenSolid.Curve
  ( Curve
  , Curve2D
  , UvCurve
  , Curve3D
  , IsDegenerate
  , CurveExists
  , Solver
  , Compiled
  , startPoint
  , endPoint
  , derivative
  , secondDerivative
  , hasDegenerateStart
  , hasDegenerateEnd
  )
where

import GHC.TypeLits (Natural)
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve1D (Curve1D)
import OpenSolid.Interval (Interval)
import OpenSolid.Point (Point)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve, VectorCurve2D, VectorCurve3D)

type role Curve nominal nominal nominal

data Curve (dimension :: Natural) (units :: Type) (space :: Type)

type Curve2D units = Curve 2 units Void

type UvCurve = Curve 2 Unitless Void

type Curve3D space = Curve 3 Meters space

type role IsDegenerate nominal nominal nominal

data IsDegenerate (dimension :: Natural) (units :: Type) (space :: Type)

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

instance At (Curve2D units) Number (Point2D units)

instance At (Curve3D space) Number (Point3D space)

instance At (Curve2D units) (Interval Unitless) (Bounds2D units)

instance At (Curve3D space) (Interval Unitless) (Bounds3D space)

instance
  (CurveExists dimension1 units1 space1, dimension1 ~ dimension2, space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve dimension1 units1 space1)
    (Curve dimension2 units2 space2)
    (VectorCurve dimension1 units1 space1)

instance
  units1 ~ units2 =>
  Subtraction (Point2D units1) (Curve2D units2) (VectorCurve2D units1)

instance Composition () (SurfaceFunction1D units) UvCurve (Curve1D units)

instance
  space1 ~ space2 =>
  Subtraction (Curve3D space1) (Point3D space2) (VectorCurve3D Meters space1)

instance
  space1 ~ space2 =>
  Subtraction (Point3D space1) (Curve3D space2) (VectorCurve3D Meters space1)

startPoint ::
  Curve dimension units space ->
  Point dimension units space
endPoint ::
  Curve dimension units space ->
  Point dimension units space
derivative ::
  Curve dimension units space ->
  VectorCurve dimension units space
secondDerivative ::
  CurveExists dimension units space =>
  Curve dimension units space ->
  VectorCurve dimension units space
hasDegenerateStart :: Curve dimension units space -> Bool
hasDegenerateEnd :: Curve dimension units space -> Bool
