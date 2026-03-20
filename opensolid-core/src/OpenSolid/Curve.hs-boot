module OpenSolid.Curve
  ( Curve
  , Exists
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
  )
where

import GHC.TypeLits (Natural)
import {-# SOURCE #-} OpenSolid.Bounds (Bounds)
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve3D (Curve3D)
import {-# SOURCE #-} OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.Interval (Interval)
import OpenSolid.Nondegenerate (IsDegenerate)
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Vector (Vector)
import OpenSolid.VectorBounds (VectorBounds)
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

type family Curve dimension units space = curve | curve -> dimension units space where
  Curve 2 units space = Curve2D units space
  Curve 3 Meters space = Curve3D space

class Exists (dimension :: Natural) (units :: Type) (space :: Type)

data HasSingularity

point ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Point dimension units space
bounds ::
  Exists dimension units space =>
  Curve dimension units space ->
  Interval Unitless ->
  Bounds dimension units space
startPoint ::
  Exists dimension units space =>
  Curve dimension units space ->
  Point dimension units space
endPoint ::
  Exists dimension units space =>
  Curve dimension units space ->
  Point dimension units space
derivative ::
  Exists dimension units space =>
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
