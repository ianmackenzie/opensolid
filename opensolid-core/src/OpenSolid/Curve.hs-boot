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
  , singular0
  , singular1
  )
where

import GHC.TypeLits (Natural)
import {-# SOURCE #-} OpenSolid.Bounds (Bounds)
import {-# SOURCE #-} OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.Interval (Interval)
import OpenSolid.Nondegenerate (IsDegenerate)
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Vector (Vector)
import OpenSolid.VectorBounds (VectorBounds)
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)
import {-# SOURCE #-} OpenSolid.VectorCurve qualified as VectorCurve

type role Curve nominal nominal nominal

data Curve (dimension :: Natural) (units :: Type) (space :: Type)

class Exists (dimension :: Natural) (units :: Type) (space :: Type)

data HasSingularity

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
