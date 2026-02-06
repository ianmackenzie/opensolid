module OpenSolid.Curve
  ( Curve
  , Exists
  , IsPoint
  , evaluate
  , evaluateBounds
  , startPoint
  , endPoint
  , derivative
  , tangentDirection
  )
where

import GHC.TypeLits (Natural)
import {-# SOURCE #-} OpenSolid.Bounds (Bounds)
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve3D (Curve3D)
import {-# SOURCE #-} OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.Interval (Interval)
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.VectorCurve (VectorCurve)

type family Curve dimension units space = curve | curve -> dimension units space where
  Curve 2 units space = Curve2D units space
  Curve 3 Meters space = Curve3D space

class Exists (dimension :: Natural) (units :: Type) (space :: Type)

data IsPoint

evaluate ::
  Exists dimension units space =>
  Curve dimension units space ->
  Number ->
  Point dimension units space
evaluateBounds ::
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
tangentDirection ::
  (Exists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Result IsPoint (DirectionCurve dimension space)
