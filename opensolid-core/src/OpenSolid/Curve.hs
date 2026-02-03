module OpenSolid.Curve
  ( Curve
  , Exists
  , IsPoint (IsPoint)
  , derivative
  , secondDerivative
  , tangentDirection
  , findPoint
  )
where

import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import {-# SOURCE #-} OpenSolid.Curve3D (Curve3D)
import {-# SOURCE #-} OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.DirectionCurve (DirectionCurve)
import OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.VectorCurve (VectorCurve)
import OpenSolid.VectorCurve qualified as VectorCurve

type family Curve dimension units space = curve | curve -> dimension units space where
  Curve 2 units space = Curve2D units space
  Curve 3 Meters space = Curve3D space

data IsPoint = IsPoint deriving (Eq, Show)

class
  ( VectorCurve.Exists dimension units space
  , Subtraction
      (Curve dimension units space)
      (Point dimension units space)
      (VectorCurve dimension units space)
  , Subtraction
      (Point dimension units space)
      (Curve dimension units space)
      (VectorCurve dimension units space)
  ) =>
  Exists dimension units space
  where
  derivative :: Curve dimension units space -> VectorCurve dimension units space

instance Exists 2 units space where
  derivative = Curve2D.derivative

instance Exists 3 Meters space where
  derivative = Curve3D.derivative

secondDerivative ::
  Exists dimension units space =>
  Curve dimension units space ->
  VectorCurve dimension units space
secondDerivative = VectorCurve.derivative . derivative

tangentDirection ::
  (Exists dimension units space, DirectionCurve.Exists dimension space, Tolerance units) =>
  Curve dimension units space ->
  Result IsPoint (DirectionCurve dimension space)
tangentDirection curve =
  case VectorCurve.direction (derivative curve) of
    Ok directionCurve -> Ok directionCurve
    Error VectorCurve.IsZero -> Error IsPoint

findPoint ::
  (Exists dimension units space, Tolerance units) =>
  Point dimension units space ->
  Curve dimension units space ->
  Result IsPoint (List Number)
findPoint point curve =
  case VectorCurve.zeros (point .-. curve) of
    Error VectorCurve.IsZero -> Error IsPoint
    Ok parameterValues -> Ok parameterValues
