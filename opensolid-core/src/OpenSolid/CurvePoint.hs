{-# LANGUAGE FieldSelectors #-}

module OpenSolid.CurvePoint
  ( CurvePoint
  , point
  , derivative
  , tangentDirection
  , parameterValue
  , on
  , isDegenerate
  )
where

import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
import OpenSolid.Direction (Direction)
import OpenSolid.Nondegenerate (Nondegenerate)
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector

data CurvePoint dimension units space = CurvePoint
  { parameterValue :: Number
  , point :: ~(Point dimension units space)
  , derivative :: ~(Vector dimension units space)
  , tangentDirection :: ~(Direction dimension space)
  }

deriving instance Curve.Exists dimension units space => Show (CurvePoint dimension units space)

on ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  CurvePoint dimension units space
on givenCurve givenParameterValue =
  CurvePoint
    { parameterValue = givenParameterValue
    , point = Curve.point (Nondegenerate.unwrap givenCurve) givenParameterValue
    , derivative = Curve.derivativeValue (Nondegenerate.unwrap givenCurve) givenParameterValue
    , tangentDirection = Curve.Nondegenerate.tangentDirectionValue givenCurve givenParameterValue
    }

isDegenerate ::
  (Curve.Exists dimension units space, Tolerance units) =>
  CurvePoint dimension units space ->
  Bool
isDegenerate curvePoint = derivative curvePoint ~= Vector.zero
