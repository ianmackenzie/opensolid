module OpenSolid.CurvePoint
  ( CurvePoint
  , point
  , derivative
  , tangentDirection
  , curvatureVector_
  , parameterValue
  , on
  , isDegenerate
  , nondegenerate
  , continuity
  )
where

import OpenSolid.Continuity (Continuity)
import OpenSolid.Continuity qualified as Continuity
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Nondegenerate qualified as Curve.Nondegenerate
import OpenSolid.Curve.Nonzero qualified as Curve.Nonzero
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.Error (IsDegenerate (IsDegenerate))
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Number qualified as Number
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector

data CurvePoint dimension units space = CurvePoint
  { parameterValue :: Number
  , point :: ~(Point dimension units space)
  , derivative :: ~(Vector dimension units space)
  , tangentDirection :: ~(Direction dimension space)
  , curvatureVector_ :: Nondegenerate.Field (Vector dimension (Unitless ?/? units) space)
  }

deriving instance Curve.Exists dimension units space => Show (CurvePoint dimension units space)

on ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  CurvePoint dimension units space
on nondegenerateCurve givenParameterValue = do
  let Nondegenerate curve = nondegenerateCurve
  recursive \curvePoint ->
    CurvePoint
      { parameterValue = givenParameterValue
      , point = Curve.point curve givenParameterValue
      , derivative = Curve.derivativeValue curve givenParameterValue
      , tangentDirection =
          Curve.Nondegenerate.tangentDirectionValue nondegenerateCurve givenParameterValue
      , curvatureVector_ =
          curvePoint
            & Nondegenerate.field do
              \_nondegeneratePoint ->
                Curve.Nonzero.curvatureVectorValue_
                  (Nondegenerate.interior nondegenerateCurve)
                  givenParameterValue
      }

parameterValue :: CurvePoint dimension units space -> Number
parameterValue = (.parameterValue)

point :: CurvePoint dimension units space -> Point dimension units space
point = (.point)

derivative :: CurvePoint dimension units space -> Vector dimension units space
derivative = (.derivative)

tangentDirection :: CurvePoint dimension units space -> Direction dimension space
tangentDirection = (.tangentDirection)

curvatureVector_ :: Nondegenerate (CurvePoint dimension units space) -> Vector dimension (Unitless ?/? units) space
curvatureVector_ = Nondegenerate.get (.curvatureVector_)

isDegenerate ::
  (Curve.Exists dimension units space, Tolerance units) =>
  CurvePoint dimension units space ->
  Bool
isDegenerate curvePoint = derivative curvePoint ~= Vector.zero

nondegenerate ::
  (Curve.Exists dimension units space, Tolerance units) =>
  CurvePoint dimension units space ->
  Result IsDegenerate (Nondegenerate (CurvePoint dimension units space))
nondegenerate curvePoint =
  if isDegenerate curvePoint then Error IsDegenerate else Ok (Nondegenerate curvePoint)

continuity ::
  (Curve.Exists dimension units space, Tolerance units) =>
  Nondegenerate (CurvePoint dimension units space) ->
  Nondegenerate (CurvePoint dimension units space) ->
  Maybe Continuity
continuity nondegenerate1 nondegenerate2 = do
  let Nondegenerate p1 = nondegenerate1
  let Nondegenerate p2 = nondegenerate2
  if point p1 ~= point p2
    then do
      let tangent1 = tangentDirection p1
      let tangent2 = tangentDirection p2
      if Direction.parallel tangent1 tangent2
        then do
          let sign = Number.sign (tangent1 `dot` tangent2)
          let l1 = Vector.magnitude (derivative p1)
          let l2 = Vector.magnitude (derivative p2)
          let l = Quantity.erase (min l1 l2)
          let k1 = curvatureVector_ nondegenerate1
          let k2 = curvatureVector_ nondegenerate2
          let k = Vector.erase (k1 - k2)
          if Vector.unerase (k * l * l / 2.0) ~= Vector.zero
            then Just (Continuity.G2 sign)
            else Just (Continuity.G1 sign)
        else Just Continuity.G0
    else Nothing
