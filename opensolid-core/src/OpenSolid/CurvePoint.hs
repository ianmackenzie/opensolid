module OpenSolid.CurvePoint
  ( CurvePoint
  , point
  , derivative
  , tangentDirection
  , curvatureVector_
  , parameterValue
  , on
  , isEndpoint
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
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point (Point)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Result qualified as Result
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector

data CurvePoint dimension units space where
  CurvePoint ::
    Curve.Exists dimension units space =>
    { nondegenerateCurve :: Nondegenerate (Curve dimension units space)
    , parameterValue :: Number
    } ->
    CurvePoint dimension units space

on ::
  Curve.Exists dimension units space =>
  Nondegenerate (Curve dimension units space) ->
  Number ->
  CurvePoint dimension units space
on = CurvePoint

{-# INLINE curve #-}
curve :: CurvePoint dimension units space -> Curve dimension units space
curve = Nondegenerate.unwrap . nondegenerateCurve

{-# INLINE nondegenerateCurve #-}
nondegenerateCurve ::
  CurvePoint dimension units space ->
  Nondegenerate (Curve dimension units space)
nondegenerateCurve = (.nondegenerateCurve)

{-# INLINE parameterValue #-}
parameterValue :: CurvePoint dimension units space -> Number
parameterValue = (.parameterValue)

point :: CurvePoint dimension units space -> Point dimension units space
point curvePoint = Curve.point (curve curvePoint) (parameterValue curvePoint)

derivative ::
  CurvePoint dimension units space ->
  Vector dimension units space
derivative curvePoint@CurvePoint{} =
  Curve.derivativeValue (curve curvePoint) (parameterValue curvePoint)

tangentDirection ::
  CurvePoint dimension units space ->
  Direction dimension space
tangentDirection curvePoint@CurvePoint{} =
  Curve.Nondegenerate.tangentDirectionValue (nondegenerateCurve curvePoint) (parameterValue curvePoint)

curvatureVector_ ::
  Nondegenerate (CurvePoint dimension units space) ->
  Vector dimension (Unitless ?/? units) space
curvatureVector_ (Nondegenerate curvePoint@CurvePoint{}) = do
  let nonZeroCurve = Nondegenerate.interior (nondegenerateCurve curvePoint)
  Curve.Nonzero.curvatureVectorValue_ nonZeroCurve (parameterValue curvePoint)

isEndpoint :: CurvePoint dimension units space -> Bool
isEndpoint = Parameter.isEndpoint . parameterValue

isDegenerate :: Tolerance units => CurvePoint dimension units space -> Bool
isDegenerate curvePoint@CurvePoint{} = derivative curvePoint ~= Vector.zero

nondegenerate ::
  Tolerance units =>
  CurvePoint dimension units space ->
  Result IsDegenerate (Nondegenerate (CurvePoint dimension units space))
nondegenerate curvePoint =
  if isDegenerate curvePoint then Error IsDegenerate else Ok (Nondegenerate curvePoint)

continuity ::
  Tolerance units =>
  CurvePoint dimension units space ->
  CurvePoint dimension units space ->
  Maybe Continuity
continuity p1@CurvePoint{} p2@CurvePoint{} = do
  if point p1 ~= point p2
    then do
      let tangent1 = tangentDirection p1
      let tangent2 = tangentDirection p2
      if Direction.parallel tangent1 tangent2
        then do
          let sign = Number.sign (tangent1 `dot` tangent2)
          case Result.map2 (,) (nondegenerate p1) (nondegenerate p2) of
            Error IsDegenerate -> Just (Continuity.Indistinguishable sign)
            Ok (nondegenerate1, nondegenerate2) -> do
              let l1 = Vector.magnitude (derivative p1)
              let l2 = Vector.magnitude (derivative p2)
              let l = Quantity.erase (min l1 l2)
              let k1_ = curvatureVector_ nondegenerate1
              let k2_ = curvatureVector_ nondegenerate2
              let k = Vector.erase (k1_ - k2_)
              if Vector.unerase (k * l * l / 2.0) ~= Vector.zero
                then Just (Continuity.Indistinguishable sign)
                else Just (Continuity.Tangent sign)
        else Just Continuity.Crossing
    else Nothing
