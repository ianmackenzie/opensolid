module OpenSolid.CurvePoint
  ( CurvePoint
  , point
  , derivativeValue
  , tangentDirectionValue
  , curvatureVectorValue_
  , location
  , parameterValue
  , isEndpoint
  , isDegenerate
  , nondegenerate
  , continuity
  )
where

import OpenSolid.Continuity (Continuity)
import OpenSolid.Continuity qualified as Continuity
import OpenSolid.CurveLocation (CurveLocation)
import OpenSolid.CurveLocation qualified as CurveLocation
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.Error (IsDegenerate (IsDegenerate))
import OpenSolid.Internal.CurvePoint (CurvePoint (..))
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Number qualified as Number
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector

location :: CurvePoint dimension units space -> CurveLocation
location = (.location)

parameterValue :: CurvePoint dimension units space -> Number
parameterValue = CurveLocation.toParameterValue . location

point :: CurvePoint dimension units space -> Point dimension units space
point = (.point)

derivativeValue :: CurvePoint dimension units space -> Vector dimension units space
derivativeValue = (.derivativeValue)

tangentDirectionValue :: CurvePoint dimension units space -> Direction dimension space
tangentDirectionValue = (.tangentDirectionValue)

curvatureVectorValue_ ::
  Nondegenerate (CurvePoint dimension units space) ->
  Vector dimension (Unitless ?/? units) space
curvatureVectorValue_ = Nondegenerate.get (.curvatureVectorValue_)

isEndpoint :: CurvePoint dimension units space -> Bool
isEndpoint = Parameter.isEndpoint . parameterValue

isDegenerate ::
  (Vector.Exists dimension units space, Tolerance units) =>
  CurvePoint dimension units space ->
  Bool
isDegenerate curvePoint = derivativeValue curvePoint ~= Vector.zero

nondegenerate ::
  (Vector.Exists dimension units space, Tolerance units) =>
  CurvePoint dimension units space ->
  Result IsDegenerate (Nondegenerate (CurvePoint dimension units space))
nondegenerate curvePoint =
  if isDegenerate curvePoint then Err IsDegenerate else Ok (Nondegenerate curvePoint)

continuity ::
  forall dimension units space.
  ( Point.Exists dimension units space
  , Direction.Exists dimension space
  , Vector.Exists dimension units space
  , Vector.Exists dimension Unitless space
  , Vector.Exists dimension (Unitless ?/? units) space
  , Tolerance units
  ) =>
  CurvePoint dimension units space ->
  CurvePoint dimension units space ->
  Maybe Continuity
continuity p1 p2 = do
  if point p1 ~= point p2
    then do
      let tangent1 = tangentDirectionValue p1
      let tangent2 = tangentDirectionValue p2
      if Direction.parallel tangent1 tangent2
        then do
          let alignment = Number.sign (tangent1 `dot` tangent2)
          if
            | Ok nondegenerate1 <- nondegenerate p1
            , Ok nondegenerate2 <- nondegenerate p2 -> do
                let l1 = Vector.magnitude (derivativeValue p1)
                let l2 = Vector.magnitude (derivativeValue p2)
                let l = Quantity.erase (min l1 l2)
                let k1_ = curvatureVectorValue_ nondegenerate1
                let k2_ = curvatureVectorValue_ nondegenerate2
                let k = Vector.erase (k1_ - k2_)
                let curvatureError :: Vector dimension units space =
                      Vector.unerase (k * l * l / 2.0)
                if curvatureError ~= Vector.zero
                  then Just (Continuity.Indistinguishable alignment)
                  else Just (Continuity.Tangent alignment)
            | otherwise -> Just (Continuity.Indistinguishable alignment)
        else Just Continuity.Crossing
    else Nothing
