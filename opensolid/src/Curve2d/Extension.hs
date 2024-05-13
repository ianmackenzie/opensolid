{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Curve2d.Extension (start, end) where

import BezierCurve2d qualified
import Curve2d (Curve2d)
import Curve2d qualified
import List qualified
import OpenSolid
import Point2d (Point2d)
import Stream qualified
import Vector2d (Vector2d)
import VectorCurve2d (VectorCurve2d)
import VectorCurve2d qualified

start ::
  Tolerance units =>
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  (Curve2d (space @ units), Int) ->
  Curve2d (space @ units)
start startCondition (endCurve, initialContinuity) = Result.do
  let endPoint = Curve2d.startPoint endCurve
  let endCurveDerivatives = Stream.iterate (Curve2d.derivative endCurve) VectorCurve2d.derivative
  let endDerivativeValues = Stream.map (VectorCurve2d.evaluateAt 0.0) endCurveDerivatives
  let endCondition continuity = (endPoint, Stream.take continuity endDerivativeValues)
  let baseCurve continuity = BezierCurve2d.hermite startCondition (endCondition continuity)
  let initialCurve = baseCurve initialContinuity
  let curveDerivative n = nthDerivative n (baseCurve (initialContinuity + n))
  Curve2d.wrap (Connector initialCurve curveDerivative)

end ::
  Tolerance units =>
  (Curve2d (space @ units), Int) ->
  (Point2d (space @ units), List (Vector2d (space @ units))) ->
  Curve2d (space @ units)
end (startCurve, initialContinuity) (endPoint, endDerivatives) = do
  let reverseDerivative i v = Negative ** (i + 1) * v
  let reversedEndDerivatives = List.mapWithIndex reverseDerivative endDerivatives
  Curve2d.reverse $
    start (endPoint, reversedEndDerivatives) (Curve2d.reverse startCurve, initialContinuity)

nthDerivative :: Int -> Curve2d (space @ units) -> VectorCurve2d (space @ units)
nthDerivative 0 _ = internalError "nthDerivative should always be called with n >= 1"
nthDerivative 1 bezierCurve = Curve2d.derivative bezierCurve
nthDerivative n bezierCurve = VectorCurve2d.derivative (nthDerivative (n - 1) bezierCurve)

data Connector coordinateSystem where
  Connector ::
    { curve :: Curve2d (space @ units)
    , curveDerivative :: Int -> VectorCurve2d (space @ units)
    } ->
    Connector (space @ units)

instance Show (Connector (space @ units)) where
  show _ = "<Connector curve 2D>"

instance Curve2d.Interface (Connector (space @ units)) (space @ units) where
  startPointImpl (Connector{curve}) = Curve2d.startPoint curve
  endPointImpl (Connector{curve}) = Curve2d.endPoint curve
  evaluateAtImpl t (Connector{curve}) = Curve2d.evaluateAt t curve
  segmentBoundsImpl t (Connector{curve}) = Curve2d.segmentBounds t curve
  boundsImpl (Connector{curve}) = Curve2d.bounds curve
  reverseImpl (Connector{curve, curveDerivative}) =
    Connector
      { curve = Curve2d.reverse curve
      , curveDerivative = \n -> (Negative ** n) * VectorCurve2d.reverse (curveDerivative n)
      }
  derivativeImpl (Connector{curveDerivative}) =
    VectorCurve2d.wrap
      ConnectorDerivative
        { n = 1
        , curve = curveDerivative 1
        , curveDerivative = curveDerivative
        }
  transformByImpl transform (Connector{curve, curveDerivative}) =
    Curve2d.wrap
      Connector
        { curve = Curve2d.transformBy transform curve
        , curveDerivative = curveDerivative >> VectorCurve2d.transformBy transform
        }

data ConnectorDerivative coordinateSystem where
  ConnectorDerivative ::
    { n :: Int
    , curve :: VectorCurve2d (space @ units)
    , curveDerivative :: Int -> VectorCurve2d (space @ units)
    } ->
    ConnectorDerivative (space @ units)

instance Show (ConnectorDerivative (space @ units)) where
  show _ = "<Connector curve derivative 2D>"

instance VectorCurve2d.Interface (ConnectorDerivative (space @ units)) (space @ units) where
  evaluateAtImpl t (ConnectorDerivative{curve}) = VectorCurve2d.evaluateAt t curve
  segmentBoundsImpl t (ConnectorDerivative{curve}) = VectorCurve2d.segmentBounds t curve
  derivativeImpl (ConnectorDerivative{n, curveDerivative}) =
    VectorCurve2d.wrap
      ConnectorDerivative
        { n = n + 1
        , curve = curveDerivative (n + 1)
        , curveDerivative = curveDerivative
        }
  transformByImpl transform (ConnectorDerivative{n, curve, curveDerivative}) =
    VectorCurve2d.wrap
      ConnectorDerivative
        { n
        , curve = VectorCurve2d.transformBy transform curve
        , curveDerivative = curveDerivative >> VectorCurve2d.transformBy transform
        }
