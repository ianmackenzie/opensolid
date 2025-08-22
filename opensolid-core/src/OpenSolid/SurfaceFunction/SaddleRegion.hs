module OpenSolid.SurfaceFunction.SaddleRegion
  ( SaddleRegion
  , Frame
  , JoiningCurve (Incoming, Outgoing)
  , point
  , quadratic
  , connectingCurves
  )
where

import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import {-# SOURCE #-} OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d (Direction2d (Direction2d))
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Domain2d (Domain2d)
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Stream (Stream)
import OpenSolid.Stream qualified as Stream
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import {-# SOURCE #-} OpenSolid.SurfaceFunction.HorizontalCurve qualified as HorizontalCurve
import OpenSolid.SurfaceFunction.Subproblem (Subproblem (Subproblem))
import OpenSolid.SurfaceFunction.Subproblem qualified as Subproblem
import {-# SOURCE #-} OpenSolid.SurfaceFunction.VerticalCurve qualified as VerticalCurve
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d

data SaddleRegion units = SaddleRegion
  { subproblem :: Subproblem units
  , frame :: Frame
  , d1 :: Direction2d UvSpace
  , d2 :: Direction2d UvSpace
  }

data PrincipalAxisSpace

type Frame = Frame2d UvCoordinates (Defines PrincipalAxisSpace)

data JoiningCurve
  = Incoming (Curve2d UvCoordinates)
  | Outgoing (Curve2d UvCoordinates)

joiningPoint :: JoiningCurve -> UvPoint
joiningPoint (Incoming curve) = curve.endPoint
joiningPoint (Outgoing curve) = curve.startPoint

point :: SaddleRegion units -> UvPoint
point SaddleRegion{frame} = Frame2d.originPoint frame

instance HasField "subdomain" (SaddleRegion units) Domain2d where
  getField = (.subproblem.subdomain)

instance HasField "bounds" (SaddleRegion units) UvBounds where
  getField = (.subproblem.uvBounds)

quadratic :: Subproblem units -> UvPoint -> SaddleRegion units
quadratic subproblem saddlePoint = do
  let f = subproblem.f
  let fuu = SurfaceFunction.evaluate f.du.du saddlePoint
  let fuv = SurfaceFunction.evaluate f.du.dv saddlePoint
  let fvv = SurfaceFunction.evaluate f.dv.dv saddlePoint
  let bDirectionCandidates = NonEmpty.three Direction2d.x Direction2d.y (Direction2d.degrees 45.0)
  let directionalSecondDerivative = secondDerivative fuu fuv fvv
  let dB = NonEmpty.maximumBy (Qty.abs . directionalSecondDerivative) bDirectionCandidates
  let dA = Direction2d.rotateRight dB
  let vA = Vector2d.unit dA
  let vB = Vector2d.unit dB
  let Vector2d ua va = vA
  let Vector2d ub vb = vB
  let faa = ua * ua * fuu + 2.0 * ua * va * fuv + va * va * fvv
  let fab = ua * ub * fuu + (ua * vb + ub * va) * fuv + va * vb * fvv
  let fbb = ub * ub * fuu + 2.0 * ub * vb * fuv + vb * vb * fvv
  let determinant = fab .*. fab - faa .*. fbb
  let sqrtD = Qty.sqrt' determinant
  let (m1, m2) = Qty.minmax ((-fab + sqrtD) / fbb, (-fab - sqrtD) / fbb)
  let v1 = Vector2d.normalize (vA + m1 * vB)
  let v2 = Vector2d.normalize (vA + m2 * vB)
  let d1 = Direction2d.unsafe v1
  let d2 = Direction2d.unsafe v2
  let vX = Vector2d.normalize (v1 + v2)
  let dX = Direction2d.unsafe vX
  let frame = Frame2d.fromXAxis (Axis2d.through saddlePoint dX)
  SaddleRegion{subproblem, frame, d1, d2}

secondDerivative :: Qty units -> Qty units -> Qty units -> Direction2d UvSpace -> Qty units
secondDerivative fuu fuv fvv direction = do
  let Direction2d du dv = direction
  du * du * fuu + 2.0 * du * dv * fuv + dv * dv * fvv

connectingCurves ::
  Tolerance units =>
  JoiningCurve ->
  SaddleRegion units ->
  NonEmpty (Curve2d UvCoordinates)
connectingCurves joiningCurve SaddleRegion{subproblem, frame, d1, d2} = do
  let Point2d x y = Point2d.relativeTo frame (joiningPoint joiningCurve)
  let saddlePoint = Frame2d.originPoint frame
  let dx = Frame2d.xDirection frame
  let dy = Frame2d.yDirection frame
  let boundingAxis direction = Axis2d.through saddlePoint direction
  case (Qty.sign x, Qty.sign y) of
    (Positive, Positive) ->
      connect subproblem frame d2 joiningCurve [boundingAxis dx, boundingAxis -dy]
    (Positive, Negative) ->
      connect subproblem frame d1 joiningCurve [boundingAxis -dx, boundingAxis -dy]
    (Negative, Positive) ->
      connect subproblem frame -d1 joiningCurve [boundingAxis dx, boundingAxis dy]
    (Negative, Negative) ->
      connect subproblem frame -d2 joiningCurve [boundingAxis -dx, boundingAxis dy]

connect ::
  Tolerance units =>
  Subproblem units ->
  Frame2d UvCoordinates (Defines PrincipalAxisSpace) ->
  Direction2d UvSpace ->
  JoiningCurve ->
  List (Axis2d UvCoordinates) ->
  NonEmpty (Curve2d UvCoordinates)
connect subproblem frame outgoingDirection joiningCurve boundingAxes = do
  let saddlePoint = Frame2d.originPoint frame
  let Subproblem{f, dvdu, dudv, uvBounds} = subproblem
  let Bounds2d uBounds vBounds = uvBounds
  let Point2d uP vP = saddlePoint
  let Point2d uC vC = joiningPoint joiningCurve
  let Direction2d du dv = outgoingDirection
  let interpolantSize = 1e-3
  let midParameter t0 t1 = t0 + interpolantSize * Qty.sign (t1 - t0) |> Qty.clampTo (Bounds t0 t1)
  if Qty.abs du >= Qty.abs dv
    then do
      let uMid = midParameter uP uC
      case joiningCurve of
        Incoming incomingCurve -> do
          let dudt = uP - uMid
          let endDerivative = Vector2d dudt (dudt * (dv / du))
          let makeSuffix = suffix saddlePoint endDerivative Direction2d.x
          if uMid == uC
            then NonEmpty.one (makeSuffix incomingCurve)
            else do
              let implicitBounds = NonEmpty.one (Bounds2d (Bounds uMid uC) vBounds)
              let implicitCurve =
                    HorizontalCurve.bounded f dvdu uC uMid implicitBounds frame boundingAxes
              NonEmpty.two implicitCurve (makeSuffix implicitCurve)
        Outgoing outgoingCurve -> do
          let dudt = uMid - uP
          let startDerivative = Vector2d dudt (dudt * (dv / du))
          let makePrefix = prefix saddlePoint startDerivative Direction2d.x
          if uMid == uC
            then NonEmpty.one (makePrefix outgoingCurve)
            else do
              let implicitBounds = NonEmpty.one (Bounds2d (Bounds uMid uC) vBounds)
              let implicitCurve =
                    HorizontalCurve.bounded f dvdu uMid uC implicitBounds frame boundingAxes
              NonEmpty.two (makePrefix implicitCurve) implicitCurve
    else do
      let vMid = midParameter vP vC
      case joiningCurve of
        Incoming incomingCurve -> do
          let dvdt = vP - vMid
          let endDerivative = Vector2d (dvdt * (du / dv)) dvdt
          let makeSuffix = suffix saddlePoint endDerivative Direction2d.y
          if vMid == vC
            then NonEmpty.one (makeSuffix incomingCurve)
            else do
              let implicitBounds = NonEmpty.one (Bounds2d uBounds (Bounds vMid vC))
              let implicitCurve =
                    VerticalCurve.bounded f dudv vC vMid implicitBounds frame boundingAxes
              NonEmpty.two implicitCurve (makeSuffix implicitCurve)
        Outgoing outgoingCurve -> do
          let dvdt = vMid - vP
          let startDerivative = Vector2d (dvdt * (du / dv)) dvdt
          let makePrefix = prefix saddlePoint startDerivative Direction2d.y
          if vMid == vC
            then NonEmpty.one (makePrefix outgoingCurve)
            else do
              let implicitBounds = NonEmpty.one (Bounds2d uBounds (Bounds vMid vC))
              let implicitCurve =
                    VerticalCurve.bounded f dudv vMid vC implicitBounds frame boundingAxes
              NonEmpty.two (makePrefix implicitCurve) implicitCurve

extensionContinuity :: Int
extensionContinuity = 2

nthDerivative :: Int -> Curve2d (space @ units) -> VectorCurve2d (space @ units)
nthDerivative 0 _ = internalError "nthDerivative should always be called with n >= 1"
nthDerivative 1 curve = curve.derivative
nthDerivative n curve = (nthDerivative (n - 1) curve).derivative

componentRatio :: Direction2d UvSpace -> Vector2d UvCoordinates -> Vector2d UvCoordinates -> Float
componentRatio d v1 v2 = Vector2d.componentIn d v1 / Vector2d.componentIn d v2

scaledDerivativeValues ::
  Float ->
  Curve2d UvCoordinates ->
  (VectorCurve2d UvCoordinates -> Vector2d UvCoordinates) ->
  Stream (Vector2d UvCoordinates)
scaledDerivativeValues scalingFactor curve derivativeEndpoint = do
  let curveDerivatives = Stream.iterate (.derivative) curve.derivative
  let scaledDerivativeValue i curveDerivative = do
        let derivativeScale = scalingFactor ** (i + 1)
        derivativeScale * derivativeEndpoint curveDerivative
  Stream.mapWithIndex scaledDerivativeValue curveDerivatives

prefix ::
  UvPoint ->
  Vector2d UvCoordinates ->
  Direction2d UvSpace ->
  Curve2d UvCoordinates ->
  Curve2d UvCoordinates
prefix startPoint startDerivative axisDirection outgoingCurve = do
  let outgoingFirstDerivative = VectorCurve2d.startValue outgoingCurve.derivative
  let derivativeScalingFactor = componentRatio axisDirection startDerivative outgoingFirstDerivative
  let endDerivativeValues =
        scaledDerivativeValues derivativeScalingFactor outgoingCurve VectorCurve2d.startValue
  let baseCurve endContinuity =
        Curve2d.hermite
          startPoint
          [startDerivative]
          outgoingCurve.startPoint
          (Stream.take endContinuity endDerivativeValues)
  synthetic baseCurve

suffix ::
  UvPoint ->
  Vector2d UvCoordinates ->
  Direction2d UvSpace ->
  Curve2d UvCoordinates ->
  Curve2d UvCoordinates
suffix endPoint endDerivative axisDirection incomingCurve = do
  let incomingFirstDerivative = VectorCurve2d.endValue incomingCurve.derivative
  let derivativeScalingFactor = componentRatio axisDirection endDerivative incomingFirstDerivative
  let startDerivativeValues =
        scaledDerivativeValues derivativeScalingFactor incomingCurve VectorCurve2d.endValue
  let baseCurve startContinuity =
        Curve2d.hermite
          incomingCurve.endPoint
          (Stream.take startContinuity startDerivativeValues)
          endPoint
          [endDerivative]
  synthetic baseCurve

synthetic :: (Int -> Curve2d UvCoordinates) -> Curve2d UvCoordinates
synthetic baseCurve = do
  let syntheticDerivative n =
        VectorCurve2d.synthetic
          (nthDerivative n (baseCurve (extensionContinuity + n)))
          (syntheticDerivative (n + 1))
  Curve2d.synthetic (baseCurve extensionContinuity) (syntheticDerivative 1)
