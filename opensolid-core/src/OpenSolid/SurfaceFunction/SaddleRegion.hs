module OpenSolid.SurfaceFunction.SaddleRegion
  ( SaddleRegion
  , Frame
  , JoiningCurve (Incoming, Outgoing)
  , point
  , quadratic
  , connectingCurve
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
import OpenSolid.Quantity qualified as Quantity
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import {-# SOURCE #-} OpenSolid.SurfaceFunction.HorizontalCurve qualified as HorizontalCurve
import OpenSolid.SurfaceFunction.Subproblem (Subproblem (Subproblem))
import OpenSolid.SurfaceFunction.Subproblem qualified as Subproblem
import {-# SOURCE #-} OpenSolid.SurfaceFunction.VerticalCurve qualified as VerticalCurve
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d

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
  let dB = NonEmpty.maximumBy (Quantity.abs . directionalSecondDerivative) bDirectionCandidates
  let dA = Direction2d.rotateRight dB
  let vA = Vector2d.unit dA
  let vB = Vector2d.unit dB
  let Vector2d ua va = vA
  let Vector2d ub vb = vB
  let faa = ua * ua * fuu + 2.0 * ua * va * fuv + va * va * fvv
  let fab = ua * ub * fuu + (ua * vb + ub * va) * fuv + va * vb * fvv
  let fbb = ub * ub * fuu + 2.0 * ub * vb * fuv + vb * vb * fvv
  let determinant = fab *# fab - faa *# fbb
  let sqrtD = Quantity.sqrt# determinant
  let (m1, m2) = Quantity.minmax ((-fab + sqrtD) / fbb, (-fab - sqrtD) / fbb)
  let v1 = Vector2d.normalize (vA + m1 * vB)
  let v2 = Vector2d.normalize (vA + m2 * vB)
  let d1 = Direction2d.unsafe v1
  let d2 = Direction2d.unsafe v2
  let vX = Vector2d.normalize (v1 + v2)
  let dX = Direction2d.unsafe vX
  let frame = Frame2d.fromXAxis (Axis2d.through saddlePoint dX)
  SaddleRegion{subproblem, frame, d1, d2}

secondDerivative ::
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Direction2d UvSpace ->
  Quantity units
secondDerivative fuu fuv fvv direction = do
  let Direction2d du dv = direction
  du * du * fuu + 2.0 * du * dv * fuv + dv * dv * fvv

connectingCurve ::
  Tolerance units =>
  JoiningCurve ->
  SaddleRegion units ->
  Curve2d UvCoordinates
connectingCurve joiningCurve SaddleRegion{subproblem, frame, d1, d2} = do
  let Point2d x y = Point2d.relativeTo frame (joiningPoint joiningCurve)
  let saddlePoint = Frame2d.originPoint frame
  let dx = Frame2d.xDirection frame
  let dy = Frame2d.yDirection frame
  let boundingAxis direction = Axis2d.through saddlePoint direction
  case (Quantity.sign x, Quantity.sign y) of
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
  Curve2d UvCoordinates
connect subproblem frame outgoingDirection joiningCurve boundingAxes = do
  let saddlePoint = Frame2d.originPoint frame
  let Subproblem{f, dvdu, dudv, uvBounds} = subproblem
  let Bounds2d uBounds vBounds = uvBounds
  let Point2d uP vP = saddlePoint
  let Point2d uC vC = joiningPoint joiningCurve
  let Direction2d du dv = outgoingDirection
  if Quantity.abs du >= Quantity.abs dv
    then do
      let implicitBounds = NonEmpty.one (Bounds2d (Bounds uP uC) vBounds)
      case joiningCurve of
        Incoming _ -> do
          let dudt = uP - uC
          let endDerivative = Vector2d dudt (dudt * (dv / du))
          let implicitCurve = HorizontalCurve.bounded f dvdu uC uP implicitBounds frame boundingAxes
          Curve2d.desingularize Nothing implicitCurve (Just (saddlePoint, endDerivative))
        Outgoing _ -> do
          let dudt = uC - uP
          let startDerivative = Vector2d dudt (dudt * (dv / du))
          let implicitCurve = HorizontalCurve.bounded f dvdu uP uC implicitBounds frame boundingAxes
          Curve2d.desingularize (Just (saddlePoint, startDerivative)) implicitCurve Nothing
    else do
      let implicitBounds = NonEmpty.one (Bounds2d uBounds (Bounds vP vC))
      case joiningCurve of
        Incoming _ -> do
          let dvdt = vP - vC
          let endDerivative = Vector2d (dvdt * (du / dv)) dvdt
          let implicitCurve = VerticalCurve.bounded f dudv vC vP implicitBounds frame boundingAxes
          Curve2d.desingularize Nothing implicitCurve (Just (saddlePoint, endDerivative))
        Outgoing _ -> do
          let dvdt = vC - vP
          let startDerivative = Vector2d (dvdt * (du / dv)) dvdt
          let implicitCurve = VerticalCurve.bounded f dudv vP vC implicitBounds frame boundingAxes
          Curve2d.desingularize (Just (saddlePoint, startDerivative)) implicitCurve Nothing
