module OpenSolid.SurfaceFunction1D.SaddleRegion
  ( SaddleRegion
  , Frame
  , JoiningCurve (Incoming, Outgoing)
  , point
  , quadratic
  , connectingCurve
  )
where

import GHC.Records (HasField (getField))
import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction2D (Direction2D (Direction2D))
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Domain2D (Domain2D)
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D.HorizontalCurve qualified as HorizontalCurve
import OpenSolid.SurfaceFunction1D.Subproblem (Subproblem (Subproblem))
import OpenSolid.SurfaceFunction1D.Subproblem qualified as Subproblem
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D.VerticalCurve qualified as VerticalCurve
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D

data SaddleRegion units = SaddleRegion
  { subproblem :: Subproblem units
  , frame :: Frame
  , d1 :: Direction2D UvSpace
  , d2 :: Direction2D UvSpace
  }

data PrincipalAxisSpace

type Frame = Frame2D Unitless UvSpace PrincipalAxisSpace

data JoiningCurve
  = Incoming (Curve2D Unitless UvSpace)
  | Outgoing (Curve2D Unitless UvSpace)

joiningPoint :: JoiningCurve -> UvPoint
joiningPoint (Incoming curve) = curve.endPoint
joiningPoint (Outgoing curve) = curve.startPoint

point :: SaddleRegion units -> UvPoint
point SaddleRegion{frame} = Frame2D.originPoint frame

instance HasField "subdomain" (SaddleRegion units) Domain2D where
  getField = (.subproblem.subdomain)

instance HasField "bounds" (SaddleRegion units) UvBounds where
  getField = (.subproblem.uvBounds)

quadratic :: Subproblem units -> UvPoint -> SaddleRegion units
quadratic subproblem saddlePoint = do
  let f = subproblem.f
  let fuu = SurfaceFunction1D.evaluate f.du.du saddlePoint
  let fuv = SurfaceFunction1D.evaluate f.du.dv saddlePoint
  let fvv = SurfaceFunction1D.evaluate f.dv.dv saddlePoint
  let bDirectionCandidates = NonEmpty.three Direction2D.x Direction2D.y (Direction2D.degrees 45)
  let directionalSecondDerivative = secondDerivative fuu fuv fvv
  let dB = NonEmpty.maximumBy (Quantity.abs . directionalSecondDerivative) bDirectionCandidates
  let dA = Direction2D.rotateRight dB
  let vA = Vector2D.unit dA
  let vB = Vector2D.unit dB
  let Vector2D ua va = vA
  let Vector2D ub vb = vB
  let faa = ua .*. ua .*. fuu .+. 2 *. ua .*. va .*. fuv .+. va .*. va .*. fvv
  let fab = ua .*. ub .*. fuu .+. (ua .*. vb .+. ub .*. va) .*. fuv .+. va .*. vb .*. fvv
  let fbb = ub .*. ub .*. fuu .+. 2 *. ub .*. vb .*. fuv .+. vb .*. vb .*. fvv
  let determinant = fab ?*? fab .-. faa ?*? fbb
  let sqrtD = Quantity.sqrt_ determinant
  let (m1, m2) = Quantity.minmax ((negative fab .+. sqrtD) ./. fbb, (negative fab .-. sqrtD) ./. fbb)
  let v1 = Tolerance.using Quantity.zero (Vector2D.normalize (vA .+. m1 .*. vB))
  let v2 = Tolerance.using Quantity.zero (Vector2D.normalize (vA .+. m2 .*. vB))
  let d1 = Direction2D.unsafe v1
  let d2 = Direction2D.unsafe v2
  let vX = Tolerance.using Quantity.zero (Vector2D.normalize (v1 .+. v2))
  let dX = Direction2D.unsafe vX
  let frame = Frame2D.fromXAxis (Axis2D.through saddlePoint dX)
  SaddleRegion{subproblem, frame, d1, d2}

secondDerivative ::
  Quantity units ->
  Quantity units ->
  Quantity units ->
  Direction2D UvSpace ->
  Quantity units
secondDerivative fuu fuv fvv direction = do
  let Direction2D du dv = direction
  du .*. du .*. fuu .+. 2 *. du .*. dv .*. fuv .+. dv .*. dv .*. fvv

connectingCurve ::
  Tolerance units =>
  JoiningCurve ->
  SaddleRegion units ->
  Curve2D Unitless UvSpace
connectingCurve joiningCurve SaddleRegion{subproblem, frame, d1, d2} = do
  let Point2D x y = Point2D.relativeTo frame (joiningPoint joiningCurve)
  let saddlePoint = Frame2D.originPoint frame
  let dx = Frame2D.xDirection frame
  let dy = Frame2D.yDirection frame
  let boundingAxis direction = Axis2D.through saddlePoint direction
  case (Quantity.sign x, Quantity.sign y) of
    (Positive, Positive) ->
      connect subproblem frame d2 joiningCurve [boundingAxis dx, boundingAxis (negative dy)]
    (Positive, Negative) ->
      connect subproblem frame d1 joiningCurve [boundingAxis (negative dx), boundingAxis (negative dy)]
    (Negative, Positive) ->
      connect subproblem frame (negative d1) joiningCurve [boundingAxis dx, boundingAxis dy]
    (Negative, Negative) ->
      connect subproblem frame (negative d2) joiningCurve [boundingAxis (negative dx), boundingAxis dy]

connect ::
  Tolerance units =>
  Subproblem units ->
  Frame2D Unitless UvSpace PrincipalAxisSpace ->
  Direction2D UvSpace ->
  JoiningCurve ->
  List (Axis2D Unitless UvSpace) ->
  Curve2D Unitless UvSpace
connect subproblem frame outgoingDirection joiningCurve boundingAxes = do
  let saddlePoint = Frame2D.originPoint frame
  let Subproblem{f, dvdu, dudv, uvBounds} = subproblem
  let Bounds2D uBounds vBounds = uvBounds
  let UvPoint uP vP = saddlePoint
  let UvPoint uC vC = joiningPoint joiningCurve
  let Direction2D du dv = outgoingDirection
  if Quantity.abs du >= Quantity.abs dv
    then do
      let implicitBounds = NonEmpty.one (Bounds2D (Interval uP uC) vBounds)
      case joiningCurve of
        Incoming _ -> do
          let dudt = uP .-. uC
          let endDerivative = Vector2D dudt (dudt .*. (dv ./. du))
          let implicitCurve = HorizontalCurve.bounded f dvdu uC uP implicitBounds frame boundingAxes
          Curve2D.desingularize Nothing implicitCurve (Just (saddlePoint, endDerivative))
        Outgoing _ -> do
          let dudt = uC .-. uP
          let startDerivative = Vector2D dudt (dudt .*. (dv ./. du))
          let implicitCurve = HorizontalCurve.bounded f dvdu uP uC implicitBounds frame boundingAxes
          Curve2D.desingularize (Just (saddlePoint, startDerivative)) implicitCurve Nothing
    else do
      let implicitBounds = NonEmpty.one (Bounds2D uBounds (Interval vP vC))
      case joiningCurve of
        Incoming _ -> do
          let dvdt = vP .-. vC
          let endDerivative = Vector2D (dvdt .*. (du ./. dv)) dvdt
          let implicitCurve = VerticalCurve.bounded f dudv vC vP implicitBounds frame boundingAxes
          Curve2D.desingularize Nothing implicitCurve (Just (saddlePoint, endDerivative))
        Outgoing _ -> do
          let dvdt = vC .-. vP
          let startDerivative = Vector2D (dvdt .*. (du ./. dv)) dvdt
          let implicitCurve = VerticalCurve.bounded f dudv vP vC implicitBounds frame boundingAxes
          Curve2D.desingularize (Just (saddlePoint, startDerivative)) implicitCurve Nothing
