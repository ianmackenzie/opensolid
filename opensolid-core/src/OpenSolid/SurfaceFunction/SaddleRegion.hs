module OpenSolid.SurfaceFunction.SaddleRegion
  ( SaddleRegion
  , Frame
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
  let determinant = fab .*. fab - faa .*. faa
  let sqrtD = Qty.sqrt' determinant
  let m1 = (-fab + sqrtD) / fbb
  let m2 = (-fab - sqrtD) / fbb
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
  UvPoint ->
  SaddleRegion units ->
  NonEmpty (Curve2d UvCoordinates)
connectingCurves boundaryPoint SaddleRegion{subproblem, frame, d1, d2} = do
  let Point2d x y = Point2d.relativeTo frame boundaryPoint
  let saddlePoint = Frame2d.originPoint frame
  let dx = Frame2d.xDirection frame
  let dy = Frame2d.yDirection frame
  case (Qty.sign x, Qty.sign y) of
    (Positive, Positive) ->
      connect subproblem frame d1 boundaryPoint $
        [ Axis2d.through saddlePoint dx
        , Axis2d.through saddlePoint -dy
        ]
    (Positive, Negative) ->
      connect subproblem frame d2 boundaryPoint $
        [ Axis2d.through saddlePoint -dx
        , Axis2d.through saddlePoint -dy
        ]
    (Negative, Positive) ->
      connect subproblem frame -d2 boundaryPoint $
        [ Axis2d.through saddlePoint dx
        , Axis2d.through saddlePoint dy
        ]
    (Negative, Negative) ->
      connect subproblem frame -d1 boundaryPoint $
        [ Axis2d.through saddlePoint -dx
        , Axis2d.through saddlePoint dy
        ]

connect ::
  Tolerance units =>
  Subproblem units ->
  Frame2d UvCoordinates (Defines PrincipalAxisSpace) ->
  Direction2d UvSpace ->
  UvPoint ->
  List (Axis2d UvCoordinates) ->
  NonEmpty (Curve2d UvCoordinates)
connect subproblem frame startDirection endPoint boundingAxes = do
  let startPoint = Frame2d.originPoint frame
  let Subproblem{f, dvdu, dudv, uvBounds} = subproblem
  let Bounds2d uBounds vBounds = uvBounds
  let Point2d u1 v1 = startPoint
  let Point2d u2 v2 = endPoint
  let Direction2d du dv = startDirection
  if Qty.abs du >= Qty.abs dv
    then do
      let uMid = u1 + 1e-3 * Qty.sign (u2 - u1) |> Qty.clampTo (Bounds u1 u2)
      let startDerivative = Vector2d (uMid - u1) ((uMid - u1) * (dv / du))
      let interpolatingBounds = NonEmpty.one (Bounds2d (Bounds u1 uMid) vBounds)
      let interpolatingCurve =
            HorizontalCurve.bounded f dvdu u1 uMid interpolatingBounds frame boundingAxes
              |> Curve2d.removeStartDegeneracy 2 startPoint [startDerivative]
      if uMid == u2
        then NonEmpty.one interpolatingCurve
        else do
          let implicitBounds = NonEmpty.one (Bounds2d (Bounds uMid u2) vBounds)
          let implicitCurve =
                HorizontalCurve.bounded f dvdu uMid u2 implicitBounds frame boundingAxes
          NonEmpty.two interpolatingCurve implicitCurve
    else do
      let vMid = v1 + 1e-3 * Qty.sign (v2 - v1) |> Qty.clampTo (Bounds v1 v2)
      let startDerivative = Vector2d ((vMid - v1) * (du / dv)) (vMid - v1)
      let interpolatingBounds = NonEmpty.one (Bounds2d uBounds (Bounds v1 vMid))
      let interpolatingCurve =
            VerticalCurve.bounded f dudv v1 vMid interpolatingBounds frame boundingAxes
              |> Curve2d.removeStartDegeneracy 2 startPoint [startDerivative]
      if vMid == v2
        then NonEmpty.one interpolatingCurve
        else do
          let implicitBounds = NonEmpty.one (Bounds2d uBounds (Bounds vMid v2))
          let implicitCurve =
                VerticalCurve.bounded f dudv vMid v2 implicitBounds frame boundingAxes
          NonEmpty.two interpolatingCurve implicitCurve
