module Surface1d.Function.SaddleRegion
  ( SaddleRegion
  , Frame
  , point
  , subdomain
  , bounds
  , quadratic
  , connectingCurves
  )
where

import Axis2d (Axis2d)
import Axis2d qualified
import Bounds2d (Bounds2d (Bounds2d))
import Curve2d (Curve2d)
import Curve2d qualified
import Direction2d qualified
import Domain2d (Domain2d)
import Frame2d (Frame2d)
import Frame2d qualified
import NonEmpty qualified
import OpenSolid
import Point2d qualified
import Qty qualified
import {-# SOURCE #-} Surface1d.Function qualified as Function
import Surface1d.Function.HorizontalCurve qualified as HorizontalCurve
import Surface1d.Function.Subproblem (Subproblem (Subproblem))
import Surface1d.Function.Subproblem qualified as Subproblem
import Surface1d.Function.VerticalCurve qualified as VerticalCurve
import SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvCoordinates, UvDirection, UvPoint)
import Uv.Derivatives qualified as Derivatives
import Vector2d qualified

data SaddleRegion units = SaddleRegion
  { subproblem :: Subproblem units
  , frame :: Frame
  , d1 :: UvDirection
  , d2 :: UvDirection
  }

data PrincipalAxisSpace

type Frame = Frame2d UvCoordinates (Defines PrincipalAxisSpace)

point :: SaddleRegion units -> UvPoint
point SaddleRegion{frame} = Frame2d.originPoint frame

subdomain :: SaddleRegion units -> Domain2d
subdomain SaddleRegion{subproblem} = Subproblem.subdomain subproblem

bounds :: SaddleRegion units -> UvBounds
bounds SaddleRegion{subproblem} = Subproblem.uvBounds subproblem

quadratic :: Subproblem units -> UvPoint -> SaddleRegion units
quadratic subproblem saddlePoint = do
  let Subproblem{derivatives} = subproblem
  let fuu = Function.evaluate (Derivatives.get (derivatives >> U >> U)) saddlePoint
  let fuv = Function.evaluate (Derivatives.get (derivatives >> U >> V)) saddlePoint
  let fvv = Function.evaluate (Derivatives.get (derivatives >> V >> V)) saddlePoint
  let bDirectionCandidates = NonEmpty.of3 Direction2d.x Direction2d.y (Direction2d.degrees 45.0)
  let directionalSecondDerivative = secondDerivative fuu fuv fvv
  let dB = NonEmpty.maximumBy (Qty.abs . directionalSecondDerivative) bDirectionCandidates
  let dA = Direction2d.rotateRight dB
  let vA = Direction2d.unwrap dA
  let vB = Direction2d.unwrap dB
  let (ua, va) = Vector2d.components vA
  let (ub, vb) = Vector2d.components vB
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

secondDerivative :: Qty units -> Qty units -> Qty units -> UvDirection -> Qty units
secondDerivative fuu fuv fvv direction = do
  let (du, dv) = Direction2d.components direction
  du * du * fuu + 2.0 * du * dv * fuv + dv * dv * fvv

connectingCurves ::
  Tolerance units =>
  UvPoint ->
  SaddleRegion units ->
  NonEmpty (Curve2d UvCoordinates)
connectingCurves boundaryPoint SaddleRegion{subproblem, frame, d1, d2} = do
  let (x, y) = Point2d.coordinates (Point2d.relativeTo frame boundaryPoint)
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
  UvDirection ->
  UvPoint ->
  List (Axis2d UvCoordinates) ->
  NonEmpty (Curve2d UvCoordinates)
connect subproblem frame startDirection endPoint boundingAxes = do
  let startPoint = Frame2d.originPoint frame
  let Subproblem{derivatives, dvdu, dudv, uvBounds} = subproblem
  let Bounds2d uBounds vBounds = uvBounds
  let (u1, v1) = Point2d.coordinates startPoint
  let (u2, v2) = Point2d.coordinates endPoint
  let (du, dv) = Direction2d.components startDirection
  if Qty.abs du >= Qty.abs dv
    then do
      let uMid = u1 + 1e-3 * Qty.sign (u2 - u1) |> Qty.clamp u1 u2
      let startDerivative = Vector2d.xy (uMid - u1) ((uMid - u1) * (dv / du))
      let interpolatingCurve =
            HorizontalCurve.bounded derivatives dvdu u1 uMid vBounds frame boundingAxes
              |> Curve2d.removeStartDegeneracy 2 (startPoint, [startDerivative])
      if uMid == u2
        then NonEmpty.singleton interpolatingCurve
        else do
          let implicitCurve = HorizontalCurve.bounded derivatives dvdu uMid u2 vBounds frame boundingAxes
          NonEmpty.of2 interpolatingCurve implicitCurve
    else do
      let vMid = v1 + 1e-3 * Qty.sign (v2 - v1) |> Qty.clamp v1 v2
      let startDerivative = Vector2d.xy ((vMid - v1) * (du / dv)) (vMid - v1)
      let interpolatingCurve =
            VerticalCurve.bounded derivatives dudv uBounds v1 vMid frame boundingAxes
              |> Curve2d.removeStartDegeneracy 2 (startPoint, [startDerivative])
      if vMid == v2
        then NonEmpty.singleton interpolatingCurve
        else do
          let implicitCurve = VerticalCurve.bounded derivatives dudv uBounds vMid v2 frame boundingAxes
          NonEmpty.of2 interpolatingCurve implicitCurve
