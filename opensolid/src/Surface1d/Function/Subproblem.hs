module Surface1d.Function.Subproblem
  ( Subproblem (..)
  , CornerValues (..)
  , new
  , leftEdgePoint
  , rightEdgePoint
  , bottomEdgePoint
  , topEdgePoint
  , bottomLeftPoint
  , bottomRightPoint
  , topLeftPoint
  , topRightPoint
  , tightBounds
  , isZeroCandidate
  , leftEdgeBounds
  , rightEdgeBounds
  , bottomEdgeBounds
  , topEdgeBounds
  )
where

import Bounds2d (Bounds2d (Bounds2d))
import Debug qualified
import Domain2d (Domain2d)
import Domain2d qualified
import OpenSolid
import Point2d qualified
import Qty qualified
import Range (Range (Range))
import Range qualified
import {-# SOURCE #-} Surface1d.Function (Function)
import {-# SOURCE #-} Surface1d.Function qualified as Function
import Surface1d.Function.Internal qualified as Internal
import Uv (Parameter (U, V))
import Uv qualified
import Uv.Derivatives (Derivatives)
import Uv.Derivatives qualified as Derivatives

data Subproblem units = Subproblem
  { derivatives :: Derivatives (Function units)
  , subdomain :: Domain2d
  , uvBounds :: Uv.Bounds
  , derivativeBounds :: Derivatives (Range units)
  , derivativeValues :: Derivatives (CornerValues units)
  }

instance Composition (Subproblem units) Uv.Parameter (Subproblem units) where
  Subproblem{derivatives, subdomain, uvBounds, derivativeBounds, derivativeValues} >> parameter =
    Subproblem
      { derivatives = derivatives >> parameter
      , subdomain = subdomain
      , uvBounds = uvBounds
      , derivativeBounds = derivativeBounds >> parameter
      , derivativeValues = derivativeValues >> parameter
      }

data CornerValues units = CornerValues
  { bottomLeft :: Qty units
  , bottomRight :: Qty units
  , topLeft :: Qty units
  , topRight :: Qty units
  }

new :: Derivatives (Function units) -> Domain2d -> Subproblem units
new derivatives subdomain = do
  let uvBounds = Domain2d.bounds subdomain
  let derivativeBounds = Derivatives.map (\d -> Function.bounds d uvBounds) derivatives
  let derivativeValues = Derivatives.map (cornerValues uvBounds) derivatives
  Subproblem{derivatives, subdomain, uvBounds, derivativeBounds, derivativeValues}

cornerValues :: Uv.Bounds -> Function units -> CornerValues units
cornerValues (Bounds2d (Range u1 u2) (Range v1 v2)) function =
  CornerValues
    { bottomLeft = Function.evaluate function (Point2d.xy u1 v1)
    , bottomRight = Function.evaluate function (Point2d.xy u2 v1)
    , topLeft = Function.evaluate function (Point2d.xy u1 v2)
    , topRight = Function.evaluate function (Point2d.xy u2 v2)
    }

leftEdgePoint :: Tolerance units => Subproblem units -> (Uv.Point, Domain2d.Boundary)
leftEdgePoint Subproblem{derivatives, subdomain, uvBounds} = do
  let Bounds2d (Range u1 _) vBounds = uvBounds
  let f = Derivatives.get derivatives
  let fv = Derivatives.get (derivatives >> V)
  (Point2d.xy u1 (Internal.solveForV f fv u1 vBounds), Domain2d.leftEdge subdomain)

rightEdgePoint :: Tolerance units => Subproblem units -> (Uv.Point, Domain2d.Boundary)
rightEdgePoint Subproblem{derivatives, subdomain, uvBounds} = do
  let Bounds2d (Range _ u2) vBounds = uvBounds
  let f = Derivatives.get derivatives
  let fv = Derivatives.get (derivatives >> V)
  (Point2d.xy u2 (Internal.solveForV f fv u2 vBounds), Domain2d.rightEdge subdomain)

bottomEdgePoint :: Tolerance units => Subproblem units -> (Uv.Point, Domain2d.Boundary)
bottomEdgePoint Subproblem{derivatives, subdomain, uvBounds} = do
  let Bounds2d uBounds (Range v1 _) = uvBounds
  let f = Derivatives.get derivatives
  let fu = Derivatives.get (derivatives >> U)
  (Point2d.xy (Internal.solveForU f fu uBounds v1) v1, Domain2d.bottomEdge subdomain)

topEdgePoint :: Tolerance units => Subproblem units -> (Uv.Point, Domain2d.Boundary)
topEdgePoint Subproblem{derivatives, subdomain, uvBounds} = do
  let Bounds2d uBounds (Range _ v2) = uvBounds
  let f = Derivatives.get derivatives
  let fu = Derivatives.get (derivatives >> U)
  (Point2d.xy (Internal.solveForU f fu uBounds v2) v2, Domain2d.topEdge subdomain)

bottomLeftPoint :: Subproblem units -> (Uv.Point, Domain2d.Boundary)
bottomLeftPoint Subproblem{subdomain, uvBounds} = do
  let Bounds2d (Range u1 _) (Range v1 _) = uvBounds
  (Point2d.xy u1 v1, Domain2d.bottomLeftCorner subdomain)

bottomRightPoint :: Subproblem units -> (Uv.Point, Domain2d.Boundary)
bottomRightPoint Subproblem{subdomain, uvBounds} = do
  let Bounds2d (Range _ u2) (Range v1 _) = uvBounds
  (Point2d.xy u2 v1, Domain2d.bottomRightCorner subdomain)

topLeftPoint :: Subproblem units -> (Uv.Point, Domain2d.Boundary)
topLeftPoint Subproblem{subdomain, uvBounds} = do
  let Bounds2d (Range u1 _) (Range _ v2) = uvBounds
  (Point2d.xy u1 v2, Domain2d.topLeftCorner subdomain)

topRightPoint :: Subproblem units -> (Uv.Point, Domain2d.Boundary)
topRightPoint Subproblem{subdomain, uvBounds} = do
  let Bounds2d (Range _ u2) (Range _ v2) = uvBounds
  (Point2d.xy u2 v2, Domain2d.topRightCorner subdomain)

tightBounds :: Subproblem units -> Range units
tightBounds Subproblem{uvBounds, derivativeBounds, derivativeValues} = do
  let fValues = Derivatives.get derivativeValues
  let CornerValues{bottomLeft = f11, bottomRight = f21, topLeft = f12, topRight = f22} = fValues
  let Bounds2d (Range u1 u2) (Range v1 v2) = uvBounds
  let fuBounds = Derivatives.get (derivativeBounds >> U)
  let fvBounds = Derivatives.get (derivativeBounds >> V)
  let u1Bounds = Internal.curveBounds v1 v2 f11 f12 fvBounds
  let u2Bounds = Internal.curveBounds v1 v2 f21 f22 fvBounds
  let v1Bounds = Internal.curveBounds u1 u2 f11 f21 fuBounds
  let v2Bounds = Internal.curveBounds u1 u2 f12 f22 fuBounds
  let Range low0 high0 = Derivatives.get derivativeBounds
  let Range lowUV highUV = Internal.curveRangeBounds v1 v2 v1Bounds v2Bounds fvBounds
  let Range lowVU highVU = Internal.curveRangeBounds u1 u2 u1Bounds u2Bounds fuBounds
  let low = Qty.max low0 (Qty.max lowUV lowVU)
  let high = Qty.min high0 (Qty.min highUV highVU)
  Debug.assert (low <= high)
  Range.from low high

isZeroCandidate :: Tolerance units => Subproblem units -> Bool
isZeroCandidate subproblem = do
  let Subproblem{derivativeBounds} = subproblem
  let fBounds = Derivatives.get derivativeBounds
  fBounds ^ Qty.zero && tightBounds subproblem ^ Qty.zero

leftEdgeBounds :: Subproblem units -> Range units
leftEdgeBounds Subproblem{uvBounds, derivativeBounds, derivativeValues} = do
  let Bounds2d _ (Range v1 v2) = uvBounds
  let fvBounds = Derivatives.get (derivativeBounds >> V)
  let CornerValues{bottomLeft, topLeft} = Derivatives.get derivativeValues
  Internal.curveBounds v1 v2 bottomLeft topLeft fvBounds

rightEdgeBounds :: Subproblem units -> Range units
rightEdgeBounds Subproblem{uvBounds, derivativeBounds, derivativeValues} = do
  let Bounds2d _ (Range v1 v2) = uvBounds
  let fvBounds = Derivatives.get (derivativeBounds >> V)
  let CornerValues{bottomRight, topRight} = Derivatives.get derivativeValues
  Internal.curveBounds v1 v2 bottomRight topRight fvBounds

bottomEdgeBounds :: Subproblem units -> Range units
bottomEdgeBounds Subproblem{uvBounds, derivativeBounds, derivativeValues} = do
  let Bounds2d (Range u1 u2) _ = uvBounds
  let fuBounds = Derivatives.get (derivativeBounds >> U)
  let CornerValues{bottomLeft, bottomRight} = Derivatives.get derivativeValues
  Internal.curveBounds u1 u2 bottomLeft bottomRight fuBounds

topEdgeBounds :: Subproblem units -> Range units
topEdgeBounds Subproblem{uvBounds, derivativeBounds, derivativeValues} = do
  let Bounds2d (Range u1 u2) _ = uvBounds
  let fuBounds = Derivatives.get (derivativeBounds >> U)
  let CornerValues{topLeft, topRight} = Derivatives.get derivativeValues
  Internal.curveBounds u1 u2 topLeft topRight fuBounds
