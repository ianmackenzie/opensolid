module OpenSolid.SurfaceFunction.Subproblem
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

import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Domain2d (Domain2d)
import OpenSolid.Domain2d qualified as Domain2d
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Internal qualified as Internal
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)

data Subproblem units = Subproblem
  { f :: SurfaceFunction units
  , dudv :: SurfaceFunction Unitless
  , dvdu :: SurfaceFunction Unitless
  , subdomain :: Domain2d
  , uvBounds :: UvBounds
  , fValues :: ~(CornerValues units)
  , fBounds :: ~(Bounds units)
  , fuBounds :: ~(Bounds units)
  , fvBounds :: ~(Bounds units)
  , fuuBounds :: ~(Bounds units)
  , fuvBounds :: ~(Bounds units)
  , fvvBounds :: ~(Bounds units)
  }

data CornerValues units = CornerValues
  { bottomLeft :: Quantity units
  , bottomRight :: Quantity units
  , topLeft :: Quantity units
  , topRight :: Quantity units
  }

new ::
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  SurfaceFunction Unitless ->
  Domain2d ->
  Subproblem units
new f dudv dvdu subdomain = do
  let uvBounds = subdomain.bounds
  Subproblem
    { f
    , dudv
    , dvdu
    , subdomain
    , uvBounds
    , fValues = cornerValues uvBounds f
    , fBounds = SurfaceFunction.evaluateBounds f uvBounds
    , fuBounds = SurfaceFunction.evaluateBounds f.du uvBounds
    , fvBounds = SurfaceFunction.evaluateBounds f.dv uvBounds
    , fuuBounds = SurfaceFunction.evaluateBounds f.du.du uvBounds
    , fuvBounds = SurfaceFunction.evaluateBounds f.du.dv uvBounds
    , fvvBounds = SurfaceFunction.evaluateBounds f.dv.dv uvBounds
    }

cornerValues :: UvBounds -> SurfaceFunction units -> CornerValues units
cornerValues (Bounds2d (Bounds u1 u2) (Bounds v1 v2)) function =
  CornerValues
    { bottomLeft = SurfaceFunction.evaluate function (Point2d u1 v1)
    , bottomRight = SurfaceFunction.evaluate function (Point2d u2 v1)
    , topLeft = SurfaceFunction.evaluate function (Point2d u1 v2)
    , topRight = SurfaceFunction.evaluate function (Point2d u2 v2)
    }

leftEdgePoint :: Tolerance units => Subproblem units -> (UvPoint, Domain2d.Boundary)
leftEdgePoint Subproblem{f, subdomain, uvBounds} = do
  let Bounds2d (Bounds u1 _) vBounds = uvBounds
  (Point2d u1 (Internal.solveForV f f.dv u1 vBounds), Domain2d.leftEdge subdomain)

rightEdgePoint :: Tolerance units => Subproblem units -> (UvPoint, Domain2d.Boundary)
rightEdgePoint Subproblem{f, subdomain, uvBounds} = do
  let Bounds2d (Bounds _ u2) vBounds = uvBounds
  (Point2d u2 (Internal.solveForV f f.dv u2 vBounds), Domain2d.rightEdge subdomain)

bottomEdgePoint :: Tolerance units => Subproblem units -> (UvPoint, Domain2d.Boundary)
bottomEdgePoint Subproblem{f, subdomain, uvBounds} = do
  let Bounds2d uBounds (Bounds v1 _) = uvBounds
  (Point2d (Internal.solveForU f f.du uBounds v1) v1, Domain2d.bottomEdge subdomain)

topEdgePoint :: Tolerance units => Subproblem units -> (UvPoint, Domain2d.Boundary)
topEdgePoint Subproblem{f, subdomain, uvBounds} = do
  let Bounds2d uBounds (Bounds _ v2) = uvBounds
  (Point2d (Internal.solveForU f f.du uBounds v2) v2, Domain2d.topEdge subdomain)

bottomLeftPoint :: Subproblem units -> (UvPoint, Domain2d.Boundary)
bottomLeftPoint Subproblem{subdomain, uvBounds} = do
  let Bounds2d (Bounds u1 _) (Bounds v1 _) = uvBounds
  (Point2d u1 v1, Domain2d.bottomLeftCorner subdomain)

bottomRightPoint :: Subproblem units -> (UvPoint, Domain2d.Boundary)
bottomRightPoint Subproblem{subdomain, uvBounds} = do
  let Bounds2d (Bounds _ u2) (Bounds v1 _) = uvBounds
  (Point2d u2 v1, Domain2d.bottomRightCorner subdomain)

topLeftPoint :: Subproblem units -> (UvPoint, Domain2d.Boundary)
topLeftPoint Subproblem{subdomain, uvBounds} = do
  let Bounds2d (Bounds u1 _) (Bounds _ v2) = uvBounds
  (Point2d u1 v2, Domain2d.topLeftCorner subdomain)

topRightPoint :: Subproblem units -> (UvPoint, Domain2d.Boundary)
topRightPoint Subproblem{subdomain, uvBounds} = do
  let Bounds2d (Bounds _ u2) (Bounds _ v2) = uvBounds
  (Point2d u2 v2, Domain2d.topRightCorner subdomain)

tightBounds :: Subproblem units -> Bounds units
tightBounds Subproblem{uvBounds, fValues, fBounds, fuBounds, fvBounds} = do
  let CornerValues{bottomLeft = f11, bottomRight = f21, topLeft = f12, topRight = f22} = fValues
  let Bounds2d (Bounds u1 u2) (Bounds v1 v2) = uvBounds
  let u1Bounds = Internal.curveBoundsAt v1 v2 f11 f12 fvBounds
  let u2Bounds = Internal.curveBoundsAt v1 v2 f21 f22 fvBounds
  let v1Bounds = Internal.curveBoundsAt u1 u2 f11 f21 fuBounds
  let v2Bounds = Internal.curveBoundsAt u1 u2 f12 f22 fuBounds
  let Bounds low0 high0 = fBounds
  let Bounds lowUV highUV = Internal.curveBoundsOver v1 v2 v1Bounds v2Bounds fvBounds
  let Bounds lowVU highVU = Internal.curveBoundsOver u1 u2 u1Bounds u2Bounds fuBounds
  let low = max low0 (max lowUV lowVU)
  let high = min high0 (min highUV highVU)
  assert (low <= high) (Bounds low high)

isZeroCandidate :: Tolerance units => Subproblem units -> Bool
isZeroCandidate subproblem = do
  let Subproblem{fBounds} = subproblem
  fBounds `intersects` Quantity.zero && tightBounds subproblem `intersects` Quantity.zero

leftEdgeBounds :: Subproblem units -> Bounds units
leftEdgeBounds Subproblem{uvBounds, fvBounds, fValues} = do
  let Bounds2d _ (Bounds v1 v2) = uvBounds
  let CornerValues{bottomLeft, topLeft} = fValues
  Internal.curveBoundsAt v1 v2 bottomLeft topLeft fvBounds

rightEdgeBounds :: Subproblem units -> Bounds units
rightEdgeBounds Subproblem{uvBounds, fvBounds, fValues} = do
  let Bounds2d _ (Bounds v1 v2) = uvBounds
  let CornerValues{bottomRight, topRight} = fValues
  Internal.curveBoundsAt v1 v2 bottomRight topRight fvBounds

bottomEdgeBounds :: Subproblem units -> Bounds units
bottomEdgeBounds Subproblem{uvBounds, fuBounds, fValues} = do
  let Bounds2d (Bounds u1 u2) _ = uvBounds
  let CornerValues{bottomLeft, bottomRight} = fValues
  Internal.curveBoundsAt u1 u2 bottomLeft bottomRight fuBounds

topEdgeBounds :: Subproblem units -> Bounds units
topEdgeBounds Subproblem{uvBounds, fuBounds, fValues} = do
  let Bounds2d (Bounds u1 u2) _ = uvBounds
  let CornerValues{topLeft, topRight} = fValues
  Internal.curveBoundsAt u1 u2 topLeft topRight fuBounds
