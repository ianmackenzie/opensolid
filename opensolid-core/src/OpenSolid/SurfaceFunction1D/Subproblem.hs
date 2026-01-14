module OpenSolid.SurfaceFunction1D.Subproblem
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

import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Domain2D (Domain2D)
import OpenSolid.Domain2D qualified as Domain2D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceFunction1D.Internal qualified as Internal
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)

data Subproblem units = Subproblem
  { f :: SurfaceFunction1D units
  , dudv :: SurfaceFunction1D Unitless
  , dvdu :: SurfaceFunction1D Unitless
  , subdomain :: Domain2D
  , uvBounds :: UvBounds
  , fValues :: ~(CornerValues units)
  , fBounds :: ~(Interval units)
  , fuBounds :: ~(Interval units)
  , fvBounds :: ~(Interval units)
  , fuuBounds :: ~(Interval units)
  , fuvBounds :: ~(Interval units)
  , fvvBounds :: ~(Interval units)
  }

data CornerValues units = CornerValues
  { bottomLeft :: Quantity units
  , bottomRight :: Quantity units
  , topLeft :: Quantity units
  , topRight :: Quantity units
  }

new ::
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  SurfaceFunction1D Unitless ->
  Domain2D ->
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
    , fBounds = SurfaceFunction1D.evaluateBounds f uvBounds
    , fuBounds = SurfaceFunction1D.evaluateBounds f.du uvBounds
    , fvBounds = SurfaceFunction1D.evaluateBounds f.dv uvBounds
    , fuuBounds = SurfaceFunction1D.evaluateBounds f.du.du uvBounds
    , fuvBounds = SurfaceFunction1D.evaluateBounds f.du.dv uvBounds
    , fvvBounds = SurfaceFunction1D.evaluateBounds f.dv.dv uvBounds
    }

cornerValues :: UvBounds -> SurfaceFunction1D units -> CornerValues units
cornerValues (Bounds2D (Interval u1 u2) (Interval v1 v2)) function =
  CornerValues
    { bottomLeft = SurfaceFunction1D.evaluate function (UvPoint u1 v1)
    , bottomRight = SurfaceFunction1D.evaluate function (UvPoint u2 v1)
    , topLeft = SurfaceFunction1D.evaluate function (UvPoint u1 v2)
    , topRight = SurfaceFunction1D.evaluate function (UvPoint u2 v2)
    }

leftEdgePoint :: Tolerance units => Subproblem units -> (UvPoint, Domain2D.Boundary)
leftEdgePoint Subproblem{f, subdomain, uvBounds} = do
  let Bounds2D (Interval u1 _) vBounds = uvBounds
  (UvPoint u1 (Internal.solveForV f f.dv u1 vBounds), Domain2D.leftEdge subdomain)

rightEdgePoint :: Tolerance units => Subproblem units -> (UvPoint, Domain2D.Boundary)
rightEdgePoint Subproblem{f, subdomain, uvBounds} = do
  let Bounds2D (Interval _ u2) vBounds = uvBounds
  (UvPoint u2 (Internal.solveForV f f.dv u2 vBounds), Domain2D.rightEdge subdomain)

bottomEdgePoint :: Tolerance units => Subproblem units -> (UvPoint, Domain2D.Boundary)
bottomEdgePoint Subproblem{f, subdomain, uvBounds} = do
  let Bounds2D uBounds (Interval v1 _) = uvBounds
  (UvPoint (Internal.solveForU f f.du uBounds v1) v1, Domain2D.bottomEdge subdomain)

topEdgePoint :: Tolerance units => Subproblem units -> (UvPoint, Domain2D.Boundary)
topEdgePoint Subproblem{f, subdomain, uvBounds} = do
  let Bounds2D uBounds (Interval _ v2) = uvBounds
  (UvPoint (Internal.solveForU f f.du uBounds v2) v2, Domain2D.topEdge subdomain)

bottomLeftPoint :: Subproblem units -> (UvPoint, Domain2D.Boundary)
bottomLeftPoint Subproblem{subdomain, uvBounds} = do
  let Bounds2D (Interval u1 _) (Interval v1 _) = uvBounds
  (UvPoint u1 v1, Domain2D.bottomLeftCorner subdomain)

bottomRightPoint :: Subproblem units -> (UvPoint, Domain2D.Boundary)
bottomRightPoint Subproblem{subdomain, uvBounds} = do
  let Bounds2D (Interval _ u2) (Interval v1 _) = uvBounds
  (UvPoint u2 v1, Domain2D.bottomRightCorner subdomain)

topLeftPoint :: Subproblem units -> (UvPoint, Domain2D.Boundary)
topLeftPoint Subproblem{subdomain, uvBounds} = do
  let Bounds2D (Interval u1 _) (Interval _ v2) = uvBounds
  (UvPoint u1 v2, Domain2D.topLeftCorner subdomain)

topRightPoint :: Subproblem units -> (UvPoint, Domain2D.Boundary)
topRightPoint Subproblem{subdomain, uvBounds} = do
  let Bounds2D (Interval _ u2) (Interval _ v2) = uvBounds
  (UvPoint u2 v2, Domain2D.topRightCorner subdomain)

tightBounds :: Subproblem units -> Interval units
tightBounds Subproblem{uvBounds, fValues, fBounds, fuBounds, fvBounds} = do
  let CornerValues{bottomLeft = f11, bottomRight = f21, topLeft = f12, topRight = f22} = fValues
  let Bounds2D (Interval u1 u2) (Interval v1 v2) = uvBounds
  let u1Bounds = Internal.curveBoundsAt v1 v2 f11 f12 fvBounds
  let u2Bounds = Internal.curveBoundsAt v1 v2 f21 f22 fvBounds
  let v1Bounds = Internal.curveBoundsAt u1 u2 f11 f21 fuBounds
  let v2Bounds = Internal.curveBoundsAt u1 u2 f12 f22 fuBounds
  let Interval low0 high0 = fBounds
  let Interval lowUV highUV = Internal.curveBoundsOver v1 v2 v1Bounds v2Bounds fvBounds
  let Interval lowVU highVU = Internal.curveBoundsOver u1 u2 u1Bounds u2Bounds fuBounds
  let low = max low0 (max lowUV lowVU)
  let high = min high0 (min highUV highVU)
  assert (low <= high) (Interval low high)

isZeroCandidate :: Tolerance units => Subproblem units -> Bool
isZeroCandidate subproblem = do
  let Subproblem{fBounds} = subproblem
  fBounds `intersects` Quantity.zero && tightBounds subproblem `intersects` Quantity.zero

leftEdgeBounds :: Subproblem units -> Interval units
leftEdgeBounds Subproblem{uvBounds, fvBounds, fValues} = do
  let Bounds2D _ (Interval v1 v2) = uvBounds
  let CornerValues{bottomLeft, topLeft} = fValues
  Internal.curveBoundsAt v1 v2 bottomLeft topLeft fvBounds

rightEdgeBounds :: Subproblem units -> Interval units
rightEdgeBounds Subproblem{uvBounds, fvBounds, fValues} = do
  let Bounds2D _ (Interval v1 v2) = uvBounds
  let CornerValues{bottomRight, topRight} = fValues
  Internal.curveBoundsAt v1 v2 bottomRight topRight fvBounds

bottomEdgeBounds :: Subproblem units -> Interval units
bottomEdgeBounds Subproblem{uvBounds, fuBounds, fValues} = do
  let Bounds2D (Interval u1 u2) _ = uvBounds
  let CornerValues{bottomLeft, bottomRight} = fValues
  Internal.curveBoundsAt u1 u2 bottomLeft bottomRight fuBounds

topEdgeBounds :: Subproblem units -> Interval units
topEdgeBounds Subproblem{uvBounds, fuBounds, fValues} = do
  let Bounds2D (Interval u1 u2) _ = uvBounds
  let CornerValues{topLeft, topRight} = fValues
  Internal.curveBoundsAt u1 u2 topLeft topRight fuBounds
