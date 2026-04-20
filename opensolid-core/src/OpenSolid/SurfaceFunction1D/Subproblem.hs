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
  , tightRange
  , isZeroCandidate
  , leftEdgeRange
  , rightEdgeRange
  , bottomEdgeRange
  , topEdgeRange
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
  , uvRange :: UvBounds
  , fValues :: ~(CornerValues units)
  , fRange :: ~(Interval units)
  , fuRange :: ~(Interval units)
  , fvRange :: ~(Interval units)
  , fuuRange :: ~(Interval units)
  , fuvRange :: ~(Interval units)
  , fvvRange :: ~(Interval units)
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
  let uvRange = Domain2D.bounds subdomain
  Subproblem
    { f
    , dudv
    , dvdu
    , subdomain
    , uvRange
    , fValues = cornerValues uvRange f
    , fRange = SurfaceFunction1D.range f uvRange
    , fuRange = SurfaceFunction1D.range f.du uvRange
    , fvRange = SurfaceFunction1D.range f.dv uvRange
    , fuuRange = SurfaceFunction1D.range f.du.du uvRange
    , fuvRange = SurfaceFunction1D.range f.du.dv uvRange
    , fvvRange = SurfaceFunction1D.range f.dv.dv uvRange
    }

cornerValues :: UvBounds -> SurfaceFunction1D units -> CornerValues units
cornerValues (Bounds2D (Interval u1 u2) (Interval v1 v2)) function =
  CornerValues
    { bottomLeft = SurfaceFunction1D.value function (UvPoint u1 v1)
    , bottomRight = SurfaceFunction1D.value function (UvPoint u2 v1)
    , topLeft = SurfaceFunction1D.value function (UvPoint u1 v2)
    , topRight = SurfaceFunction1D.value function (UvPoint u2 v2)
    }

leftEdgePoint :: Tolerance units => Subproblem units -> (UvPoint, Domain2D.Boundary)
leftEdgePoint Subproblem{f, subdomain, uvRange} = do
  let Bounds2D (Interval u1 _) vRange = uvRange
  (UvPoint u1 (Internal.solveForV f f.dv u1 vRange), Domain2D.leftEdge subdomain)

rightEdgePoint :: Tolerance units => Subproblem units -> (UvPoint, Domain2D.Boundary)
rightEdgePoint Subproblem{f, subdomain, uvRange} = do
  let Bounds2D (Interval _ u2) vRange = uvRange
  (UvPoint u2 (Internal.solveForV f f.dv u2 vRange), Domain2D.rightEdge subdomain)

bottomEdgePoint :: Tolerance units => Subproblem units -> (UvPoint, Domain2D.Boundary)
bottomEdgePoint Subproblem{f, subdomain, uvRange} = do
  let Bounds2D uRange (Interval v1 _) = uvRange
  (UvPoint (Internal.solveForU f f.du uRange v1) v1, Domain2D.bottomEdge subdomain)

topEdgePoint :: Tolerance units => Subproblem units -> (UvPoint, Domain2D.Boundary)
topEdgePoint Subproblem{f, subdomain, uvRange} = do
  let Bounds2D uRange (Interval _ v2) = uvRange
  (UvPoint (Internal.solveForU f f.du uRange v2) v2, Domain2D.topEdge subdomain)

bottomLeftPoint :: Subproblem units -> (UvPoint, Domain2D.Boundary)
bottomLeftPoint Subproblem{subdomain, uvRange} = do
  let Bounds2D (Interval u1 _) (Interval v1 _) = uvRange
  (UvPoint u1 v1, Domain2D.bottomLeftCorner subdomain)

bottomRightPoint :: Subproblem units -> (UvPoint, Domain2D.Boundary)
bottomRightPoint Subproblem{subdomain, uvRange} = do
  let Bounds2D (Interval _ u2) (Interval v1 _) = uvRange
  (UvPoint u2 v1, Domain2D.bottomRightCorner subdomain)

topLeftPoint :: Subproblem units -> (UvPoint, Domain2D.Boundary)
topLeftPoint Subproblem{subdomain, uvRange} = do
  let Bounds2D (Interval u1 _) (Interval _ v2) = uvRange
  (UvPoint u1 v2, Domain2D.topLeftCorner subdomain)

topRightPoint :: Subproblem units -> (UvPoint, Domain2D.Boundary)
topRightPoint Subproblem{subdomain, uvRange} = do
  let Bounds2D (Interval _ u2) (Interval _ v2) = uvRange
  (UvPoint u2 v2, Domain2D.topRightCorner subdomain)

tightRange :: Subproblem units -> Interval units
tightRange Subproblem{uvRange, fValues, fRange, fuRange, fvRange} = do
  let CornerValues{bottomLeft = f11, bottomRight = f21, topLeft = f12, topRight = f22} = fValues
  let Bounds2D (Interval u1 u2) (Interval v1 v2) = uvRange
  let u1Range = Internal.curveRangeAt v1 v2 f11 f12 fvRange
  let u2Range = Internal.curveRangeAt v1 v2 f21 f22 fvRange
  let v1Range = Internal.curveRangeAt u1 u2 f11 f21 fuRange
  let v2Range = Internal.curveRangeAt u1 u2 f12 f22 fuRange
  let Interval low0 high0 = fRange
  let Interval lowUV highUV = Internal.curveRangeOver v1 v2 v1Range v2Range fvRange
  let Interval lowVU highVU = Internal.curveRangeOver u1 u2 u1Range u2Range fuRange
  let low = max low0 (max lowUV lowVU)
  let high = min high0 (min highUV highVU)
  assert (low <= high) (Interval low high)

isZeroCandidate :: Tolerance units => Subproblem units -> Bool
isZeroCandidate subproblem = do
  let Subproblem{fRange} = subproblem
  fRange `intersects` Quantity.zero && tightRange subproblem `intersects` Quantity.zero

leftEdgeRange :: Subproblem units -> Interval units
leftEdgeRange Subproblem{uvRange, fvRange, fValues} = do
  let Bounds2D _ (Interval v1 v2) = uvRange
  let CornerValues{bottomLeft, topLeft} = fValues
  Internal.curveRangeAt v1 v2 bottomLeft topLeft fvRange

rightEdgeRange :: Subproblem units -> Interval units
rightEdgeRange Subproblem{uvRange, fvRange, fValues} = do
  let Bounds2D _ (Interval v1 v2) = uvRange
  let CornerValues{bottomRight, topRight} = fValues
  Internal.curveRangeAt v1 v2 bottomRight topRight fvRange

bottomEdgeRange :: Subproblem units -> Interval units
bottomEdgeRange Subproblem{uvRange, fuRange, fValues} = do
  let Bounds2D (Interval u1 u2) _ = uvRange
  let CornerValues{bottomLeft, bottomRight} = fValues
  Internal.curveRangeAt u1 u2 bottomLeft bottomRight fuRange

topEdgeRange :: Subproblem units -> Interval units
topEdgeRange Subproblem{uvRange, fuRange, fValues} = do
  let Bounds2D (Interval u1 u2) _ = uvRange
  let CornerValues{topLeft, topRight} = fValues
  Internal.curveRangeAt u1 u2 topLeft topRight fuRange
