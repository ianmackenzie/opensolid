module OpenSolid.SurfaceFunction1D.PartialZeros
  ( PartialZeros (..)
  , Parameterization (..)
  , CrossingSegment (..)
  , empty
  , diagonalSegment
  , horizontalSegment
  , verticalSegment
  , addCrossingSegment
  , addTangentPoint
  , addSaddleRegion
  , finalize
  )
where

import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Domain2D qualified as Domain2D
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D.HorizontalCurve qualified as HorizontalCurve
import OpenSolid.SurfaceFunction1D.SaddleRegion (SaddleRegion)
import OpenSolid.SurfaceFunction1D.SaddleRegion qualified as SaddleRegion
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D.VerticalCurve qualified as VerticalCurve
import OpenSolid.SurfaceFunction1D.Zeros (Zeros (Zeros))
import OpenSolid.SurfaceFunction1D.Zeros qualified as Zeros
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint, pattern UvPoint)
import OpenSolid.UvSpace (UvSpace)

data PartialZeros units = PartialZeros
  { crossingSegments :: List CrossingSegment
  , tangentPoints :: List (UvPoint, Sign)
  , saddleRegions :: List (SaddleRegion units)
  }

data Parameterization = Horizontal | Vertical | Diagonal

data CrossingSegment
  = CrossingSegment
      Parameterization
      (UvPoint, Domain2D.Boundary)
      (UvPoint, Domain2D.Boundary)
      (NonEmpty UvBounds)

diagonalSegment ::
  (UvPoint, Domain2D.Boundary) ->
  (UvPoint, Domain2D.Boundary) ->
  UvBounds ->
  CrossingSegment
diagonalSegment start end uvBounds = CrossingSegment Diagonal start end (NonEmpty.one uvBounds)

horizontalSegment ::
  (UvPoint, Domain2D.Boundary) ->
  (UvPoint, Domain2D.Boundary) ->
  UvBounds ->
  CrossingSegment
horizontalSegment start end uvBounds = CrossingSegment Horizontal start end (NonEmpty.one uvBounds)

verticalSegment ::
  (UvPoint, Domain2D.Boundary) ->
  (UvPoint, Domain2D.Boundary) ->
  UvBounds ->
  CrossingSegment
verticalSegment start end uvBounds = CrossingSegment Vertical start end (NonEmpty.one uvBounds)

empty :: PartialZeros units
empty =
  PartialZeros
    { crossingSegments = []
    , tangentPoints = []
    , saddleRegions = []
    }

addCrossingSegment :: CrossingSegment -> PartialZeros units -> PartialZeros units
addCrossingSegment newCrossingSegment partialZeros = do
  let PartialZeros{crossingSegments} = partialZeros
  let updatedCrossingSegments = insertCrossingSegment newCrossingSegment crossingSegments
  partialZeros{crossingSegments = updatedCrossingSegments}

insertCrossingSegment ::
  CrossingSegment ->
  List CrossingSegment ->
  List CrossingSegment
insertCrossingSegment newCrossingSegment crossingSegments =
  case crossingSegments of
    [] -> [newCrossingSegment]
    firstCrossingSegment : remainingCrossingSegments ->
      case joinCrossingSegments newCrossingSegment firstCrossingSegment of
        Just joinedCrossingSegment ->
          insertCrossingSegment joinedCrossingSegment remainingCrossingSegments
        Nothing ->
          firstCrossingSegment : insertCrossingSegment newCrossingSegment remainingCrossingSegments

joinCrossingSegments :: CrossingSegment -> CrossingSegment -> Maybe CrossingSegment
joinCrossingSegments segment1 segment2 = do
  let CrossingSegment parameterization1 start1 end1 boxes1 = segment1
  let (_, startBoundary1) = start1
  let (_, endBoundary1) = end1
  let CrossingSegment parameterization2 start2 end2 boxes2 = segment2
  let (_, startBoundary2) = start2
  let (_, endBoundary2) = end2
  parameterization <- jointParameterization parameterization1 parameterization2
  if
    | Domain2D.adjacent endBoundary1 startBoundary2 ->
        Just (CrossingSegment parameterization start1 end2 (boxes1 <> boxes2))
    | Domain2D.adjacent endBoundary2 startBoundary1 ->
        Just (CrossingSegment parameterization start2 end1 (boxes2 <> boxes1))
    | otherwise -> Nothing

jointParameterization :: Parameterization -> Parameterization -> Maybe Parameterization
jointParameterization Horizontal Horizontal = Just Horizontal
jointParameterization Vertical Vertical = Just Vertical
jointParameterization Diagonal Diagonal = Just Diagonal
jointParameterization Horizontal Diagonal = Just Horizontal
jointParameterization Diagonal Horizontal = Just Horizontal
jointParameterization Vertical Diagonal = Just Vertical
jointParameterization Diagonal Vertical = Just Vertical
jointParameterization Horizontal Vertical = Nothing
jointParameterization Vertical Horizontal = Nothing

addTangentPoint :: (UvPoint, Sign) -> PartialZeros units -> PartialZeros units
addTangentPoint tangentPoint partialZeros = do
  let PartialZeros{tangentPoints} = partialZeros
  partialZeros{tangentPoints = tangentPoint : tangentPoints}

addSaddleRegion :: SaddleRegion units -> PartialZeros units -> PartialZeros units
addSaddleRegion saddleRegion partialZeros = do
  let PartialZeros{saddleRegions} = partialZeros
  partialZeros{saddleRegions = saddleRegion : saddleRegions}

data PiecewiseCurve
  = PiecewiseCurve Domain2D.Boundary Domain2D.Boundary (NonEmpty (Curve2D Unitless UvSpace))

piecewiseCurve ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  SurfaceFunction1D Unitless ->
  CrossingSegment ->
  PiecewiseCurve
piecewiseCurve function dvdu dudv (CrossingSegment parameterization start end boxes) = do
  let (UvPoint uStart vStart, startBoundary) = start
  let (UvPoint uEnd vEnd, endBoundary) = end
  let curve = case parameterization of
        Horizontal -> HorizontalCurve.new function dvdu uStart uEnd boxes
        Vertical -> VerticalCurve.new function dudv vStart vEnd boxes
        Diagonal -> HorizontalCurve.new function dvdu uStart uEnd boxes
  PiecewiseCurve startBoundary endBoundary (NonEmpty.one curve)

type PartialCurves = (List PiecewiseCurve, List (NonEmpty (Curve2D Unitless UvSpace)))

insertPiecewiseCurve :: PiecewiseCurve -> PartialCurves -> PartialCurves
insertPiecewiseCurve newPiecewiseCurve (piecewiseCurves, crossingLoops) =
  case piecewiseCurves of
    [] -> ([newPiecewiseCurve], crossingLoops)
    firstPiecewiseCurve : remainingPiecewiseCurves ->
      case joinPiecewiseCurves newPiecewiseCurve firstPiecewiseCurve of
        Just (JoinedPiecewiseCurve joinedPiecewiseCurve) ->
          insertPiecewiseCurve joinedPiecewiseCurve (remainingPiecewiseCurves, crossingLoops)
        Just (NewCrossingLoop newCrossingLoop) ->
          (remainingPiecewiseCurves, newCrossingLoop : crossingLoops)
        Nothing ->
          Pair.mapFirst (firstPiecewiseCurve :) $
            insertPiecewiseCurve newPiecewiseCurve (remainingPiecewiseCurves, crossingLoops)

data JoinPiecewiseCurveResult
  = JoinedPiecewiseCurve PiecewiseCurve
  | NewCrossingLoop (NonEmpty (Curve2D Unitless UvSpace))

joinPiecewiseCurves :: PiecewiseCurve -> PiecewiseCurve -> Maybe JoinPiecewiseCurveResult
joinPiecewiseCurves (PiecewiseCurve start1 end1 segments1) (PiecewiseCurve start2 end2 segments2)
  | Domain2D.adjacent end1 start2 && Domain2D.adjacent end2 start1 =
      Just (NewCrossingLoop (segments1 <> segments2))
  | Domain2D.adjacent end1 start2 =
      Just (JoinedPiecewiseCurve (PiecewiseCurve start1 end2 (segments1 <> segments2)))
  | Domain2D.adjacent end2 start1 =
      Just (JoinedPiecewiseCurve (PiecewiseCurve start2 end1 (segments2 <> segments1)))
  | otherwise = Nothing

finalize ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  SurfaceFunction1D Unitless ->
  PartialZeros units ->
  Zeros
finalize function dvdu dudv partialZeros = do
  let PartialZeros{crossingSegments, tangentPoints, saddleRegions} = partialZeros
  let piecewiseCurveSegments =
        List.map (piecewiseCurve function dvdu dudv) crossingSegments
  let (piecewiseCurves, crossingLoopSegments) =
        List.foldr insertPiecewiseCurve ([], []) piecewiseCurveSegments
  let extendedPiecewiseCurves =
        List.map (\initialCurve -> List.foldl extend initialCurve saddleRegions) piecewiseCurves
  let crossingCurveSegments =
        List.map (\(PiecewiseCurve _ _ segments) -> segments) extendedPiecewiseCurves
  Zeros
    { tangentPoints
    , saddlePoints =
        List.map SaddleRegion.point saddleRegions
    , crossingCurves =
        List.map (Tolerance.using Quantity.zero Curve2D.piecewise) crossingCurveSegments
    , crossingLoops =
        List.map (Tolerance.using Quantity.zero Curve2D.piecewise) crossingLoopSegments
    }

extend :: Tolerance units => PiecewiseCurve -> SaddleRegion units -> PiecewiseCurve
extend curve saddleRegion = do
  let (PiecewiseCurve start end segments) = curve
  let extendStart = Domain2D.contacts saddleRegion.subdomain start
  let extendEnd = Domain2D.contacts saddleRegion.subdomain end
  let outgoingSegment = SaddleRegion.Outgoing (NonEmpty.first segments)
  let incomingSegment = SaddleRegion.Incoming (NonEmpty.last segments)
  let startExtension = NonEmpty.one (SaddleRegion.connectingCurve outgoingSegment saddleRegion)
  let endExtension = NonEmpty.one (SaddleRegion.connectingCurve incomingSegment saddleRegion)
  case (extendStart, extendEnd) of
    (False, False) -> curve
    (True, False) -> PiecewiseCurve start end (startExtension <> segments)
    (False, True) -> PiecewiseCurve start end (segments <> endExtension)
    (True, True) -> PiecewiseCurve start end (startExtension <> segments <> endExtension)
