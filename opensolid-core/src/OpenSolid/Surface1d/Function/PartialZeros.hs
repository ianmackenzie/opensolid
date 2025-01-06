{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module OpenSolid.Surface1d.Function.PartialZeros
  ( PartialZeros (..)
  , CrossingCurve (..)
  , empty
  , crossingCurve
  , addCrossingCurve
  , addTangentPoint
  , addSaddleRegion
  , finalize
  )
where

import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import {-# SOURCE #-} OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Domain2d qualified as Domain2d
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Surface1d.Function.SaddleRegion (SaddleRegion)
import OpenSolid.Surface1d.Function.SaddleRegion qualified as SaddleRegion
import OpenSolid.Surface1d.Function.Zeros (Zeros (..))
import OpenSolid.Surface1d.Function.Zeros qualified as Zeros
import OpenSolid.SurfaceParameter (UvBounds, UvCoordinates, UvPoint)

data PartialZeros units = PartialZeros
  { crossingCurves :: List CrossingCurve
  , crossingLoops :: List CrossingLoop
  , tangentPoints :: List (UvPoint, Sign, UvBounds)
  , saddleRegions :: List (SaddleRegion units)
  }

data CrossingCurve
  = CrossingCurve Domain2d.Boundary Domain2d.Boundary (NonEmpty (Curve2d UvCoordinates, UvBounds))

crossingCurve ::
  Domain2d.Boundary ->
  Domain2d.Boundary ->
  UvBounds ->
  Curve2d UvCoordinates ->
  CrossingCurve
crossingCurve startBoundary endBoundary uvBounds curve =
  CrossingCurve startBoundary endBoundary (NonEmpty.one (curve, uvBounds))

type CrossingLoop = NonEmpty (Curve2d UvCoordinates, UvBounds)

empty :: PartialZeros units
empty =
  PartialZeros
    { crossingCurves = []
    , crossingLoops = []
    , tangentPoints = []
    , saddleRegions = []
    }

addCrossingCurve :: CrossingCurve -> PartialZeros units -> PartialZeros units
addCrossingCurve newCrossingCurve partialZeros = do
  let PartialZeros{crossingCurves, crossingLoops} = partialZeros
  let (updatedCrossingCurves, updatedCrossingLoops) =
        insertCrossingCurve newCrossingCurve crossingCurves crossingLoops
  partialZeros
    { OpenSolid.Surface1d.Function.PartialZeros.crossingCurves = updatedCrossingCurves
    , OpenSolid.Surface1d.Function.PartialZeros.crossingLoops = updatedCrossingLoops
    }

insertCrossingCurve ::
  CrossingCurve ->
  List CrossingCurve ->
  List CrossingLoop ->
  (List CrossingCurve, List CrossingLoop)
insertCrossingCurve newCrossingCurve crossingCurves crossingLoops =
  case crossingCurves of
    [] -> ([newCrossingCurve], crossingLoops)
    firstCrossingCurve : remainingCrossingCurves ->
      case joinCrossingCurves newCrossingCurve firstCrossingCurve of
        Just (JoinedCrossingCurve joinedCrossingCurve) ->
          insertCrossingCurve joinedCrossingCurve remainingCrossingCurves crossingLoops
        Just (NewCrossingLoop newCrossingLoop) ->
          (remainingCrossingCurves, newCrossingLoop : crossingLoops)
        Nothing ->
          Pair.mapFirst (firstCrossingCurve :) $
            insertCrossingCurve newCrossingCurve remainingCrossingCurves crossingLoops

data JoinCrossingCurveResult
  = JoinedCrossingCurve CrossingCurve
  | NewCrossingLoop CrossingLoop

joinCrossingCurves :: CrossingCurve -> CrossingCurve -> Maybe JoinCrossingCurveResult
joinCrossingCurves (CrossingCurve start1 end1 segments1) (CrossingCurve start2 end2 segments2)
  | Domain2d.adjacent end1 start2 && Domain2d.adjacent end2 start1 =
      Just (NewCrossingLoop (segments1 + segments2))
  | Domain2d.adjacent end1 start2 =
      Just (JoinedCrossingCurve (CrossingCurve start1 end2 (segments1 + segments2)))
  | Domain2d.adjacent end2 start1 =
      Just (JoinedCrossingCurve (CrossingCurve start2 end1 (segments2 + segments1)))
  | otherwise = Nothing

addTangentPoint :: (UvPoint, Sign, UvBounds) -> PartialZeros units -> PartialZeros units
addTangentPoint tangentPoint partialZeros = do
  let PartialZeros{tangentPoints} = partialZeros
  partialZeros
    { OpenSolid.Surface1d.Function.PartialZeros.tangentPoints = tangentPoint : tangentPoints
    }

addSaddleRegion :: SaddleRegion units -> PartialZeros units -> PartialZeros units
addSaddleRegion saddleRegion partialZeros = do
  let PartialZeros{saddleRegions} = partialZeros
  partialZeros{saddleRegions = saddleRegion : saddleRegions}

finalize :: Tolerance units => PartialZeros units -> Zeros
finalize partialZeros = do
  let PartialZeros{crossingCurves, crossingLoops, tangentPoints, saddleRegions} = partialZeros
  let extendedCrossingCurves =
        List.map (\initialCurve -> List.foldl extend initialCurve saddleRegions) crossingCurves
  -- TODO add logic to check for saddle regions that touch a UV domain boundary,
  -- and then add connecting curves to zeros found on that boundary
  -- (because there will be no crossing curve to connect to in that case)
  Zeros
    { Zeros.crossingCurves = Maybe.collect filterDegenerateCurves extendedCrossingCurves
    , Zeros.crossingLoops = crossingLoops
    , Zeros.tangentPoints = tangentPoints
    , Zeros.saddlePoints =
        saddleRegions
          |> List.map
            ( \saddleRegion ->
                (SaddleRegion.point saddleRegion, SaddleRegion.bounds saddleRegion)
            )
    }

filterDegenerateCurves :: CrossingCurve -> Maybe (NonEmpty (Curve2d UvCoordinates, UvBounds))
filterDegenerateCurves (CrossingCurve _ _ segments) = do
  let isNondegenerate (curve, _) = Curve2d.startPoint curve /= Curve2d.endPoint curve
  case NonEmpty.filter isNondegenerate segments of
    NonEmpty filtered -> Just filtered
    [] -> Nothing

extend :: Tolerance units => CrossingCurve -> SaddleRegion units -> CrossingCurve
extend curve saddleRegion = do
  let (CrossingCurve start end segments) = curve
  let subdomain = SaddleRegion.subdomain saddleRegion
  let uvBounds = Domain2d.bounds subdomain
  let extendStart = Domain2d.contacts subdomain start
  let extendEnd = Domain2d.contacts subdomain end
  let startExtension =
        SaddleRegion.connectingCurves (Curve2d.startPoint (Pair.first (NonEmpty.first segments))) saddleRegion
          |> NonEmpty.map (,uvBounds)
  let endExtension =
        SaddleRegion.connectingCurves (Curve2d.endPoint (Pair.first (NonEmpty.last segments))) saddleRegion
          |> NonEmpty.reverseMap (Curve2d.reverse >> (,uvBounds))
  case (extendStart, extendEnd) of
    (False, False) -> curve
    (True, False) -> CrossingCurve start end (startExtension + segments)
    (False, True) -> CrossingCurve start end (segments + endExtension)
    (True, True) -> CrossingCurve start end (startExtension + segments + endExtension)
