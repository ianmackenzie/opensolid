{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Surface1d.Function.PartialZeros
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

import Curve2d (Curve2d)
import Curve2d qualified
import Domain2d (Domain2d)
import Domain2d qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Pair qualified
import Surface1d.Function.SaddleRegion (SaddleRegion (..))
import Surface1d.Function.SaddleRegion qualified as SaddleRegion
import Surface1d.Function.Zeros (Zeros (..))
import Surface1d.Function.Zeros qualified as Zeros
import Uv qualified

data PartialZeros = PartialZeros
  { crossingCurves :: List CrossingCurve
  , crossingLoops :: List CrossingLoop
  , tangentPoints :: List (Uv.Point, Sign)
  , saddleRegions :: List (Domain2d, SaddleRegion)
  }

data CrossingCurve
  = CrossingCurve Domain2d.Boundary Domain2d.Boundary (NonEmpty (Curve2d Uv.Coordinates))
  deriving (Show)

crossingCurve :: Domain2d.Boundary -> Domain2d.Boundary -> Curve2d Uv.Coordinates -> CrossingCurve
crossingCurve startBoundary endBoundary curve =
  CrossingCurve startBoundary endBoundary (NonEmpty.singleton curve)

type CrossingLoop = NonEmpty (Curve2d Uv.Coordinates)

empty :: PartialZeros
empty =
  PartialZeros
    { crossingCurves = []
    , crossingLoops = []
    , tangentPoints = []
    , saddleRegions = []
    }

addCrossingCurve :: CrossingCurve -> PartialZeros -> PartialZeros
addCrossingCurve newCrossingCurve partialZeros = do
  let PartialZeros{crossingCurves, crossingLoops} = partialZeros
  let (updatedCrossingCurves, updatedCrossingLoops) =
        insertCrossingCurve newCrossingCurve crossingCurves crossingLoops
  partialZeros
    { Surface1d.Function.PartialZeros.crossingCurves = updatedCrossingCurves
    , Surface1d.Function.PartialZeros.crossingLoops = updatedCrossingLoops
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

addTangentPoint :: (Uv.Point, Sign) -> PartialZeros -> PartialZeros
addTangentPoint tangentPoint partialZeros = do
  let PartialZeros{tangentPoints} = partialZeros
  partialZeros
    { Surface1d.Function.PartialZeros.tangentPoints = tangentPoint : tangentPoints
    }

addSaddleRegion :: Domain2d -> SaddleRegion -> PartialZeros -> PartialZeros
addSaddleRegion subdomain saddleRegion partialZeros = do
  let PartialZeros{saddleRegions} = partialZeros
  partialZeros{saddleRegions = (subdomain, saddleRegion) : saddleRegions}

finalize :: PartialZeros -> Zeros
finalize partialZeros = do
  let PartialZeros{crossingCurves, crossingLoops, tangentPoints, saddleRegions} = partialZeros
  let extendedCrossingCurves =
        List.map (\crossingCurve -> List.foldl extend crossingCurve saddleRegions) crossingCurves
  -- TODO add logic to check for saddle regions that touch a UV domain boundary,
  -- and then add connecting curves to zeros found on that boundary
  -- (because there will be no crossing curve to connect to in that case)
  Zeros
    { Zeros.crossingCurves =
        List.map (\(CrossingCurve _ _ segments) -> segments) extendedCrossingCurves
    , Zeros.crossingLoops = crossingLoops
    , Zeros.tangentPoints = tangentPoints
    , Zeros.saddlePoints =
        List.map (\(subdomain, saddleRegion) -> (SaddleRegion.point saddleRegion, Domain2d.bounds subdomain)) saddleRegions
    }

extend :: CrossingCurve -> (Domain2d, SaddleRegion) -> CrossingCurve
extend curve (subdomain, saddleRegion) = do
  let (CrossingCurve start end segments) = curve
  let SaddleRegion{connectingCurves} = saddleRegion
  let extendStart = Domain2d.contacts subdomain start
  let extendEnd = Domain2d.contacts subdomain end
  let startExtension = connectingCurves (Curve2d.startPoint (NonEmpty.first segments))
  let endExtension =
        connectingCurves (Curve2d.endPoint (NonEmpty.last segments))
          |> List.reverseMap Curve2d.reverse
  case (extendStart, extendEnd) of
    (False, False) -> curve
    (True, False) -> CrossingCurve start end (startExtension + segments)
    (False, True) -> CrossingCurve start end (segments + endExtension)
    (True, True) -> CrossingCurve start end (startExtension + segments + endExtension)
