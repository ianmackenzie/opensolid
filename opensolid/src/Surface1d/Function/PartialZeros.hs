{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Surface1d.Function.PartialZeros
  ( PartialZeros (..)
  , CrossingCurve (..)
  , TangentCurve (..)
  , TangentLoop (..)
  , TangentPoint (..)
  , empty
  , crossingCurve
  , degenerateCrossingCurve
  , tangentPoint
  , saddleRegion
  , merge
  )
where

import Curve2d (Curve2d)
import List qualified
import NonEmpty qualified
import OpenSolid
import Pair qualified
import Surface1d.Function.Boundary (Boundary)
import Surface1d.Function.Boundary qualified as Boundary
import Surface1d.Function.SaddleRegion (SaddleRegion)
import Uv qualified

data PartialZeros = PartialZeros
  { crossingCurves :: List CrossingCurve
  , crossingLoops :: List CrossingLoop
  , tangentCurves :: List TangentCurve
  , tangentLoops :: List TangentLoop
  , tangentPoints :: List TangentPoint
  , saddleRegions :: List SaddleRegion
  }

data CrossingCurve
  = CrossingCurve {start :: Boundary, end :: Boundary, segments :: NonEmpty (Curve2d Uv.Coordinates)}
  | DegenerateCrossingCurve {start :: Boundary, end :: Boundary}

type CrossingLoop =
  NonEmpty (Curve2d Uv.Coordinates)

data TangentCurve
  = TangentCurve {start :: Boundary, end :: Boundary, segments :: NonEmpty (Curve2d Uv.Coordinates), sign :: Sign}
  | DegenerateTangentCurve {start :: Boundary, end :: Boundary, sign :: Sign}

data TangentLoop = TangentLoop
  { segments :: NonEmpty (Curve2d Uv.Coordinates)
  , sign :: Sign
  }

data TangentPoint = TangentPoint
  { point :: Uv.Point
  , sign :: Sign
  }

empty :: PartialZeros
empty =
  PartialZeros
    { crossingCurves = []
    , crossingLoops = []
    , tangentCurves = []
    , tangentLoops = []
    , tangentPoints = []
    , saddleRegions = []
    }

crossingCurve :: Boundary -> Boundary -> Curve2d Uv.Coordinates -> PartialZeros
crossingCurve start end curve =
  empty{crossingCurves = [CrossingCurve{start, end, segments = NonEmpty.singleton curve}]}

degenerateCrossingCurve :: Boundary -> Boundary -> PartialZeros
degenerateCrossingCurve start end =
  empty{crossingCurves = [DegenerateCrossingCurve{start, end}]}

tangentPoint :: Uv.Point -> Sign -> PartialZeros
tangentPoint point sign =
  empty{tangentPoints = [TangentPoint{point, sign}]}

saddleRegion :: SaddleRegion -> PartialZeros
saddleRegion region =
  empty{saddleRegions = [region]}

merge :: PartialZeros -> PartialZeros -> PartialZeros
merge left right =
  let (mergedCrossingCurves, newCrossingLoops) = mergeCrossingCurves leftCrossingCurves rightCrossingCurves
      (mergedTangentCurves, newTangentLoops) = mergeTangentCurves leftTangentCurves rightTangentCurves
   in PartialZeros
        { crossingCurves = mergedCrossingCurves
        , crossingLoops = List.concat [newCrossingLoops, leftCrossingLoops, rightCrossingLoops]
        , tangentCurves = mergedTangentCurves
        , tangentLoops = List.concat [newTangentLoops, leftTangentLoops, rightTangentLoops]
        , tangentPoints = leftTangentPoints ++ rightTangentPoints
        , saddleRegions = leftSaddleRegions ++ rightSaddleRegions
        }
 where
  PartialZeros
    { crossingCurves = leftCrossingCurves
    , crossingLoops = leftCrossingLoops
    , tangentCurves = leftTangentCurves
    , tangentLoops = leftTangentLoops
    , tangentPoints = leftTangentPoints
    , saddleRegions = leftSaddleRegions
    } = left
  PartialZeros
    { crossingCurves = rightCrossingCurves
    , crossingLoops = rightCrossingLoops
    , tangentCurves = rightTangentCurves
    , tangentLoops = rightTangentLoops
    , tangentPoints = rightTangentPoints
    , saddleRegions = rightSaddleRegions
    } = right

mergeCrossingCurves :: List CrossingCurve -> List CrossingCurve -> (List CrossingCurve, List CrossingLoop)
mergeCrossingCurves left right = List.foldr addCrossingCurve (right, []) left

data JoinCrossingCurveResult
  = JoinedCrossingCurve CrossingCurve
  | NewCrossingLoop CrossingLoop

addCrossingCurve :: CrossingCurve -> (List CrossingCurve, List CrossingLoop) -> (List CrossingCurve, List CrossingLoop)
addCrossingCurve givenCrossingCurve ([], loops) = ([givenCrossingCurve], loops)
addCrossingCurve givenCrossingCurve (first : rest, loops) =
  case joinCrossingCurves givenCrossingCurve first of
    Just (JoinedCrossingCurve joinedCrossingCurve) -> addCrossingCurve joinedCrossingCurve (rest, loops)
    Just (NewCrossingLoop newCrossingLoop) -> (rest, newCrossingLoop : loops)
    Nothing -> Pair.mapFirst (first :) (addCrossingCurve givenCrossingCurve (rest, loops))

joinCrossingCurves :: CrossingCurve -> CrossingCurve -> Maybe JoinCrossingCurveResult
joinCrossingCurves (CrossingCurve start1 end1 segments1) (CrossingCurve start2 end2 segments2)
  | Boundary.adjacent end1 start2 && Boundary.adjacent end2 start1 =
      Just (NewCrossingLoop (segments1 ++ segments2))
  | Boundary.adjacent end1 start2 =
      Just (JoinedCrossingCurve (CrossingCurve start1 end2 (segments1 ++ segments2)))
  | Boundary.adjacent end2 start1 =
      Just (JoinedCrossingCurve (CrossingCurve start2 end1 (segments2 ++ segments1)))
  | otherwise = Nothing
joinCrossingCurves (CrossingCurve start1 end1 segments) (DegenerateCrossingCurve start2 end2)
  | Boundary.adjacent end1 start2 && Boundary.adjacent end2 start1 =
      Just (NewCrossingLoop segments)
  | Boundary.adjacent end1 start2 =
      Just (JoinedCrossingCurve (CrossingCurve start1 end2 segments))
  | Boundary.adjacent end2 start1 =
      Just (JoinedCrossingCurve (CrossingCurve start2 end1 segments))
  | otherwise = Nothing
joinCrossingCurves (DegenerateCrossingCurve start1 end1) (CrossingCurve start2 end2 segments)
  | Boundary.adjacent end1 start2 && Boundary.adjacent end2 start1 =
      Just (NewCrossingLoop segments)
  | Boundary.adjacent end1 start2 =
      Just (JoinedCrossingCurve (CrossingCurve start1 end2 segments))
  | Boundary.adjacent end2 start1 =
      Just (JoinedCrossingCurve (CrossingCurve start2 end1 segments))
  | otherwise = Nothing
joinCrossingCurves (DegenerateCrossingCurve{}) (DegenerateCrossingCurve{}) = Nothing

mergeTangentCurves :: List TangentCurve -> List TangentCurve -> (List TangentCurve, List TangentLoop)
mergeTangentCurves left right = List.foldr addTangentCurve (right, []) left

data JoinTangentCurveResult
  = JoinedTangentCurve TangentCurve
  | NewTangentLoop TangentLoop

addTangentCurve :: TangentCurve -> (List TangentCurve, List TangentLoop) -> (List TangentCurve, List TangentLoop)
addTangentCurve tangentCurve ([], loops) = ([tangentCurve], loops)
addTangentCurve tangentCurve (first : rest, loops) =
  case joinTangentCurves tangentCurve first of
    Just (JoinedTangentCurve joinedTangentCurve) -> addTangentCurve joinedTangentCurve (rest, loops)
    Just (NewTangentLoop newTangentLoop) -> (rest, newTangentLoop : loops)
    Nothing -> Pair.mapFirst (first :) (addTangentCurve tangentCurve (rest, loops))

joinTangentCurves :: TangentCurve -> TangentCurve -> Maybe JoinTangentCurveResult
joinTangentCurves (TangentCurve start1 end1 segments1 sign1) (TangentCurve start2 end2 segments2 sign2)
  | sign1 /= sign2 = Nothing
  | Boundary.adjacent end1 start2 && Boundary.adjacent end2 start1 =
      Just (NewTangentLoop (TangentLoop (segments1 ++ segments2) sign1))
  | Boundary.adjacent end1 start2 =
      Just (JoinedTangentCurve (TangentCurve start1 end2 (segments1 ++ segments2) sign1))
  | Boundary.adjacent end2 start1 =
      Just (JoinedTangentCurve (TangentCurve start2 end1 (segments2 ++ segments1) sign1))
  | otherwise = Nothing
joinTangentCurves (TangentCurve start1 end1 segments1 sign1) (DegenerateTangentCurve start2 end2 sign2)
  | sign1 /= sign2 = Nothing
  | Boundary.adjacent end1 start2 && Boundary.adjacent end2 start1 =
      Just (NewTangentLoop (TangentLoop segments1 sign1))
  | Boundary.adjacent end1 start2 =
      Just (JoinedTangentCurve (TangentCurve start1 end2 segments1 sign1))
  | Boundary.adjacent end2 start1 =
      Just (JoinedTangentCurve (TangentCurve start2 end1 segments1 sign1))
  | otherwise = Nothing
joinTangentCurves (DegenerateTangentCurve start1 end1 sign1) (TangentCurve start2 end2 segments2 sign2)
  | sign1 /= sign2 = Nothing
  | Boundary.adjacent end1 start2 && Boundary.adjacent end2 start1 =
      Just (NewTangentLoop (TangentLoop segments2 sign1))
  | Boundary.adjacent end1 start2 =
      Just (JoinedTangentCurve (TangentCurve start1 end2 segments2 sign1))
  | Boundary.adjacent end2 start1 =
      Just (JoinedTangentCurve (TangentCurve start2 end1 segments2 sign1))
  | otherwise = Nothing
joinTangentCurves (DegenerateTangentCurve{}) (DegenerateTangentCurve{}) = Nothing
