{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Surface1d.Solution
  ( Solution (..)
  , merge
  )
where

import Curve2d (Curve2d)
import List qualified
import OpenSolid
import Point2d (Point2d)
import Surface1d.Solution.Boundary (Boundary)
import Surface1d.Solution.Boundary qualified as Boundary
import Uv qualified

data Solution
  = BoundaryEdge (Curve2d Uv.Coordinates)
  | BoundaryPoint Uv.Point
  | CrossingCurve {start :: Boundary, end :: Boundary, segments :: NonEmpty (Curve2d Uv.Coordinates)}
  | CrossingLoop {segments :: NonEmpty (Curve2d Uv.Coordinates)}
  | TangentCurve {start :: Boundary, end :: Boundary, segments :: NonEmpty (Curve2d Uv.Coordinates), sign :: Sign}
  | TangentLoop {segments :: NonEmpty (Curve2d Uv.Coordinates), sign :: Sign}
  | TangentPoint {point :: Point2d Uv.Coordinates, sign :: Sign}
  deriving (Show)

merge :: List Solution -> List Solution -> List Solution
merge left right = List.foldRight add right left

add :: Solution -> List Solution -> List Solution
add solution [] = [solution]
add solution (first : rest) =
  case join solution first of
    Just joined -> add joined rest
    Nothing -> first : add solution rest

join :: Solution -> Solution -> Maybe Solution
join (CrossingCurve start1 end1 segments1) (CrossingCurve start2 end2 segments2)
  | Boundary.adjacent end1 start2 && Boundary.adjacent end2 start1 =
      Just (CrossingLoop (segments1 ++ segments2))
  | Boundary.adjacent end1 start2 =
      Just (CrossingCurve start1 end2 (segments1 ++ segments2))
  | Boundary.adjacent end2 start1 =
      Just (CrossingCurve start2 end1 (segments2 ++ segments1))
  | otherwise = Nothing
join (TangentCurve start1 end1 segments1 sign1) (TangentCurve start2 end2 segments2 sign2)
  | Boundary.adjacent end1 start2 && Boundary.adjacent end2 start1 && sign1 == sign2 =
      Just (TangentLoop (segments1 ++ segments2) sign1)
  | Boundary.adjacent end1 start2 && sign1 == sign2 =
      Just (TangentCurve start1 end2 (segments1 ++ segments2) sign1)
  | Boundary.adjacent end2 start1 && sign1 == sign2 =
      Just (TangentCurve start2 end1 (segments2 ++ segments1) sign1)
  | otherwise = Nothing
join _ _ = Nothing
