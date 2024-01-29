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
import Surface1d.Solution.Boundary (Boundary)
import Surface1d.Solution.Boundary qualified as Boundary
import Uv qualified

data Solution
  = BoundaryEdge (Curve2d Uv.Coordinates)
  | BoundaryPoint Uv.Point
  | CrossingCurve {start :: Boundary, end :: Boundary, segments :: NonEmpty (Curve2d Uv.Coordinates)}
  | DegenerateCrossingCurve {start :: Boundary, end :: Boundary}
  | CrossingLoop {segments :: NonEmpty (Curve2d Uv.Coordinates)}
  | TangentCurve {start :: Boundary, end :: Boundary, segments :: NonEmpty (Curve2d Uv.Coordinates), sign :: Sign}
  | TangentLoop {segments :: NonEmpty (Curve2d Uv.Coordinates), sign :: Sign}
  | TangentPoint {point :: Uv.Point, sign :: Sign}
  | SaddlePoint {point :: Uv.Point, region :: Uv.Bounds}
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
join (CrossingCurve start1 end1 segments) (DegenerateCrossingCurve start2 end2)
  | Boundary.adjacent end1 start2 && Boundary.adjacent end2 start1 =
      Just (CrossingLoop segments)
  | Boundary.adjacent end1 start2 =
      Just (CrossingCurve start1 end2 segments)
  | Boundary.adjacent end2 start1 =
      Just (CrossingCurve start2 end1 segments)
  | otherwise = Nothing
join (DegenerateCrossingCurve start1 end1) (CrossingCurve start2 end2 segments)
  | Boundary.adjacent end1 start2 && Boundary.adjacent end2 start1 =
      Just (CrossingLoop segments)
  | Boundary.adjacent end1 start2 =
      Just (CrossingCurve start1 end2 segments)
  | Boundary.adjacent end2 start1 =
      Just (CrossingCurve start2 end1 segments)
  | otherwise = Nothing
join (TangentCurve start1 end1 segments1 sign1) (TangentCurve start2 end2 segments2 sign2)
  | Boundary.adjacent end1 start2 && Boundary.adjacent end2 start1 && sign1 == sign2 =
      Just (TangentLoop (segments1 ++ segments2) sign1)
  | Boundary.adjacent end1 start2 && sign1 == sign2 =
      Just (TangentCurve start1 end2 (segments1 ++ segments2) sign1)
  | Boundary.adjacent end2 start1 && sign1 == sign2 =
      Just (TangentCurve start2 end1 (segments2 ++ segments1) sign1)
  | otherwise = Nothing
-- TODO join tangent curves to degenerate tangent curves
join _ _ = Nothing
