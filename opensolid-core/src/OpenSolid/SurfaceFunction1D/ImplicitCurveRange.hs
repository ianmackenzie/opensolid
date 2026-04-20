module OpenSolid.SurfaceFunction1D.ImplicitCurveRange
  ( ImplicitCurveRange
  , build
  , at
  , over
  )
where

import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude

data ImplicitCurveRange
  = Node ImplicitCurveRange Number ImplicitCurveRange
  | Leaf (Interval Unitless)

build :: NonEmpty (Interval Unitless, Interval Unitless) -> ImplicitCurveRange
build boxes = do
  let array = Array.fromNonEmpty (NonEmpty.sortBy (Interval.lower . Pair.first) boxes)
  subtree array 0 (Array.length array)

subtree :: Array (Interval Unitless, Interval Unitless) -> Int -> Int -> ImplicitCurveRange
subtree boxes begin end = case end - begin of
  1 -> Leaf (Pair.second (boxes !! begin))
  n -> assert (n >= 2) do
    let mid = begin + n // 2
    let left = subtree boxes begin mid
    let right = subtree boxes mid end
    let split = Interval.lower (Pair.first (boxes !! mid))
    Node left split right

at :: Number -> ImplicitCurveRange -> Interval Unitless
at x implicitCurveRange = case implicitCurveRange of
  Node left split right -> if x < split then at x left else at x right
  Leaf leafRange -> leafRange

over :: Interval Unitless -> ImplicitCurveRange -> Interval Unitless
over x implicitCurveRange = case implicitCurveRange of
  Node left split right
    | Interval.upper x <= split -> over x left
    | Interval.lower x >= split -> over x right
    | otherwise -> Interval.aggregate2 (over x left) (over x right)
  Leaf leafRange -> leafRange
