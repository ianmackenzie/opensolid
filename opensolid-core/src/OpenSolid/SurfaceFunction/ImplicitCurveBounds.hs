module OpenSolid.SurfaceFunction.ImplicitCurveBounds
  ( ImplicitCurveBounds
  , build
  , evaluate
  , evaluateBounds
  )
where

import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude

data ImplicitCurveBounds
  = Node ImplicitCurveBounds Number ImplicitCurveBounds
  | Leaf (Interval Unitless)

build :: NonEmpty (Interval Unitless, Interval Unitless) -> ImplicitCurveBounds
build boxes = do
  let array = Array.fromNonEmpty (NonEmpty.sortBy (Interval.lower . Pair.first) boxes)
  subtree array 0 (Array.length array)

subtree :: Array (Interval Unitless, Interval Unitless) -> Int -> Int -> ImplicitCurveBounds
subtree boxes begin end = case end - begin of
  1 -> Leaf (Pair.second (Array.get begin boxes))
  n -> assert (n >= 2) do
    let mid = begin + n `div` 2
    let left = subtree boxes begin mid
    let right = subtree boxes mid end
    let split = Interval.lower (Pair.first (Array.get mid boxes))
    Node left split right

evaluate :: ImplicitCurveBounds -> Number -> Interval Unitless
evaluate implicitCurveBounds x = case implicitCurveBounds of
  Node left split right -> if x < split then evaluate left x else evaluate right x
  Leaf bounds -> bounds

evaluateBounds :: ImplicitCurveBounds -> Interval Unitless -> Interval Unitless
evaluateBounds implicitCurveBounds x = case implicitCurveBounds of
  Node left split right
    | Interval.upper x <= split -> evaluateBounds left x
    | Interval.lower x >= split -> evaluateBounds right x
    | otherwise -> Interval.aggregate2 (evaluateBounds left x) (evaluateBounds right x)
  Leaf bounds -> bounds
