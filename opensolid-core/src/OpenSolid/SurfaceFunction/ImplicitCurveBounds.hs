module OpenSolid.SurfaceFunction.ImplicitCurveBounds
  ( ImplicitCurveBounds
  , build
  , evaluate
  , evaluateBounds
  )
where

import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude

data ImplicitCurveBounds
  = Node ImplicitCurveBounds Float ImplicitCurveBounds
  | Leaf (Bounds Unitless)

build :: NonEmpty (Bounds Unitless, Bounds Unitless) -> ImplicitCurveBounds
build boxes = do
  let array = Array.fromNonEmpty (NonEmpty.sortBy (Bounds.lower . Pair.first) boxes)
  subtree array 0 array.length

subtree :: Array (Bounds Unitless, Bounds Unitless) -> Int -> Int -> ImplicitCurveBounds
subtree boxes begin end = case end - begin of
  1 -> Leaf (Pair.second (Array.get begin boxes))
  n -> assert (n >= 2) do
    let mid = begin + n // 2
    let left = subtree boxes begin mid
    let right = subtree boxes mid end
    let split = Bounds.lower (Pair.first (Array.get mid boxes))
    Node left split right

evaluate :: ImplicitCurveBounds -> Float -> Bounds Unitless
evaluate implicitCurveBounds x = case implicitCurveBounds of
  Node left split right -> if x < split then evaluate left x else evaluate right x
  Leaf bounds -> bounds

evaluateBounds :: ImplicitCurveBounds -> Bounds Unitless -> Bounds Unitless
evaluateBounds implicitCurveBounds x = case implicitCurveBounds of
  Node left split right
    | Bounds.upper x <= split -> evaluateBounds left x
    | Bounds.lower x >= split -> evaluateBounds right x
    | otherwise -> Bounds.aggregate2 (evaluateBounds left x) (evaluateBounds right x)
  Leaf bounds -> bounds
