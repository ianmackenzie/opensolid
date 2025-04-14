module OpenSolid.SurfaceFunction.ImplicitCurveBounds
  ( ImplicitCurveBounds
  , build
  , evaluate
  , evaluateBounds
  )
where

import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Debug qualified as Debug
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range

data ImplicitCurveBounds
  = Node ImplicitCurveBounds Float ImplicitCurveBounds
  | Leaf (Range Unitless)

build :: NonEmpty (Range Unitless, Range Unitless) -> ImplicitCurveBounds
build boxes = do
  let array = Array.fromNonEmpty (NonEmpty.sortBy (Range.lowerBound . Pair.first) boxes)
  subtree array 0 (Array.length array)

subtree :: Array (Range Unitless, Range Unitless) -> Int -> Int -> ImplicitCurveBounds
subtree boxes begin end = case end - begin of
  1 -> Leaf (Pair.second (Array.get begin boxes))
  n -> do
    Debug.assert (n >= 2)
    let mid = begin + n // 2
    let left = subtree boxes begin mid
    let right = subtree boxes mid end
    let split = Range.lowerBound (Pair.first (Array.get mid boxes))
    Node left split right

evaluate :: ImplicitCurveBounds -> Float -> Range Unitless
evaluate bounds x = case bounds of
  Node left split right -> if x < split then evaluate left x else evaluate right x
  Leaf range -> range

evaluateBounds :: ImplicitCurveBounds -> Range Unitless -> Range Unitless
evaluateBounds bounds x = case bounds of
  Node left split right
    | Range.upperBound x <= split -> evaluateBounds left x
    | Range.lowerBound x >= split -> evaluateBounds right x
    | otherwise -> Range.aggregate2 (evaluateBounds left x) (evaluateBounds right x)
  Leaf range -> range
