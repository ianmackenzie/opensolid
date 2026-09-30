module OpenSolid.SurfaceFunction1D.ImplicitCurveRange
  ( ImplicitCurveRange
  , build
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
  1 -> Leaf (Pair.second (boxes @ begin))
  n -> assert (n >= 2) do
    let mid = begin + n // 2
    let left = subtree boxes begin mid
    let right = subtree boxes mid end
    let split = Interval.lower (Pair.first (boxes @ mid))
    Node left split right

instance At ImplicitCurveRange Number (Interval Unitless) where
  implicitCurveRange @ xValue = case implicitCurveRange of
    Node left split right -> if xValue < split then left @ xValue else right @ xValue
    Leaf leafRange -> leafRange

instance At ImplicitCurveRange (Interval Unitless) (Interval Unitless) where
  implicitCurveRange @ xRange = case implicitCurveRange of
    Node left split right
      | Interval.upper xRange <= split -> left @ xRange
      | Interval.lower xRange >= split -> right @ xRange
      | otherwise -> Interval.aggregate2 (left @ xRange) (right @ xRange)
    Leaf leafRange -> leafRange
