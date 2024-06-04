module Solve1d
  ( expand
  , overlaps
  , isResolved
  , resolvedSign
  )
where

import Float qualified
import OpenSolid
import Qty qualified
import Range (Range (Range))
import Range qualified

expand :: Range Unitless -> Range Unitless
expand domain = do
  let (Range low high) = domain
  let expansion = 0.5 * (high - low)
  Range.unsafe (Float.max 0.0 (low - expansion)) (Float.min 1.0 (high + expansion))

overlaps :: Range Unitless -> Range Unitless -> Bool
overlaps domain exclusion = Range.overlap domain exclusion > Qty.zero

isResolved :: Range units -> Bool
isResolved range = resolvedSign range /= Nothing

resolvedSign :: Range units -> Maybe Sign
resolvedSign range = do
  let resolution = Range.resolution range
  if Qty.abs resolution >= 0.5 then Just (Qty.sign resolution) else Nothing
