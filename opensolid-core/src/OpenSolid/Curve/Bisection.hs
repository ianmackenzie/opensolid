module OpenSolid.Curve.Bisection (deduplicate) where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude

deduplicate :: List (Bounds Unitless, Float) -> List Float
deduplicate solutions = List.map Pair.second (deduplicateImpl solutions [])

deduplicateImpl ::
  List (Bounds Unitless, Float) ->
  List (Bounds Unitless, Float) ->
  List (Bounds Unitless, Float)
deduplicateImpl [] accumulated = accumulated
deduplicateImpl (first : remaining) accumulated =
  if List.anySatisfy (isDuplicate first) accumulated
    then deduplicateImpl remaining accumulated
    else deduplicateImpl remaining (first : accumulated)

isDuplicate :: (Bounds Unitless, Float) -> (Bounds Unitless, Float) -> Bool
isDuplicate (subdomain1, _) (subdomain2, _) =
  Bounds.overlap subdomain1 subdomain2 >= 0.0
