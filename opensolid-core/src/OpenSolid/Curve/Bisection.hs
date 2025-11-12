module OpenSolid.Curve.Bisection (deduplicate) where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude

deduplicate :: List (Bounds Unitless, Number) -> List Number
deduplicate solutions =
  deduplicateImpl solutions [] & List.map Pair.second & List.sort

deduplicateImpl ::
  List (Bounds Unitless, Number) ->
  List (Bounds Unitless, Number) ->
  List (Bounds Unitless, Number)
deduplicateImpl [] accumulated = accumulated
deduplicateImpl (first : remaining) accumulated =
  if List.anySatisfy (isDuplicate first) accumulated
    then deduplicateImpl remaining accumulated
    else deduplicateImpl remaining (first : accumulated)

isDuplicate :: (Bounds Unitless, Number) -> (Bounds Unitless, Number) -> Bool
isDuplicate (subdomain1, _) (subdomain2, _) =
  Bounds.overlap subdomain1 subdomain2 >= 0
