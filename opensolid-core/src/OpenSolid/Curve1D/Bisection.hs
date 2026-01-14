module OpenSolid.Curve1D.Bisection (deduplicate) where

import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.List qualified as List
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude

deduplicate :: List (Interval Unitless, Number) -> List Number
deduplicate solutions =
  deduplicateImpl solutions [] & List.map Pair.second & List.sort

deduplicateImpl ::
  List (Interval Unitless, Number) ->
  List (Interval Unitless, Number) ->
  List (Interval Unitless, Number)
deduplicateImpl [] accumulated = accumulated
deduplicateImpl (first : remaining) accumulated =
  if List.anySatisfy (isDuplicate first) accumulated
    then deduplicateImpl remaining accumulated
    else deduplicateImpl remaining (first : accumulated)

isDuplicate :: (Interval Unitless, Number) -> (Interval Unitless, Number) -> Bool
isDuplicate (subdomain1, _) (subdomain2, _) =
  Interval.overlap subdomain1 subdomain2 >= 0
