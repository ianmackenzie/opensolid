module OpenSolid.NewtonRaphson1D (curve) where

import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

curve :: (Number -> Quantity units) -> (Number -> Quantity units) -> Number -> Number
curve function derivative x1 = curveImpl function derivative x1 (function x1)

curveImpl ::
  (Number -> Quantity units) ->
  (Number -> Quantity units) ->
  Number ->
  Quantity units ->
  Number
curveImpl function derivative x1 y1 = do
  let dy1 = derivative x1
  let x2 = x1 - y1 / dy1
  let y2 = function x2
  if Quantity.abs y2 < 0.5 * Quantity.abs y1
    then curveImpl function derivative x2 y2
    else x1
