module OpenSolid.NewtonRaphson1D (curve) where

import OpenSolid.Prelude

curve :: (Number -> Quantity units) -> (Number -> Quantity units) -> Number -> Number
