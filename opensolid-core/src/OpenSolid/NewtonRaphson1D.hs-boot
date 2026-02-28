{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson1D (curve) where

import OpenSolid.Prelude

curve :: (Number -> (# Quantity units, Quantity units #)) -> Number -> Number
