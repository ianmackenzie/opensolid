{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson1D (curve) where

import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

curve :: (Number -> (# Quantity units, Quantity units #)) -> Number -> Number
curve evaluateFirstOrder x1 = do
  let (# y1, dy1 #) = evaluateFirstOrder x1
  curveImpl evaluateFirstOrder x1 y1 dy1

curveImpl ::
  (Number -> (# Quantity units, Quantity units #)) ->
  Number ->
  Quantity units ->
  Quantity units ->
  Number
curveImpl evaluateFirstOrder x1 y1 dy1 = do
  let x2 = x1 - y1 / dy1
  let (# y2, dy2 #) = evaluateFirstOrder x2
  if Quantity.abs y2 < 0.5 * Quantity.abs y1
    then curveImpl evaluateFirstOrder x2 y2 dy2
    else x1
