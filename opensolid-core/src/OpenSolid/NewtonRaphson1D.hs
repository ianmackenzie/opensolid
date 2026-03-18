{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson1D (EvaluateCurve, curve) where

import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

type EvaluateCurve units = Number -> (# Quantity units, Quantity units #)

curve :: EvaluateCurve units -> Number -> Number
curve evaluate x1 = do
  let (# y1, dy1 #) = evaluate x1
  curveImpl evaluate x1 y1 dy1

curveImpl :: EvaluateCurve units -> Number -> Quantity units -> Quantity units -> Number
curveImpl evaluate x1 y1 dy1 = do
  let x2 = x1 - y1 / dy1
  let (# y2, dy2 #) = evaluate x2
  if Quantity.abs y2 < 0.5 * Quantity.abs y1 then curveImpl evaluate x2 y2 dy2 else x1
