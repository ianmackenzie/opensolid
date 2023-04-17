module Curve2d.Derivatives (Derivatives (..)) where

import {-# SOURCE #-} Curve2d (Curve2d)
import OpenSolid
import VectorCurve2d (VectorCurve2d)

data Derivatives (coordinateSystem :: CoordinateSystem)
  = Derivatives
      (Curve2d coordinateSystem)
      (VectorCurve2d coordinateSystem)
      (VectorCurve2d coordinateSystem)

instance
  (space ~ space', units ~ units')
  => HasField "curve" (Derivatives (space @ units)) (Curve2d (space' @ units'))
  where
  getField (Derivatives curve _ _) = curve

instance
  (space ~ space', units ~ units')
  => HasField "first" (Derivatives (space @ units)) (VectorCurve2d (space' @ units'))
  where
  getField (Derivatives _ first _) = first

instance
  (space ~ space', units ~ units')
  => HasField "second" (Derivatives (space @ units)) (VectorCurve2d (space' @ units'))
  where
  getField (Derivatives _ _ second) = second
