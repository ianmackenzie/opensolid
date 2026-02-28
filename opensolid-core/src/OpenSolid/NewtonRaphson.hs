{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.NewtonRaphson
  ( Curve
  , curve
  , Surface
  , surface
  )
where

import Data.Void (Void)
import OpenSolid.NewtonRaphson1D qualified as NewtonRaphson1D
import OpenSolid.NewtonRaphson2D qualified as NewtonRaphson2D
import OpenSolid.NewtonRaphson3D qualified as NewtonRaphson3D
import OpenSolid.Prelude
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector (Vector)

class Curve dimension units space where
  curve ::
    (Number -> (# Vector dimension units space, Vector dimension units space #)) ->
    Number ->
    Number

instance Curve 1 units Void where
  curve = NewtonRaphson1D.curve

instance Curve 2 units space where
  curve = NewtonRaphson2D.curve

instance Curve 3 units space where
  curve = NewtonRaphson3D.curve

class Surface dimension units space where
  surface ::
    (UvPoint -> (# Vector dimension units space, Vector dimension units space, Vector dimension units space #)) ->
    UvPoint ->
    UvPoint

instance Surface 2 units space where
  surface = NewtonRaphson2D.surface

instance Surface 3 units space where
  surface = NewtonRaphson3D.surface
