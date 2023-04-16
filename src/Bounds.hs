module Bounds (Bounds (..)) where

import OpenSolid

class Bounds b where
  aggregate :: b -> b -> b
  overlaps :: b -> b -> Bool
  intersection :: b -> b -> Maybe b
