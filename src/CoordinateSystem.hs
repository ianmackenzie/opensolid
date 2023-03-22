module CoordinateSystem
  ( CoordinateSystem (Coordinates)
  , Units
  , Space
  )
where

import Basics

data CoordinateSystem = Coordinates Type Type

type family Space (coordinateSystem :: CoordinateSystem) where
  Space (Coordinates space units) = space

type family Units (coordinateSystem :: CoordinateSystem) where
  Units (Coordinates space units) = units
