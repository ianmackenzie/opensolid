module CoordinateSystem
  ( CoordinateSystem
  , type (@)
  , Units
  , Space
  , LocalCoordinateSystem
  , Defines
  )
where

import Basics

data CoordinateSystem = CoordinateSystem Type Type

type space @ units = 'CoordinateSystem space units

type family Space (coordinateSystem :: CoordinateSystem) where
  Space (space @ units) = space

type family Units (coordinateSystem :: CoordinateSystem) where
  Units (space @ units) = units

newtype LocalCoordinateSystem = LocalCoordinateSystem CoordinateSystem

type Defines coordinateSystem = 'LocalCoordinateSystem coordinateSystem
