module CoordinateSystem
  ( CoordinateSystem
  , type (@)
  , Units
  , Space
  , LocalSpace
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

newtype LocalSpace = LocalSpace Type

type Defines space = 'LocalSpace space