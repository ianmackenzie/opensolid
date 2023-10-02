module CoordinateSystem
  ( CoordinateSystem
  , type (@)
  , Units
  , Space
  , LocalSpace
  , Defines
  , UvSpace
  , UvCoordinates
  )
where

import Basics
import Units (Unitless)

data CoordinateSystem = CoordinateSystem Type Type

type space @ units = 'CoordinateSystem space units

type family Space (coordinateSystem :: CoordinateSystem) where
  Space (space @ units) = space

type family Units (coordinateSystem :: CoordinateSystem) where
  Units (space @ units) = units

newtype LocalSpace = LocalSpace Type

type Defines space = 'LocalSpace space

data UvSpace

type UvCoordinates = UvSpace @ Unitless
