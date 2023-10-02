module CoordinateSystem
  ( CoordinateSystem
  , type (@)
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

newtype LocalSpace = LocalSpace Type

type Defines space = 'LocalSpace space

data UvSpace

type UvCoordinates = UvSpace @ Unitless
