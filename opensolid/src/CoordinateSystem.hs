module CoordinateSystem
  ( CoordinateSystem
  , type (@)
  , LocalSpace
  , Defines
  , UvSpace
  , UvCoordinates
  , UvwSpace
  , UvwCoordinates
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

data UvwSpace

type UvwCoordinates = UvwSpace @ Unitless
