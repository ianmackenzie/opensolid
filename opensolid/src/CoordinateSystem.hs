module CoordinateSystem
  ( CoordinateSystem
  , type (@)
  , LocalSpace
  , Defines
  , Space
  )
where

import Basics
import Units (HasUnits, Unitless)
import Units qualified

data CoordinateSystem = CoordinateSystem Type Type

type space @ units = 'CoordinateSystem space units

newtype LocalSpace = LocalSpace Type

type Defines space = 'LocalSpace space

instance HasUnits (space @ units) where
  type Units (space @ units) = units
  type Erase (space @ units) = space @ Unitless

type Space :: CoordinateSystem -> Type
type family Space coordinateSystem where
  Space ('CoordinateSystem space units) = space
