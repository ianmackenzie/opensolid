module OpenSolid.CoordinateSystem
  ( CoordinateSystem
  , type (@)
  , LocalSpace
  , Defines
  , Space
  , Units
  , UvSpace
  , UvCoordinates
  )
where

import OpenSolid.Bootstrap
import OpenSolid.Units (HasUnits, Unitless)

data CoordinateSystem = CoordinateSystem Type Type

type space @ units = 'CoordinateSystem space units

newtype LocalSpace = LocalSpace Type

type Defines space = 'LocalSpace space

instance HasUnits (space @ units) units

type Space :: CoordinateSystem -> Type
type family Space coordinateSystem where
  Space ('CoordinateSystem space units) = space

type Units :: CoordinateSystem -> Type
type family Units coordinateSystem where
  Units ('CoordinateSystem space units) = units

data UvSpace deriving (Eq, Show)

type UvCoordinates = UvSpace @ Unitless
