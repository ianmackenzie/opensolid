module OpenSolid.CoordinateSystem
  ( CoordinateSystem
  , type (@)
  , LocalSpace
  , Defines
  , Space
  )
where

import OpenSolid.Bootstrap
import OpenSolid.Units (HasUnits)

data CoordinateSystem = CoordinateSystem Type Type

type space @ units = 'CoordinateSystem space units

newtype LocalSpace = LocalSpace Type

type Defines space = 'LocalSpace space

instance HasUnits (space @ units) units

type Space :: CoordinateSystem -> Type
type family Space coordinateSystem where
  Space ('CoordinateSystem space units) = space
