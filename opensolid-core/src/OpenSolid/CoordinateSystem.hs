module OpenSolid.CoordinateSystem
  ( CoordinateSystem
  , type (@)
  , LocalSpace
  , Defines
  , Space
  , Units
  , RightPlane
  , LeftPlane
  , FrontPlane
  , BackPlane
  , TopPlane
  , BottomPlane
  )
where

import OpenSolid.Bootstrap
import OpenSolid.Units (HasUnits, Unitless)

data CoordinateSystem = CoordinateSystem Type Type

type space @ units = 'CoordinateSystem space units

newtype LocalSpace = LocalSpace Type

type Defines space = 'LocalSpace space

instance HasUnits (space @ units) units (space @ Unitless)

type Space :: CoordinateSystem -> Type
type family Space coordinateSystem where
  Space ('CoordinateSystem space units) = space

type Units :: CoordinateSystem -> Type
type family Units coordinateSystem where
  Units ('CoordinateSystem space units) = units

data RightPlane space

data LeftPlane space

data FrontPlane space

data BackPlane space

data TopPlane space

data BottomPlane space
