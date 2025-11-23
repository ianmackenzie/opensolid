module OpenSolid.CoordinateSystem (UvSpace, Defines, LocalSpace) where

import Data.Kind (Type)
import Prelude (Eq, Show)

newtype LocalSpace = LocalSpace Type

type Defines space = 'LocalSpace space

data UvSpace deriving (Eq, Show)
