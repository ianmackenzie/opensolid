module OpenSolid.Curve.IntersectionPoint (IntersectionPoint) where

import GHC.TypeLits (Natural)
import OpenSolid.Prelude

type role IntersectionPoint nominal nominal nominal

data IntersectionPoint (dimension :: Natural) (units :: Type) (space :: Type)
