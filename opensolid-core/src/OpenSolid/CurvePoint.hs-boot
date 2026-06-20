module OpenSolid.CurvePoint (CurvePoint) where

import GHC.TypeLits (Natural)
import OpenSolid.Prelude

type role CurvePoint nominal nominal nominal

data CurvePoint (dimension :: Natural) (units :: Type) (space :: Type)
