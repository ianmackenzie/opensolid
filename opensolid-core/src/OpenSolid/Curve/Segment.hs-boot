module OpenSolid.Curve.Segment (Segment) where

import GHC.TypeLits (Natural)
import OpenSolid.Prelude

type role Segment nominal nominal nominal

data Segment (dimension :: Natural) (units :: Type) (space :: Type)
