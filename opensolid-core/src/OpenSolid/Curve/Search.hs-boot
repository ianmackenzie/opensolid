module OpenSolid.Curve.Search (Tree) where

import GHC.TypeLits (Natural)
import {-# SOURCE #-} OpenSolid.Curve.Segment (Segment)
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import OpenSolid.Search qualified as Search

type Tree (dimension :: Natural) (units :: Type) (space :: Type) =
  Search.Tree (Interval Unitless) (Segment dimension units space)
