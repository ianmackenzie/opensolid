module OpenSolid.Bounds
  ( Bounds
  , contains
  , aggregate2
  )
where

import OpenSolid.CoordinateSystem (Bounds, CoordinateSystem)
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import OpenSolid.Prelude

aggregate2 ::
  CoordinateSystem dimension units space =>
  Bounds dimension units space ->
  Bounds dimension units space ->
  Bounds dimension units space
aggregate2 = CoordinateSystem.boundsAggregate2

contains ::
  CoordinateSystem dimension units space =>
  Bounds dimension units space ->
  Bounds dimension units space ->
  Bool
contains = CoordinateSystem.boundsContains
