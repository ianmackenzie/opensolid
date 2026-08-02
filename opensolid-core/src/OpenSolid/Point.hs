module OpenSolid.Point
  ( Point
  , PointExists
  , distanceFrom
  , transformBy
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives.Abstract (Point, PointExists, Transform)
import OpenSolid.Primitives.Abstract qualified as Primitives.Abstract

distanceFrom ::
  PointExists dimension units space =>
  Point dimension units space ->
  Point dimension units space ->
  Quantity units
distanceFrom = Primitives.Abstract.pointDistanceFrom

transformBy ::
  PointExists dimension units space =>
  Transform dimension tag units space ->
  Point dimension units space ->
  Point dimension units space
transformBy = Primitives.Abstract.pointTransformBy
