module OpenSolid.Bounds
  ( Bounds
  , BoundsExists
  , constant
  , contains
  , hull
  , hull2
  , aggregate
  , aggregate2
  , intersection
  , diameter
  , transformBy
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives.Abstract (Bounds, BoundsExists, Point, Transform)
import OpenSolid.Primitives.Abstract qualified as Primitives.Abstract

constant ::
  BoundsExists dimension units space =>
  Point dimension units space ->
  Bounds dimension units space
constant = Primitives.Abstract.boundsConstant

contains ::
  BoundsExists dimension units space =>
  Bounds dimension units space ->
  Bounds dimension units space ->
  Bool
contains = Primitives.Abstract.boundsContains

hull ::
  BoundsExists dimension units space =>
  NonEmpty (Point dimension units space) ->
  Bounds dimension units space
hull = Primitives.Abstract.boundsHull

hull2 ::
  BoundsExists dimension units space =>
  Point dimension units space ->
  Point dimension units space ->
  Bounds dimension units space
hull2 = Primitives.Abstract.boundsHull2

aggregate ::
  BoundsExists dimension units space =>
  NonEmpty (Bounds dimension units space) ->
  Bounds dimension units space
aggregate = Primitives.Abstract.boundsAggregate

aggregate2 ::
  BoundsExists dimension units space =>
  Bounds dimension units space ->
  Bounds dimension units space ->
  Bounds dimension units space
aggregate2 = Primitives.Abstract.boundsAggregate2

intersection ::
  BoundsExists dimension units space =>
  Bounds dimension units space ->
  Bounds dimension units space ->
  Maybe (Bounds dimension units space)
intersection = Primitives.Abstract.boundsIntersection

diameter ::
  BoundsExists dimension units space =>
  Bounds dimension units space ->
  Quantity units
diameter = Primitives.Abstract.boundsDiameter

transformBy ::
  BoundsExists dimension units space =>
  Transform dimension tag units space ->
  Bounds dimension units space ->
  Bounds dimension units space
transformBy = Primitives.Abstract.boundsTransformBy
