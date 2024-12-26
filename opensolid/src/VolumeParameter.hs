module VolumeParameter
  ( VolumeParameter (U, V, W)
  , UvwSpace
  , UvwCoordinates
  , UvwPoint
  , UvwDirection
  , UvwBounds
  , domain
  , samples
  )
where

import Bounds3d (Bounds3d)
import Bounds3d qualified
import Direction3d (Direction3d)
import OpenSolid.Prelude
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Point3d (Point3d)
import Quadrature qualified
import Range qualified

data VolumeParameter = U | V | W deriving (Eq, Show, Ord)

data UvwSpace deriving (Eq, Show)

type UvwCoordinates = UvwSpace @ Unitless

type UvwPoint = Point3d UvwCoordinates

type UvwBounds = Bounds3d UvwCoordinates

type UvwDirection = Direction3d UvwSpace

domain :: UvwBounds
domain = Bounds3d.xyz Range.unit Range.unit Range.unit

samples :: List UvwPoint
samples = do
  let (t1, t2, t3, t4) = Quadrature.abscissae4
  [Point3d.xyz t1 t3 t1, Point3d.xyz t2 t1 t3, Point3d.xyz t3 t4 t2, Point3d.xyz t4 t2 t4]
