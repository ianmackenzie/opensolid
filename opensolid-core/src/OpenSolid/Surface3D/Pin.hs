module OpenSolid.Surface3D.Pin
  ( Pin
  , with
  , point
  , uvPoint
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvBounds qualified as UvBounds
import OpenSolid.UvPoint (UvPoint)

data Pin space = Pin
  { uvPoint :: UvPoint
  , point :: Point3D space
  }
  deriving (Show)

instance Bounded (Pin space) UvBounds where
  bounds = UvBounds.constant . uvPoint

instance space1 ~ space2 => Bounded (Pin space1) (Bounds3D space2) where
  bounds = Bounds3D.constant . point

instance Intersects (Pin space) UvPoint Unitless where
  {-# INLINE (^) #-}
  pin ^ givenUvPoint = uvPoint pin ~= givenUvPoint

instance space1 ~ space2 => Intersects (Pin space1) (Point3D space2) Meters where
  {-# INLINE (^) #-}
  pin ^ givenPoint = point pin ~= givenPoint

with :: ("uvPoint" ::: UvPoint, "point" ::: Point3D space) -> Pin space
with params = Pin {uvPoint = params.uvPoint, point = params.point}

point :: Pin space -> Point3D space
point pin = pin.point

uvPoint :: Pin space -> UvPoint
uvPoint pin = pin.uvPoint

placeIn :: Frame3D global local -> Pin local -> Pin global
placeIn frame pin =
  Pin
    { uvPoint = uvPoint pin
    , point = Point3D.placeIn frame (point pin)
    }

relativeTo :: Frame3D global local -> Pin global -> Pin local
relativeTo frame pin = placeIn (Frame3D.inverse frame) pin
