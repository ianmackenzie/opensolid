module OpenSolid.Frame2D
  ( Frame2D
  , with
  , coerce
  , xy
  , atPoint
  , originPoint
  , orientation
  , xDirection
  , yDirection
  , xAxis
  , yAxis
  , fromXAxis
  , fromYAxis
  , placeIn
  , relativeTo
  , placeOn
  , inverse
  )
where

import Data.Coerce qualified
import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Orientation2D (Orientation2D)
import OpenSolid.Orientation2D qualified as Orientation2D
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Frame2D (..)
  , Plane3D (..)
  , PlaneOrientation3D (..)
  )

with :: "originPoint" ::: Point2D units -> "orientation" ::: Orientation2D -> Frame2D units
with ("originPoint" ::: givenOriginPoint) ("orientation" ::: givenOrientation) =
  recursive \self ->
    Frame2D
      { originPoint = givenOriginPoint
      , orientation = givenOrientation
      , inverse =
          Frame2D
            { originPoint = Point2D.relativeTo self Point2D.origin
            , orientation = Orientation2D.relativeTo self Orientation2D.horizontal
            , inverse = self
            }
      }

originPoint :: Frame2D units -> Point2D units
originPoint = (.originPoint)

orientation :: Frame2D units -> Orientation2D
orientation = (.orientation)

{-# INLINE coerce #-}
coerce :: Frame2D units1 -> Frame2D units2
coerce = Data.Coerce.coerce

xDirection :: Frame2D units -> Direction2D
xDirection frame = Orientation2D.xDirection (orientation frame)

yDirection :: Frame2D units -> Direction2D
yDirection frame = Orientation2D.yDirection (orientation frame)

xy :: Frame2D units
xy = atPoint Point2D.origin

atPoint :: Point2D units -> Frame2D units
atPoint p0 = with {originPoint = p0, orientation = Orientation2D.horizontal}

fromXAxis :: Axis2D units -> Frame2D units
fromXAxis axis =
  with
    { originPoint = Axis2D.originPoint axis
    , orientation = Orientation2D.fromXDirection (Axis2D.direction axis)
    }

fromYAxis :: Axis2D units -> Frame2D units
fromYAxis axis =
  with
    { originPoint = Axis2D.originPoint axis
    , orientation = Orientation2D.fromYDirection (Axis2D.direction axis)
    }

xAxis :: Frame2D units -> Axis2D units
xAxis frame = Axis2D.with {originPoint = originPoint frame, direction = xDirection frame}

yAxis :: Frame2D units -> Axis2D units
yAxis frame = Axis2D.with {originPoint = originPoint frame, direction = yDirection frame}

placeIn :: Frame2D units -> Frame2D units -> Frame2D units
placeIn globalFrame frame =
  with
    { originPoint = Point2D.placeIn globalFrame (originPoint frame)
    , orientation = Orientation2D.placeIn globalFrame (orientation frame)
    }

relativeTo :: Frame2D units -> Frame2D units -> Frame2D units
relativeTo globalFrame frame =
  with
    { originPoint = Point2D.relativeTo globalFrame (originPoint frame)
    , orientation = Orientation2D.relativeTo globalFrame (orientation frame)
    }

placeOn :: Plane3D space -> Frame2D Meters -> Plane3D space
placeOn plane frame =
  Plane3D
    { originPoint = Point2D.placeOn plane (originPoint frame)
    , orientation =
        PlaneOrientation3D
          { xDirection = Direction2D.placeOn plane (xDirection frame)
          , yDirection = Direction2D.placeOn plane (yDirection frame)
          , normalDirection = plane.orientation.normalDirection
          }
    }

inverse :: Frame2D units -> Frame2D units
inverse = (.inverse)
