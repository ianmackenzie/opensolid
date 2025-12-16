module OpenSolid.Arc2d
  ( Arc2d
  , centerPoint
  , radius
  , startAngle
  , endAngle
  , startPoint
  , endPoint
  , pointOn
  , polar
  , sweptAround
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Polymorphic.Point2d (Point2d)
import OpenSolid.Polymorphic.Point2d qualified as Point2d
import OpenSolid.Polymorphic.Vector2d qualified as Vector2d
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

-- | An arc in 2D.
data Arc2d units space = Arc2d
  { centerPoint :: Point2d units space
  , radius :: Quantity units
  , startAngle :: Angle
  , endAngle :: Angle
  }
  deriving (Show)

centerPoint :: Arc2d units space -> Point2d units space
centerPoint = (.centerPoint)

radius :: Arc2d units space -> Quantity units
radius = (.radius)

startAngle :: Arc2d units space -> Angle
startAngle = (.startAngle)

endAngle :: Arc2d units space -> Angle
endAngle = (.endAngle)

startPoint :: Arc2d units space -> Point2d units space
startPoint arc = centerPoint arc .+. Vector2d.polar (radius arc) (startAngle arc)

endPoint :: Arc2d units space -> Point2d units space
endPoint arc = centerPoint arc .+. Vector2d.polar (radius arc) (endAngle arc)

pointOn :: Arc2d units space -> Number -> Point2d units space
pointOn arc t = do
  let angle = Quantity.interpolateFrom (startAngle arc) (endAngle arc) t
  centerPoint arc .+. Vector2d.polar (radius arc) angle

polar ::
  "centerPoint" ::: Point2d units space ->
  "radius" ::: Quantity units ->
  "startAngle" ::: Angle ->
  "endAngle" ::: Angle ->
  Arc2d units space
polar (Named givenCenterPoint) (Named givenRadius) (Named givenStartAngle) (Named givenEndAngle) =
  Arc2d
    { centerPoint = givenCenterPoint
    , radius = givenRadius
    , startAngle = givenStartAngle
    , endAngle = givenEndAngle
    }

sweptAround :: Point2d units space -> Point2d units space -> Angle -> Arc2d units space
sweptAround givenCenterPoint givenStartPoint givenSweptAngle = do
  let computedRadius = Point2d.distanceFrom givenCenterPoint givenStartPoint
  let computedStartAngle = Point2d.angleFrom givenCenterPoint givenStartPoint
  Arc2d
    { centerPoint = givenCenterPoint
    , radius = computedRadius
    , startAngle = computedStartAngle
    , endAngle = computedStartAngle .+. givenSweptAngle
    }
