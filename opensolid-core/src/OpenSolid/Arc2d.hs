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
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity

-- | An arc in 2D.
data Arc2d units space = Arc2d
  { centerPoint :: Point2D units space
  , radius :: Quantity units
  , startAngle :: Angle
  , endAngle :: Angle
  }
  deriving (Show)

centerPoint :: Arc2d units space -> Point2D units space
centerPoint = (.centerPoint)

radius :: Arc2d units space -> Quantity units
radius = (.radius)

startAngle :: Arc2d units space -> Angle
startAngle = (.startAngle)

endAngle :: Arc2d units space -> Angle
endAngle = (.endAngle)

startPoint :: Arc2d units space -> Point2D units space
startPoint arc = centerPoint arc .+. Vector2D.polar (radius arc) (startAngle arc)

endPoint :: Arc2d units space -> Point2D units space
endPoint arc = centerPoint arc .+. Vector2D.polar (radius arc) (endAngle arc)

pointOn :: Arc2d units space -> Number -> Point2D units space
pointOn arc t = do
  let angle = Quantity.interpolateFrom (startAngle arc) (endAngle arc) t
  centerPoint arc .+. Vector2D.polar (radius arc) angle

polar ::
  "centerPoint" ::: Point2D units space ->
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

sweptAround :: Point2D units space -> Point2D units space -> Angle -> Arc2d units space
sweptAround givenCenterPoint givenStartPoint givenSweptAngle = do
  let computedRadius = Point2D.distanceFrom givenCenterPoint givenStartPoint
  let computedStartAngle = Point2D.angleFrom givenCenterPoint givenStartPoint
  Arc2d
    { centerPoint = givenCenterPoint
    , radius = computedRadius
    , startAngle = computedStartAngle
    , endAngle = computedStartAngle .+. givenSweptAngle
    }
