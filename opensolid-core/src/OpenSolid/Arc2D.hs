module OpenSolid.Arc2D
  ( Arc2D
  , new
  , centerPoint
  , radius
  , startAngle
  , endAngle
  , sweptAngle
  , startPoint
  , endPoint
  , toCircle
  , point
  , tangentDirection
  , sweptAround
  , reverse
  , offsetLeftwardBy
  , offsetRightwardBy
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Circle2D (Circle2D)
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Vector2D qualified as Vector2D

-- | An arc in 2D.
data Arc2D units
  = Arc2D
  { centerPoint :: Point2D units
  , radius :: Quantity units
  , startAngle :: Angle
  , endAngle :: Angle
  }
  deriving (Show)

new ::
  ("centerPoint" ::: Point2D units) ->
  ("radius" ::: Quantity units) ->
  ("startAngle" ::: Angle) ->
  ("endAngle" ::: Angle) ->
  Arc2D units
new
  ("centerPoint" ::: givenCenterPoint)
  ("radius" ::: givenRadius)
  ("startAngle" ::: givenStartAngle)
  ("endAngle" ::: givenEndAngle) =
    Arc2D
      { centerPoint = givenCenterPoint
      , radius = givenRadius
      , startAngle = givenStartAngle
      , endAngle = givenEndAngle
      }

centerPoint :: Arc2D units -> Point2D units
centerPoint = (.centerPoint)

radius :: Arc2D units -> Quantity units
radius = (.radius)

startAngle :: Arc2D units -> Angle
startAngle = (.startAngle)

endAngle :: Arc2D units -> Angle
endAngle = (.endAngle)

sweptAngle :: Arc2D units -> Angle
sweptAngle arc = endAngle arc - startAngle arc

startPoint :: Arc2D units -> Point2D units
startPoint arc = centerPoint arc + Vector2D.polar (radius arc) (startAngle arc)

endPoint :: Arc2D units -> Point2D units
endPoint arc = centerPoint arc + Vector2D.polar (radius arc) (endAngle arc)

toCircle :: Arc2D units -> Circle2D units
toCircle arc = Circle2D.withRadius (radius arc) (centerPoint arc)

point :: Arc2D units -> Number -> Point2D units
point arc t = do
  let angle = Quantity.interpolateFrom (startAngle arc) (endAngle arc) t
  centerPoint arc + Vector2D.polar (radius arc) angle

tangentDirection :: Arc2D units -> Number -> Direction2D
tangentDirection arc t = do
  let positionAngle = Quantity.interpolateFrom (startAngle arc) (endAngle arc) t
  let sign = Quantity.sign (sweptAngle arc)
  Direction2D.fromAngle (positionAngle + sign * Angle.halfPi)

sweptAround :: Point2D units -> Point2D units -> Angle -> Arc2D units
sweptAround givenCenterPoint givenStartPoint givenSweptAngle = do
  let computedStartAngle = Point2D.angleFrom givenCenterPoint givenStartPoint
  Arc2D
    { centerPoint = givenCenterPoint
    , radius = (Point2D.distanceFrom givenCenterPoint givenStartPoint)
    , startAngle = computedStartAngle
    , endAngle = (computedStartAngle + givenSweptAngle)
    }

reverse :: Arc2D units -> Arc2D units
reverse arc =
  Arc2D
    { centerPoint = arc.centerPoint
    , radius = arc.radius
    , startAngle = arc.endAngle
    , endAngle = arc.startAngle
    }

offsetLeftwardBy :: Quantity units -> Arc2D units -> Arc2D units
offsetLeftwardBy distance arc =
  Arc2D
    { centerPoint = arc.centerPoint
    , radius = arc.radius - Quantity.sign (sweptAngle arc) * distance
    , startAngle = arc.startAngle
    , endAngle = arc.endAngle
    }

offsetRightwardBy :: Quantity units -> Arc2D units -> Arc2D units
offsetRightwardBy distance = offsetLeftwardBy -distance
