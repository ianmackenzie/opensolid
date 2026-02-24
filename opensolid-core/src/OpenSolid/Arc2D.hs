module OpenSolid.Arc2D
  ( Arc2D (Arc2D)
  , centerPoint
  , radius
  , startAngle
  , endAngle
  , startPoint
  , endPoint
  , pointOn
  , sweptAround
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Vector2D qualified as Vector2D

-- | An arc in 2D.
newtype Arc2D units space
  = Arc2D
      ( "centerPoint" ::: Point2D units space
      , "radius" ::: Quantity units
      , "startAngle" ::: Angle
      , "endAngle" ::: Angle
      )
  deriving (Show)

centerPoint :: Arc2D units space -> Point2D units space
centerPoint (Arc2D fields) = fields.centerPoint

radius :: Arc2D units space -> Quantity units
radius (Arc2D fields) = fields.radius

startAngle :: Arc2D units space -> Angle
startAngle (Arc2D fields) = fields.startAngle

endAngle :: Arc2D units space -> Angle
endAngle (Arc2D fields) = fields.endAngle

startPoint :: Arc2D units space -> Point2D units space
startPoint arc = centerPoint arc + Vector2D.polar (radius arc) (startAngle arc)

endPoint :: Arc2D units space -> Point2D units space
endPoint arc = centerPoint arc + Vector2D.polar (radius arc) (endAngle arc)

pointOn :: Arc2D units space -> Number -> Point2D units space
pointOn arc t = do
  let angle = Quantity.interpolateFrom (startAngle arc) (endAngle arc) t
  centerPoint arc + Vector2D.polar (radius arc) angle

sweptAround :: Point2D units space -> Point2D units space -> Angle -> Arc2D units space
sweptAround givenCenterPoint givenStartPoint givenSweptAngle = do
  let computedStartAngle = Point2D.angleFrom givenCenterPoint givenStartPoint
  Arc2D
    ( #centerPoint givenCenterPoint
    , #radius (Point2D.distanceFrom givenCenterPoint givenStartPoint)
    , #startAngle computedStartAngle
    , #endAngle (computedStartAngle + givenSweptAngle)
    )
