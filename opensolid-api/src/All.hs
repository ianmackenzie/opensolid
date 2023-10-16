module All (classes) where

import Internal (Class)
import OpenSolidAPI.Axis2d (axis2d)
import OpenSolidAPI.Direction2d (direction2d)
import OpenSolidAPI.Frame2d (frame2d)
import OpenSolidAPI.Point2d (point2d)
import OpenSolidAPI.Vector2d (vector2d)

classes :: [Class]
classes =
  [ axis2d
  , direction2d
  , frame2d
  , point2d
  , vector2d
  ]
