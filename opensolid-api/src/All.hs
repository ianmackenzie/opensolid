module All (classes) where

import Internal (Class)
import OpenSolid (List)
import OpenSolidAPI.Axis2d (axis2d)
import OpenSolidAPI.Basis2d (basis2d)
import OpenSolidAPI.Bounds2d (bounds2d)
import OpenSolidAPI.Direction2d (direction2d)
import OpenSolidAPI.Frame2d (frame2d)
import OpenSolidAPI.Point2d (point2d)
import OpenSolidAPI.Range (range)
import OpenSolidAPI.Vector2d (vector2d)

classes :: List Class
classes =
  [ axis2d
  , basis2d
  , bounds2d
  , direction2d
  , frame2d
  , point2d
  , range
  , vector2d
  ]
