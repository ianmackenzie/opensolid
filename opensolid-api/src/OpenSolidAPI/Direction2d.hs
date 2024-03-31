module OpenSolidAPI.Direction2d (direction2d) where

import Direction2d qualified
import Internal (Class, cls, method, static)
import OpenSolid

direction2d :: Class
direction2d =
  cls ''Direction2d.Direction2d ['Direction2d.xComponent, 'Direction2d.yComponent] [] $
    [ method 'Direction2d.xComponent ["dir"]
    , method 'Direction2d.yComponent ["dir"]
    , static 'Direction2d.unsafe ["v"]
    , method 'Direction2d.vector ["dir"]
    , static 'Direction2d.x []
    , static 'Direction2d.positiveX []
    , static 'Direction2d.negativeX []
    , static 'Direction2d.y []
    , static 'Direction2d.positiveY []
    , static 'Direction2d.negativeY []
    , -- TODO: from doesn't work in Python
      -- , static 'Direction2d.from ["p1", "p2"]
      static 'Direction2d.fromAngle ["angle"]
    , static 'Direction2d.toAngle ["dir"]
    , static 'Direction2d.degrees ["value"]
    , static 'Direction2d.radians ["value"]
    , method 'Direction2d.angleFrom ["d1", "d2"]
    , method 'Direction2d.perpendicularTo ["dir"]
    , method 'Direction2d.rotateLeft ["dir"]
    , method 'Direction2d.rotateRight ["dir"]
    , method 'Direction2d.placeIn ["frame", "dir"]
    , method 'Direction2d.relativeTo ["frame", "dir"]
    ]
