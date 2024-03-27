module OpenSolidAPI.Vector2d (vector2d) where

import Internal (Class, cls, method, static)
import OpenSolid
import Vector2d qualified

vector2d :: Class
vector2d =
  cls ''Vector2d.Vector2d ['Vector2d.xComponent, 'Vector2d.yComponent] [''Vector2d.IsZero] $
    [ static 'Vector2d.zero []
    , static 'Vector2d.x ["vx"]
    , static 'Vector2d.y ["vy"]
    , static 'Vector2d.xy ["vx", "vy"]
    , -- TODO: p2 - p1, or p1.vector_to(p2)
      -- from is a keyword in Python
      -- , static 'Vector2d.from ["p1", "p2"]
      static 'Vector2d.meters ["vx", "vy"]
    , static 'Vector2d.squareMeters ["vx", "vy"]
    , static 'Vector2d.polar ["r", "theta"]
    , method 'Vector2d.xComponent ["vector"]
    , method 'Vector2d.yComponent ["vector"]
    , static 'Vector2d.midpoint ["v1", "v2"]
    , static 'Vector2d.interpolateFrom ["v1", "v2", "t"]
    , method 'Vector2d.magnitude ["vector"]
    , method 'Vector2d.squaredMagnitude ["vector"]
    , method 'Vector2d.angle ["vector"]
    , method 'Vector2d.direction ["vector"]
    , method 'Vector2d.magnitudeAndDirection ["vector"]
    , method 'Vector2d.normalize ["vector"]
    , method 'Vector2d.rotateRight ["vector"]
    , method 'Vector2d.rotateLeft ["vector"]
    , method 'Vector2d.placeIn ["frame", "vector"]
    , method 'Vector2d.relativeTo ["frame", "vector"]
    ]
