module OpenSolidAPI.Point2d (point2d) where

import Internal (Class, cls, method, static)
import OpenSolid
import Point2d qualified

point2d :: Class
point2d =
  cls ''Point2d.Point2d ['Point2d.xCoordinate, 'Point2d.yCoordinate] [] $
    [ method 'Point2d.xCoordinate ["point"]
    , method 'Point2d.yCoordinate ["point"]
    , static 'Point2d.angleFrom ["p1", "p2"]
    , static 'Point2d.distanceFrom ["p1", "p2"]
    , static 'Point2d.interpolateFrom ["p1", "p2", "t"]
    , static 'Point2d.meters ["px", "py"]
    , static 'Point2d.midpoint ["p1", "p2"]
    , static 'Point2d.origin []
    , method 'Point2d.placeIn ["frame", "point"]
    , method 'Point2d.relativeTo ["frame", "point"]
    , method 'Point2d.signedDistanceAlong ["axis", "point"]
    , method 'Point2d.signedDistanceFrom ["axis", "point"]
    , static 'Point2d.x ["px"]
    , static 'Point2d.xy ["x", "y"]
    , static 'Point2d.y ["py"]
    ]
