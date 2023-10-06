module OpenSolidFFI.Point2d () where

import FFIWrapper (wrapFunction)
import OpenSolid hiding ((>>=))
import Point2d qualified
import Prelude (concat, fmap, mapM, (>>=))

$( fmap concat $
    mapM wrapFunction $
      [ 'Point2d.angleFrom
      , 'Point2d.distanceFrom
      , 'Point2d.interpolateFrom
      , 'Point2d.meters
      , 'Point2d.midpoint
      , 'Point2d.origin
      , 'Point2d.placeIn
      , 'Point2d.relativeTo
      , 'Point2d.signedDistanceAlong
      , 'Point2d.signedDistanceFrom
      , 'Point2d.uv
      , 'Point2d.x
      , 'Point2d.xCoordinate
      , 'Point2d.xy
      , 'Point2d.y
      , 'Point2d.yCoordinate
      ]
 )
