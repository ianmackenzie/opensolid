module OpenSolidFFI.Point2d () where

import FFIWrapper (wrapFunction)
import OpenSolid hiding ((>>=))
import Point2d
  ( angleFrom
  , distanceFrom
  , interpolateFrom
  , meters
  , midpoint
  , origin
  , placeIn
  , relativeTo
  , signedDistanceAlong
  , signedDistanceFrom
  , uv
  , x
  , xCoordinate
  , xy
  , y
  , yCoordinate
  )
import Prelude (concat, mapM, return, (>>=))

$( do
    results <-
      mapM wrapFunction $
        [ 'angleFrom
        , 'distanceFrom
        , 'interpolateFrom
        , 'meters
        , 'midpoint
        , 'origin
        , 'placeIn
        , 'relativeTo
        , 'signedDistanceAlong
        , 'signedDistanceFrom
        , 'uv
        , 'x
        , 'xCoordinate
        , 'xy
        , 'y
        , 'yCoordinate
        ]
    return $ concat results
 )
