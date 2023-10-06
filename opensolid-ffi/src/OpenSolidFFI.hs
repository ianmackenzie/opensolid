module OpenSolidFFI () where

import Data.String (fromString)
import FFIWrapper (wrapFunction)
import Foreign.StablePtr (freeStablePtr)
import Language.Haskell.TH qualified as TH
import OpenSolid hiding (fail, fromString, (++), (>>=))
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

-- Generate 'foreign export' declarations using Template Haskell
$( do
    functionType <- TH.reifyType 'freeStablePtr
    return [TH.ForeignD (TH.ExportF TH.CCall "opensolid_free" 'freeStablePtr functionType)]
 )

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
