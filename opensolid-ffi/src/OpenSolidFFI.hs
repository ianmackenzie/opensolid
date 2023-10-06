module OpenSolidFFI () where

import Data.String (fromString)
import FFIWrapper (wrapFunction)
import Foreign.StablePtr (StablePtr, freeStablePtr)
import Language.Haskell.TH qualified as TH
import OpenSolid hiding (fail, fromString, (++), (>>=))
import Point2d
  ( Point2d
  , angleFrom
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
import Prelude (concat, mapM, return, sequence, (>>=))

data WorldSpace

type WorldCoordinates = WorldSpace @ Unitless

freePoint :: StablePtr (Point2d WorldCoordinates) -> IO ()
freePoint = freeStablePtr

-- Generate 'foreign export' declarations using Template Haskell
$( do
    let export exportedName function = do
          functionType <- TH.reifyType function
          return (TH.ForeignD (TH.ExportF TH.CCall exportedName function functionType))
    sequence
      [ export "opensolid_point2d_free" 'freePoint
      ]
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
