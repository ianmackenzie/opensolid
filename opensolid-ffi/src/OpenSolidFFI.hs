module OpenSolidFFI () where

import Data.String (fromString)
import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Language.Haskell.TH qualified as TH
import OpenSolid hiding (fail, fromString, (>>=))
import Point2d (Point2d)
import Point2d qualified
import Prelude (fail, return, sequence, (>>=))

data WorldSpace

type WorldCoordinates = WorldSpace @ Unitless

xy :: Float -> Float -> IO (StablePtr (Point2d WorldCoordinates))
xy x y = newStablePtr (Point2d.xy x y)

xCoordinate :: StablePtr (Point2d WorldCoordinates) -> IO Float
xCoordinate ptr = do
  point <- deRefStablePtr ptr
  return $ Point2d.xCoordinate point

yCoordinate :: StablePtr (Point2d WorldCoordinates) -> IO Float
yCoordinate ptr = do
  point <- deRefStablePtr ptr
  return $ Point2d.yCoordinate point

freePoint :: StablePtr (Point2d WorldCoordinates) -> IO ()
freePoint = freeStablePtr

-- Generate 'foreign export' declarations using Template Haskell
$( do
    let export exportedName function = do
          TH.VarI _ functionType _ <- TH.reify function
          return (TH.ForeignD (TH.ExportF TH.CCall exportedName function functionType))
    sequence
      [ export "opensolid_point2d_xy" 'xy
      , export "opensolid_point2d_x_coordinate" 'xCoordinate
      , export "opensolid_point2d_y_coordinate" 'yCoordinate
      , export "opensolid_point2d_free" 'freePoint
      ]
 )
