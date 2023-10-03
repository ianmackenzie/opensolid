module OpenSolidFFI () where

import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import OpenSolid (Float, ($), type (@))
import Point2d (Point2d)
import Point2d qualified
import Qty (Qty (Qty))
import Units (Unitless)
import Prelude (IO, return, (>>=))

data Space

type CoordinateSystem = Space @ Unitless

foreign export ccall xy :: Float -> Float -> IO (StablePtr (Point2d CoordinateSystem))

xy :: Float -> Float -> IO (StablePtr (Point2d CoordinateSystem))
xy x y = newStablePtr (Point2d.xy x y)

foreign export ccall xCoordinate :: StablePtr (Point2d CoordinateSystem) -> IO Float

xCoordinate :: StablePtr (Point2d CoordinateSystem) -> IO Float
xCoordinate ptr = do
  point <- deRefStablePtr ptr
  return $ Point2d.xCoordinate point

foreign export ccall yCoordinate :: StablePtr (Point2d CoordinateSystem) -> IO Float

yCoordinate :: StablePtr (Point2d CoordinateSystem) -> IO Float
yCoordinate ptr = do
  point <- deRefStablePtr ptr
  return $ Point2d.yCoordinate point

foreign export ccall freePoint :: StablePtr (Point2d CoordinateSystem) -> IO ()

freePoint :: StablePtr (Point2d CoordinateSystem) -> IO ()
freePoint = freeStablePtr
