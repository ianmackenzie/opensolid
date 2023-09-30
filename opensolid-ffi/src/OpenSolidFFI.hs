module OpenSolidFFI () where

import CoordinateSystem
import Foreign.C.Types (CDouble (..))
import Foreign.StablePtr
import Point2d (Point2d (..))
import Point2d qualified
import Qty (Qty (..))
import Prelude

foreign export ccall xy :: CDouble -> CDouble -> IO (StablePtr (Point2d (space @ units)))

xy :: CDouble -> CDouble -> IO (StablePtr (Point2d (space @ units)))
xy (CDouble x) (CDouble y) = do
  newStablePtr $ Point2d.xy (Qty x) (Qty y)

foreign export ccall xCoordinate :: StablePtr (Point2d (space @ units)) -> IO CDouble

xCoordinate :: StablePtr (Point2d (space @ units)) -> IO CDouble
xCoordinate ptr =
  do
    point <- deRefStablePtr ptr
    let (Qty x) = Point2d.xCoordinate point
    return $ CDouble x

foreign export ccall yCoordinate :: StablePtr (Point2d (space @ units)) -> IO CDouble

yCoordinate :: StablePtr (Point2d (space @ units)) -> IO CDouble
yCoordinate ptr =
  do
    point <- deRefStablePtr ptr
    let (Qty x) = Point2d.yCoordinate point
    return $ CDouble x

foreign export ccall freePoint :: StablePtr (Point2d (space @ units)) -> IO ()

freePoint :: StablePtr (Point2d (space @ units)) -> IO ()
freePoint = freeStablePtr
