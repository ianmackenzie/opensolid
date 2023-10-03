module OpenSolidFFI () where

import Foreign.StablePtr (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Language.Haskell.TH qualified as TH
import OpenSolid hiding ((>>=))
import Point2d (Point2d)
import Point2d qualified
import Text qualified
import Prelude (return, (>>=))

data WorldSpace

type WorldCoordinates = WorldSpace @ Unitless

xy :: Float -> Float -> IO (StablePtr (Point2d WorldCoordinates))
xy x y = newStablePtr (Point2d.xy x y)

-- Generate 'foreign export' declaration for 'xy' using Template Haskell
$( let floatType = TH.ConT ''Float
       ioType = TH.ConT ''IO
       stablePtrType = TH.ConT ''StablePtr
       point2dType = TH.ConT ''Point2d
       worldCoordinatesType = TH.ConT ''WorldCoordinates

       functionType :: List TH.Type -> TH.Type -> TH.Type
       functionType [] returnType = returnType
       functionType (firstArgumentType : remainingArgumentTypes) returnType =
        TH.AppT
          (TH.AppT TH.ArrowT firstArgumentType)
          (functionType remainingArgumentTypes returnType)

       xyReturnType =
        TH.AppT ioType $
          TH.AppT stablePtrType $
            TH.AppT point2dType worldCoordinatesType

       xyFunctionType = functionType [floatType, floatType] xyReturnType
    in return [TH.ForeignD (TH.ExportF TH.CCall (Text.toChars "xy") 'xy xyFunctionType)]
 )

foreign export ccall xCoordinate :: StablePtr (Point2d WorldCoordinates) -> IO Float

xCoordinate :: StablePtr (Point2d WorldCoordinates) -> IO Float
xCoordinate ptr = do
  point <- deRefStablePtr ptr
  return $ Point2d.xCoordinate point

foreign export ccall yCoordinate :: StablePtr (Point2d WorldCoordinates) -> IO Float

yCoordinate :: StablePtr (Point2d WorldCoordinates) -> IO Float
yCoordinate ptr = do
  point <- deRefStablePtr ptr
  return $ Point2d.yCoordinate point

foreign export ccall freePoint :: StablePtr (Point2d WorldCoordinates) -> IO ()

freePoint :: StablePtr (Point2d WorldCoordinates) -> IO ()
freePoint = freeStablePtr
