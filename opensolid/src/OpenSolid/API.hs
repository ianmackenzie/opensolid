module OpenSolid.API (classes) where

import Array (Array)
import Array qualified
import Foreign (Ptr)
import List qualified
import OpenSolid
import OpenSolid.API.Class (Class (Class), Constraint (..), Function (..))
import OpenSolid.API.Class qualified as Class
import OpenSolid.FFI (FFI)
import Point2d (Point2d)
import Point2d qualified

data Space

classes :: List Class
classes =
  [ point2f
  , point2d
  ]

point2f :: Class
point2f = Class "Point2f" (point2dMembers F)

point2d :: Class
point2d = Class "Point2d" (point2dMembers L)

point2dMembers ::
  (FFI (Qty units), FFI (Point2d (Space @ units))) =>
  Constraint (Tolerance units) ->
  List (Function (Point2d (Space @ units)))
point2dMembers _ =
  [ C2 N "xy" "x" "y" Point2d.xy
  , F0 N "origin" Point2d.origin
  , F1 N "x" "x" Point2d.x
  , F1 N "y" "y" Point2d.y
  , M1 N "distance to" "other" Point2d.distanceFrom
  , M1 N "midpoint" "other" Point2d.midpoint
  ]

unsafeArray :: List a -> Array a
unsafeArray (NonEmpty nonEmpty) = Array.fromNonEmpty nonEmpty
unsafeArray [] = internalError "Should never have foreign API class with zero functions"

classFunctions :: Class -> Array (Ptr () -> Ptr () -> IO ())
classFunctions (Class _ functions) = unsafeArray (List.map Class.call functions)

allFunctions :: Array (Array (Ptr () -> Ptr () -> IO ()))
allFunctions = unsafeArray (List.map classFunctions classes)

foreign export ccall opensolid_call :: Int -> Int -> Ptr () -> Ptr () -> IO ()

opensolid_call :: Int -> Int -> Ptr () -> Ptr () -> IO ()
opensolid_call classId functionId inputPtr outputPtr = do
  let function = allFunctions |> Array.get classId |> Array.get functionId
  function inputPtr outputPtr
