-- Avoid errors when running Fourmolu
{-# LANGUAGE GHC2021 #-}

module OpenSolidFFI () where

import Array (Array)
import Array qualified
import Foreign (Ptr, StablePtr)
import Foreign qualified
import List qualified
import OpenSolid
import OpenSolid.API qualified as API
import OpenSolid.API.Class (Class (Class))
import OpenSolid.API.Class qualified as Class
import Pair qualified

unsafeArray :: List a -> Array a
unsafeArray (NonEmpty nonEmpty) = Array.fromNonEmpty nonEmpty
unsafeArray [] = internalError "Should never have foreign API class with zero functions"

classFunctions :: Class -> Array (Ptr () -> Ptr () -> IO ())
classFunctions (Class _ constructors staticFunctions memberFunctions) =
  unsafeArray $
    List.concat
      [ List.map Class.callConstructor constructors
      , List.map Class.callStaticFunction (List.collect Pair.second staticFunctions)
      , List.map Class.callMemberFunction (List.collect Pair.second memberFunctions)
      ]

allFunctions :: Array (Array (Ptr () -> Ptr () -> IO ()))
allFunctions = unsafeArray (List.map classFunctions API.classes)

foreign export ccall opensolid_invoke :: Int -> Int -> Ptr () -> Ptr () -> IO ()

opensolid_invoke :: Int -> Int -> Ptr () -> Ptr () -> IO ()
opensolid_invoke classId functionId inputPtr outputPtr = do
  let function = allFunctions |> Array.get classId |> Array.get functionId
  function inputPtr outputPtr

foreign export ccall opensolid_release :: StablePtr a -> IO ()

opensolid_release :: StablePtr a -> IO ()
opensolid_release = Foreign.freeStablePtr
