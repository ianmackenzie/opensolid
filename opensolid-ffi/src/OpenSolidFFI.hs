{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolidFFI () where

import Foreign (Storable)
import Foreign qualified
import OpenSolid hiding (fromInteger, ($), (+), (>>), (>>=))
import OpenSolidAPI (generateForeignFunctions)
import Vector2d (IsZero (IsZero))
import Prelude (($), (+))
import Prelude qualified

instance Storable (Result IsZero a) where
  sizeOf _ = Foreign.sizeOf (Prelude.undefined :: Foreign.Ptr ()) + 1
  alignment _ = Foreign.alignment (Prelude.undefined :: Foreign.Ptr ())
  peek ptr = do
    tag <- Foreign.peekByteOff ptr $ Foreign.sizeOf (Prelude.undefined :: Foreign.Ptr ())
    case tag of
      (0 :: Foreign.Word8) -> do
        voidPtr <- Foreign.peek $ stripResultPtr ptr
        success <- Foreign.deRefStablePtr (Foreign.castPtrToStablePtr voidPtr)
        Prelude.return $ Ok success
      (1 :: Foreign.Word8) -> Prelude.return (Error IsZero)
      _ -> Prelude.error "unexpected variant"

  poke ptr (Error IsZero) = Foreign.pokeByteOff ptr (Foreign.sizeOf (Prelude.undefined :: Foreign.Ptr ())) (1 :: Foreign.Word8)
  poke ptr (Ok success) = do
    stablePtr <- Foreign.newStablePtr success
    Foreign.poke (stripResultPtr ptr) (Foreign.castStablePtrToPtr stablePtr)
    Foreign.pokeByteOff ptr (Foreign.sizeOf (Prelude.undefined :: Foreign.Ptr ())) (0 :: Foreign.Word8)

stripResultPtr :: Foreign.Ptr (Result err a) -> Foreign.Ptr (Foreign.Ptr ())
stripResultPtr = Foreign.castPtr

$(generateForeignFunctions)
