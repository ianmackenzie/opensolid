{-# OPTIONS_GHC -Wno-orphans #-}

module Pointers
  ( newMaybePtr
  , freeStablePtr
  , freeStorablePtr
  , newStorablePtr
  , newStablePtr
  , derefStablePtr
  , TaggedError (..)
  )
where

import Bounds2d qualified
import Direction2d qualified
import Foreign qualified
import Foreign.Storable (Storable)
import OpenSolid hiding (fromInteger, (*), (+))
import Range qualified
import Text
import Vector2d qualified
import Prelude (fromInteger, (*), (+))
import Prelude qualified

newMaybePtr :: (VoidPtr a) => Maybe a -> IO (Foreign.Ptr ())
newMaybePtr (Just val) = toVoidPtr val
newMaybePtr Nothing = Prelude.return Foreign.nullPtr

newStorablePtr :: (Storable a) => a -> IO (Foreign.Ptr a)
newStorablePtr result = Prelude.do
  ptr <- Foreign.malloc
  Foreign.poke ptr result
  Prelude.return ptr

newStablePtr :: a -> IO (Foreign.Ptr ())
newStablePtr val = Prelude.do
  stablePtr <- Foreign.newStablePtr val
  Prelude.return $ Foreign.castStablePtrToPtr stablePtr

derefStablePtr :: Foreign.Ptr () -> IO a
derefStablePtr = Foreign.castPtrToStablePtr >> Foreign.deRefStablePtr

freeStablePtr :: Foreign.Ptr () -> IO ()
freeStablePtr ptr
  | ptr == Foreign.nullPtr = Prelude.return ()
  | otherwise = Foreign.freeStablePtr (Foreign.castPtrToStablePtr ptr)

freeStorablePtr :: Foreign.Ptr () -> IO ()
freeStorablePtr ptr
  | ptr == Foreign.nullPtr = Prelude.return ()
  | otherwise = Foreign.free ptr

-- A typeclass that knows how to convert types to and from `Ptr ()`
-- for types that implement `Storable` we use malloc/poke/free
-- for other types, we create `StablePtr typ` and cast it into a normal `Ptr`
class VoidPtr a where
  fromVoidPtr :: Foreign.Ptr () -> IO a
  toVoidPtr :: a -> IO (Foreign.Ptr ())

-- TODO: codegen this for all library classes
instance (VoidPtr (Range.Range a)) where
  fromVoidPtr = derefStablePtr
  toVoidPtr = newStablePtr

instance (VoidPtr (Bounds2d.Bounds2d a)) where
  fromVoidPtr = derefStablePtr
  toVoidPtr = newStablePtr

instance (VoidPtr (Direction2d.Direction2d a)) where
  fromVoidPtr = derefStablePtr
  toVoidPtr = newStablePtr

instance (Storable (Qty u)) => VoidPtr (Qty u) where
  fromVoidPtr ptr = Foreign.peek (Foreign.castPtr ptr)
  toVoidPtr a = Prelude.fmap Foreign.castPtr (newStorablePtr a)

instance (Storable (a, b)) => VoidPtr (a, b) where
  fromVoidPtr ptr = Foreign.peek (Foreign.castPtr ptr)
  toVoidPtr a = Prelude.fmap Foreign.castPtr (newStorablePtr a)

pointerSize :: Int
pointerSize = Foreign.sizeOf Foreign.nullPtr

instance (VoidPtr a, VoidPtr b) => Storable (a, b) where
  sizeOf _ = pointerSize * (2 :: Int)
  alignment _ = Foreign.alignment Foreign.nullPtr
  peek ptr = Prelude.do
    aPtr <- Foreign.peek (Foreign.castPtr ptr)
    bPtr <- Foreign.peekElemOff (Foreign.castPtr ptr) 1
    a <- fromVoidPtr aPtr
    b <- fromVoidPtr bPtr
    Prelude.return (a, b)
  poke ptr (a, b) = Prelude.do
    aPtr <- toVoidPtr a
    bPtr <- toVoidPtr b
    Foreign.poke (Foreign.castPtr ptr) aPtr
    Foreign.pokeElemOff (Foreign.castPtr ptr) 1 bPtr

class (ErrorMessage error) => TaggedError error where
  fromTaggedPtr :: Foreign.Word8 -> Foreign.Ptr () -> IO error
  toTaggedPtr :: error -> IO (Foreign.Word8, Foreign.Ptr ())

instance (TaggedError error, VoidPtr success) => Storable (Result error success) where
  sizeOf _ = pointerSize + (1 :: Int) -- size of a pointer + 1 byte for the tag
  alignment _ = Foreign.alignment Foreign.nullPtr
  peek ptr = Prelude.do
    voidPtr <- Foreign.peek (Foreign.castPtr ptr)
    tag <- Foreign.peekByteOff ptr pointerSize
    case tag of
      (0 :: Foreign.Word8) -> Prelude.fmap Ok $ fromVoidPtr voidPtr
      _ -> Prelude.fmap Error $ fromTaggedPtr tag voidPtr
  poke ptr (Error err) = Prelude.do
    (tag, errPtr) <- toTaggedPtr err
    Foreign.poke (Foreign.castPtr ptr) errPtr
    Foreign.pokeByteOff ptr pointerSize tag
  poke ptr (Ok success) = Prelude.do
    successPtr <- toVoidPtr success
    Foreign.poke (Foreign.castPtr ptr) successPtr
    Foreign.pokeByteOff ptr pointerSize (0 :: Foreign.Word8)

-- TODO: codegen this
instance TaggedError Vector2d.IsZero where
  fromTaggedPtr (1 :: Foreign.Word8) _ = Prelude.return Vector2d.IsZero
  fromTaggedPtr tag _ = Prelude.error (Text.toChars (Text.concat ["Unexpected tag ", Text.fromChars (Prelude.show tag)]))
  toTaggedPtr Vector2d.IsZero = Prelude.return (1 :: Foreign.Word8, Foreign.nullPtr)
