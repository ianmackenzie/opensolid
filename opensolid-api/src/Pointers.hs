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
import OpenSolid
import Range qualified
import Vector2d qualified

newMaybePtr :: (VoidPtr a) => Maybe a -> IO (Foreign.Ptr ())
newMaybePtr (Just val) = toVoidPtr val
newMaybePtr Nothing = return Foreign.nullPtr

newStorablePtr :: (Storable a) => a -> IO (Foreign.Ptr a)
newStorablePtr result = do
  ptr <- Foreign.malloc
  Foreign.poke ptr result
  return ptr

newStablePtr :: a -> IO (Foreign.Ptr ())
newStablePtr val = do
  stablePtr <- Foreign.newStablePtr val
  return $ Foreign.castStablePtrToPtr stablePtr

derefStablePtr :: Foreign.Ptr () -> IO a
derefStablePtr = Foreign.deRefStablePtr . Foreign.castPtrToStablePtr

freeStablePtr :: Foreign.Ptr () -> IO ()
freeStablePtr ptr
  | ptr == Foreign.nullPtr = return ()
  | otherwise = Foreign.freeStablePtr (Foreign.castPtrToStablePtr ptr)

freeStorablePtr :: Foreign.Ptr () -> IO ()
freeStorablePtr ptr
  | ptr == Foreign.nullPtr = return ()
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
  toVoidPtr a = fmap Foreign.castPtr (newStorablePtr a)

instance (Storable (a, b)) => VoidPtr (a, b) where
  fromVoidPtr ptr = Foreign.peek (Foreign.castPtr ptr)
  toVoidPtr a = fmap Foreign.castPtr (newStorablePtr a)

pointerSize :: Int
pointerSize = Foreign.sizeOf Foreign.nullPtr

instance (VoidPtr a, VoidPtr b) => Storable (a, b) where
  sizeOf _ = pointerSize * (2 :: Int)
  alignment _ = Foreign.alignment Foreign.nullPtr
  peek ptr = do
    aPtr <- Foreign.peek (Foreign.castPtr ptr)
    bPtr <- Foreign.peekElemOff (Foreign.castPtr ptr) 1
    a <- fromVoidPtr aPtr
    b <- fromVoidPtr bPtr
    return (a, b)
  poke ptr (a, b) = do
    aPtr <- toVoidPtr a
    bPtr <- toVoidPtr b
    Foreign.poke (Foreign.castPtr ptr) aPtr
    Foreign.pokeElemOff (Foreign.castPtr ptr) 1 bPtr

class (ErrorMessage error) => TaggedError error where
  fromTaggedPtr :: Foreign.Word8 -> Foreign.Ptr () -> IO error
  toTaggedPtr :: error -> IO (Foreign.Word8, Foreign.Ptr ())

instance (TaggedError error, VoidPtr success) => Storable (Result error success) where
  sizeOf _ = pointerSize + 1 -- size of a pointer + 1 byte for the tag
  alignment _ = Foreign.alignment Foreign.nullPtr
  peek ptr = do
    voidPtr <- Foreign.peek (Foreign.castPtr ptr)
    tag <- Foreign.peekByteOff ptr pointerSize
    case fromIntegral tag of
      0 -> fmap Ok $ fromVoidPtr voidPtr
      _ -> fmap Error $ fromTaggedPtr tag voidPtr
  poke ptr (Error err) = do
    (tag, errPtr) <- toTaggedPtr err
    Foreign.poke (Foreign.castPtr ptr) errPtr
    Foreign.pokeByteOff ptr pointerSize tag
  poke ptr (Ok success) = do
    successPtr <- toVoidPtr success
    Foreign.poke (Foreign.castPtr ptr) successPtr
    Foreign.pokeByteOff ptr pointerSize 0

-- TODO: codegen this
instance TaggedError Vector2d.IsZero where
  fromTaggedPtr (fromIntegral -> 1) _ = return Vector2d.IsZero
  fromTaggedPtr tag _ = internalError ("Unexpected tag " ++ show tag)
  toTaggedPtr Vector2d.IsZero = return (fromIntegral 1, Foreign.nullPtr)
