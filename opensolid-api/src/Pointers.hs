module Pointers
  ( freeStablePtr
  , fromVoidPtr
  , toVoidPtr
  , TaggedError (..)
  )
where

import Axis2d qualified
import Bounds2d qualified
import Direction2d qualified
import Foreign qualified
import Foreign.Storable (Storable)
import Frame2d qualified
import OpenSolid
import Point2d qualified
import Range qualified
import Vector2d qualified

{- | Define a pattern that effectively lets us pretend that there's a Word8 constructor
that takes an Int and returns a Word8; we can then use this to
'construct' a Word8 from an Int or 'deconstruct' an Int from a Word8
-}
pattern Word8 :: Int -> Foreign.Word8
pattern Word8 n <- (fromIntegral -> n)
  where
    Word8 = fromIntegral

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

-- A typeclass that knows how to convert types to and from `Ptr ()`
-- for container and primitive types we use peek/mallocBytes/poke
-- for library classes, we create `StablePtr typ` and cast it into a normal `Ptr`
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

instance (VoidPtr (Vector2d.Vector2d a)) where
  fromVoidPtr = derefStablePtr
  toVoidPtr = newStablePtr

instance (VoidPtr (Frame2d.Frame2d a b)) where
  fromVoidPtr = derefStablePtr
  toVoidPtr = newStablePtr

instance (VoidPtr (Point2d.Point2d a)) where
  fromVoidPtr = derefStablePtr
  toVoidPtr = newStablePtr

instance (VoidPtr (Axis2d.Axis2d a)) where
  fromVoidPtr = derefStablePtr
  toVoidPtr = newStablePtr

instance (Storable (Qty u)) => VoidPtr (Qty u) where
  fromVoidPtr ptr = Foreign.peek (Foreign.castPtr ptr)
  toVoidPtr a = fmap Foreign.castPtr (newStorablePtr a)

instance (VoidPtr a) => VoidPtr (Maybe a) where
  fromVoidPtr ptr
    | ptr == Foreign.nullPtr = return Nothing
    | otherwise = Just <$> fromVoidPtr ptr
  toVoidPtr (Just val) = toVoidPtr val
  toVoidPtr Nothing = return Foreign.nullPtr

instance (VoidPtr a, VoidPtr b) => VoidPtr (a, b) where
  fromVoidPtr ptr = do
    aPtr <- Foreign.peek (Foreign.castPtr ptr)
    bPtr <- Foreign.peekElemOff (Foreign.castPtr ptr) 1
    a <- fromVoidPtr aPtr
    b <- fromVoidPtr bPtr
    return (a, b)
  toVoidPtr (a, b) = do
    ptr <- Foreign.mallocBytes (pointerSize * (2 :: Int))
    aPtr <- toVoidPtr a
    bPtr <- toVoidPtr b
    Foreign.poke ptr aPtr
    Foreign.pokeElemOff ptr 1 bPtr
    return (Foreign.castPtr ptr)

pointerSize :: Int
pointerSize = Foreign.sizeOf Foreign.nullPtr

class (ErrorMessage error) => TaggedError error where
  fromTaggedPtr :: Foreign.Word8 -> Foreign.Ptr () -> IO error
  toTaggedPtr :: error -> IO (Foreign.Word8, Foreign.Ptr ())

instance (TaggedError error, VoidPtr success) => VoidPtr (Result error success) where
  fromVoidPtr ptr = do
    voidPtr <- Foreign.peek (Foreign.castPtr ptr)
    tag <- Foreign.peekByteOff ptr pointerSize
    case tag of
      Word8 0 -> fmap Ok $ fromVoidPtr voidPtr
      _ -> fmap Error $ fromTaggedPtr tag voidPtr
  toVoidPtr res = do
    ptr <- Foreign.mallocBytes (pointerSize + (1 :: Int))
    (tag, nestedPtr) <- case res of
      Error err -> toTaggedPtr err
      Ok success -> do
        succPtr <- toVoidPtr success
        return (Word8 0, succPtr)
    Foreign.poke ptr nestedPtr
    Foreign.pokeByteOff ptr pointerSize tag
    return (Foreign.castPtr ptr)

-- TODO: codegen this
instance TaggedError Vector2d.IsZero where
  fromTaggedPtr (Word8 1) _ = return Vector2d.IsZero
  fromTaggedPtr tag _ = internalError ("Unexpected tag " ++ show tag)
  toTaggedPtr Vector2d.IsZero = return (Word8 1, Foreign.nullPtr)
