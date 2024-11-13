module OpenSolid.FFI
  ( FFI (representation)
  , Representation (..)
  , Value (Value)
  , Space
  , Function (..)
  , FloatCoordinates
  , LengthCoordinates
  )
where

import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Float qualified
import Foreign (Ptr, Storable)
import Foreign qualified
import Foreign.Marshal.Alloc qualified
import Foreign.StablePtr qualified
import IO qualified
import Length (Length)
import List qualified
import OpenSolid
import OpenSolid.FFI.Exception (Exception (Exception))
import OpenSolid.FFI.Exception qualified as Exception
import Qty qualified
import Units (Meters)

data Space

type FloatCoordinates = Space @ Unitless

type LengthCoordinates = Space @ Meters

class FFI a where
  representation :: Proxy a -> Representation a

data Representation a where
  -- A 64-bit integer
  Int :: Representation Int
  -- A 64-bit float
  Float :: Representation Float
  -- A 64-bit float, wrapped inside a target language class
  Qty :: Text -> Representation (Qty units)
  -- A struct with a 64-bit integer size and a pointer to an array of items
  List :: FFI a => Representation (List a)
  -- A struct with the first item and then the second
  Pair :: (FFI a, FFI b) => Representation (a, b)
  -- A struct with the three items in order
  Triple :: (FFI a, FFI b, FFI c) => Representation (a, b, c)
  -- A struct with a 64-bit integer tag (0 = Just, 1 = Nothing)
  -- followed by the representation of the value
  Maybe :: FFI a => Representation (Maybe a)
  -- A struct with a 64-bit signed integer tag (0 = Success, 1+ = Failure)
  -- followed by the representation of the successful value or exception
  Result :: FFI a => (x -> Exception) -> Representation (Result x a)
  -- An opaque pointer to a Haskell value
  Class :: Text -> List (Function a) -> Representation a

data Function value where
  Constructor1 ::
    (FFI a, FFI value) =>
    Text ->
    (a -> value) ->
    Function value
  Constructor2 ::
    (FFI a, FFI b, FFI value) =>
    Text ->
    Text ->
    (a -> b -> value) ->
    Function value
  Constructor3 ::
    (FFI a, FFI b, FFI c, FFI value) =>
    Text ->
    Text ->
    Text ->
    (a -> b -> c -> value) ->
    Function value
  Constructor4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value) =>
    Text ->
    Text ->
    Text ->
    Text ->
    (a -> b -> c -> d -> value) ->
    Function value
  Factory0 ::
    FFI value =>
    Text ->
    value ->
    Function value
  Factory1 ::
    (FFI a, FFI value) =>
    Text ->
    Text ->
    (a -> value) ->
    Function value
  Factory2 ::
    (FFI a, FFI b, FFI value) =>
    Text ->
    Text ->
    Text ->
    (a -> b -> value) ->
    Function value
  Factory3 ::
    (FFI a, FFI b, FFI c, FFI value) =>
    Text ->
    Text ->
    Text ->
    Text ->
    (a -> b -> c -> value) ->
    Function value
  Factory4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value) =>
    Text ->
    Text ->
    Text ->
    Text ->
    Text ->
    (a -> b -> c -> d -> value) ->
    Function value
  Static0 ::
    FFI result =>
    Text ->
    result ->
    Function value
  Static1 ::
    (FFI a, FFI result) =>
    Text ->
    Text ->
    (a -> result) ->
    Function value
  Static2 ::
    (FFI a, FFI b, FFI result) =>
    Text ->
    Text ->
    Text ->
    (a -> b -> result) ->
    Function value
  Static3 ::
    (FFI a, FFI b, FFI c, FFI result) =>
    Text ->
    Text ->
    Text ->
    Text ->
    (a -> b -> c -> result) ->
    Function value
  Static4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI result) =>
    Text ->
    Text ->
    Text ->
    Text ->
    Text ->
    (a -> b -> c -> d -> result) ->
    Function value
  Member0 ::
    (FFI value, FFI result) =>
    Text ->
    (value -> result) ->
    Function value
  Member1 ::
    (FFI a, FFI value, FFI result) =>
    Text ->
    Text ->
    (a -> value -> result) ->
    Function value
  Member2 ::
    (FFI a, FFI b, FFI value, FFI result) =>
    Text ->
    Text ->
    Text ->
    (a -> b -> value -> result) ->
    Function value
  Member3 ::
    (FFI a, FFI b, FFI c, FFI value, FFI result) =>
    Text ->
    Text ->
    Text ->
    Text ->
    (a -> b -> c -> value -> result) ->
    Function value
  Member4 ::
    (FFI a, FFI b, FFI c, FFI d, FFI value, FFI result) =>
    Text ->
    Text ->
    Text ->
    Text ->
    Text ->
    (a -> b -> c -> d -> value -> result) ->
    Function value

size :: FFI a => Proxy a -> Int
size proxy = case representation proxy of
  Int -> 8
  Float -> 8
  Qty{} -> 8
  List -> 16
  Pair -> pairSize proxy
  Triple -> tripleSize proxy
  Maybe -> maybeSize proxy
  Result _ -> resultSize proxy
  Class{} -> 8

pairSize :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> Int
pairSize _ = do
  let sizeA = size (Proxy :: Proxy a)
  let sizeB = size (Proxy :: Proxy b)
  sizeA + sizeB

tripleSize :: forall a b c. (FFI a, FFI b, FFI c) => Proxy (a, b, c) -> Int
tripleSize _ = do
  let sizeA = size (Proxy :: Proxy a)
  let sizeB = size (Proxy :: Proxy b)
  let sizeC = size (Proxy :: Proxy c)
  sizeA + sizeB + sizeC

maybeSize :: forall a. FFI a => Proxy (Maybe a) -> Int
maybeSize _ = 8 + size (Proxy :: Proxy a)

resultSize :: forall x a. FFI a => Proxy (Result x a) -> Int
resultSize _ = 8 + size (Proxy :: Proxy a)

instance FFI Float where
  representation _ = Float

instance FFI Int where
  representation _ = Int

instance FFI Length where
  representation _ = Qty "Length"

instance FFI item => FFI (List item) where
  representation _ = List

instance (FFI first, FFI second) => FFI (first, second) where
  representation _ = Pair

instance (FFI first, FFI second, FFI third) => FFI (first, second, third) where
  representation _ = Triple

newtype Value value = Value value

instance FFI value => Storable (Value value) where
  alignment _ = 8
  sizeOf _ = size (Proxy @value)
  poke ptr (Value value) = store ptr 0 value
  peek ptr = IO.map Value (load ptr 0)

store :: forall parent value. FFI value => Ptr parent -> Int -> value -> IO ()
store ptr offset value = do
  let proxy = Proxy @value
  case representation proxy of
    Int -> Foreign.pokeByteOff ptr offset (fromIntegral value :: Int64)
    Float -> Foreign.pokeByteOff ptr offset (Float.toDouble value)
    Qty{} -> do
      let Qty.Qty doubleValue = value
      Foreign.pokeByteOff ptr offset doubleValue
    List -> IO.do
      let numItems = List.length value
      let itemSize = listItemSize proxy
      itemsPtr <- Foreign.Marshal.Alloc.callocBytes (numItems * itemSize)
      let storeItem index item = store itemsPtr (index * itemSize) item
      IO.forEachWithIndex value storeItem
      Foreign.pokeByteOff ptr offset (toInt64 numItems)
      Foreign.pokeByteOff ptr (offset + 8) itemsPtr
    Pair -> IO.do
      let (a, b) = value
      let (sizeA, _) = pairItemSizes proxy
      store ptr offset a
      store ptr (offset + sizeA) b
    Triple -> IO.do
      let (a, b, c) = value
      let (sizeA, sizeB, _) = tripleItemSizes proxy
      store ptr offset a
      store ptr (offset + sizeA) b
      store ptr (offset + sizeA + sizeB) c
    Maybe -> IO.do
      let tag = case value of Just _ -> 0; Nothing -> 1
      Foreign.pokeByteOff ptr offset (toInt64 tag)
      case value of
        Just actualValue -> store ptr (offset + 8) actualValue
        Nothing -> IO.succeed ()
    Result toException ->
      case value of
        Success successfulValue -> IO.do
          Foreign.pokeByteOff ptr offset (toInt64 0)
          store ptr (offset + 8) successfulValue
        Failure errorValue -> IO.do
          let exception = toException errorValue
          Foreign.pokeByteOff ptr offset (toInt64 (exceptionCode exception))
          storeException ptr (offset + 8) exception
    Class{} -> IO.do
      stablePtr <- Foreign.StablePtr.newStablePtr value
      Foreign.pokeByteOff ptr offset stablePtr

exceptionCode :: Exception -> Int
exceptionCode (Exception exception) = exceptionCodeImpl exception

exceptionCodeImpl :: forall x. Exception.Interface x => x -> Int
exceptionCodeImpl _ = Exception.code (Proxy :: Proxy x)

storeException :: Ptr parent -> Int -> Exception -> IO ()
storeException ptr offset (Exception exception) = IO.do
  exceptionPtr <- Foreign.StablePtr.newStablePtr exception
  Foreign.pokeByteOff ptr offset exceptionPtr

load :: forall parent value. FFI value => Ptr parent -> Int -> IO value
load ptr offset = do
  let proxy = Proxy @value
  case representation proxy of
    Int -> IO.map fromInt64 (Foreign.peekByteOff ptr offset)
    Float -> IO.map Float.fromDouble (Foreign.peekByteOff ptr offset)
    Qty{} -> IO.map Qty.Qty (Foreign.peekByteOff ptr offset)
    List -> IO.do
      let itemSize = listItemSize proxy
      numItems <- IO.map fromInt64 (Foreign.peekByteOff ptr offset)
      itemsPtr <- Foreign.peekByteOff ptr (offset + 8)
      let loadItem index = load itemsPtr (index * itemSize)
      IO.collect loadItem (List.range 0 (numItems - 1))
    Pair -> IO.do
      let (firstSize, _) = pairItemSizes proxy
      firstValue <- load ptr offset
      secondValue <- load ptr (offset + firstSize)
      IO.succeed (firstValue, secondValue)
    Triple -> IO.do
      let (firstSize, secondSize, _) = tripleItemSizes proxy
      firstValue <- load ptr offset
      secondValue <- load ptr (offset + firstSize)
      thirdValue <- load ptr (offset + firstSize + secondSize)
      IO.succeed (firstValue, secondValue, thirdValue)
    Maybe -> IO.do
      tag <- IO.map fromInt64 (Foreign.peekByteOff ptr offset)
      if tag == 0
        then IO.map Just (load ptr (offset + 8))
        else IO.succeed Nothing
    Result{} -> internalError "Passing Result values as FFI arguments is not supported"
    Class{} -> IO.do
      stablePtr <- Foreign.peekByteOff ptr offset
      Foreign.StablePtr.deRefStablePtr stablePtr

listItemSize :: forall item. FFI item => Proxy (List item) -> Int
listItemSize _ = size (Proxy @item)

pairItemSizes ::
  forall first second.
  (FFI first, FFI second) =>
  Proxy (first, second) ->
  (Int, Int)
pairItemSizes _ =
  ( size (Proxy @first)
  , size (Proxy @second)
  )

tripleItemSizes ::
  forall first second third.
  (FFI first, FFI second, FFI third) =>
  Proxy (first, second, third) ->
  (Int, Int, Int)
tripleItemSizes _ =
  ( size (Proxy @first)
  , size (Proxy @second)
  , size (Proxy @third)
  )

toInt64 :: Int -> Int64
toInt64 = fromIntegral

fromInt64 :: Int64 -> Int
fromInt64 = fromIntegral
