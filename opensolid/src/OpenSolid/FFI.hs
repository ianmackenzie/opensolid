module OpenSolid.FFI
  ( FFI (representation)
  , typeName
  , compositeName
  , size
  , store
  , load
  , Representation (..)
  )
where

import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Foreign qualified
import Data.Word (Word8)
import Error qualified
import Float qualified
import Foreign (Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified
import IO qualified
import Length (Length)
import List qualified
import OpenSolid
import Qty qualified
import Text qualified

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
  Tuple2 :: (FFI a, FFI b) => Representation (a, b)
  -- A struct with the three items in order
  Tuple3 :: (FFI a, FFI b, FFI c) => Representation (a, b, c)
  -- A struct with the four items in order
  Tuple4 :: (FFI a, FFI b, FFI c, FFI d) => Representation (a, b, c, d)
  -- A struct with the five items in order
  Tuple5 :: (FFI a, FFI b, FFI c, FFI d, FFI e) => Representation (a, b, c, d, e)
  -- A struct with the six items in order
  Tuple6 :: (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) => Representation (a, b, c, d, e, f)
  -- A struct with a 64-bit integer tag (0 = Just, 1 = Nothing)
  -- followed by the representation of the value
  Maybe :: FFI a => Representation (Maybe a)
  -- A struct with a 64-bit signed integer tag (0 = Success, 1+ = Failure)
  -- followed by the representation of the successful value or exception
  Result :: FFI a => Representation (Result x a)
  -- A class containing an opaque pointer to a Haskell value
  Class :: Text -> Representation a

typeName :: FFI a => Proxy a -> Text
typeName proxy = case representation proxy of
  Int -> "Int"
  Float -> "Float"
  Qty qtyName -> qtyName
  List -> listTypeName proxy
  Tuple2 -> tuple2TypeName proxy
  Tuple3 -> tuple3TypeName proxy
  Tuple4 -> tuple4TypeName proxy
  Tuple5 -> tuple5TypeName proxy
  Tuple6 -> tuple6TypeName proxy
  Maybe -> maybeTypeName proxy
  Result -> resultTypeName proxy
  Class className -> className

compositeName :: List Text -> Text
compositeName = Text.join "_"

listTypeName :: forall a. FFI a => Proxy (List a) -> Text
listTypeName _ = compositeName ["List", typeName @a Proxy]

tuple2TypeName :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> Text
tuple2TypeName _ = compositeName ["Tuple2", typeName @a Proxy, typeName @b Proxy]

tuple3TypeName ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  Proxy (a, b, c) ->
  Text
tuple3TypeName _ =
  compositeName
    [ "Tuple3"
    , typeName @a Proxy
    , typeName @b Proxy
    , typeName @c Proxy
    ]

tuple4TypeName ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Proxy (a, b, c, d) ->
  Text
tuple4TypeName _ =
  compositeName
    [ "Tuple4"
    , typeName @a Proxy
    , typeName @b Proxy
    , typeName @c Proxy
    , typeName @d Proxy
    ]

tuple5TypeName ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Proxy (a, b, c, d, e) ->
  Text
tuple5TypeName _ =
  compositeName
    [ "Tuple5"
    , typeName @a Proxy
    , typeName @b Proxy
    , typeName @c Proxy
    , typeName @d Proxy
    , typeName @e Proxy
    ]

tuple6TypeName ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  Proxy (a, b, c, d, e, f) ->
  Text
tuple6TypeName _ =
  compositeName
    [ "Tuple6"
    , typeName @a Proxy
    , typeName @b Proxy
    , typeName @c Proxy
    , typeName @d Proxy
    , typeName @e Proxy
    , typeName @f Proxy
    ]

maybeTypeName :: forall a. FFI a => Proxy (Maybe a) -> Text
maybeTypeName _ = compositeName ["Maybe", typeName @a Proxy]

resultTypeName :: forall x a. FFI a => Proxy (Result x a) -> Text
resultTypeName _ = compositeName ["Result", typeName @a Proxy]

size :: FFI a => Proxy a -> Int
size proxy = case representation proxy of
  Int -> 8
  Float -> 8
  Qty _ -> 8
  List -> 16
  Tuple2 -> tuple2Size proxy
  Tuple3 -> tuple3Size proxy
  Tuple4 -> tuple4Size proxy
  Tuple5 -> tuple5Size proxy
  Tuple6 -> tuple6Size proxy
  Maybe -> maybeSize proxy
  Result -> resultSize proxy
  Class _ -> 8

tuple2Size :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> Int
tuple2Size _ = do
  let size1 = size @a Proxy
  let size2 = size @b Proxy
  size1 + size2

tuple3Size :: forall a b c. (FFI a, FFI b, FFI c) => Proxy (a, b, c) -> Int
tuple3Size _ = do
  let size1 = size @a Proxy
  let size2 = size @b Proxy
  let size3 = size @c Proxy
  size1 + size2 + size3

tuple4Size :: forall a b c d. (FFI a, FFI b, FFI c, FFI d) => Proxy (a, b, c, d) -> Int
tuple4Size _ = do
  let size1 = size @a Proxy
  let size2 = size @b Proxy
  let size3 = size @c Proxy
  let size4 = size @d Proxy
  size1 + size2 + size3 + size4

tuple5Size :: forall a b c d e. (FFI a, FFI b, FFI c, FFI d, FFI e) => Proxy (a, b, c, d, e) -> Int
tuple5Size _ = do
  let size1 = size @a Proxy
  let size2 = size @b Proxy
  let size3 = size @c Proxy
  let size4 = size @d Proxy
  let size5 = size @e Proxy
  size1 + size2 + size3 + size4 + size5

tuple6Size ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  Proxy (a, b, c, d, e, f) ->
  Int
tuple6Size _ = do
  let size1 = size @a Proxy
  let size2 = size @b Proxy
  let size3 = size @c Proxy
  let size4 = size @d Proxy
  let size5 = size @e Proxy
  let size6 = size @f Proxy
  size1 + size2 + size3 + size4 + size5 + size6

maybeSize :: forall a. FFI a => Proxy (Maybe a) -> Int
maybeSize _ = 8 + size @a Proxy

resultSize :: forall x a. FFI a => Proxy (Result x a) -> Int
resultSize _ = 16 + size @a Proxy

instance FFI Float where
  representation _ = Float

instance FFI Int where
  representation _ = Int

instance FFI Length where
  representation _ = Qty "Length"

instance FFI Angle where
  representation _ = Qty "Angle"

instance FFI item => FFI (List item) where
  representation _ = List

instance (FFI a, FFI b) => FFI (a, b) where
  representation _ = Tuple2

instance (FFI a, FFI b, FFI c) => FFI (a, b, c) where
  representation _ = Tuple3

instance (FFI a, FFI b, FFI c, FFI d) => FFI (a, b, c, d) where
  representation _ = Tuple4

instance (FFI a, FFI b, FFI c, FFI d, FFI e) => FFI (a, b, c, d, e) where
  representation _ = Tuple5

instance (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) => FFI (a, b, c, d, e, f) where
  representation _ = Tuple6

instance FFI a => FFI (Maybe a) where
  representation _ = Maybe

instance FFI a => FFI (Result x a) where
  representation _ = Result

store :: forall parent value. FFI value => Ptr parent -> Int -> value -> IO ()
store ptr offset value = do
  let proxy = Proxy @value
  case representation proxy of
    Int -> Foreign.pokeByteOff ptr offset (fromIntegral value :: Int64)
    Float -> Foreign.pokeByteOff ptr offset (Float.toDouble value)
    Qty _ -> do
      let Qty.Qty double = value
      Foreign.pokeByteOff ptr offset double
    List -> IO.do
      let numItems = List.length value
      let itemSize = listItemSize proxy
      itemsPtr <- Foreign.Marshal.Alloc.callocBytes (numItems * itemSize)
      let storeItem index item = store itemsPtr (index * itemSize) item
      IO.forEachWithIndex value storeItem
      Foreign.pokeByteOff ptr offset (toInt64 numItems)
      Foreign.pokeByteOff ptr (offset + 8) itemsPtr
    Tuple2 -> IO.do
      let (value1, value2) = value
      let (size1, _) = tuple2ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      store ptr offset1 value1
      store ptr offset2 value2
    Tuple3 -> IO.do
      let (value1, value2, value3) = value
      let (size1, size2, _) = tuple3ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
    Tuple4 -> IO.do
      let (value1, value2, value3, value4) = value
      let (size1, size2, size3, _) = tuple4ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      let offset4 = offset3 + size3
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
      store ptr offset4 value4
    Tuple5 -> IO.do
      let (value1, value2, value3, value4, value5) = value
      let (size1, size2, size3, size4, _) = tuple5ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      let offset4 = offset3 + size3
      let offset5 = offset4 + size4
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
      store ptr offset4 value4
      store ptr offset5 value5
    Tuple6 -> IO.do
      let (value1, value2, value3, value4, value5, value6) = value
      let (size1, size2, size3, size4, size5, _) = tuple6ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      let offset4 = offset3 + size3
      let offset5 = offset4 + size4
      let offset6 = offset5 + size5
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
      store ptr offset4 value4
      store ptr offset5 value5
      store ptr offset6 value6
    Maybe -> IO.do
      let tag = case value of Just _ -> 0; Nothing -> 1
      Foreign.pokeByteOff ptr offset (toInt64 tag)
      case value of
        Just actualValue -> store ptr (offset + 8) actualValue
        Nothing -> IO.succeed ()
    Result ->
      case value of
        Success successfulValue -> IO.do
          Foreign.pokeByteOff ptr offset (toInt64 0)
          store ptr (offset + 16) successfulValue
        Failure errorValue -> IO.do
          -- Construct null-terminated string with error message
          let message = Error.message errorValue
          let messageBytes = Data.Text.Foreign.lengthWord8 message
          messagePtr <- Foreign.Marshal.Alloc.mallocBytes (messageBytes + 1)
          Data.Text.Foreign.unsafeCopyToPtr message messagePtr
          Foreign.pokeByteOff messagePtr messageBytes (fromIntegral 0 :: Word8)
          -- Set error code and store pointer to string in output
          Foreign.pokeByteOff ptr offset (toInt64 1)
          Foreign.pokeByteOff ptr (offset + 8) messagePtr
    Class _ -> IO.do
      stablePtr <- Foreign.newStablePtr value
      Foreign.pokeByteOff ptr offset stablePtr

load :: forall parent value. FFI value => Ptr parent -> Int -> IO value
load ptr offset = do
  let proxy = Proxy @value
  case representation proxy of
    Int -> IO.map fromInt64 (Foreign.peekByteOff ptr offset)
    Float -> IO.map Float.fromDouble (Foreign.peekByteOff ptr offset)
    Qty _ -> IO.map Qty.Qty (Foreign.peekByteOff ptr offset)
    List -> IO.do
      let itemSize = listItemSize proxy
      numItems <- IO.map fromInt64 (Foreign.peekByteOff ptr offset)
      itemsPtr <- Foreign.peekByteOff ptr (offset + 8)
      let loadItem index = load itemsPtr (index * itemSize)
      IO.collect loadItem (List.range 0 (numItems - 1))
    Tuple2 -> IO.do
      let (size1, _) = tuple2ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      IO.succeed (value1, value2)
    Tuple3 -> IO.do
      let (size1, size2, _) = tuple3ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      value3 <- load ptr offset3
      IO.succeed (value1, value2, value3)
    Tuple4 -> IO.do
      let (size1, size2, size3, _) = tuple4ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      let offset4 = offset3 + size3
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      value3 <- load ptr offset3
      value4 <- load ptr offset4
      IO.succeed (value1, value2, value3, value4)
    Tuple5 -> IO.do
      let (size1, size2, size3, size4, _) = tuple5ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      let offset4 = offset3 + size3
      let offset5 = offset4 + size4
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      value3 <- load ptr offset3
      value4 <- load ptr offset4
      value5 <- load ptr offset5
      IO.succeed (value1, value2, value3, value4, value5)
    Tuple6 -> IO.do
      let (size1, size2, size3, size4, size5, _) = tuple6ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      let offset4 = offset3 + size3
      let offset5 = offset4 + size4
      let offset6 = offset5 + size5
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      value3 <- load ptr offset3
      value4 <- load ptr offset4
      value5 <- load ptr offset5
      value6 <- load ptr offset6
      IO.succeed (value1, value2, value3, value4, value5, value6)
    Maybe -> IO.do
      tag <- IO.map fromInt64 (Foreign.peekByteOff ptr offset)
      if tag == 0
        then IO.map Just (load ptr (offset + 8))
        else IO.succeed Nothing
    Result{} -> internalError "Passing Result values as FFI arguments is not supported"
    Class _ -> IO.do
      stablePtr <- Foreign.peekByteOff ptr offset
      Foreign.deRefStablePtr stablePtr

listItemSize :: forall item. FFI item => Proxy (List item) -> Int
listItemSize _ = size (Proxy @item)

tuple2ItemSizes :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> (Int, Int)
tuple2ItemSizes _ =
  ( size @a Proxy
  , size @b Proxy
  )

tuple3ItemSizes :: forall a b c. (FFI a, FFI b, FFI c) => Proxy (a, b, c) -> (Int, Int, Int)
tuple3ItemSizes _ =
  ( size @a Proxy
  , size @b Proxy
  , size @c Proxy
  )

tuple4ItemSizes ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Proxy (a, b, c, d) ->
  (Int, Int, Int, Int)
tuple4ItemSizes _ =
  ( size @a Proxy
  , size @b Proxy
  , size @c Proxy
  , size @d Proxy
  )

tuple5ItemSizes ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Proxy (a, b, c, d, e) ->
  (Int, Int, Int, Int, Int)
tuple5ItemSizes _ =
  ( size @a Proxy
  , size @b Proxy
  , size @c Proxy
  , size @d Proxy
  , size @e Proxy
  )

tuple6ItemSizes ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  Proxy (a, b, c, d, e, f) ->
  (Int, Int, Int, Int, Int, Int)
tuple6ItemSizes _ =
  ( size @a Proxy
  , size @b Proxy
  , size @c Proxy
  , size @d Proxy
  , size @e Proxy
  , size @f Proxy
  )

toInt64 :: Int -> Int64
toInt64 = fromIntegral

fromInt64 :: Int64 -> Int
fromInt64 = fromIntegral
