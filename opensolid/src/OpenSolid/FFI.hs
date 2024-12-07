module OpenSolid.FFI
  ( FFI (representation)
  , classId
  , nestedClassId
  , classRepresentation
  , nestedClassRepresentation
  , Type (..)
  , Id (..)
  , typeOf
  , typeName
  , className
  , size
  , store
  , load
  , Representation
  )
where

import Colour (Colour)
import Data.ByteString.Unsafe qualified
import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Data.Text.Encoding qualified
import Data.Text.Foreign qualified
import Data.Word (Word8)
import Error qualified
import Float qualified
import Foreign (Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified
import IO qualified
import Int qualified
import Length (Length)
import List qualified
import NonEmpty qualified
import OpenSolid hiding (Type, pattern NonEmpty)
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import Text qualified

class FFI a where
  representation :: Proxy a -> Representation a

data Representation a where
  -- A 64-bit integer
  IntRep :: Representation Int
  -- A 64-bit float
  FloatRep :: Representation Float
  -- A Boolean value
  BoolRep :: Representation Bool
  -- UTF-8 text
  TextRep :: Representation Text
  -- A struct with a 64-bit integer size and a pointer to an array of items
  ListRep :: FFI a => Representation (List a)
  -- Same representation as a list, but with a runtime check for emptiness
  NonEmptyRep :: FFI a => Representation (NonEmpty a)
  -- A struct with the first item and then the second
  Tuple2Rep :: (FFI a, FFI b) => Representation (a, b)
  -- A struct with the three items in order
  Tuple3Rep :: (FFI a, FFI b, FFI c) => Representation (a, b, c)
  -- A struct with the four items in order
  Tuple4Rep :: (FFI a, FFI b, FFI c, FFI d) => Representation (a, b, c, d)
  -- A struct with the five items in order
  Tuple5Rep :: (FFI a, FFI b, FFI c, FFI d, FFI e) => Representation (a, b, c, d, e)
  -- A struct with the six items in order
  Tuple6Rep :: (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) => Representation (a, b, c, d, e, f)
  -- A struct with a 64-bit integer tag (0 = Just, 1 = Nothing)
  -- followed by the representation of the value
  MaybeRep :: FFI a => Representation (Maybe a)
  -- A struct with a 64-bit signed integer tag (0 = Success, 1+ = Failure)
  -- followed by the representation of the successful value or exception
  ResultRep :: FFI a => Representation (Result x a)
  -- A class containing an opaque pointer to a Haskell value
  ClassRep :: FFI a => Id a -> Representation a

classId :: FFI a => Proxy a -> Text -> Id a
classId proxy name =
  Id proxy (NonEmpty.singleton (Name.parse name))

nestedClassId :: FFI a => Proxy a -> Text -> Text -> Id a
nestedClassId proxy parentName childName =
  Id proxy (NonEmpty.of2 (Name.parse parentName) (Name.parse childName))

classRepresentation :: FFI a => Text -> Proxy a -> Representation a
classRepresentation name proxy =
  ClassRep (classId proxy name)

nestedClassRepresentation :: FFI a => Text -> Text -> Proxy a -> Representation a
nestedClassRepresentation parentName childName proxy =
  ClassRep (nestedClassId proxy parentName childName)

data Type where
  Int :: Type
  Float :: Type
  Bool :: Type
  Text :: Type
  List :: Type -> Type
  NonEmpty :: Type -> Type
  Tuple :: Type -> Type -> List Type -> Type
  Maybe :: Type -> Type
  Result :: Type -> Type
  Class :: FFI a => Id a -> Type

data Id a where
  Id :: FFI a => Proxy a -> NonEmpty Name -> Id a

typeOf :: FFI a => Proxy a -> Type
typeOf proxy = case representation proxy of
  IntRep -> Int
  FloatRep -> Float
  BoolRep -> Bool
  TextRep -> Text
  ListRep -> listType proxy
  NonEmptyRep -> nonEmptyType proxy
  Tuple2Rep -> tuple2Type proxy
  Tuple3Rep -> tuple3Type proxy
  Tuple4Rep -> tuple4Type proxy
  Tuple5Rep -> tuple5Type proxy
  Tuple6Rep -> tuple6Type proxy
  MaybeRep -> maybeType proxy
  ResultRep -> resultType proxy
  ClassRep id -> Class id

listType :: forall a. FFI a => Proxy (List a) -> Type
listType _ = List (typeOf @a Proxy)

nonEmptyType :: forall a. FFI a => Proxy (NonEmpty a) -> Type
nonEmptyType _ = NonEmpty (typeOf @a Proxy)

tuple2Type :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> Type
tuple2Type _ = Tuple (typeOf @a Proxy) (typeOf @b Proxy) []

tuple3Type :: forall a b c. (FFI a, FFI b, FFI c) => Proxy (a, b, c) -> Type
tuple3Type _ = Tuple (typeOf @a Proxy) (typeOf @b Proxy) [typeOf @c Proxy]

tuple4Type :: forall a b c d. (FFI a, FFI b, FFI c, FFI d) => Proxy (a, b, c, d) -> Type
tuple4Type _ = Tuple (typeOf @a Proxy) (typeOf @b Proxy) [typeOf @c Proxy, typeOf @d Proxy]

tuple5Type ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Proxy (a, b, c, d, e) ->
  Type
tuple5Type _ =
  Tuple
    (typeOf @a Proxy)
    (typeOf @b Proxy)
    [ typeOf @c Proxy
    , typeOf @d Proxy
    , typeOf @e Proxy
    ]

tuple6Type ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  Proxy (a, b, c, d, e, f) ->
  Type
tuple6Type _ =
  Tuple
    (typeOf @a Proxy)
    (typeOf @b Proxy)
    [ typeOf @c Proxy
    , typeOf @d Proxy
    , typeOf @e Proxy
    , typeOf @f Proxy
    ]

maybeType :: forall a. FFI a => Proxy (Maybe a) -> Type
maybeType _ = Maybe (typeOf @a Proxy)

resultType :: forall x a. FFI a => Proxy (Result x a) -> Type
resultType _ = Result (typeOf @a Proxy)

typeName :: Type -> Text
typeName ffiType = case ffiType of
  Int -> "Int"
  Float -> "Float"
  Bool -> "Bool"
  Text -> "Text"
  List itemType -> "List" + typeName itemType
  NonEmpty itemType -> "NonEmpty" + typeName itemType
  Tuple type1 type2 rest -> do
    let itemTypes = type1 : type2 : rest
    let tupleSize = List.length itemTypes
    "Tuple" + Text.int tupleSize + Text.concat (List.map typeName itemTypes)
  Maybe valueType -> "Maybe" + typeName valueType
  Result valueType -> "Result" + typeName valueType
  Class id -> className id

className :: Id a -> Text
className (Id _ classNames) =
  Text.concat (List.map Name.pascalCase (NonEmpty.toList classNames))

size :: Type -> Int
size ffiType = case ffiType of
  Int -> 8
  Float -> 8
  Bool -> 8
  Text -> 8
  List _ -> 16
  NonEmpty _ -> 16
  Tuple type1 type2 rest -> Int.sumOf size (type1 : type2 : rest)
  Maybe valueType -> 8 + size valueType
  Result valueType -> 16 + size valueType
  Class _ -> 8

instance FFI Float where
  representation _ = FloatRep

instance FFI Int where
  representation _ = IntRep

instance FFI Bool where
  representation _ = BoolRep

instance FFI Text where
  representation _ = TextRep

instance FFI Colour where
  representation = classRepresentation "Color"

instance FFI Length where
  representation = classRepresentation "Length"

instance FFI Angle where
  representation = classRepresentation "Angle"

instance FFI item => FFI (List item) where
  representation _ = ListRep

instance FFI item => FFI (NonEmpty item) where
  representation _ = NonEmptyRep

instance (FFI a, FFI b) => FFI (a, b) where
  representation _ = Tuple2Rep

instance (FFI a, FFI b, FFI c) => FFI (a, b, c) where
  representation _ = Tuple3Rep

instance (FFI a, FFI b, FFI c, FFI d) => FFI (a, b, c, d) where
  representation _ = Tuple4Rep

instance (FFI a, FFI b, FFI c, FFI d, FFI e) => FFI (a, b, c, d, e) where
  representation _ = Tuple5Rep

instance (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) => FFI (a, b, c, d, e, f) where
  representation _ = Tuple6Rep

instance FFI a => FFI (Maybe a) where
  representation _ = MaybeRep

instance FFI a => FFI (Result x a) where
  representation _ = ResultRep

store :: forall parent value. FFI value => Ptr parent -> Int -> value -> IO ()
store ptr offset value = do
  let proxy = Proxy @value
  case representation proxy of
    IntRep -> Foreign.pokeByteOff ptr offset (toInt64 value)
    FloatRep -> Foreign.pokeByteOff ptr offset (Float.toDouble value)
    BoolRep -> Foreign.pokeByteOff ptr offset (toInt64 (if value then 1 else 0))
    TextRep -> IO.do
      let numBytes = Data.Text.Foreign.lengthWord8 value
      contentsPtr <- Foreign.Marshal.Alloc.mallocBytes (numBytes + 1)
      Data.Text.Foreign.unsafeCopyToPtr value contentsPtr
      Foreign.pokeByteOff contentsPtr numBytes (fromIntegral 0 :: Word8)
      Foreign.pokeByteOff ptr offset contentsPtr
    ListRep -> IO.do
      let numItems = List.length value
      let itemSize = listItemSize proxy
      itemsPtr <- Foreign.Marshal.Alloc.callocBytes (numItems * itemSize)
      let storeItem index item = store itemsPtr (index * itemSize) item
      IO.forEachWithIndex value storeItem
      Foreign.pokeByteOff ptr offset (toInt64 numItems)
      Foreign.pokeByteOff ptr (offset + 8) itemsPtr
    NonEmptyRep -> store ptr offset (NonEmpty.toList value)
    Tuple2Rep -> IO.do
      let (value1, value2) = value
      let (size1, _) = tuple2ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      store ptr offset1 value1
      store ptr offset2 value2
    Tuple3Rep -> IO.do
      let (value1, value2, value3) = value
      let (size1, size2, _) = tuple3ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
    Tuple4Rep -> IO.do
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
    Tuple5Rep -> IO.do
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
    Tuple6Rep -> IO.do
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
    MaybeRep -> IO.do
      let tag = case value of Just _ -> 0; Nothing -> 1
      Foreign.pokeByteOff ptr offset (toInt64 tag)
      case value of
        Just actualValue -> store ptr (offset + 8) actualValue
        Nothing -> IO.succeed ()
    ResultRep ->
      case value of
        Success successfulValue -> IO.do
          Foreign.pokeByteOff ptr offset (toInt64 0)
          store ptr (offset + 16) successfulValue
        Failure errorValue -> IO.do
          Foreign.pokeByteOff ptr offset (toInt64 1)
          store ptr (offset + 8) (Error.message errorValue)
    ClassRep _ -> IO.do
      stablePtr <- Foreign.newStablePtr value
      Foreign.pokeByteOff ptr offset stablePtr

load :: forall parent value. FFI value => Ptr parent -> Int -> IO value
load ptr offset = do
  let proxy = Proxy @value
  case representation proxy of
    IntRep -> IO.map fromInt64 (Foreign.peekByteOff ptr offset)
    FloatRep -> IO.map Float.fromDouble (Foreign.peekByteOff ptr offset)
    BoolRep -> IO.map ((/=) 0 . fromInt64) (Foreign.peekByteOff ptr offset)
    TextRep -> IO.do
      dataPtr <- Foreign.peekByteOff ptr offset
      byteString <- Data.ByteString.Unsafe.unsafePackCString dataPtr
      IO.succeed (Data.Text.Encoding.decodeUtf8 byteString)
    ListRep -> IO.do
      let itemSize = listItemSize proxy
      numItems <- IO.map fromInt64 (Foreign.peekByteOff ptr offset)
      itemsPtr <- Foreign.peekByteOff ptr (offset + 8)
      let loadItem index = load itemsPtr (index * itemSize)
      if numItems > 0
        then IO.collect loadItem (List.range 0 (numItems - 1))
        else IO.succeed []
    NonEmptyRep -> IO.do
      list <- load ptr offset
      case list of
        [] -> internalError "Empty list passed to FFI function expecting a non-empty list"
        first : rest -> IO.succeed (first :| rest)
    Tuple2Rep -> IO.do
      let (size1, _) = tuple2ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      IO.succeed (value1, value2)
    Tuple3Rep -> IO.do
      let (size1, size2, _) = tuple3ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      value3 <- load ptr offset3
      IO.succeed (value1, value2, value3)
    Tuple4Rep -> IO.do
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
    Tuple5Rep -> IO.do
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
    Tuple6Rep -> IO.do
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
    MaybeRep -> IO.do
      tag <- IO.map fromInt64 (Foreign.peekByteOff ptr offset)
      if tag == 0
        then IO.map Just (load ptr (offset + 8))
        else IO.succeed Nothing
    ResultRep{} -> internalError "Passing Result values as FFI arguments is not supported"
    ClassRep _ -> IO.do
      stablePtr <- Foreign.peekByteOff ptr offset
      Foreign.deRefStablePtr stablePtr

listItemSize :: forall item. FFI item => Proxy (List item) -> Int
listItemSize _ = size (typeOf @item Proxy)

tuple2ItemSizes :: forall a b. (FFI a, FFI b) => Proxy (a, b) -> (Int, Int)
tuple2ItemSizes _ =
  ( size (typeOf @a Proxy)
  , size (typeOf @b Proxy)
  )

tuple3ItemSizes :: forall a b c. (FFI a, FFI b, FFI c) => Proxy (a, b, c) -> (Int, Int, Int)
tuple3ItemSizes _ =
  ( size (typeOf @a Proxy)
  , size (typeOf @b Proxy)
  , size (typeOf @c Proxy)
  )

tuple4ItemSizes ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  Proxy (a, b, c, d) ->
  (Int, Int, Int, Int)
tuple4ItemSizes _ =
  ( size (typeOf @a Proxy)
  , size (typeOf @b Proxy)
  , size (typeOf @c Proxy)
  , size (typeOf @d Proxy)
  )

tuple5ItemSizes ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  Proxy (a, b, c, d, e) ->
  (Int, Int, Int, Int, Int)
tuple5ItemSizes _ =
  ( size (typeOf @a Proxy)
  , size (typeOf @b Proxy)
  , size (typeOf @c Proxy)
  , size (typeOf @d Proxy)
  , size (typeOf @e Proxy)
  )

tuple6ItemSizes ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  Proxy (a, b, c, d, e, f) ->
  (Int, Int, Int, Int, Int, Int)
tuple6ItemSizes _ =
  ( size (typeOf @a Proxy)
  , size (typeOf @b Proxy)
  , size (typeOf @c Proxy)
  , size (typeOf @d Proxy)
  , size (typeOf @e Proxy)
  , size (typeOf @f Proxy)
  )

toInt64 :: Int -> Int64
toInt64 = fromIntegral

fromInt64 :: Int64 -> Int
fromInt64 = fromIntegral
