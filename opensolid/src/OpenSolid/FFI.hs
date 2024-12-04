module OpenSolid.FFI
  ( FFI (representation)
  , classId
  , nestedClassId
  , classRepresentation
  , abstractClassRepresentation
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
import Int qualified
import Length (Length)
import List qualified
import Maybe qualified
import NonEmpty qualified
import OpenSolid hiding (Type)
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
  -- A struct with a 64-bit integer size and a pointer to an array of items
  ListRep :: FFI a => Representation (List a)
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

classId :: FFI a => Proxy a -> Text -> Maybe Text -> Id a
classId proxy name maybeUnits =
  Id proxy (NonEmpty.singleton (Name.parse name)) (Maybe.map Name.parse maybeUnits)

nestedClassId :: FFI a => Proxy a -> Text -> Text -> Maybe Text -> Id a
nestedClassId proxy parentName childName maybeUnits = do
  let classNames = NonEmpty.of2 (Name.parse parentName) (Name.parse childName)
  Id proxy classNames (Maybe.map Name.parse maybeUnits)

classRepresentation :: FFI a => Text -> Maybe Text -> Proxy a -> Representation a
classRepresentation name maybeUnits proxy =
  ClassRep (classId proxy name maybeUnits)

abstractClassRepresentation :: FFI a => Text -> Proxy a -> Representation a
abstractClassRepresentation name proxy =
  classRepresentation name Nothing proxy

nestedClassRepresentation :: FFI a => Text -> Text -> Maybe Text -> Proxy a -> Representation a
nestedClassRepresentation parentName childName maybeUnits proxy =
  ClassRep (nestedClassId proxy parentName childName maybeUnits)

data Type where
  Int :: Type
  Float :: Type
  Bool :: Type
  List :: Type -> Type
  Tuple :: Type -> Type -> List Type -> Type
  Maybe :: Type -> Type
  Result :: Type -> Type
  Class :: FFI a => Id a -> Type

data Id a where
  Id :: FFI a => Proxy a -> NonEmpty Name -> Maybe Name -> Id a

typeOf :: FFI a => Proxy a -> Type
typeOf proxy = case representation proxy of
  IntRep -> Int
  FloatRep -> Float
  BoolRep -> Bool
  ListRep -> listType proxy
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
  List itemType -> "List" + typeName itemType
  Tuple type1 type2 rest -> do
    let itemTypes = type1 : type2 : rest
    let tupleSize = List.length itemTypes
    "Tuple" + Text.int tupleSize + Text.concat (List.map typeName itemTypes)
  Maybe valueType -> "Maybe" + typeName valueType
  Result valueType -> "Result" + typeName valueType
  Class id -> className id

className :: Id a -> Text
className (Id _ classNames maybeUnits) =
  Text.concat (List.map Name.pascalCase (NonEmpty.toList classNames))
    + Maybe.map Name.pascalCase maybeUnits

size :: Type -> Int
size ffiType = case ffiType of
  Int -> 8
  Float -> 8
  Bool -> 8
  List _ -> 16
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

instance FFI Length where
  representation = classRepresentation "Length" Nothing

instance FFI Angle where
  representation = classRepresentation "Angle" Nothing

instance FFI item => FFI (List item) where
  representation _ = ListRep

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
    ListRep -> IO.do
      let numItems = List.length value
      let itemSize = listItemSize proxy
      itemsPtr <- Foreign.Marshal.Alloc.callocBytes (numItems * itemSize)
      let storeItem index item = store itemsPtr (index * itemSize) item
      IO.forEachWithIndex value storeItem
      Foreign.pokeByteOff ptr offset (toInt64 numItems)
      Foreign.pokeByteOff ptr (offset + 8) itemsPtr
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
          -- Construct null-terminated string with error message
          let message = Error.message errorValue
          let messageBytes = Data.Text.Foreign.lengthWord8 message
          messagePtr <- Foreign.Marshal.Alloc.mallocBytes (messageBytes + 1)
          Data.Text.Foreign.unsafeCopyToPtr message messagePtr
          Foreign.pokeByteOff messagePtr messageBytes (fromIntegral 0 :: Word8)
          -- Set error code and store pointer to string in output
          Foreign.pokeByteOff ptr offset (toInt64 1)
          Foreign.pokeByteOff ptr (offset + 8) messagePtr
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
    ListRep -> IO.do
      let itemSize = listItemSize proxy
      numItems <- IO.map fromInt64 (Foreign.peekByteOff ptr offset)
      itemsPtr <- Foreign.peekByteOff ptr (offset + 8)
      let loadItem index = load itemsPtr (index * itemSize)
      IO.collect loadItem (List.range 0 (numItems - 1))
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
