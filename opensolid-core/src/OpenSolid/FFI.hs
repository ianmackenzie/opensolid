{-# LANGUAGE NoFieldSelectors #-}

module OpenSolid.FFI
  ( FFI (representation)
  , Space
  , Coordinates
  , Name
  , ClassName
  , name
  , pascalCase
  , camelCase
  , snakeCase
  , classRepresentation
  , nestedClassRepresentation
  , staticClassName
  , Type (..)
  , typeOf
  , className
  , typeName
  , qualifiedName
  , unqualifiedName
  , concatenatedName
  , size
  , store
  , load
  , Representation
  , argumentName
  , splitCamelCase
  )
where

import Data.ByteString.Unsafe qualified
import Data.Char qualified
import Data.Coerce (Coercible)
import Data.Coerce qualified
import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Data.Text qualified
import Data.Text.Encoding qualified
import Data.Text.Foreign qualified
import Data.Word (Word8)
import Foreign (Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified
import GHC.TypeLits (KnownSymbol)
import GHC.TypeLits qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Area (Area)
import OpenSolid.Array (Array)
import OpenSolid.Array qualified as Array
import OpenSolid.Color (Color)
import OpenSolid.IO qualified as IO
import OpenSolid.Int qualified as Int
import OpenSolid.InternalError (InternalError (InternalError))
import OpenSolid.Length (Length)
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Prelude hiding (Type, pattern NonEmpty)
import OpenSolid.Text qualified as Text

class FFI a where
  representation :: Proxy a -> Representation a

data Space deriving (Eq, Show)

type Coordinates = Space @ Meters

newtype Name = Name (NonEmpty Text) deriving (Eq, Ord, Show)

newtype ClassName = ClassName (NonEmpty Text)

name :: Text -> Name
name input =
  case Text.split " " input of
    first : rest ->
      if NonEmpty.allSatisfy isCapitalized (first :| rest)
        then Name (first :| rest)
        else throw (InternalError ("API name has non-capitalized component: " <> input))
    _ -> throw (InternalError "Text.split should always return at least one component")

isCapitalized :: Text -> Bool
isCapitalized component = Text.capitalize component == component

pascalCase :: Name -> Text
pascalCase (Name components) = Text.concat (NonEmpty.toList components)

camelCase :: Name -> Text
camelCase (Name (first :| rest)) = Text.toLower first <> Text.concat rest

snakeCase :: Name -> Text
snakeCase (Name components) = Text.join "_" (List.map Text.toLower (NonEmpty.toList components))

data Representation a where
  -- The unit value, always encoded as a 64-bit signed integer zero
  UnitRep :: Representation ()
  -- A 64-bit integer
  IntRep :: Representation Int
  -- A 64-bit float
  NumberRep :: Representation Number
  -- A Boolean value
  BoolRep :: Representation Bool
  -- A Sign value
  SignRep :: Representation Sign
  -- UTF-8 text
  TextRep :: Representation Text
  -- A struct with a 64-bit integer size and a pointer to an array of items
  ListRep :: FFI a => Representation (List a)
  -- Same representation as a list, but with a runtime check for emptiness
  NonEmptyRep :: FFI a => Representation (NonEmpty a)
  -- Same representation as a list, but with a runtime check for emptiness
  ArrayRep :: FFI a => Representation (Array a)
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
  -- A struct with the seven items in order
  Tuple7Rep :: (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g) => Representation (a, b, c, d, e, f, g)
  -- A struct with the eight items in order
  Tuple8Rep :: (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g, FFI h) => Representation (a, b, c, d, e, f, g, h)
  -- A struct with a 64-bit integer tag (0 = Just, 1 = Nothing)
  -- followed by the representation of the value
  MaybeRep :: FFI a => Representation (Maybe a)
  -- A struct with a 64-bit signed integer tag (0 = Ok, 1+ = Error)
  -- followed by the representation of the successful value or exception
  ResultRep :: FFI a => Representation (Result x a)
  -- A class containing an opaque pointer to a Haskell value
  ClassRep :: FFI a => ClassName -> Representation a
  -- Some IO that returns a representable value
  IORep :: FFI a => Representation (IO a)
  -- A function argument that should be named-only if supported
  NamedArgumentRep :: (FFI a, Coercible n a) => Name -> Proxy a -> Representation n

classRepresentation :: FFI a => Text -> Proxy a -> Representation a
classRepresentation givenName _ =
  ClassRep (ClassName (NonEmpty.one givenName))

nestedClassRepresentation :: FFI a => Text -> Text -> Proxy a -> Representation a
nestedClassRepresentation outerName innerName _ =
  ClassRep (ClassName (NonEmpty.two outerName innerName))

staticClassName :: Text -> ClassName
staticClassName givenName = ClassName (NonEmpty.one givenName)

data Type where
  Unit :: Type
  Int :: Type
  Number :: Type
  Bool :: Type
  Sign :: Type
  Text :: Type
  List :: Type -> Type
  NonEmpty :: Type -> Type
  Array :: Type -> Type
  Tuple :: Type -> Type -> List Type -> Type
  Maybe :: Type -> Type
  Result :: Type -> Type
  Class :: ClassName -> Type

className :: FFI a => Proxy a -> ClassName
className proxy = case representation proxy of
  ClassRep className_ -> className_
  _ -> throw (InternalError "Attempting to get the class name of a non-class type")

typeOf :: FFI a => Proxy a -> Type
typeOf proxy = case representation proxy of
  UnitRep -> Unit
  IntRep -> Int
  NumberRep -> Number
  BoolRep -> Bool
  SignRep -> Sign
  TextRep -> Text
  ListRep -> listType proxy
  NonEmptyRep -> nonEmptyType proxy
  ArrayRep -> arrayType proxy
  Tuple2Rep -> tuple2Type proxy
  Tuple3Rep -> tuple3Type proxy
  Tuple4Rep -> tuple4Type proxy
  Tuple5Rep -> tuple5Type proxy
  Tuple6Rep -> tuple6Type proxy
  Tuple7Rep -> tuple7Type proxy
  Tuple8Rep -> tuple8Type proxy
  MaybeRep -> maybeType proxy
  ResultRep -> resultType proxy
  ClassRep class_ -> Class class_
  IORep -> ioResultType proxy
  NamedArgumentRep _ innerProxy -> typeOf innerProxy

listType :: forall a. FFI a => Proxy (List a) -> Type
listType _ = List (typeOf @a Proxy)

nonEmptyType :: forall a. FFI a => Proxy (NonEmpty a) -> Type
nonEmptyType _ = NonEmpty (typeOf @a Proxy)

arrayType :: forall a. FFI a => Proxy (Array a) -> Type
arrayType _ = Array (typeOf @a Proxy)

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

tuple7Type ::
  forall a b c d e f g.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g) =>
  Proxy (a, b, c, d, e, f, g) ->
  Type
tuple7Type _ =
  Tuple
    (typeOf @a Proxy)
    (typeOf @b Proxy)
    [ typeOf @c Proxy
    , typeOf @d Proxy
    , typeOf @e Proxy
    , typeOf @f Proxy
    , typeOf @g Proxy
    ]

tuple8Type ::
  forall a b c d e f g h.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g, FFI h) =>
  Proxy (a, b, c, d, e, f, g, h) ->
  Type
tuple8Type _ =
  Tuple
    (typeOf @a Proxy)
    (typeOf @b Proxy)
    [ typeOf @c Proxy
    , typeOf @d Proxy
    , typeOf @e Proxy
    , typeOf @f Proxy
    , typeOf @g Proxy
    , typeOf @h Proxy
    ]

maybeType :: forall a. FFI a => Proxy (Maybe a) -> Type
maybeType _ = Maybe (typeOf @a Proxy)

resultType :: forall x a. FFI a => Proxy (Result x a) -> Type
resultType _ = Result (typeOf @a Proxy)

ioResultType :: forall a. FFI a => Proxy (IO a) -> Type
ioResultType _ = Result (typeOf @a Proxy)

typeName :: Type -> Text
typeName ffiType = case ffiType of
  Unit -> "Unit"
  Int -> "Int"
  Number -> "Number"
  Bool -> "Bool"
  Sign -> "Sign"
  Text -> "Text"
  List itemType -> "List" <> typeName itemType
  NonEmpty itemType -> "NonEmpty" <> typeName itemType
  Array itemType -> "Array" <> typeName itemType
  Tuple type1 type2 rest -> do
    let itemTypes = type1 : type2 : rest
    let tupleSize = List.length itemTypes
    "Tuple" <> Text.int tupleSize <> Text.concat (List.map typeName itemTypes)
  Maybe valueType -> "Maybe" <> typeName valueType
  Result valueType -> "Result" <> typeName valueType
  Class class_ -> concatenatedName class_

concatenatedName :: ClassName -> Text
concatenatedName = qualifiedName ""

qualifiedName :: Text -> ClassName -> Text
qualifiedName separator (ClassName components) = Text.join separator (NonEmpty.toList components)

unqualifiedName :: ClassName -> Text
unqualifiedName (ClassName components) = NonEmpty.last components

size :: Type -> Int
size ffiType = case ffiType of
  Unit -> 8
  Int -> 8
  Number -> 8
  Bool -> 8
  Sign -> 8
  Text -> 8
  List _ -> 16
  NonEmpty _ -> 16
  Array _ -> 16
  Tuple type1 type2 rest -> Int.sumOf size (type1 : type2 : rest)
  Maybe valueType -> 8 + size valueType
  Result valueType -> 16 + size valueType
  Class _ -> 8

instance FFI () where
  representation _ = UnitRep

instance FFI Number where
  representation _ = NumberRep

instance FFI Int where
  representation _ = IntRep

instance FFI Bool where
  representation _ = BoolRep

instance FFI Sign where
  representation _ = SignRep

instance FFI Text where
  representation _ = TextRep

instance FFI Color where
  representation = classRepresentation "Color"

instance FFI Length where
  representation = classRepresentation "Length"

instance FFI Area where
  representation = classRepresentation "Area"

instance FFI Angle where
  representation = classRepresentation "Angle"

instance (KnownSymbol name, FFI a) => FFI (name ::: a) where
  representation _ = do
    let camelCaseName = Text.pack (GHC.TypeLits.symbolVal @name Proxy)
    NamedArgumentRep (splitCamelCase camelCaseName) (Proxy @a)

splitCamelCase :: Text -> Name
splitCamelCase text = do
  let (first, rest) = Data.Text.span Data.Char.isLower text
  Name (Text.capitalize first :| splitPascalCase rest)

splitPascalCase :: Text -> List Text
splitPascalCase text = case Data.Text.uncons text of
  Nothing -> []
  Just (firstHead, rest) -> do
    let (firstTail, following) = Data.Text.span Data.Char.isLower rest
    Data.Text.cons firstHead firstTail : splitPascalCase following

instance FFI item => FFI (List item) where
  representation _ = ListRep

instance FFI item => FFI (NonEmpty item) where
  representation _ = NonEmptyRep

instance FFI item => FFI (Array item) where
  representation _ = ArrayRep

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

instance (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g) => FFI (a, b, c, d, e, f, g) where
  representation _ = Tuple7Rep

instance (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g, FFI h) => FFI (a, b, c, d, e, f, g, h) where
  representation _ = Tuple8Rep

instance FFI a => FFI (Maybe a) where
  representation _ = MaybeRep

instance FFI a => FFI (Result x a) where
  representation _ = ResultRep

instance FFI a => FFI (IO a) where
  representation _ = IORep

store :: forall parent value. FFI value => Ptr parent -> Int -> value -> IO ()
store ptr offset value = do
  let proxy = Proxy @value
  case representation proxy of
    UnitRep -> Foreign.pokeByteOff @Int64 ptr offset 0
    IntRep -> Foreign.pokeByteOff @Int64 ptr offset (fromIntegral value)
    NumberRep -> Foreign.pokeByteOff ptr offset (Number.toDouble value)
    BoolRep -> Foreign.pokeByteOff @Int64 ptr offset (if value then 1 else 0)
    SignRep -> Foreign.pokeByteOff @Int64 ptr offset (case value of Positive -> 1; Negative -> -1)
    TextRep -> do
      let numBytes = Data.Text.Foreign.lengthWord8 value
      contentsPtr <- Foreign.Marshal.Alloc.mallocBytes (numBytes + 1)
      Data.Text.Foreign.unsafeCopyToPtr value contentsPtr
      Foreign.pokeByteOff contentsPtr numBytes (0 :: Word8)
      Foreign.pokeByteOff ptr offset contentsPtr
    ListRep -> do
      let numItems = List.length value
      let itemSize = listItemSize proxy
      itemsPtr <- Foreign.Marshal.Alloc.callocBytes (numItems * itemSize)
      let storeItem index item = store itemsPtr (index * itemSize) item
      IO.forEachWithIndex value storeItem
      Foreign.pokeByteOff @Int64 ptr offset (fromIntegral numItems)
      Foreign.pokeByteOff ptr (offset + 8) itemsPtr
    NonEmptyRep -> store ptr offset (NonEmpty.toList value)
    ArrayRep -> store ptr offset (Array.toList value)
    Tuple2Rep -> do
      let (value1, value2) = value
      let (size1, _) = tuple2ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      store ptr offset1 value1
      store ptr offset2 value2
    Tuple3Rep -> do
      let (value1, value2, value3) = value
      let (size1, size2, _) = tuple3ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
    Tuple4Rep -> do
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
    Tuple5Rep -> do
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
    Tuple6Rep -> do
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
    Tuple7Rep -> do
      let (value1, value2, value3, value4, value5, value6, value7) = value
      let (size1, size2, size3, size4, size5, size6, _) = tuple7ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      let offset4 = offset3 + size3
      let offset5 = offset4 + size4
      let offset6 = offset5 + size5
      let offset7 = offset6 + size6
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
      store ptr offset4 value4
      store ptr offset5 value5
      store ptr offset6 value6
      store ptr offset7 value7
    Tuple8Rep -> do
      let (value1, value2, value3, value4, value5, value6, value7, value8) = value
      let (size1, size2, size3, size4, size5, size6, size7, _) = tuple8ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      let offset4 = offset3 + size3
      let offset5 = offset4 + size4
      let offset6 = offset5 + size5
      let offset7 = offset6 + size6
      let offset8 = offset7 + size7
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
      store ptr offset4 value4
      store ptr offset5 value5
      store ptr offset6 value6
      store ptr offset7 value7
      store ptr offset8 value8
    MaybeRep -> do
      let tag = case value of Just{} -> 0; Nothing -> 1
      Foreign.pokeByteOff @Int64 ptr offset tag
      IO.forEach value (store ptr (offset + 8))
    ResultRep ->
      case value of
        Ok successfulValue -> do
          Foreign.pokeByteOff @Int64 ptr offset 0
          store ptr (offset + 16) successfulValue
        Error errorValue -> do
          Foreign.pokeByteOff @Int64 ptr offset 1
          store ptr (offset + 8) (Text.show errorValue)
    ClassRep _ -> do
      stablePtr <- Foreign.newStablePtr value
      Foreign.pokeByteOff ptr offset stablePtr
    IORep -> do
      result <- IO.attempt value
      store ptr offset result
    NamedArgumentRep{} ->
      throw (InternalError "Should never have a named argument as a Haskell return type")

load :: forall parent value. FFI value => Ptr parent -> Int -> IO value
load ptr offset = do
  let proxy = Proxy @value
  case representation proxy of
    UnitRep -> IO.succeed ()
    IntRep -> IO.map fromIntegral (Foreign.peekByteOff @Int64 ptr offset)
    NumberRep -> IO.map Number.fromDouble (Foreign.peekByteOff ptr offset)
    BoolRep -> IO.map (/= 0) (Foreign.peekByteOff @Int64 ptr offset)
    SignRep -> IO.map Int.sign (load ptr offset)
    TextRep -> do
      dataPtr <- Foreign.peekByteOff ptr offset
      byteString <- Data.ByteString.Unsafe.unsafePackCString dataPtr
      IO.succeed (Data.Text.Encoding.decodeUtf8 byteString)
    ListRep -> do
      let itemSize = listItemSize proxy
      numItems <- IO.map fromIntegral (Foreign.peekByteOff @Int64 ptr offset)
      itemsPtr <- Foreign.peekByteOff ptr (offset + 8)
      let loadItem index = load itemsPtr (index * itemSize)
      IO.collect loadItem [0 .. numItems - 1]
    NonEmptyRep -> do
      list <- load ptr offset
      case list of
        [] -> IO.fail "Empty list passed to FFI function expecting a non-empty list"
        first : rest -> IO.succeed (first :| rest)
    ArrayRep -> IO.map Array.fromList (load ptr offset)
    Tuple2Rep -> do
      let (size1, _) = tuple2ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      IO.succeed (value1, value2)
    Tuple3Rep -> do
      let (size1, size2, _) = tuple3ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      value3 <- load ptr offset3
      IO.succeed (value1, value2, value3)
    Tuple4Rep -> do
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
    Tuple5Rep -> do
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
    Tuple6Rep -> do
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
    Tuple7Rep -> do
      let (size1, size2, size3, size4, size5, size6, _) = tuple7ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      let offset4 = offset3 + size3
      let offset5 = offset4 + size4
      let offset6 = offset5 + size5
      let offset7 = offset6 + size6
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      value3 <- load ptr offset3
      value4 <- load ptr offset4
      value5 <- load ptr offset5
      value6 <- load ptr offset6
      value7 <- load ptr offset7
      IO.succeed (value1, value2, value3, value4, value5, value6, value7)
    Tuple8Rep -> do
      let (size1, size2, size3, size4, size5, size6, size7, _) = tuple8ItemSizes proxy
      let offset1 = offset
      let offset2 = offset1 + size1
      let offset3 = offset2 + size2
      let offset4 = offset3 + size3
      let offset5 = offset4 + size4
      let offset6 = offset5 + size5
      let offset7 = offset6 + size6
      let offset8 = offset7 + size7
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      value3 <- load ptr offset3
      value4 <- load ptr offset4
      value5 <- load ptr offset5
      value6 <- load ptr offset6
      value7 <- load ptr offset7
      value8 <- load ptr offset8
      IO.succeed (value1, value2, value3, value4, value5, value6, value7, value8)
    MaybeRep -> do
      tag <- Foreign.peekByteOff @Int64 ptr offset
      if tag == 0
        then IO.map Just (load ptr (offset + 8))
        else IO.succeed Nothing
    ResultRep{} -> throw (InternalError "Passing Result values as FFI arguments is not supported")
    ClassRep _ -> do
      stablePtr <- Foreign.peekByteOff ptr offset
      Foreign.deRefStablePtr stablePtr
    IORep -> throw (InternalError "Passing IO values as FFI arguments is not supported")
    NamedArgumentRep _ innerProxy -> IO.map (wrapNamedArgument innerProxy) (load ptr offset)

wrapNamedArgument :: Coercible n a => Proxy a -> a -> n
wrapNamedArgument _ = Data.Coerce.coerce

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

tuple7ItemSizes ::
  forall a b c d e f g.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g) =>
  Proxy (a, b, c, d, e, f, g) ->
  (Int, Int, Int, Int, Int, Int, Int)
tuple7ItemSizes _ =
  ( size (typeOf @a Proxy)
  , size (typeOf @b Proxy)
  , size (typeOf @c Proxy)
  , size (typeOf @d Proxy)
  , size (typeOf @e Proxy)
  , size (typeOf @f Proxy)
  , size (typeOf @g Proxy)
  )

tuple8ItemSizes ::
  forall a b c d e f g h.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g, FFI h) =>
  Proxy (a, b, c, d, e, f, g, h) ->
  (Int, Int, Int, Int, Int, Int, Int, Int)
tuple8ItemSizes _ =
  ( size (typeOf @a Proxy)
  , size (typeOf @b Proxy)
  , size (typeOf @c Proxy)
  , size (typeOf @d Proxy)
  , size (typeOf @e Proxy)
  , size (typeOf @f Proxy)
  , size (typeOf @g Proxy)
  , size (typeOf @h Proxy)
  )

argumentName :: FFI a => Proxy a -> Maybe Name
argumentName proxy = case representation proxy of
  NamedArgumentRep argName _ -> Just argName
  _ -> Nothing
