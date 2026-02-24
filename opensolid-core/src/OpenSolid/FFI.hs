module OpenSolid.FFI
  ( FFI (representation)
  , Space
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
import OpenSolid.Prelude hiding (Type, pattern NonEmpty, pattern Sign)
import OpenSolid.Text qualified as Text

class FFI a where
  representation :: Proxy a -> Representation a

data Space deriving (Eq, Show)

newtype Name = Name (NonEmpty Text) deriving (Eq, Ord, Show)

newtype ClassName = ClassName (NonEmpty Text)

name :: Text -> Name
name input =
  case Text.split " " input of
    first : rest ->
      if NonEmpty.all isCapitalized (first :| rest)
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
  Tuple2Rep ::
    (FFI a, FFI b) =>
    Representation (a, b)
  -- A struct with the three items in order
  Tuple3Rep ::
    (FFI a, FFI b, FFI c) =>
    Representation (a, b, c)
  -- A struct with the four items in order
  Tuple4Rep ::
    (FFI a, FFI b, FFI c, FFI d) =>
    Representation (a, b, c, d)
  -- A struct with the five items in order
  Tuple5Rep ::
    (FFI a, FFI b, FFI c, FFI d, FFI e) =>
    Representation (a, b, c, d, e)
  -- A struct with the six items in order
  Tuple6Rep ::
    (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
    Representation (a, b, c, d, e, f)
  -- A struct with the seven items in order
  Tuple7Rep ::
    (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g) =>
    Representation (a, b, c, d, e, f, g)
  -- A struct with the eight items in order
  Tuple8Rep ::
    (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g, FFI h) =>
    Representation (a, b, c, d, e, f, g, h)
  -- A struct with a 64-bit integer tag (0 = Just, 1 = Nothing)
  -- followed by the representation of the value
  MaybeRep :: FFI a => Representation (Maybe a)
  -- A struct with a 64-bit signed integer tag (0 = Ok, 1+ = Error)
  -- followed by the representation of the successful value or exception
  ResultRep :: forall x a. FFI a => Representation (Result x a)
  -- A class containing an opaque pointer to a Haskell value
  ClassRep :: FFI a => ClassName -> Representation a
  -- Some IO that returns a representable value
  IORep :: FFI a => Representation (IO a)
  -- A function argument that should be named-only if supported
  NamedArgumentRep :: (KnownSymbol name, FFI a) => Representation (name ::: a)

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

className :: forall t -> FFI t => ClassName
className t = case representation (Proxy @t) of
  ClassRep className_ -> className_
  _ -> throw (InternalError "Attempting to get the class name of a non-class type")

typeOf :: forall t -> FFI t => Type
typeOf t = case representation (Proxy @t) of
  UnitRep -> Unit
  IntRep -> Int
  NumberRep -> Number
  BoolRep -> Bool
  SignRep -> Sign
  TextRep -> Text
  ListRep @a -> List (typeOf a)
  NonEmptyRep @a -> NonEmpty (typeOf a)
  ArrayRep @a -> Array (typeOf a)
  Tuple2Rep @a @b ->
    Tuple (typeOf a) (typeOf b) []
  Tuple3Rep @a @b @c ->
    Tuple (typeOf a) (typeOf b) [typeOf c]
  Tuple4Rep @a @b @c @d ->
    Tuple (typeOf a) (typeOf b) [typeOf c, typeOf d]
  Tuple5Rep @a @b @c @d @e ->
    Tuple (typeOf a) (typeOf b) [typeOf c, typeOf d, typeOf e]
  Tuple6Rep @a @b @c @d @e @f ->
    Tuple (typeOf a) (typeOf b) [typeOf c, typeOf d, typeOf e, typeOf f]
  Tuple7Rep @a @b @c @d @e @f @g ->
    Tuple (typeOf a) (typeOf b) [typeOf c, typeOf d, typeOf e, typeOf f, typeOf g]
  Tuple8Rep @a @b @c @d @e @f @g @h ->
    Tuple (typeOf a) (typeOf b) [typeOf c, typeOf d, typeOf e, typeOf f, typeOf g, typeOf h]
  MaybeRep @a -> Maybe (typeOf a)
  ResultRep @_x @a -> Result (typeOf a)
  ClassRep class_ -> Class class_
  IORep @a -> Result (typeOf a)
  NamedArgumentRep @_name @a -> typeOf a

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

sizeOf :: forall t -> FFI t => Int
sizeOf t = size (typeOf t)

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

instance forall name a. (KnownSymbol name, FFI a) => FFI (name ::: a) where
  representation _ = NamedArgumentRep

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

instance
  (FFI a, FFI b) =>
  FFI (a, b)
  where
  representation _ = Tuple2Rep

instance
  (FFI a, FFI b, FFI c) =>
  FFI (a, b, c)
  where
  representation _ = Tuple3Rep

instance
  (FFI a, FFI b, FFI c, FFI d) =>
  FFI (a, b, c, d)
  where
  representation _ = Tuple4Rep

instance
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  FFI (a, b, c, d, e)
  where
  representation _ = Tuple5Rep

instance
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  FFI (a, b, c, d, e, f)
  where
  representation _ = Tuple6Rep

instance
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g) =>
  FFI (a, b, c, d, e, f, g)
  where
  representation _ = Tuple7Rep

instance
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f, FFI g, FFI h) =>
  FFI (a, b, c, d, e, f, g, h)
  where
  representation _ = Tuple8Rep

instance FFI a => FFI (Maybe a) where
  representation _ = MaybeRep

instance FFI a => FFI (Result x a) where
  representation _ = ResultRep

instance FFI a => FFI (IO a) where
  representation _ = IORep

store :: forall value parent. FFI value => Ptr parent -> Int -> value -> IO ()
store ptr offset value = do
  let proxy = Proxy @value
  case representation proxy of
    UnitRep -> store @Int ptr offset 0
    IntRep -> Foreign.pokeByteOff @Int64 ptr offset (fromIntegral value)
    NumberRep -> Foreign.pokeByteOff ptr offset (Number.toDouble value)
    BoolRep -> store @Int ptr offset (if value then 1 else 0)
    SignRep -> store @Int ptr offset (case value of Positive -> 1; Negative -> -1)
    TextRep -> do
      let numBytes = Data.Text.Foreign.lengthWord8 value
      contentsPtr <- Foreign.Marshal.Alloc.mallocBytes (numBytes + 1)
      Data.Text.Foreign.unsafeCopyToPtr value contentsPtr
      Foreign.pokeByteOff contentsPtr numBytes (fromIntegral 0 :: Word8)
      Foreign.pokeByteOff ptr offset contentsPtr
    ListRep @item -> do
      let numItems = List.length value
      let itemSize = sizeOf item
      itemsPtr <- Foreign.Marshal.Alloc.callocBytes (numItems * itemSize)
      let storeItem index item = store itemsPtr (index * itemSize) item
      IO.forEachWithIndex value storeItem
      store @Int ptr offset numItems
      Foreign.pokeByteOff ptr (offset + 8) itemsPtr
    NonEmptyRep -> store ptr offset (NonEmpty.toList value)
    ArrayRep -> store ptr offset (Array.toList value)
    Tuple2Rep @a @_ -> do
      let (value1, value2) = value
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      store ptr offset1 value1
      store ptr offset2 value2
    Tuple3Rep @a @b @_ -> do
      let (value1, value2, value3) = value
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      let offset3 = offset2 + sizeOf b
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
    Tuple4Rep @a @b @c @_ -> do
      let (value1, value2, value3, value4) = value
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      let offset3 = offset2 + sizeOf b
      let offset4 = offset3 + sizeOf c
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
      store ptr offset4 value4
    Tuple5Rep @a @b @c @d @_ -> do
      let (value1, value2, value3, value4, value5) = value
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      let offset3 = offset2 + sizeOf b
      let offset4 = offset3 + sizeOf c
      let offset5 = offset4 + sizeOf d
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
      store ptr offset4 value4
      store ptr offset5 value5
    Tuple6Rep @a @b @c @d @e @_ -> do
      let (value1, value2, value3, value4, value5, value6) = value
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      let offset3 = offset2 + sizeOf b
      let offset4 = offset3 + sizeOf c
      let offset5 = offset4 + sizeOf d
      let offset6 = offset5 + sizeOf e
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
      store ptr offset4 value4
      store ptr offset5 value5
      store ptr offset6 value6
    Tuple7Rep @a @b @c @d @e @f @_ -> do
      let (value1, value2, value3, value4, value5, value6, value7) = value
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      let offset3 = offset2 + sizeOf b
      let offset4 = offset3 + sizeOf c
      let offset5 = offset4 + sizeOf d
      let offset6 = offset5 + sizeOf e
      let offset7 = offset6 + sizeOf f
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
      store ptr offset4 value4
      store ptr offset5 value5
      store ptr offset6 value6
      store ptr offset7 value7
    Tuple8Rep @a @b @c @d @e @f @g @_ -> do
      let (value1, value2, value3, value4, value5, value6, value7, value8) = value
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      let offset3 = offset2 + sizeOf b
      let offset4 = offset3 + sizeOf c
      let offset5 = offset4 + sizeOf d
      let offset6 = offset5 + sizeOf e
      let offset7 = offset6 + sizeOf f
      let offset8 = offset7 + sizeOf g
      store ptr offset1 value1
      store ptr offset2 value2
      store ptr offset3 value3
      store ptr offset4 value4
      store ptr offset5 value5
      store ptr offset6 value6
      store ptr offset7 value7
      store ptr offset8 value8
    MaybeRep ->
      case value of
        Just item -> do
          store @Int ptr offset 0
          store ptr (offset + 8) item
        Nothing ->
          store @Int ptr offset 1
    ResultRep ->
      case value of
        Ok successfulValue -> do
          store @Int ptr offset 0
          store ptr (offset + 16) successfulValue
        Error errorValue -> do
          store @Int ptr offset 1
          store ptr (offset + 8) (Text.show errorValue)
    ClassRep _ -> do
      stablePtr <- Foreign.newStablePtr value
      Foreign.pokeByteOff ptr offset stablePtr
    IORep -> do
      result <- IO.attempt value
      store ptr offset result
    NamedArgumentRep{} ->
      throw (InternalError "Should never have a named argument as a Haskell return type")

load :: forall value parent. FFI value => Ptr parent -> Int -> IO value
load ptr offset = do
  let proxy = Proxy @value
  case representation proxy of
    UnitRep -> IO.succeed ()
    IntRep -> IO.map fromIntegral (Foreign.peekByteOff @Int64 ptr offset)
    NumberRep -> IO.map Number.fromDouble (Foreign.peekByteOff ptr offset)
    BoolRep -> IO.map (/= 0) (load @Int ptr offset)
    SignRep -> IO.map Int.sign (load @Int ptr offset)
    TextRep -> do
      dataPtr <- Foreign.peekByteOff ptr offset
      byteString <- Data.ByteString.Unsafe.unsafePackCString dataPtr
      IO.succeed (Data.Text.Encoding.decodeUtf8 byteString)
    ListRep @item -> do
      let itemSize = sizeOf item
      numItems <- load @Int ptr offset
      itemsPtr <- Foreign.peekByteOff ptr (offset + 8)
      let loadItem index = load itemsPtr (index * itemSize)
      IO.collect loadItem [0 .. numItems - 1]
    NonEmptyRep -> do
      list <- load ptr offset
      case list of
        [] -> IO.fail "Empty list passed to FFI function expecting a non-empty list"
        first : rest -> IO.succeed (first :| rest)
    ArrayRep -> IO.map Array.fromList (load ptr offset)
    Tuple2Rep @a @_ -> do
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      IO.succeed (value1, value2)
    Tuple3Rep @a @b @_ -> do
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      let offset3 = offset2 + sizeOf b
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      value3 <- load ptr offset3
      IO.succeed (value1, value2, value3)
    Tuple4Rep @a @b @c @_ -> do
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      let offset3 = offset2 + sizeOf b
      let offset4 = offset3 + sizeOf c
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      value3 <- load ptr offset3
      value4 <- load ptr offset4
      IO.succeed (value1, value2, value3, value4)
    Tuple5Rep @a @b @c @d @_ -> do
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      let offset3 = offset2 + sizeOf b
      let offset4 = offset3 + sizeOf c
      let offset5 = offset4 + sizeOf d
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      value3 <- load ptr offset3
      value4 <- load ptr offset4
      value5 <- load ptr offset5
      IO.succeed (value1, value2, value3, value4, value5)
    Tuple6Rep @a @b @c @d @e @_ -> do
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      let offset3 = offset2 + sizeOf b
      let offset4 = offset3 + sizeOf c
      let offset5 = offset4 + sizeOf d
      let offset6 = offset5 + sizeOf e
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      value3 <- load ptr offset3
      value4 <- load ptr offset4
      value5 <- load ptr offset5
      value6 <- load ptr offset6
      IO.succeed (value1, value2, value3, value4, value5, value6)
    Tuple7Rep @a @b @c @d @e @f @_ -> do
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      let offset3 = offset2 + sizeOf b
      let offset4 = offset3 + sizeOf c
      let offset5 = offset4 + sizeOf d
      let offset6 = offset5 + sizeOf e
      let offset7 = offset6 + sizeOf f
      value1 <- load ptr offset1
      value2 <- load ptr offset2
      value3 <- load ptr offset3
      value4 <- load ptr offset4
      value5 <- load ptr offset5
      value6 <- load ptr offset6
      value7 <- load ptr offset7
      IO.succeed (value1, value2, value3, value4, value5, value6, value7)
    Tuple8Rep @a @b @c @d @e @f @g @_ -> do
      let offset1 = offset
      let offset2 = offset1 + sizeOf a
      let offset3 = offset2 + sizeOf b
      let offset4 = offset3 + sizeOf c
      let offset5 = offset4 + sizeOf d
      let offset6 = offset5 + sizeOf e
      let offset7 = offset6 + sizeOf f
      let offset8 = offset7 + sizeOf g
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
      tag <- load @Int ptr offset
      if tag == 0
        then IO.map Just (load ptr (offset + 8))
        else IO.succeed Nothing
    ResultRep{} -> throw (InternalError "Passing Result values as FFI arguments is not supported")
    ClassRep _ -> do
      stablePtr <- Foreign.peekByteOff ptr offset
      Foreign.deRefStablePtr stablePtr
    IORep -> throw (InternalError "Passing IO values as FFI arguments is not supported")
    NamedArgumentRep @name_ -> IO.map (name_ :::) (load ptr offset)

argumentName :: forall t -> FFI t => Maybe Name
argumentName t = case representation (Proxy @t) of
  NamedArgumentRep @name @_a -> do
    let camelCaseName = Text.pack (GHC.TypeLits.symbolVal @name Proxy)
    Just (splitCamelCase camelCaseName)
  _ -> Nothing
