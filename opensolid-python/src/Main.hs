{-# OPTIONS_GHC -Wno-unused-binds -Wno-unused-top-binds #-}

module Main (main) where

import CTypes qualified
import Constructor qualified
import Data.Proxy (Proxy (Proxy))
import IO qualified
import Length (Length)
import List qualified
import MemberFunction qualified
import OpenSolid
import OpenSolid.API qualified as API
import OpenSolid.API.Class (Class (Class), Constraint (..), Constructor (..), MemberFunction (..), StaticFunction (..))
import OpenSolid.API.Class qualified as Class
import OpenSolid.FFI (FFI)
import Pair qualified
import Python qualified
import StaticFunction qualified
import TypeRegistry (TypeRegistry)
import TypeRegistry qualified
import Units (Meters)

preamble :: Text
preamble =
  Python.lines
    [ "\"\"\"A collection of classes for 2D/3D geometric modelling.\"\"\""
    , ""
    , "from __future__ import annotations"
    , ""
    , "import ctypes"
    , "import os"
    , "from contextlib import contextmanager"
    , "from ctypes import CDLL, Structure, Union, c_char_p, c_double, c_int, c_int64, c_size_t, c_void_p"
    , "from pathlib import Path"
    , "from typing import Any, Generator, overload"
    , ""
    , "# Load the native library, assuming it's located in the same directory as __init__.py"
    , "_lib_dir = Path(__file__).parent"
    , "match os.uname().sysname:"
    , "    case \"Windows\":"
    , "        _load_path = _lib_dir / \"opensolid-ffi.dll\""
    , "    case \"Darwin\":"
    , "        _load_path = _lib_dir / \"libopensolid-ffi.dylib\""
    , "    case \"Linux\":"
    , "        _load_path = _lib_dir / \"libopensolid-ffi.so\""
    , "    case unsupported_system:"
    , "        raise OSError(unsupported_system + \" is not yet supported\")"
    , "_lib : CDLL = ctypes.cdll.LoadLibrary(str(_load_path))"
    , ""
    , "# Define the signatures of the C API functions"
    , "_lib.opensolid_init.argtypes = []"
    , "_lib.opensolid_invoke.argtypes = [c_int, c_int, c_void_p, c_void_p]"
    , "_lib.opensolid_malloc.argtypes = [c_size_t]"
    , "_lib.opensolid_malloc.restype = c_void_p"
    , "_lib.opensolid_free.argtypes = [c_void_p]"
    , "_lib.opensolid_release.argtypes = [c_void_p]"
    , ""
    , "# Initialize the Haskell runtime"
    , "_lib.opensolid_init()"
    , ""
    , "class Error(Exception):"
    , "    pass"
    , ""
    , "class _ErrorMessage(Union):"
    , "    _fields_ = ((\"as_char\", c_char_p), (\"as_void\", c_void_p))"
    , ""
    , "def _error(output: Any) -> Any: # noqa: ANN401"
    , "    message = output.field1.as_char.decode(\"utf-8\")"
    , "    _lib.opensolid_free(output.field1.as_void)"
    , "    raise Error(message)"
    , ""
    , "class Length:"
    , "    \"\"\"A length, stored as a value in meters.\"\"\""
    , ""
    , "    def __init__(self, value: float) -> None:"
    , "        \"\"\"Create a Length from a float value in meters.\"\"\""
    , "        self.value = value"
    , ""
    , "class Angle:"
    , "    \"\"\"An angle, stored as a value in radians.\"\"\""
    , ""
    , "    def __init__(self, value: float) -> None:"
    , "        \"\"\"Create an Angle from a float value in radians.\"\"\""
    , "        self.value = value"
    , ""
    , "class Tolerance:"
    , "    \"\"\"Manages a global tolerance value.\"\"\""
    , ""
    , "    current: float | Length | Angle | None = None"
    , ""
    , "    @staticmethod"
    , "    @contextmanager"
    , "    def of(value: float | Length | Angle | None) -> Generator[None]:"
    , "        \"\"\"Set the implicit tolerance to be used in the nested block."
    , ""
    , "        The tolerance will be restored to its previous value when the block exits."
    , "        \"\"\""
    , "        saved = Tolerance.current"
    , "        Tolerance.current = value"
    , "        try:"
    , "            yield"
    , "        finally:"
    , "            Tolerance.current = saved"
    , ""
    , "def _float_tolerance() -> float:"
    , "    if isinstance(Tolerance.current, float):"
    , "        return Tolerance.current"
    , "    if Tolerance.current is None:"
    , "        message = \"No float tolerance set, please set one using Tolerance.of\""
    , "        raise TypeError(message)"
    , "    message = \"Expected a tolerance of type float but current tolerance is of type \" + type(Tolerance.current).__name__"
    , "    raise TypeError(message)"
    , ""
    , "def _length_tolerance() -> Length:"
    , "    if isinstance(Tolerance.current, Length):"
    , "        return Tolerance.current"
    , "    if Tolerance.current is None:"
    , "        message = \"No length tolerance set, please set one using Tolerance.of\""
    , "        raise TypeError(message)"
    , "    message = \"Expected a tolerance of type Length but current tolerance is of type \" + type(Tolerance.current).__name__"
    , "    raise TypeError(message)"
    , ""
    , "def _angle_tolerance() -> Angle:"
    , "    if isinstance(Tolerance.current, Angle):"
    , "        return Tolerance.current"
    , "    if Tolerance.current is None:"
    , "        message = \"No angle tolerance set, please set one using Tolerance.of\""
    , "        raise TypeError(message)"
    , "    message = \"Expected a tolerance of type Angle but current tolerance is of type \" + type(Tolerance.current).__name__"
    , "    raise TypeError(message)"
    ]

classDefinition :: Int -> Class -> Text
classDefinition classId cls = do
  Python.lines
    [ "class " + Class.name cls + ":"
    , Python.indent [Class.withConstructors cls (Constructor.definition classId)]
    , Python.indent (Class.mapStaticFunctions cls (StaticFunction.definition classId))
    , Python.indent (Class.mapMemberFunctions cls (MemberFunction.definition classId))
    ]

ffiTypeDeclarations :: Text
ffiTypeDeclarations = do
  let registry = List.foldr registerClassTypes TypeRegistry.empty API.classes
  TypeRegistry.typeDeclarations registry

registerClassTypes :: Class -> TypeRegistry -> TypeRegistry
registerClassTypes (Class _ constructors staticFunctions memberFunctions) registry0 = do
  let staticFunctionOverloads = List.collect Pair.second staticFunctions
  let memberFunctionOverloads = List.collect Pair.second memberFunctions
  let registry1 = List.foldr registerConstructorTypes registry0 constructors
  let registry2 = List.foldr registerStaticFunctionTypes registry1 staticFunctionOverloads
  let registry3 = List.foldr registerMemberFunctionTypes registry2 memberFunctionOverloads
  registry3

registerConstructorTypes :: Constructor value -> TypeRegistry -> TypeRegistry
registerConstructorTypes constructor registry = case constructor of
  C0 N v -> register0N v registry
  C0 F v -> register0F v registry
  C0 L v -> register0L v registry
  C1 N _ f -> register1N f registry
  C1 F _ f -> register1F f registry
  C1 L _ f -> register1L f registry
  C2 N _ _ f -> register2N f registry
  C2 F _ _ f -> register2F f registry
  C2 L _ _ f -> register2L f registry
  C3 N _ _ _ f -> register3N f registry
  C3 F _ _ _ f -> register3F f registry
  C3 L _ _ _ f -> register3L f registry
  C4 N _ _ _ _ f -> register4N f registry
  C4 F _ _ _ _ f -> register4F f registry
  C4 L _ _ _ _ f -> register4L f registry

registerStaticFunctionTypes :: StaticFunction -> TypeRegistry -> TypeRegistry
registerStaticFunctionTypes function registry = case function of
  S0 N v -> register0N v registry
  S0 F v -> register0F v registry
  S0 L v -> register0L v registry
  S1 N _ f -> register1N f registry
  S1 F _ f -> register1F f registry
  S1 L _ f -> register1L f registry
  S2 N _ _ f -> register2N f registry
  S2 F _ _ f -> register2F f registry
  S2 L _ _ f -> register2L f registry
  S3 N _ _ _ f -> register3N f registry
  S3 F _ _ _ f -> register3F f registry
  S3 L _ _ _ f -> register3L f registry
  S4 N _ _ _ _ f -> register4N f registry
  S4 F _ _ _ _ f -> register4F f registry
  S4 L _ _ _ _ f -> register4L f registry

registerMemberFunctionTypes :: MemberFunction value -> TypeRegistry -> TypeRegistry
registerMemberFunctionTypes function registry = case function of
  M0 N f -> register1N f registry
  M0 F f -> register1F f registry
  M0 L f -> register1L f registry
  M1 N _ f -> register2N f registry
  M1 F _ f -> register2F f registry
  M1 L _ f -> register2L f registry
  M2 N _ _ f -> register3N f registry
  M2 F _ _ f -> register3F f registry
  M2 L _ _ f -> register3L f registry
  M3 N _ _ _ f -> register4N f registry
  M3 F _ _ _ f -> register4F f registry
  M3 L _ _ _ f -> register4L f registry
  M4 N _ _ _ _ f -> register5N f registry
  M4 F _ _ _ _ f -> register5F f registry
  M4 L _ _ _ _ f -> register5L f registry

register0N :: forall a. FFI a => a -> TypeRegistry -> TypeRegistry
register0N _ registry =
  registry
    |> CTypes.registerType @a Proxy

register0F :: forall a. FFI a => (Tolerance Unitless => a) -> TypeRegistry -> TypeRegistry
register0F _ registry =
  registry
    |> CTypes.registerType @Float Proxy
    |> CTypes.registerType @a Proxy

register0L :: forall a. FFI a => (Tolerance Meters => a) -> TypeRegistry -> TypeRegistry
register0L _ registry =
  registry
    |> CTypes.registerType @Length Proxy
    |> CTypes.registerType @a Proxy

register1N :: forall a b. (FFI a, FFI b) => (a -> b) -> TypeRegistry -> TypeRegistry
register1N _ registry =
  registry
    |> CTypes.registerType @a Proxy
    |> CTypes.registerType @b Proxy

register1F :: forall a b. (FFI a, FFI b) => (Tolerance Unitless => a -> b) -> TypeRegistry -> TypeRegistry
register1F _ registry =
  registry
    |> CTypes.registerType @(Float, a) Proxy
    |> CTypes.registerType @b Proxy

register1L :: forall a b. (FFI a, FFI b) => (Tolerance Meters => a -> b) -> TypeRegistry -> TypeRegistry
register1L _ registry =
  registry
    |> CTypes.registerType @(Length, a) Proxy
    |> CTypes.registerType @b Proxy

register2N ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  (a -> b -> c) ->
  TypeRegistry ->
  TypeRegistry
register2N _ registry =
  registry
    |> CTypes.registerType @(a, b) Proxy
    |> CTypes.registerType @c Proxy

register2F ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  (Tolerance Unitless => a -> b -> c) ->
  TypeRegistry ->
  TypeRegistry
register2F _ registry =
  registry
    |> CTypes.registerType @(Float, a, b) Proxy
    |> CTypes.registerType @c Proxy

register2L ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  (Tolerance Meters => a -> b -> c) ->
  TypeRegistry ->
  TypeRegistry
register2L _ registry =
  registry
    |> CTypes.registerType @(Length, a, b) Proxy
    |> CTypes.registerType @c Proxy

register3N ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  (a -> b -> c -> d) ->
  TypeRegistry ->
  TypeRegistry
register3N _ registry =
  registry
    |> CTypes.registerType @(a, b, c) Proxy
    |> CTypes.registerType @d Proxy

register3F ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  (Tolerance Unitless => a -> b -> c -> d) ->
  TypeRegistry ->
  TypeRegistry
register3F _ registry =
  registry
    |> CTypes.registerType @(Float, a, b, c) Proxy
    |> CTypes.registerType @d Proxy

register3L ::
  forall a b c d.
  (FFI a, FFI b, FFI c, FFI d) =>
  (Tolerance Meters => a -> b -> c -> d) ->
  TypeRegistry ->
  TypeRegistry
register3L _ registry =
  registry
    |> CTypes.registerType @(Length, a, b, c) Proxy
    |> CTypes.registerType @d Proxy

register4N ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  (a -> b -> c -> d -> e) ->
  TypeRegistry ->
  TypeRegistry
register4N _ registry =
  registry
    |> CTypes.registerType @(a, b, c, d) Proxy
    |> CTypes.registerType @e Proxy

register4F ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  (Tolerance Unitless => a -> b -> c -> d -> e) ->
  TypeRegistry ->
  TypeRegistry
register4F _ registry =
  registry
    |> CTypes.registerType @(Float, a, b, c, d) Proxy
    |> CTypes.registerType @e Proxy

register4L ::
  forall a b c d e.
  (FFI a, FFI b, FFI c, FFI d, FFI e) =>
  (Tolerance Meters => a -> b -> c -> d -> e) ->
  TypeRegistry ->
  TypeRegistry
register4L _ registry =
  registry
    |> CTypes.registerType @(Length, a, b, c, d) Proxy
    |> CTypes.registerType @e Proxy

register5N ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  (a -> b -> c -> d -> e -> f) ->
  TypeRegistry ->
  TypeRegistry
register5N _ registry =
  registry
    |> CTypes.registerType @(a, b, c, d, e) Proxy
    |> CTypes.registerType @f Proxy

register5F ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  (Tolerance Unitless => a -> b -> c -> d -> e -> f) ->
  TypeRegistry ->
  TypeRegistry
register5F _ registry =
  registry
    |> CTypes.registerType @(Float, a, b, c, d, e) Proxy
    |> CTypes.registerType @f Proxy

register5L ::
  forall a b c d e f.
  (FFI a, FFI b, FFI c, FFI d, FFI e, FFI f) =>
  (Tolerance Meters => a -> b -> c -> d -> e -> f) ->
  TypeRegistry ->
  TypeRegistry
register5L _ registry =
  registry
    |> CTypes.registerType @(Length, a, b, c, d, e) Proxy
    |> CTypes.registerType @f Proxy

classDefinitions :: Text
classDefinitions = Python.separate (List.mapWithIndex classDefinition API.classes)

main :: IO ()
main = IO.do
  let pythonCode = Python.separate [preamble, ffiTypeDeclarations, classDefinitions]
  IO.printLine pythonCode
