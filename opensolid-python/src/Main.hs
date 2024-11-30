module Main (main) where

import File qualified
import List qualified
import Maybe qualified
import OpenSolid
import OpenSolid.API qualified as API
import OpenSolid.API.Class (Class (Class))
import OpenSolid.API.Constraint (Constraint)
import OpenSolid.API.MemberFunction (MemberFunction (..))
import OpenSolid.API.MemberFunction qualified as MemberFunction
import OpenSolid.API.StaticFunction (StaticFunction (..))
import OpenSolid.API.StaticFunction qualified as StaticFunction
import OpenSolid.FFI qualified as FFI
import Pair qualified
import Python qualified
import Python.Class qualified
import Python.FFI qualified
import Python.Function qualified
import Python.MemberFunction qualified
import Python.StaticFunction qualified
import Python.Type.Registry (Registry)
import Python.Type.Registry qualified

preamble :: Text
preamble =
  Python.lines
    [ "\"\"\"A collection of classes for 2D/3D geometric modelling.\"\"\""
    , ""
    , "from __future__ import annotations"
    , ""
    , "import ctypes"
    , "import platform"
    , "from ctypes import CDLL, Structure, Union, c_char_p, c_double, c_int64, c_size_t, c_void_p"
    , "from pathlib import Path"
    , "from typing import Any, overload"
    , ""
    , "# Load the native library, assuming it's located in the same directory as __init__.py"
    , "_lib_dir = Path(__file__).parent"
    , "match platform.system():"
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
    , "# (also an early sanity check to make sure the library has been loaded OK)"
    , "_lib.opensolid_init.argtypes = []"
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
    , "    def __init__(self, value: float | Length | Angle | None) -> None:"
    , "        self.value = value"
    , "        self.saved = None"
    , ""
    , "    def __enter__(self) -> None:"
    , "        self.saved = Tolerance.current"
    , "        Tolerance.current = self.value"
    , ""
    , "    def __exit__(self, _exception_type: object, _exception_value: object, _traceback: object) -> None:"
    , "        Tolerance.current = self.saved"
    , "        self.saved = None"
    , ""
    , "def _float_tolerance() -> float:"
    , "    if isinstance(Tolerance.current, float):"
    , "        return Tolerance.current"
    , "    if Tolerance.current is None:"
    , "        message = \"No float tolerance set, please set one using 'with Tolerance(...)'\""
    , "        raise TypeError(message)"
    , "    message = \"Expected a tolerance of type float but current tolerance is of type \" + type(Tolerance.current).__name__"
    , "    raise TypeError(message)"
    , ""
    , "def _length_tolerance() -> Length:"
    , "    if isinstance(Tolerance.current, Length):"
    , "        return Tolerance.current"
    , "    if Tolerance.current is None:"
    , "        message = \"No length tolerance set, please set one using 'with Tolerance(...)'\""
    , "        raise TypeError(message)"
    , "    message = \"Expected a tolerance of type Length but current tolerance is of type \" + type(Tolerance.current).__name__"
    , "    raise TypeError(message)"
    , ""
    , "def _angle_tolerance() -> Angle:"
    , "    if isinstance(Tolerance.current, Angle):"
    , "        return Tolerance.current"
    , "    if Tolerance.current is None:"
    , "        message = \"No angle tolerance set, please set one using 'with Tolerance(...)'\""
    , "        raise TypeError(message)"
    , "    message = \"Expected a tolerance of type Angle but current tolerance is of type \" + type(Tolerance.current).__name__"
    , "    raise TypeError(message)"
    ]

classDefinition :: Class -> Text
classDefinition (Class baseName maybeUnits staticFunctions memberFunctions) = do
  let functionPrefix = "opensolid_" + FFI.className baseName maybeUnits + "_"
  Python.lines
    [ "class " + Python.Class.name baseName maybeUnits + ":"
    , "    def __init__(self, *, ptr : c_void_p) -> None:"
    , "        self.__ptr__ = ptr"
    , Python.indent (List.map (Python.StaticFunction.definition functionPrefix) staticFunctions)
    , Python.indent (List.map (Python.MemberFunction.definition functionPrefix) memberFunctions)
    ]

ffiTypeDeclarations :: Text
ffiTypeDeclarations = do
  let registry = List.foldr registerClassTypes Python.Type.Registry.empty API.classes
  Python.Type.Registry.typeDeclarations registry

registerClassTypes :: Class -> Registry -> Registry
registerClassTypes (Class _ _ staticFunctions memberFunctions) registry0 = do
  let staticFunctionOverloads = List.collect Pair.second staticFunctions
  let memberFunctionOverloads = List.collect Pair.second memberFunctions
  let registry1 = List.foldr registerStaticFunctionTypes registry0 staticFunctionOverloads
  let registry2 = List.foldr registerMemberFunctionTypes registry1 memberFunctionOverloads
  registry2

registerStaticFunctionTypes :: StaticFunction -> Registry -> Registry
registerStaticFunctionTypes staticFunction registry = do
  let (maybeConstraint, arguments, returnType) = StaticFunction.signature staticFunction
  let argumentTypes = List.map Pair.second arguments
  List.foldr Python.FFI.registerType registry argumentTypes
    |> Python.FFI.registerType returnType
    |> registerArgumentsTupleType maybeConstraint argumentTypes

registerMemberFunctionTypes :: MemberFunction value -> Registry -> Registry
registerMemberFunctionTypes memberFunction registry = do
  let (maybeConstraint, arguments, selfType, returnType) = MemberFunction.signature memberFunction
  let argumentTypes = List.map Pair.second arguments + [selfType]
  List.foldr Python.FFI.registerType registry argumentTypes
    |> Python.FFI.registerType returnType
    |> registerArgumentsTupleType maybeConstraint argumentTypes

registerArgumentsTupleType :: Maybe Constraint -> List FFI.Type -> Registry -> Registry
registerArgumentsTupleType maybeConstraint argumentTypes registry = do
  let toleranceType constraint = Pair.second (Python.Function.toleranceArgument constraint)
  let maybeToleranceType = Maybe.map toleranceType maybeConstraint
  case maybeToleranceType + argumentTypes of
    [] -> registry
    [_] -> registry
    type1 : type2 : rest -> Python.FFI.registerType (FFI.Tuple type1 type2 rest) registry

classDefinitions :: Text
classDefinitions = Python.separate (List.map classDefinition API.classes)

main :: IO ()
main = IO.do
  let pythonCode = Python.separate [preamble, ffiTypeDeclarations, classDefinitions]
  File.writeTo "opensolid-python/opensolid/__init__.py" pythonCode
