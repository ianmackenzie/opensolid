module Main (main) where

import File qualified
import List qualified
import Maybe qualified
import OpenSolid
import OpenSolid.API (Class (Class))
import OpenSolid.API qualified as API
import OpenSolid.API.Constraint (Constraint)
import OpenSolid.FFI qualified as FFI
import Pair qualified
import Python qualified
import Python.Class qualified
import Python.ComparisonFunction qualified
import Python.EqualityFunction qualified
import Python.FFI qualified
import Python.Function qualified
import Python.MemberFunction qualified
import Python.NegationOperator qualified
import Python.PostOperator qualified
import Python.PreOperator qualified
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
    , "from ctypes import CDLL, POINTER, Structure, Union, c_char_p, c_double, c_int64, c_size_t, c_void_p"
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
classDefinition
  ( Class
      classId
      staticFunctions
      memberFunctions
      equalityFunction
      comparisonFunction
      negationFunction
      preOperators
      postOperators
      nestedClasses
    ) = do
    Python.lines
      [ "class " + Python.Class.unqualifiedName classId + ":"
      , "    def __init__(self, *, ptr : c_void_p) -> None:"
      , "        self.__ptr__ = ptr"
      , Python.indent (List.map (Python.StaticFunction.definition classId) staticFunctions)
      , Python.indent (List.map (Python.MemberFunction.definition classId) memberFunctions)
      , Python.indent [Python.EqualityFunction.definition classId equalityFunction]
      , Python.indent [Python.ComparisonFunction.definitions classId comparisonFunction]
      , Python.indent [Python.NegationOperator.definition classId negationFunction]
      , Python.indent (List.map (Python.PostOperator.definition classId) postOperators)
      , Python.indent (List.map (Python.PreOperator.definition classId) preOperators)
      , Python.indent (List.map classDefinition nestedClasses)
      ]

ffiTypeDeclarations :: Text
ffiTypeDeclarations = do
  let registry = List.foldr registerFunctionTypes Python.Type.Registry.empty API.functions
  Python.Type.Registry.typeDeclarations registry

registerFunctionTypes :: API.Function -> Registry -> Registry
registerFunctionTypes function registry =
  registry
    |> registerArgumentTypes (API.constraint function) (API.argumentTypes function)
    |> Python.FFI.registerType (API.returnType function)

registerArgumentTypes :: Maybe Constraint -> List FFI.Type -> Registry -> Registry
registerArgumentTypes maybeConstraint argumentTypes registry = do
  let toleranceType constraint = Pair.second (Python.Function.toleranceArgument constraint)
  let maybeToleranceType = Maybe.map toleranceType maybeConstraint
  case maybeToleranceType + argumentTypes of
    [] -> registry
    List.One type1 -> Python.FFI.registerType type1 registry
    List.TwoOrMore type1 type2 rest -> Python.FFI.registerType (FFI.Tuple type1 type2 rest) registry

classDefinitions :: Text
classDefinitions = Python.lines (List.map classDefinition API.classes)

main :: IO ()
main = IO.do
  let pythonCode = Python.lines [preamble, ffiTypeDeclarations, classDefinitions]
  File.writeTo "opensolid-python/opensolid/__init__.py" pythonCode
