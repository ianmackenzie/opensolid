module Main (main) where

import API (Class (Class))
import API qualified
import API.Constraint (Constraint)
import File qualified
import List qualified
import Maybe qualified
import OpenSolid
import OpenSolid.FFI qualified as FFI
import Pair qualified
import Python qualified
import Python.AbsFunction qualified
import Python.Class qualified
import Python.ComparisonFunction qualified
import Python.Constant qualified
import Python.EqualityFunction qualified
import Python.FFI qualified
import Python.Function qualified
import Python.MemberFunction qualified
import Python.NegationFunction qualified
import Python.PostOperator qualified
import Python.PreOperator qualified
import Python.StaticFunction qualified
import Python.Type.Registry (Registry)
import Python.Type.Registry qualified
import Text qualified

preamble :: Text
preamble =
  Python.lines
    [ "\"\"\"A collection of classes for 2D/3D geometric modelling.\"\"\""
    , ""
    , "from __future__ import annotations"
    , ""
    , "import ctypes"
    , "import platform"
    , "import threading"
    , "from ctypes import CDLL, POINTER, Structure, Union, c_char_p, c_double, c_int64, c_size_t, c_void_p"
    , "from pathlib import Path"
    , "from typing import Any, overload"
    , ""
    , "def _load_library() -> CDLL:"
    , "    \"\"\"Load the native library from the same directory as __init__.py.\"\"\""
    , "    match platform.system():"
    , "        case 'Windows':"
    , "            lib_file_name = 'opensolid-ffi.dll'"
    , "        case 'Darwin':"
    , "            lib_file_name = 'libopensolid-ffi.dylib'"
    , "        case 'Linux':"
    , "            lib_file_name = 'libopensolid-ffi.so'"
    , "        case unsupported_system:"
    , "            raise OSError(unsupported_system + ' is not yet supported')"
    , "    self_dir = Path(__file__).parent"
    , "    lib_path = self_dir / lib_file_name"
    , "    return ctypes.cdll.LoadLibrary(str(lib_path))"
    , ""
    , "_lib : CDLL = _load_library()"
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
    , "    \"\"\"An error that may be thrown by OpenSolid functions.\"\"\""
    , ""
    , "class _Text(Union):"
    , "    _fields_ = (('as_char', c_char_p), ('as_void', c_void_p))"
    , ""
    , "def _text_to_str(ptr: _Text) -> str:"
    , "    decoded = ptr.as_char.decode('utf-8')"
    , "    _lib.opensolid_free(ptr.as_void)"
    , "    return decoded"
    , ""
    , "def _str_to_text(s: str) -> _Text:"
    , "    encoded = s.encode('utf-8')"
    , "    buffer = ctypes.create_string_buffer(encoded)"
    , "    return _Text(as_char=ctypes.cast(buffer, c_char_p))"
    , ""
    , "def _list_argument(list_type: Any, array: Any) -> Any: # noqa: ANN401"
    , "    return list_type(len(array), array)"
    , ""
    , "def _error(message: str) -> Any: # noqa: ANN401"
    , "    raise Error(message)"
    , ""
    , "type ToleranceValue = float | Length | Area | Angle"
    , ""
    , "class _Tolerance(threading.local):"
    , "    value : ToleranceValue | None = None"
    , ""
    , "class Tolerance:"
    , "    \"\"\"Manages a thread-local tolerance value.\"\"\""
    , ""
    , "    _value: ToleranceValue | None = None"
    , "    _saved: ToleranceValue | None = None"
    , ""
    , "    def __init__(self, value: ToleranceValue | None) -> None:"
    , "        self._value = value"
    , ""
    , "    def __enter__(self) -> None:"
    , "        self._saved = _Tolerance.value"
    , "        _Tolerance.value = self._value"
    , ""
    , "    def __exit__(self, _exception_type: object, _exception_value: object, _traceback: object) -> None:"
    , "        _Tolerance.value = self._saved"
    , "        self._saved = None"
    , ""
    , "    @staticmethod"
    , "    def current() -> ToleranceValue | None:"
    , "        \"\"\"Get the current tolerance value.\"\"\""
    , "        return _Tolerance.value"
    , ""
    , "def _float_tolerance() -> float:"
    , "    if isinstance(_Tolerance.value, float):"
    , "        return _Tolerance.value"
    , "    if _Tolerance.value is None:"
    , "        message = 'No float tolerance set, please set one using \"with Tolerance(...)\"'"
    , "        raise TypeError(message)"
    , "    message = 'Expected a tolerance of type float but current tolerance is of type ' + type(_Tolerance.value).__name__"
    , "    raise TypeError(message)"
    , ""
    , "def _length_tolerance() -> Length:"
    , "    if isinstance(_Tolerance.value, Length):"
    , "        return _Tolerance.value"
    , "    if _Tolerance.value is None:"
    , "        message = 'No length tolerance set, please set one using \"with Tolerance(...)\"'"
    , "        raise TypeError(message)"
    , "    message = 'Expected a tolerance of type Length but current tolerance is of type ' + type(_Tolerance.value).__name__"
    , "    raise TypeError(message)"
    , ""
    , "def _area_tolerance() -> Area:"
    , "    if isinstance(_Tolerance.value, Area):"
    , "        return _Tolerance.value"
    , "    if _Tolerance.value is None:"
    , "        message = 'No area tolerance set, please set one using \"with Tolerance(...)\"'"
    , "        raise TypeError(message)"
    , "    message = 'Expected a tolerance of type Area but current tolerance is of type ' + type(_Tolerance.value).__name__"
    , "    raise TypeError(message)"
    , ""
    , "def _angle_tolerance() -> Angle:"
    , "    if isinstance(_Tolerance.value, Angle):"
    , "        return _Tolerance.value"
    , "    if _Tolerance.value is None:"
    , "        message = 'No angle tolerance set, please set one using \"with Tolerance(...)\"'"
    , "        raise TypeError(message)"
    , "    message = \"Expected a tolerance of type Angle but current tolerance is of type \" + type(_Tolerance.value).__name__"
    , "    raise TypeError(message)"
    ]

classDefinition :: Class -> (Text, Text)
classDefinition
  ( Class
      classId
      documentation
      constants
      staticFunctions
      memberFunctions
      equalityFunction
      comparisonFunction
      maybeNegationFunction
      maybeAbsFunction
      preOperators
      postOperators
      nestedClasses
    ) = do
    let (nestedClassDefinitions, nestedClassConstants) = List.unzip2 (List.map classDefinition nestedClasses)
    let definition =
          Python.lines
            [ "class " + Python.Class.unqualifiedName classId + ":"
            , Python.indent [Python.docstring documentation]
            , "    def __init__(self, *, ptr : c_void_p) -> None:"
            , "        self._ptr = ptr"
            , Python.indent (List.map Python.Constant.declaration constants)
            , Python.indent (List.map (Python.StaticFunction.definition classId) staticFunctions)
            , Python.indent (List.map (Python.MemberFunction.definition classId) memberFunctions)
            , Python.indent [Python.EqualityFunction.definition classId equalityFunction]
            , Python.indent [Python.ComparisonFunction.definitions classId comparisonFunction]
            , Python.indent [Python.NegationFunction.definition classId maybeNegationFunction]
            , Python.indent [Python.AbsFunction.definition classId maybeAbsFunction]
            , Python.indent (List.map (Python.PostOperator.definition classId) postOperators)
            , Python.indent (List.map (Python.PreOperator.definition classId) preOperators)
            , Python.indent (extraMemberFunctions (Python.Class.qualifiedName classId))
            , Python.indent nestedClassDefinitions
            ]
    let constantDefinitions =
          Python.lines ((List.map (Python.Constant.definition classId) constants) + nestedClassConstants)
    (definition, constantDefinitions)

extraMemberFunctions :: Text -> List Text
extraMemberFunctions className = case className of
  "Length" ->
    [ "def __repr__(self) -> str:"
    , "    if self == Length.zero:"
    , "        return 'Length.zero'"
    , "    return 'Length.meters(' + str(self.in_meters()) + ')'"
    ]
  "Angle" ->
    [ "def __repr__(self) -> str:"
    , "    if self == Angle.zero:"
    , "        return 'Angle.zero'"
    , "    return 'Angle.degrees(' + str(self.in_degrees()) + ')'"
    ]
  "Range" ->
    [ "def __repr__(self) -> str:"
    , "    low, high = self.endpoints()"
    , "    return 'Range.from_endpoints(' + str(low) + ',' + str(high) + ')'"
    ]
  "LengthRange" ->
    [ "def __repr__(self) -> str:"
    , "    low, high = self.endpoints()"
    , "    return 'LengthRange.meters(' + str(low.in_meters()) + ',' + str(high.in_meters()) + ')'"
    ]
  "AngleRange" ->
    [ "def __repr__(self) -> str:"
    , "    low, high = self.endpoints()"
    , "    return 'AngleRange.degrees(' + str(low.in_degrees()) + ',' + str(high.in_degrees()) + ')'"
    ]
  "Color" ->
    [ "def __repr__(self) -> str:"
    , "    r, g, b = self.components_255()"
    , "    return 'Color.rgb_255(' + str(r) + ',' + str(g) + ',' + str(b) + ')'"
    ]
  "Vector2d" ->
    [ "def __repr__(self) -> str:"
    , "    x, y = self.components()"
    , "    return 'Vector2d.xy(' + str(x) + ',' + str(y) + ')'"
    ]
  "Displacement2d" ->
    [ "def __repr__(self) -> str:"
    , "    x, y = self.components()"
    , "    return 'Displacement2d.meters(' + str(x.in_meters()) + ',' + str(y.in_meters()) + ')'"
    ]
  "Direction2d" ->
    [ "def __repr__(self) -> str:"
    , "    return 'Direction2d.degrees(' + str(self.to_angle().in_degrees()) + ')'"
    ]
  "Point2d" ->
    [ "def __repr__(self) -> str:"
    , "    x, y = self.coordinates()"
    , "    return 'Point2d.meters(' + str(x.in_meters()) + ',' + str(y.in_meters()) + ')'"
    ]
  "UvPoint" ->
    [ "def __repr__(self) -> str:"
    , "    x, y = self.coordinates()"
    , "    return 'UvPoint.uv(' + str(x) + ',' + str(y) + ')'"
    ]
  "Bounds2d" ->
    [ "def __repr__(self) -> str:"
    , "    x, y = self.coordinates()"
    , "    return 'Bounds2d.xy(' + repr(x) + ',' + repr(y) + ')'"
    ]
  "UvBounds" ->
    [ "def __repr__(self) -> str:"
    , "    u, v = self.coordinates()"
    , "    return 'UvBounds.uv(' + repr(u) + ',' + repr(v) + ')'"
    ]
  _ -> []

ffiTypeDeclarations :: Text
ffiTypeDeclarations = do
  let registry = List.foldr registerFunctionTypes Python.Type.Registry.empty API.functions
  Python.Type.Registry.typeDeclarations registry

topLevelClassName :: Class -> Maybe Text
topLevelClassName Class{API.id = FFI.Id _ (topLevelName :| nestedNames)} =
  case nestedNames of
    [] -> Just (Python.str (FFI.pascalCase topLevelName))
    List.OneOrMore -> Nothing

allExportsDefinition :: Text
allExportsDefinition =
  Python.lines
    [ "__all__ = ["
    , Python.indent
        [Text.join "," (Python.str "Tolerance" : Maybe.collect topLevelClassName API.classes)]
    , "]"
    ]

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
    [type1] -> Python.FFI.registerType type1 registry
    type1 : type2 : rest -> Python.FFI.registerType (FFI.Tuple type1 type2 rest) registry

main :: IO ()
main = IO.do
  let (classDefinitions, constantDefinitions) = List.unzip2 (List.map classDefinition API.classes)
  let pythonCode =
        Python.lines
          [ preamble
          , ffiTypeDeclarations
          , Python.lines classDefinitions
          , Python.lines constantDefinitions
          , allExportsDefinition
          ]
  File.writeTo "opensolid-python/lib/src/opensolid/__init__.py" pythonCode
