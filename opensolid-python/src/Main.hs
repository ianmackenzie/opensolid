module Main (main) where

import API qualified
import API.Class (Class (Class))
import API.Class qualified as Class
import API.Constraint (Constraint)
import API.Function (Function)
import API.Function qualified as Function
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.List qualified as List
import OpenSolid.Maybe qualified as Maybe
import OpenSolid.Pair qualified as Pair
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text
import Python qualified
import Python.AbsFunction qualified
import Python.Class qualified
import Python.ComparisonFunction qualified
import Python.Constant qualified
import Python.Constructor qualified
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
    , "class _Tolerance(threading.local):"
    , "    value : float | Length | Area | Angle | None = None"
    , ""
    , "class Tolerance:"
    , "    \"\"\"Manages a thread-local tolerance value."
    , ""
    , "    Many functions in OpenSolid require a tolerance to be set."
    , "    You should generally choose a value that is"
    , "    much smaller than any meaningful size/dimension in the geometry you're modelling,"
    , "    but significantly *larger* than any expected numerical roundoff that might occur."
    , "    A good default choice is roughly one-billionth of the overall size of your geometry;"
    , "    for 'human-scale' things (say, from an earring up to a house)"
    , "    that means that one nanometer is a reasonable value to use."
    , ""
    , "    Passing a tolerance into every function that needed one would get very verbose,"
    , "    and it's very common to choose a single tolerance value and use it throughout a project."
    , "    However, it's also occasionally necessary to set a different tolerance for some code."
    , "    This class allows managing tolerances using Python's ``with`` statement, e.g.::"
    , ""
    , "        with Tolerance(Length.nanometer):"
    , "            do_something()"
    , "            do_something_else()"
    , "            with Tolerance(Angle.degrees(0.001)):"
    , "                compare_two_angles()"
    , "            do_more_things()"
    , ""
    , "    In the above code, the ``Length.nanometer`` tolerance value"
    , "    will be used for ``do_something()`` and ``do_something_else()``"
    , "    (and any functions they call)."
    , "    The ``Angle.degrees(0.001))`` tolerance value"
    , "    will then be used for ``compare_two_angles()``,"
    , "    and then the ``Length.nanometer`` tolerance value will be restored"
    , "    and used for ``do_more_things()``."
    , "    \"\"\""
    , ""
    , "    _value: float | Length | Area | Angle | None = None"
    , "    _saved: float | Length | Area | Angle | None = None"
    , ""
    , "    def __init__(self, value: float | Length | Area | Angle | None) -> None:"
    , "        self._value = value"
    , ""
    , "    def __enter__(self) -> None:"
    , "        \"\"\"Set the given tolerance as the currently active one.\"\"\""
    , "        self._saved = _Tolerance.value"
    , "        _Tolerance.value = self._value"
    , ""
    , "    def __exit__(self, _exception_type: object, _exception_value: object, _traceback: object) -> None:"
    , "        \"\"\"Restore the previous tolerance as the currently active one.\"\"\""
    , "        _Tolerance.value = self._saved"
    , "        self._saved = None"
    , ""
    , "    @staticmethod"
    , "    def current() -> float | Length | Area | Angle | None:"
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
      maybeConstructor
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
            [ "class " <> Python.Class.unqualifiedName classId <> ":"
            , Python.indent [Python.docstring documentation]
            , Python.indent ["_ptr: c_void_p"]
            , Python.indent [Python.Constructor.definition classId maybeConstructor]
            , Python.indent
                [ "@staticmethod"
                , "def _new(ptr: c_void_p) -> " <> Python.Class.qualifiedName classId <> ":"
                , Python.indent
                    [ "\"\"\"Construct directly from an underlying C pointer.\"\"\""
                    , "obj = object.__new__(" <> Python.Class.qualifiedName classId <> ")"
                    , "obj._ptr = ptr"
                    , "return obj"
                    ]
                ]
            , Python.indent
                [ "def __del__(self) -> None:"
                , Python.indent
                    [ "\"\"\"Free the underlying Haskell value.\"\"\""
                    , "_lib.opensolid_release(self._ptr)"
                    ]
                ]
            , Python.indent (List.map Python.Constant.declaration constants)
            , Python.indent (List.map (Python.StaticFunction.definition classId) staticFunctions)
            , Python.indent (List.map (Python.MemberFunction.definition classId) memberFunctions)
            , Python.indent [Python.EqualityFunction.definition classId equalityFunction]
            , Python.indent [Python.ComparisonFunction.definitions classId comparisonFunction]
            , Python.indent [Python.NegationFunction.definition classId maybeNegationFunction]
            , Python.indent [Python.AbsFunction.definition classId maybeAbsFunction]
            , Python.indent (List.map (Python.PostOperator.definition classId) postOperators)
            , Python.indent (List.map (Python.PreOperator.definition classId) preOperators)
            , Python.indent [extraMemberFunctions (Python.Class.qualifiedName classId)]
            , Python.indent nestedClassDefinitions
            ]
    let constantDefinitions =
          Python.lines ((List.map (Python.Constant.definition classId) constants) <> nestedClassConstants)
    (definition, constantDefinitions)

extraMemberFunctions :: Text -> Text
extraMemberFunctions className = do
  let repr body =
        Python.lines
          [ "def __repr__(self) -> str:"
          , Python.indent
              [ Python.docstring "Return a human-readable representation of this value."
              , Python.lines body
              ]
          ]
  case className of
    "Length" -> repr ["return 'Length.meters(' + str(self.in_meters()) + ')'"]
    "Area" -> repr ["return 'Area.square_meters(' + str(self.in_square_meters()) + ')'"]
    "Angle" -> repr ["return 'Angle.degrees(' + str(self.in_degrees()) + ')'"]
    "Range" ->
      repr
        [ "low, high = self.endpoints()"
        , "return 'Range(' + str(low) + ',' + str(high) + ')'"
        ]
    "LengthRange" ->
      repr
        [ "low, high = self.endpoints()"
        , "return 'LengthRange.meters(' + str(low.in_meters()) + ',' + str(high.in_meters()) + ')'"
        ]
    "AreaRange" ->
      repr
        [ "low, high = self.endpoints()"
        , "return 'AreaRange.square_meters(' + str(low.in_square_meters()) + ',' + str(high.in_square_meters()) + ')'"
        ]
    "AngleRange" ->
      repr
        [ "low, high = self.endpoints()"
        , "return 'AngleRange.degrees(' + str(low.in_degrees()) + ',' + str(high.in_degrees()) + ')'"
        ]
    "Color" ->
      repr
        [ "r, g, b = self.components_255()"
        , "return 'Color.rgb_255(' + str(r) + ',' + str(g) + ',' + str(b) + ')'"
        ]
    "Vector2d" ->
      repr
        [ "x, y = self.components()"
        , "return 'Vector2d.xy(' + str(x) + ',' + str(y) + ')'"
        ]
    "Displacement2d" ->
      repr
        [ "x, y = self.components()"
        , "return 'Displacement2d.meters(' + str(x.in_meters()) + ',' + str(y.in_meters()) + ')'"
        ]
    "AreaVector2d" ->
      repr
        [ "x, y = self.components()"
        , "return 'AreaVector2d.square_meters(' + str(x.in_square_meters()) + ',' + str(y.in_square_meters()) + ')'"
        ]
    "Direction2d" ->
      repr
        [ "return 'Direction2d.degrees(' + str(self.to_angle().in_degrees()) + ')'"
        ]
    "Point2d" ->
      repr
        [ "x, y = self.coordinates()"
        , "return 'Point2d.meters(' + str(x.in_meters()) + ',' + str(y.in_meters()) + ')'"
        ]
    "UvPoint" ->
      repr
        [ "x, y = self.coordinates()"
        , "return 'UvPoint(' + str(x) + ',' + str(y) + ')'"
        ]
    "Bounds2d" ->
      repr
        [ "x, y = self.coordinates()"
        , "return 'Bounds2d(' + repr(x) + ',' + repr(y) + ')'"
        ]
    "UvBounds" ->
      repr
        [ "u, v = self.coordinates()"
        , "return 'UvBounds(' + repr(u) + ',' + repr(v) + ')'"
        ]
    _ -> ""

ffiTypeDeclarations :: Text
ffiTypeDeclarations = do
  let registry = List.foldr registerFunctionTypes Python.Type.Registry.empty API.functions
  Python.Type.Registry.typeDeclarations registry

topLevelClassName :: Class -> Maybe Text
topLevelClassName Class{Class.id = FFI.Id _ (topLevelName :| nestedNames)} =
  case nestedNames of
    [] -> Just (FFI.pascalCase topLevelName)
    List.OneOrMore -> Nothing

allExportsDefinition :: Text
allExportsDefinition = do
  let names = List.sort ("Tolerance" : Maybe.collect topLevelClassName API.classes)
  Python.lines
    [ "__all__ = ["
    , Python.indent
        [Text.join "," (List.map Python.str names)]
    , "]"
    ]

registerFunctionTypes :: Function -> Registry -> Registry
registerFunctionTypes function registry =
  registry
    |> registerArgumentTypes (Function.constraint function) (Function.argumentTypes function)
    |> Python.FFI.registerType (Function.returnType function)

registerArgumentTypes :: Maybe Constraint -> List FFI.Type -> Registry -> Registry
registerArgumentTypes maybeConstraint argumentTypes registry = do
  let toleranceType constraint = Pair.second (Python.Function.toleranceArgument constraint)
  let maybeToleranceType = Maybe.map toleranceType maybeConstraint
  case List.maybe maybeToleranceType <> argumentTypes of
    [] -> registry
    [type1] -> Python.FFI.registerType type1 registry
    type1 : type2 : rest -> Python.FFI.registerType (FFI.Tuple type1 type2 rest) registry

main :: IO ()
main = do
  let flatten (classLines, constantLines) = Python.lines [classLines, constantLines]
  IO.writeUtf8 "opensolid-python/lib/src/opensolid/__init__.py" $
    Python.lines
      [ preamble
      , ffiTypeDeclarations
      , Python.lines (List.map (flatten . classDefinition) API.classes)
      , allExportsDefinition
      ]
