module Main (main) where

import API qualified
import API.Class (Class (Class))
import API.Class qualified as Class
import API.Function (Function)
import API.Function qualified as Function
import API.ImplicitArgument (ImplicitArgument)
import API.Upcast (Upcast)
import API.Upcast qualified as Upcast
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
import Python.Property qualified
import Python.StaticFunction qualified
import Python.Type.Registry (Registry)
import Python.Type.Registry qualified
import Python.Upcast qualified

preamble :: Text
preamble =
  Python.lines
    [ "\"\"\"A collection of classes for 2D/3D geometric modelling.\"\"\""
    , ""
    , "from __future__ import annotations"
    , ""
    , "import contextvars"
    , "import ctypes"
    , "import platform"
    , "from contextvars import ContextVar"
    , "from ctypes import CDLL, POINTER, c_char_p, c_double, c_int64, c_size_t, c_void_p"
    , "from functools import cached_property"
    , "from pathlib import Path"
    , "from typing import Any, Never, Union, overload"
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
    , "class _Text(ctypes.Union):"
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
    , "def _sign_argument(value: int) -> int:"
    , "    if value in (1, -1):"
    , "        return value"
    , "    return _error('Sign value must be 1 or -1')"
    , ""
    , "def _error(message: str) -> Never:"
    , "    raise Error(message)"
    , ""
    , "class Tolerance:"
    , "    \"\"\"Manages a tolerance context value."
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
    , "    Value = Union[float, 'Length', 'Area', 'Angle']"
    , ""
    , "    _value: Value"
    , "    _token: contextvars.Token[Value] | None = None"
    , ""
    , "    def __init__(self, value: Value) -> None:"
    , "        self._value = value"
    , ""
    , "    def __enter__(self) -> None:"
    , "        \"\"\"Set the given tolerance as the currently active one.\"\"\""
    , "        assert self._token is None"
    , "        self._token = _tolerance.set(self._value)"
    , ""
    , "    def __exit__(self, _exception_type: object, _exception_value: object, _traceback: object) -> None:"
    , "        \"\"\"Restore the previous tolerance as the currently active one.\"\"\""
    , "        assert self._token is not None"
    , "        _tolerance.reset(self._token)"
    , "        self._token = None"
    , ""
    , "    @staticmethod"
    , "    def current() -> Value:"
    , "        \"\"\"Get the current tolerance value.\"\"\""
    , "        try:"
    , "            return _tolerance.get()"
    , "        except LookupError as error:"
    , "            message = 'No tolerance set, please set one using \"with Tolerance(...)\"'"
    , "            raise LookupError(message) from error"
    , ""
    , "_tolerance : ContextVar[Tolerance.Value] = ContextVar('tolerance')"
    , ""
    , "def _current_tolerance[T](expected_type: type[T]) -> T:"
    , "    current_tolerance = Tolerance.current()"
    , "    if not isinstance(current_tolerance, expected_type):"
    , "        message = 'Expected a tolerance of type ' + expected_type.__name__ + ' but current tolerance is of type ' + type(current_tolerance).__name__"
    , "        raise TypeError(message)"
    , "    return current_tolerance"
    , ""
    , "def _float_tolerance() -> float:"
    , "    return _current_tolerance(float)"
    , ""
    , "def _length_tolerance() -> Length:"
    , "    return _current_tolerance(Length)"
    , ""
    , "def _area_tolerance() -> Area:"
    , "    return _current_tolerance(Area)"
    , ""
    , "def _angle_tolerance() -> Angle:"
    , "    return _current_tolerance(Angle)"
    ]

classDefinition :: Class -> (Text, Text)
classDefinition
  ( Class
      className
      documentation
      maybeUpcast
      constants
      maybeConstructor
      staticFunctions
      properties
      memberFunctions
      equalityFunction
      comparisonFunction
      negationFunction
      absFunction
      preOperators
      postOperators
      nestedClasses
    ) = do
    let (nestedClassDefinitions, nestedClassConstants) = List.unzip2 (List.map classDefinition nestedClasses)
    let pointerFieldName = Python.Class.pointerFieldName className
    let definition =
          Python.lines
            [ "class " <> Python.Class.unqualifiedName className <> parentClass maybeUpcast <> ":"
            , Python.indent [Python.docstring documentation]
            , Python.indent [pointerFieldName <> ": c_void_p"]
            , Python.indent [Python.Constructor.definition className maybeConstructor maybeUpcast]
            , Python.indent
                [ "@staticmethod"
                , "def _new(ptr: c_void_p) -> " <> Python.Class.qualifiedName className <> ":"
                , Python.indent
                    [ "\"\"\"Construct directly from an underlying C pointer.\"\"\""
                    , "obj = object.__new__(" <> Python.Class.qualifiedName className <> ")"
                    , "obj." <> pointerFieldName <> " = ptr"
                    , Python.Upcast.lines className "obj" maybeUpcast
                    , "return obj"
                    ]
                ]
            , Python.indent
                [ "def __del__(self) -> None:"
                , Python.indent
                    [ "\"\"\"Free the underlying Haskell value.\"\"\""
                    , "_lib.opensolid_release(self." <> pointerFieldName <> ")"
                    ]
                , Python.indent $ case maybeUpcast of
                    Just _ -> ["super().__del__()"]
                    Nothing -> []
                ]
            , Python.indent (List.map Python.Constant.declaration constants)
            , Python.indent (List.map (Python.StaticFunction.definition className) staticFunctions)
            , Python.indent (List.map (Python.Property.definition className) properties)
            , Python.indent (List.map (Python.MemberFunction.definition className) memberFunctions)
            , case equalityFunction of
                Just _ -> Python.indent [Python.EqualityFunction.definition className]
                Nothing -> ""
            , case comparisonFunction of
                Just _ -> Python.indent [Python.ComparisonFunction.definitions className]
                Nothing -> ""
            , case negationFunction of
                Just _ -> Python.indent [Python.NegationFunction.definition className]
                Nothing -> ""
            , case absFunction of
                Just _ -> Python.indent [Python.AbsFunction.definition className]
                Nothing -> ""
            , Python.indent (List.map (Python.PostOperator.definition className) postOperators)
            , Python.indent (List.map (Python.PreOperator.definition className) preOperators)
            , Python.indent [extraMemberFunctions (Python.Class.qualifiedName className)]
            , Python.indent nestedClassDefinitions
            ]
    let constantDefinitions =
          Python.lines ((List.map (Python.Constant.definition className) constants) <> nestedClassConstants)
    (definition, constantDefinitions)

parentClass :: Maybe Upcast -> Text
parentClass Nothing = ""
parentClass (Just upcast) = "(" <> Python.Class.qualifiedName (Upcast.parentClassName upcast) <> ")"

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
    "Angle" -> repr ["return 'Angle.radians(' + str(self.in_radians()) + ')'"]
    "Bounds" ->
      repr
        [ "low, high = self.endpoints"
        , "return 'Bounds(' + str(low) + ',' + str(high) + ')'"
        ]
    "LengthBounds" ->
      repr
        [ "low, high = self.endpoints"
        , "return 'LengthBounds(' + repr(low) + ',' + repr(high) + ')'"
        ]
    "AreaBounds" ->
      repr
        [ "low, high = self.endpoints"
        , "return 'AreaBounds(' + repr(low) + ',' + repr(high) + ')'"
        ]
    "AngleBounds" ->
      repr
        [ "low, high = self.endpoints"
        , "return 'AngleBounds(' + repr(low) + ',' + repr(high) + ')'"
        ]
    "Color" ->
      repr
        [ "r, g, b = self.rgb_int_components"
        , "return 'Color.rgb_int(' + str(r) + ',' + str(g) + ',' + str(b) + ')'"
        ]
    "Vector2d" ->
      repr
        [ "x, y = self.components"
        , "return 'Vector2d(' + str(x) + ',' + str(y) + ')'"
        ]
    "Displacement2d" ->
      repr
        [ "x, y = self.components"
        , "return 'Displacement2d(' + repr(x) + ',' + repr(y) + ')'"
        ]
    "AreaVector2d" ->
      repr
        [ "x, y = self.components"
        , "return 'AreaVector2d(' + repr(x) + ',' + repr(y) + ')'"
        ]
    "Point2d" ->
      repr
        [ "x, y = self.coordinates"
        , "return 'Point2d(' + repr(x) + ',' + repr(y) + ')'"
        ]
    "UvPoint" ->
      repr
        [ "x, y = self.coordinates"
        , "return 'UvPoint(' + str(x) + ',' + str(y) + ')'"
        ]
    "Bounds2d" ->
      repr
        [ "x, y = self.coordinates"
        , "return 'Bounds2d(' + repr(x) + ',' + repr(y) + ')'"
        ]
    "UvBounds" ->
      repr
        [ "u, v = self.coordinates"
        , "return 'UvBounds(' + repr(u) + ',' + repr(v) + ')'"
        ]
    _ -> ""

ffiTypeDeclarations :: Text
ffiTypeDeclarations = do
  let registry = List.foldr registerFunctionTypes Python.Type.Registry.empty API.functions
  Python.Type.Registry.typeDeclarations registry

topLevelClassName :: Class -> Maybe Text
topLevelClassName class_ = do
  let qualifiedName = Python.Class.qualifiedName (Class.name class_)
  if Text.contains "." qualifiedName then Nothing else Just qualifiedName

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
    |> registerArgumentTypes (Function.implicitArgument function) (Function.argumentTypes function)
    |> Python.FFI.registerType (Function.returnType function)

registerArgumentTypes :: Maybe ImplicitArgument -> List FFI.Type -> Registry -> Registry
registerArgumentTypes maybeImplicitArgument argumentTypes registry = do
  let implicitArgumentType implicitArgument =
        Pair.second (Python.Function.implicitValue implicitArgument)
  let maybeImplicitArgumentType = Maybe.map implicitArgumentType maybeImplicitArgument
  case List.maybe maybeImplicitArgumentType <> argumentTypes of
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
