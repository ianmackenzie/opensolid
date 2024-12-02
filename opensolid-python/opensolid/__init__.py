"""A collection of classes for 2D/3D geometric modelling."""

from __future__ import annotations

import ctypes
import platform
from ctypes import (
    CDLL,
    POINTER,
    Structure,
    Union,
    c_char_p,
    c_double,
    c_int64,
    c_size_t,
    c_void_p,
)
from pathlib import Path
from typing import Any, overload

# Load the native library, assuming it's located in the same directory as __init__.py
_lib_dir = Path(__file__).parent
match platform.system():
    case "Windows":
        _load_path = _lib_dir / "opensolid-ffi.dll"
    case "Darwin":
        _load_path = _lib_dir / "libopensolid-ffi.dylib"
    case "Linux":
        _load_path = _lib_dir / "libopensolid-ffi.so"
    case unsupported_system:
        raise OSError(unsupported_system + " is not yet supported")
_lib: CDLL = ctypes.cdll.LoadLibrary(str(_load_path))

# Define the signatures of the C API functions
# (also an early sanity check to make sure the library has been loaded OK)
_lib.opensolid_init.argtypes = []
_lib.opensolid_malloc.argtypes = [c_size_t]
_lib.opensolid_malloc.restype = c_void_p
_lib.opensolid_free.argtypes = [c_void_p]
_lib.opensolid_release.argtypes = [c_void_p]

# Initialize the Haskell runtime
_lib.opensolid_init()


class Error(Exception):
    pass


class _ErrorMessage(Union):
    _fields_ = (("as_char", c_char_p), ("as_void", c_void_p))


def _error(output: Any) -> Any:  # noqa: ANN401
    message = output.field1.as_char.decode("utf-8")
    _lib.opensolid_free(output.field1.as_void)
    raise Error(message)


class Length:
    """A length, stored as a value in meters."""

    def __init__(self, value: float) -> None:
        """Create a Length from a float value in meters."""
        self.value = value


class Angle:
    """An angle, stored as a value in radians."""

    def __init__(self, value: float) -> None:
        """Create an Angle from a float value in radians."""
        self.value = value


class Tolerance:
    """Manages a global tolerance value."""

    current: float | Length | Angle | None = None

    def __init__(self, value: float | Length | Angle | None) -> None:
        self.value = value
        self.saved = None

    def __enter__(self) -> None:
        self.saved = Tolerance.current
        Tolerance.current = self.value

    def __exit__(
        self, _exception_type: object, _exception_value: object, _traceback: object
    ) -> None:
        Tolerance.current = self.saved
        self.saved = None


def _float_tolerance() -> float:
    if isinstance(Tolerance.current, float):
        return Tolerance.current
    if Tolerance.current is None:
        message = "No float tolerance set, please set one using 'with Tolerance(...)'"
        raise TypeError(message)
    message = (
        "Expected a tolerance of type float but current tolerance is of type "
        + type(Tolerance.current).__name__
    )
    raise TypeError(message)


def _length_tolerance() -> Length:
    if isinstance(Tolerance.current, Length):
        return Tolerance.current
    if Tolerance.current is None:
        message = "No length tolerance set, please set one using 'with Tolerance(...)'"
        raise TypeError(message)
    message = (
        "Expected a tolerance of type Length but current tolerance is of type "
        + type(Tolerance.current).__name__
    )
    raise TypeError(message)


def _angle_tolerance() -> Angle:
    if isinstance(Tolerance.current, Angle):
        return Tolerance.current
    if Tolerance.current is None:
        message = "No angle tolerance set, please set one using 'with Tolerance(...)'"
        raise TypeError(message)
    message = (
        "Expected a tolerance of type Angle but current tolerance is of type "
        + type(Tolerance.current).__name__
    )
    raise TypeError(message)


class _Tuple2_c_double_c_void_p(Structure):
    _fields_ = [("field0", c_double), ("field1", c_void_p)]


class _List_c_void_p(Structure):
    _fields_ = [("field0", c_int64), ("field1", POINTER(c_void_p))]


class _Result_List_c_void_p(Structure):
    _fields_ = [
        ("field0", c_int64),
        ("field1", _ErrorMessage),
        ("field2", _List_c_void_p),
    ]


class _Tuple2_c_void_p_c_void_p(Structure):
    _fields_ = [("field0", c_void_p), ("field1", c_void_p)]


class _Tuple2_c_void_p_c_double(Structure):
    _fields_ = [("field0", c_void_p), ("field1", c_double)]


class _Tuple2_c_double_c_double(Structure):
    _fields_ = [("field0", c_double), ("field1", c_double)]


class _Result_c_void_p(Structure):
    _fields_ = [("field0", c_int64), ("field1", _ErrorMessage), ("field2", c_void_p)]


class _Maybe_c_void_p(Structure):
    _fields_ = [("field0", c_int64), ("field1", c_void_p)]


class _Tuple3_c_void_p_c_void_p_c_void_p(Structure):
    _fields_ = [("field0", c_void_p), ("field1", c_void_p), ("field2", c_void_p)]


class Range:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    @staticmethod
    def unit() -> Range_Unitless:
        inputs = c_void_p()
        output = c_void_p()
        _lib.opensolid_Range_unit(ctypes.byref(inputs), ctypes.byref(output))
        return Range_Unitless(ptr=output)

    @overload
    @staticmethod
    def constant(value: float) -> Range_Unitless:
        pass

    @overload
    @staticmethod
    def constant(value: Length) -> Range_Meters:
        pass

    @staticmethod
    def constant(*args, **keywords):
        match (args, keywords):
            case (
                ([float() | int() as value], {})
                | ([], {"value": float() | int() as value})
            ):
                inputs = c_double(value)
                output = c_void_p()
                _lib.opensolid_Range_constant_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case ([Length() as value], {}) | ([], {"value": Length() as value}):
                inputs = c_double(value.value)
                output = c_void_p()
                _lib.opensolid_Range_constant_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def from_endpoints(a: float, b: float) -> Range_Unitless:
        pass

    @overload
    @staticmethod
    def from_endpoints(a: Length, b: Length) -> Range_Meters:
        pass

    @staticmethod
    def from_endpoints(*args, **keywords):
        match (args, keywords):
            case (
                ([float() | int() as a, float() | int() as b], {})
                | ([], {"a": float() | int() as a, "b": float() | int() as b})
            ):
                inputs = _Tuple2_c_double_c_double(a, b)
                output = c_void_p()
                _lib.opensolid_Range_fromEndpoints_Float_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case (
                ([Length() as a, Length() as b], {})
                | ([], {"a": Length() as a, "b": Length() as b})
            ):
                inputs = _Tuple2_c_double_c_double(a.value, b.value)
                output = c_void_p()
                _lib.opensolid_Range_fromEndpoints_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def aggregate(a: Range_Unitless, b: Range_Unitless) -> Range_Unitless:
        pass

    @overload
    @staticmethod
    def aggregate(
        a: Range_Unitless, b: Range_Unitless, c: Range_Unitless
    ) -> Range_Unitless:
        pass

    @overload
    @staticmethod
    def aggregate(a: Range_Meters, b: Range_Meters) -> Range_Meters:
        pass

    @overload
    @staticmethod
    def aggregate(a: Range_Meters, b: Range_Meters, c: Range_Meters) -> Range_Meters:
        pass

    @staticmethod
    def aggregate(*args, **keywords):
        match (args, keywords):
            case (
                ([Range_Unitless() as a, Range_Unitless() as b], {})
                | ([], {"a": Range_Unitless() as a, "b": Range_Unitless() as b})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(a.__ptr__, b.__ptr__)
                output = c_void_p()
                _lib.opensolid_Range_aggregate_RangeUnitless_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case (
                (
                    [
                        Range_Unitless() as a,
                        Range_Unitless() as b,
                        Range_Unitless() as c,
                    ],
                    {},
                )
                | (
                    [],
                    {
                        "a": Range_Unitless() as a,
                        "b": Range_Unitless() as b,
                        "c": Range_Unitless() as c,
                    },
                )
            ):
                inputs = _Tuple3_c_void_p_c_void_p_c_void_p(
                    a.__ptr__, b.__ptr__, c.__ptr__
                )
                output = c_void_p()
                _lib.opensolid_Range_aggregate_RangeUnitless_RangeUnitless_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case (
                ([Range_Meters() as a, Range_Meters() as b], {})
                | ([], {"a": Range_Meters() as a, "b": Range_Meters() as b})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(a.__ptr__, b.__ptr__)
                output = c_void_p()
                _lib.opensolid_Range_aggregate_RangeMeters_RangeMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case (
                ([Range_Meters() as a, Range_Meters() as b, Range_Meters() as c], {})
                | (
                    [],
                    {
                        "a": Range_Meters() as a,
                        "b": Range_Meters() as b,
                        "c": Range_Meters() as c,
                    },
                )
            ):
                inputs = _Tuple3_c_void_p_c_void_p_c_void_p(
                    a.__ptr__, b.__ptr__, c.__ptr__
                )
                output = c_void_p()
                _lib.opensolid_Range_aggregate_RangeMeters_RangeMeters_RangeMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)


class Range_Unitless:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def endpoints(self) -> tuple[float, float]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_RangeUnitless_endpoints(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (output.field0, output.field1)

    def intersection(self, other: Range_Unitless) -> Range_Unitless | None:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = _Maybe_c_void_p()
        _lib.opensolid_RangeUnitless_intersection_RangeUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            Range_Unitless(ptr=c_void_p(output.field1)) if output.field0 == 0 else None
        )

    def __neg__(self) -> Range_Unitless:
        output = c_void_p()
        _lib.opensolid_RangeUnitless_neg(
            ctypes.byref(self.__ptr__), ctypes.byref(output)
        )
        return Range_Unitless(ptr=output)

    @overload
    def __add__(self, rhs: float) -> Range_Unitless:
        pass

    @overload
    def __add__(self, rhs: Range_Unitless) -> Range_Unitless:
        pass

    def __add__(self, *args, **keywords):
        match (args, keywords):
            case ([float() | int() as rhs], {}) | ([], {"rhs": float() | int() as rhs}):
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_add_RangeUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case (
                ([Range_Unitless() as rhs], {})
                | ([], {"rhs": Range_Unitless() as rhs})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_add_RangeUnitless_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: float) -> Range_Unitless:
        pass

    @overload
    def __sub__(self, rhs: Range_Unitless) -> Range_Unitless:
        pass

    def __sub__(self, *args, **keywords):
        match (args, keywords):
            case ([float() | int() as rhs], {}) | ([], {"rhs": float() | int() as rhs}):
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_sub_RangeUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case (
                ([Range_Unitless() as rhs], {})
                | ([], {"rhs": Range_Unitless() as rhs})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_sub_RangeUnitless_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __mul__(self, rhs: float) -> Range_Unitless:
        pass

    @overload
    def __mul__(self, rhs: Range_Unitless) -> Range_Unitless:
        pass

    @overload
    def __mul__(self, rhs: Length) -> Range_Meters:
        pass

    def __mul__(self, *args, **keywords):
        match (args, keywords):
            case ([float() | int() as rhs], {}) | ([], {"rhs": float() | int() as rhs}):
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_mul_RangeUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case (
                ([Range_Unitless() as rhs], {})
                | ([], {"rhs": Range_Unitless() as rhs})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_mul_RangeUnitless_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case ([Length() as rhs], {}) | ([], {"rhs": Length() as rhs}):
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs.value)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_mul_RangeUnitless_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Range_Unitless:
        pass

    @overload
    def __truediv__(self, rhs: Range_Unitless) -> Range_Unitless:
        pass

    def __truediv__(self, *args, **keywords):
        match (args, keywords):
            case ([float() | int() as rhs], {}) | ([], {"rhs": float() | int() as rhs}):
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_div_RangeUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case (
                ([Range_Unitless() as rhs], {})
                | ([], {"rhs": Range_Unitless() as rhs})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_div_RangeUnitless_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case _:
                return NotImplemented

    def __radd__(self, lhs: float) -> Range_Unitless:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_RangeUnitless_add_Float_RangeUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Unitless(ptr=output)

    def __rsub__(self, lhs: float) -> Range_Unitless:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_RangeUnitless_sub_Float_RangeUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Unitless(ptr=output)

    @overload
    def __rmul__(self, lhs: float) -> Range_Unitless:
        pass

    @overload
    def __rmul__(self, lhs: Length) -> Range_Meters:
        pass

    def __rmul__(self, *args, **keywords):
        match (args, keywords):
            case ([float() | int() as lhs], {}) | ([], {"lhs": float() | int() as lhs}):
                inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_mul_Float_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case ([Length() as lhs], {}) | ([], {"lhs": Length() as lhs}):
                inputs = _Tuple2_c_double_c_void_p(lhs.value, self.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_mul_Length_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case _:
                return NotImplemented

    def __rtruediv__(self, lhs: float) -> Range_Unitless:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_RangeUnitless_div_Float_RangeUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Unitless(ptr=output)


class Range_Meters:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def endpoints(self) -> tuple[Length, Length]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_RangeMeters_endpoints(ctypes.byref(inputs), ctypes.byref(output))
        return (Length(output.field0), Length(output.field1))

    def intersection(self, other: Range_Meters) -> Range_Meters | None:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = _Maybe_c_void_p()
        _lib.opensolid_RangeMeters_intersection_RangeMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Meters(ptr=c_void_p(output.field1)) if output.field0 == 0 else None

    def __neg__(self) -> Range_Meters:
        output = c_void_p()
        _lib.opensolid_RangeMeters_neg(ctypes.byref(self.__ptr__), ctypes.byref(output))
        return Range_Meters(ptr=output)


class Vector2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    @staticmethod
    def zero() -> Vector2d_Meters:
        inputs = c_void_p()
        output = c_void_p()
        _lib.opensolid_Vector2d_zero(ctypes.byref(inputs), ctypes.byref(output))
        return Vector2d_Meters(ptr=output)

    @staticmethod
    def unit(direction: Direction2d) -> Vector2d_Unitless:
        inputs = direction.__ptr__
        output = c_void_p()
        _lib.opensolid_Vector2d_unit_Direction2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Unitless(ptr=output)

    @overload
    @staticmethod
    def xy(x_component: float, y_component: float) -> Vector2d_Unitless:
        pass

    @overload
    @staticmethod
    def xy(x_component: Length, y_component: Length) -> Vector2d_Meters:
        pass

    @staticmethod
    def xy(*args, **keywords):
        match (args, keywords):
            case (
                ([float() | int() as x_component, float() | int() as y_component], {})
                | (
                    [],
                    {
                        "x_component": float() | int() as x_component,
                        "y_component": float() | int() as y_component,
                    },
                )
            ):
                inputs = _Tuple2_c_double_c_double(x_component, y_component)
                output = c_void_p()
                _lib.opensolid_Vector2d_xy_Float_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Unitless(ptr=output)
            case (
                ([Length() as x_component, Length() as y_component], {})
                | (
                    [],
                    {
                        "x_component": Length() as x_component,
                        "y_component": Length() as y_component,
                    },
                )
            ):
                inputs = _Tuple2_c_double_c_double(x_component.value, y_component.value)
                output = c_void_p()
                _lib.opensolid_Vector2d_xy_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Meters(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def x(x_component: float) -> Vector2d_Unitless:
        pass

    @overload
    @staticmethod
    def x(x_component: Length) -> Vector2d_Meters:
        pass

    @staticmethod
    def x(*args, **keywords):
        match (args, keywords):
            case (
                ([float() | int() as x_component], {})
                | ([], {"x_component": float() | int() as x_component})
            ):
                inputs = c_double(x_component)
                output = c_void_p()
                _lib.opensolid_Vector2d_x_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Unitless(ptr=output)
            case (
                ([Length() as x_component], {})
                | ([], {"x_component": Length() as x_component})
            ):
                inputs = c_double(x_component.value)
                output = c_void_p()
                _lib.opensolid_Vector2d_x_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Meters(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def y(y_component: float) -> Vector2d_Unitless:
        pass

    @overload
    @staticmethod
    def y(y_component: Length) -> Vector2d_Meters:
        pass

    @staticmethod
    def y(*args, **keywords):
        match (args, keywords):
            case (
                ([float() | int() as y_component], {})
                | ([], {"y_component": float() | int() as y_component})
            ):
                inputs = c_double(y_component)
                output = c_void_p()
                _lib.opensolid_Vector2d_y_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Unitless(ptr=output)
            case (
                ([Length() as y_component], {})
                | ([], {"y_component": Length() as y_component})
            ):
                inputs = c_double(y_component.value)
                output = c_void_p()
                _lib.opensolid_Vector2d_y_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Meters(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def from_components(components: tuple[float, float]) -> Vector2d_Unitless:
        pass

    @overload
    @staticmethod
    def from_components(components: tuple[Length, Length]) -> Vector2d_Meters:
        pass

    @staticmethod
    def from_components(*args, **keywords):
        match (args, keywords):
            case (
                ([(float() | int(), float() | int()) as components], {})
                | ([], {"components": (float() | int(), float() | int()) as components})
            ):
                inputs = _Tuple2_c_double_c_double(components[0], components[1])
                output = c_void_p()
                _lib.opensolid_Vector2d_fromComponents_Tuple2FloatFloat(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Unitless(ptr=output)
            case (
                ([(Length(), Length()) as components], {})
                | ([], {"components": (Length(), Length()) as components})
            ):
                inputs = _Tuple2_c_double_c_double(
                    components[0].value, components[1].value
                )
                output = c_void_p()
                _lib.opensolid_Vector2d_fromComponents_Tuple2LengthLength(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Meters(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)


class Vector2d_Unitless:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def components(self) -> tuple[float, float]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_Vector2dUnitless_components(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (output.field0, output.field1)

    def direction(self) -> Direction2d:
        inputs = _Tuple2_c_double_c_void_p(_float_tolerance(), self.__ptr__)
        output = _Result_c_void_p()
        _lib.opensolid_Vector2dUnitless_direction(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            Direction2d(ptr=c_void_p(output.field2))
            if output.field0 == 0
            else _error(output)
        )


class Vector2d_Meters:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def components(self) -> tuple[Length, Length]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_Vector2dMeters_components(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (Length(output.field0), Length(output.field1))

    def direction(self) -> Direction2d:
        inputs = _Tuple2_c_double_c_void_p(_length_tolerance().value, self.__ptr__)
        output = _Result_c_void_p()
        _lib.opensolid_Vector2dMeters_direction(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            Direction2d(ptr=c_void_p(output.field2))
            if output.field0 == 0
            else _error(output)
        )


class Direction2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    @staticmethod
    def x() -> Direction2d:
        inputs = c_void_p()
        output = c_void_p()
        _lib.opensolid_Direction2d_x(ctypes.byref(inputs), ctypes.byref(output))
        return Direction2d(ptr=output)

    @staticmethod
    def y() -> Direction2d:
        inputs = c_void_p()
        output = c_void_p()
        _lib.opensolid_Direction2d_y(ctypes.byref(inputs), ctypes.byref(output))
        return Direction2d(ptr=output)

    @staticmethod
    def positive_x() -> Direction2d:
        inputs = c_void_p()
        output = c_void_p()
        _lib.opensolid_Direction2d_positiveX(ctypes.byref(inputs), ctypes.byref(output))
        return Direction2d(ptr=output)

    @staticmethod
    def positive_y() -> Direction2d:
        inputs = c_void_p()
        output = c_void_p()
        _lib.opensolid_Direction2d_positiveY(ctypes.byref(inputs), ctypes.byref(output))
        return Direction2d(ptr=output)

    @staticmethod
    def negative_x() -> Direction2d:
        inputs = c_void_p()
        output = c_void_p()
        _lib.opensolid_Direction2d_negativeX(ctypes.byref(inputs), ctypes.byref(output))
        return Direction2d(ptr=output)

    @staticmethod
    def negative_y() -> Direction2d:
        inputs = c_void_p()
        output = c_void_p()
        _lib.opensolid_Direction2d_negativeY(ctypes.byref(inputs), ctypes.byref(output))
        return Direction2d(ptr=output)

    @staticmethod
    def from_angle(angle: Angle) -> Direction2d:
        inputs = c_double(angle.value)
        output = c_void_p()
        _lib.opensolid_Direction2d_fromAngle_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Direction2d(ptr=output)

    def to_angle(self) -> Angle:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Direction2d_toAngle(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(output.value)


class Point2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    @staticmethod
    def origin() -> Point2d_Meters:
        inputs = c_void_p()
        output = c_void_p()
        _lib.opensolid_Point2d_origin(ctypes.byref(inputs), ctypes.byref(output))
        return Point2d_Meters(ptr=output)

    @overload
    @staticmethod
    def xy(x_coordinate: float, y_coordinate: float) -> Point2d_Unitless:
        pass

    @overload
    @staticmethod
    def xy(x_coordinate: Length, y_coordinate: Length) -> Point2d_Meters:
        pass

    @staticmethod
    def xy(*args, **keywords):
        match (args, keywords):
            case (
                ([float() | int() as x_coordinate, float() | int() as y_coordinate], {})
                | (
                    [],
                    {
                        "x_coordinate": float() | int() as x_coordinate,
                        "y_coordinate": float() | int() as y_coordinate,
                    },
                )
            ):
                inputs = _Tuple2_c_double_c_double(x_coordinate, y_coordinate)
                output = c_void_p()
                _lib.opensolid_Point2d_xy_Float_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Point2d_Unitless(ptr=output)
            case (
                ([Length() as x_coordinate, Length() as y_coordinate], {})
                | (
                    [],
                    {
                        "x_coordinate": Length() as x_coordinate,
                        "y_coordinate": Length() as y_coordinate,
                    },
                )
            ):
                inputs = _Tuple2_c_double_c_double(
                    x_coordinate.value, y_coordinate.value
                )
                output = c_void_p()
                _lib.opensolid_Point2d_xy_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Point2d_Meters(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def x(x_coordinate: float) -> Point2d_Unitless:
        pass

    @overload
    @staticmethod
    def x(x_coordinate: Length) -> Point2d_Meters:
        pass

    @staticmethod
    def x(*args, **keywords):
        match (args, keywords):
            case (
                ([float() | int() as x_coordinate], {})
                | ([], {"x_coordinate": float() | int() as x_coordinate})
            ):
                inputs = c_double(x_coordinate)
                output = c_void_p()
                _lib.opensolid_Point2d_x_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Point2d_Unitless(ptr=output)
            case (
                ([Length() as x_coordinate], {})
                | ([], {"x_coordinate": Length() as x_coordinate})
            ):
                inputs = c_double(x_coordinate.value)
                output = c_void_p()
                _lib.opensolid_Point2d_x_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Point2d_Meters(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def y(y_coordinate: float) -> Point2d_Unitless:
        pass

    @overload
    @staticmethod
    def y(y_coordinate: Length) -> Point2d_Meters:
        pass

    @staticmethod
    def y(*args, **keywords):
        match (args, keywords):
            case (
                ([float() | int() as y_coordinate], {})
                | ([], {"y_coordinate": float() | int() as y_coordinate})
            ):
                inputs = c_double(y_coordinate)
                output = c_void_p()
                _lib.opensolid_Point2d_y_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Point2d_Unitless(ptr=output)
            case (
                ([Length() as y_coordinate], {})
                | ([], {"y_coordinate": Length() as y_coordinate})
            ):
                inputs = c_double(y_coordinate.value)
                output = c_void_p()
                _lib.opensolid_Point2d_y_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Point2d_Meters(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def from_coordinates(coordinates: tuple[float, float]) -> Point2d_Unitless:
        pass

    @overload
    @staticmethod
    def from_coordinates(coordinates: tuple[Length, Length]) -> Point2d_Meters:
        pass

    @staticmethod
    def from_coordinates(*args, **keywords):
        match (args, keywords):
            case (
                ([(float() | int(), float() | int()) as coordinates], {})
                | (
                    [],
                    {"coordinates": (float() | int(), float() | int()) as coordinates},
                )
            ):
                inputs = _Tuple2_c_double_c_double(coordinates[0], coordinates[1])
                output = c_void_p()
                _lib.opensolid_Point2d_fromCoordinates_Tuple2FloatFloat(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Point2d_Unitless(ptr=output)
            case (
                ([(Length(), Length()) as coordinates], {})
                | ([], {"coordinates": (Length(), Length()) as coordinates})
            ):
                inputs = _Tuple2_c_double_c_double(
                    coordinates[0].value, coordinates[1].value
                )
                output = c_void_p()
                _lib.opensolid_Point2d_fromCoordinates_Tuple2LengthLength(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Point2d_Meters(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)


class Point2d_Unitless:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def coordinates(self) -> tuple[float, float]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_Point2dUnitless_coordinates(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (output.field0, output.field1)

    def distance_to(self, other: Point2d_Unitless) -> float:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_double()
        _lib.opensolid_Point2dUnitless_distanceTo_Point2dUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def midpoint(self, other: Point2d_Unitless) -> Point2d_Unitless:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Point2dUnitless_midpoint_Point2dUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d_Unitless(ptr=output)


class Point2d_Meters:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def coordinates(self) -> tuple[Length, Length]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_Point2dMeters_coordinates(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (Length(output.field0), Length(output.field1))

    def distance_to(self, other: Point2d_Meters) -> Length:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_double()
        _lib.opensolid_Point2dMeters_distanceTo_Point2dMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(output.value)

    def midpoint(self, other: Point2d_Meters) -> Point2d_Meters:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Point2dMeters_midpoint_Point2dMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d_Meters(ptr=output)


class Curve1d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    @staticmethod
    def t() -> Curve1d_Unitless:
        inputs = c_void_p()
        output = c_void_p()
        _lib.opensolid_Curve1d_t(ctypes.byref(inputs), ctypes.byref(output))
        return Curve1d_Unitless(ptr=output)

    @overload
    @staticmethod
    def sin(curve: Curve1d_Radians) -> Curve1d_Unitless:
        pass

    @overload
    @staticmethod
    def sin(curve: Curve1d_Unitless) -> Curve1d_Unitless:
        pass

    @staticmethod
    def sin(*args, **keywords):
        match (args, keywords):
            case (
                ([Curve1d_Radians() as curve], {})
                | ([], {"curve": Curve1d_Radians() as curve})
            ):
                inputs = curve.__ptr__
                output = c_void_p()
                _lib.opensolid_Curve1d_sin_Curve1dRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case (
                ([Curve1d_Unitless() as curve], {})
                | ([], {"curve": Curve1d_Unitless() as curve})
            ):
                inputs = curve.__ptr__
                output = c_void_p()
                _lib.opensolid_Curve1d_sin_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def cos(curve: Curve1d_Radians) -> Curve1d_Unitless:
        pass

    @overload
    @staticmethod
    def cos(curve: Curve1d_Unitless) -> Curve1d_Unitless:
        pass

    @staticmethod
    def cos(*args, **keywords):
        match (args, keywords):
            case (
                ([Curve1d_Radians() as curve], {})
                | ([], {"curve": Curve1d_Radians() as curve})
            ):
                inputs = curve.__ptr__
                output = c_void_p()
                _lib.opensolid_Curve1d_cos_Curve1dRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case (
                ([Curve1d_Unitless() as curve], {})
                | ([], {"curve": Curve1d_Unitless() as curve})
            ):
                inputs = curve.__ptr__
                output = c_void_p()
                _lib.opensolid_Curve1d_cos_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @staticmethod
    def sqrt(curve: Curve1d_Unitless) -> Curve1d_Unitless:
        inputs = curve.__ptr__
        output = c_void_p()
        _lib.opensolid_Curve1d_sqrt_Curve1dUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Unitless(ptr=output)

    class Root:
        def __init__(self, *, ptr: c_void_p) -> None:
            self.__ptr__ = ptr

        def value(self) -> float:
            inputs = self.__ptr__
            output = c_double()
            _lib.opensolid_Curve1dRoot_value(ctypes.byref(inputs), ctypes.byref(output))
            return output.value

        def order(self) -> int:
            inputs = self.__ptr__
            output = c_int64()
            _lib.opensolid_Curve1dRoot_order(ctypes.byref(inputs), ctypes.byref(output))
            return output.value

        def sign(self) -> int:
            inputs = self.__ptr__
            output = c_int64()
            _lib.opensolid_Curve1dRoot_sign(ctypes.byref(inputs), ctypes.byref(output))
            return output.value


class Curve1d_Unitless:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def squared(self) -> Curve1d_Unitless:
        inputs = self.__ptr__
        output = c_void_p()
        _lib.opensolid_Curve1dUnitless_squared(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Unitless(ptr=output)

    def evaluate(self, parameter_value: float) -> float:
        inputs = _Tuple2_c_double_c_void_p(parameter_value, self.__ptr__)
        output = c_double()
        _lib.opensolid_Curve1dUnitless_evaluate_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def zeros(self) -> list[Curve1d.Root]:
        inputs = _Tuple2_c_double_c_void_p(_float_tolerance(), self.__ptr__)
        output = _Result_List_c_void_p()
        _lib.opensolid_Curve1dUnitless_zeros(ctypes.byref(inputs), ctypes.byref(output))
        return (
            [
                Curve1d.Root(ptr=c_void_p(item))
                for item in [
                    output.field2.field1[index] for index in range(output.field2.field0)
                ]
            ]
            if output.field0 == 0
            else _error(output)
        )

    def __neg__(self) -> Curve1d_Unitless:
        output = c_void_p()
        _lib.opensolid_Curve1dUnitless_neg(
            ctypes.byref(self.__ptr__), ctypes.byref(output)
        )
        return Curve1d_Unitless(ptr=output)

    @overload
    def __add__(self, rhs: float) -> Curve1d_Unitless:
        pass

    @overload
    def __add__(self, rhs: Curve1d_Unitless) -> Curve1d_Unitless:
        pass

    def __add__(self, *args, **keywords):
        match (args, keywords):
            case ([float() | int() as rhs], {}) | ([], {"rhs": float() | int() as rhs}):
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_add_Curve1dUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case (
                ([Curve1d_Unitless() as rhs], {})
                | ([], {"rhs": Curve1d_Unitless() as rhs})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_add_Curve1dUnitless_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: float) -> Curve1d_Unitless:
        pass

    @overload
    def __sub__(self, rhs: Curve1d_Unitless) -> Curve1d_Unitless:
        pass

    def __sub__(self, *args, **keywords):
        match (args, keywords):
            case ([float() | int() as rhs], {}) | ([], {"rhs": float() | int() as rhs}):
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_sub_Curve1dUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case (
                ([Curve1d_Unitless() as rhs], {})
                | ([], {"rhs": Curve1d_Unitless() as rhs})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_sub_Curve1dUnitless_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __mul__(self, rhs: float) -> Curve1d_Unitless:
        pass

    @overload
    def __mul__(self, rhs: Curve1d_Unitless) -> Curve1d_Unitless:
        pass

    def __mul__(self, *args, **keywords):
        match (args, keywords):
            case ([float() | int() as rhs], {}) | ([], {"rhs": float() | int() as rhs}):
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_mul_Curve1dUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case (
                ([Curve1d_Unitless() as rhs], {})
                | ([], {"rhs": Curve1d_Unitless() as rhs})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_mul_Curve1dUnitless_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Curve1d_Unitless:
        pass

    @overload
    def __truediv__(self, rhs: Curve1d_Unitless) -> Curve1d_Unitless:
        pass

    def __truediv__(self, *args, **keywords):
        match (args, keywords):
            case ([float() | int() as rhs], {}) | ([], {"rhs": float() | int() as rhs}):
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_div_Curve1dUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case (
                ([Curve1d_Unitless() as rhs], {})
                | ([], {"rhs": Curve1d_Unitless() as rhs})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_div_Curve1dUnitless_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case _:
                return NotImplemented

    def __radd__(self, lhs: float) -> Curve1d_Unitless:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Curve1dUnitless_add_Float_Curve1dUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Unitless(ptr=output)

    def __rsub__(self, lhs: float) -> Curve1d_Unitless:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Curve1dUnitless_sub_Float_Curve1dUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Unitless(ptr=output)

    def __rmul__(self, lhs: float) -> Curve1d_Unitless:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Curve1dUnitless_mul_Float_Curve1dUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Unitless(ptr=output)

    def __rtruediv__(self, lhs: float) -> Curve1d_Unitless:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Curve1dUnitless_div_Float_Curve1dUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Unitless(ptr=output)


class Curve1d_Radians:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def evaluate(self, parameter_value: float) -> Angle:
        inputs = _Tuple2_c_double_c_void_p(parameter_value, self.__ptr__)
        output = c_double()
        _lib.opensolid_Curve1dRadians_evaluate_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Angle(output.value)

    def zeros(self) -> list[Curve1d.Root]:
        inputs = _Tuple2_c_double_c_void_p(_angle_tolerance().value, self.__ptr__)
        output = _Result_List_c_void_p()
        _lib.opensolid_Curve1dRadians_zeros(ctypes.byref(inputs), ctypes.byref(output))
        return (
            [
                Curve1d.Root(ptr=c_void_p(item))
                for item in [
                    output.field2.field1[index] for index in range(output.field2.field0)
                ]
            ]
            if output.field0 == 0
            else _error(output)
        )


class Curve1d_Meters:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def evaluate(self, parameter_value: float) -> Length:
        inputs = _Tuple2_c_double_c_void_p(parameter_value, self.__ptr__)
        output = c_double()
        _lib.opensolid_Curve1dMeters_evaluate_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(output.value)

    def zeros(self) -> list[Curve1d.Root]:
        inputs = _Tuple2_c_double_c_void_p(_length_tolerance().value, self.__ptr__)
        output = _Result_List_c_void_p()
        _lib.opensolid_Curve1dMeters_zeros(ctypes.byref(inputs), ctypes.byref(output))
        return (
            [
                Curve1d.Root(ptr=c_void_p(item))
                for item in [
                    output.field2.field1[index] for index in range(output.field2.field0)
                ]
            ]
            if output.field0 == 0
            else _error(output)
        )
