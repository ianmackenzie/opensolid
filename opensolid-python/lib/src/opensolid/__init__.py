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


class _Text(Union):
    _fields_ = (("as_char", c_char_p), ("as_void", c_void_p))


def _text_to_str(ptr: _Text) -> str:
    decoded = ptr.as_char.decode("utf-8")
    _lib.opensolid_free(ptr.as_void)
    return decoded


def _str_to_text(s: str) -> _Text:
    encoded = s.encode("utf-8")
    buffer = ctypes.create_string_buffer(encoded)
    return _Text(as_char=ctypes.cast(buffer, c_char_p))


def _list_argument(list_type: Any, array: Any) -> Any:  # noqa: ANN401
    return list_type(len(array), array)


def _error(output: Any) -> Any:  # noqa: ANN401
    raise Error(_text_to_str(output.field1))


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


class _List_c_void_p(Structure):
    _fields_ = [("field0", c_int64), ("field1", POINTER(c_void_p))]


class _Tuple3_List_c_void_p_c_void_p_c_void_p(Structure):
    _fields_ = [("field0", _List_c_void_p), ("field1", c_void_p), ("field2", c_void_p)]


class _Tuple2_List_c_void_p_List_c_void_p(Structure):
    _fields_ = [("field0", _List_c_void_p), ("field1", _List_c_void_p)]


class _Tuple2_c_void_p_List_c_void_p(Structure):
    _fields_ = [("field0", c_void_p), ("field1", _List_c_void_p)]


class _Tuple2_c_void_p_c_void_p(Structure):
    _fields_ = [("field0", c_void_p), ("field1", c_void_p)]


class _Tuple2_c_void_p_c_double(Structure):
    _fields_ = [("field0", c_void_p), ("field1", c_double)]


class _Tuple2_c_double_c_void_p(Structure):
    _fields_ = [("field0", c_double), ("field1", c_void_p)]


class _Result_List_c_void_p(Structure):
    _fields_ = [("field0", c_int64), ("field1", _Text), ("field2", _List_c_void_p)]


class _Tuple4_c_void_p_c_void_p_c_void_p_c_void_p(Structure):
    _fields_ = [
        ("field0", c_void_p),
        ("field1", c_void_p),
        ("field2", c_void_p),
        ("field3", c_void_p),
    ]


class _Tuple3_c_void_p_c_void_p_c_void_p(Structure):
    _fields_ = [("field0", c_void_p), ("field1", c_void_p), ("field2", c_void_p)]


class _Tuple2_c_double_c_double(Structure):
    _fields_ = [("field0", c_double), ("field1", c_double)]


class _Result_c_void_p(Structure):
    _fields_ = [("field0", c_int64), ("field1", _Text), ("field2", c_void_p)]


class _Tuple3_c_int64_c_int64_c_int64(Structure):
    _fields_ = [("field0", c_int64), ("field1", c_int64), ("field2", c_int64)]


class _Tuple3_c_double_c_double_c_double(Structure):
    _fields_ = [("field0", c_double), ("field1", c_double), ("field2", c_double)]


class _Tuple3_c_void_p_c_double_c_double(Structure):
    _fields_ = [("field0", c_void_p), ("field1", c_double), ("field2", c_double)]


class _Maybe_c_void_p(Structure):
    _fields_ = [("field0", c_int64), ("field1", c_void_p)]


class Length:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    zero: Length = None  # type: ignore[assignment]

    @staticmethod
    def meters(value: float) -> Length:
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Length_meters_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Length(ptr=output)

    @staticmethod
    def centimeters(value: float) -> Length:
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Length_centimeters_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    @staticmethod
    def millimeters(value: float) -> Length:
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Length_millimeters_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    @staticmethod
    def inches(value: float) -> Length:
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Length_inches_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Length(ptr=output)

    def in_meters(self) -> float:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Length_inMeters(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_centimeters(self) -> float:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Length_inCentimeters(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_millimeters(self) -> float:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Length_inMillimeters(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_inches(self) -> float:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Length_inInches(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Length):
            inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, other.__ptr__)
            output = c_int64()
            _lib.opensolid_Length_eq(ctypes.byref(inputs), ctypes.byref(output))
            return bool(output.value)
        return False

    def _compare(self, other: Length) -> int:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, other.__ptr__)
        output = c_int64()
        _lib.opensolid_Length_compare(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def __lt__(self, other: Length) -> bool:
        return self._compare(other) < 0

    def __le__(self, other: Length) -> bool:
        return self._compare(other) <= 0

    def __ge__(self, other: Length) -> bool:
        return self._compare(other) >= 0

    def __gt__(self, other: Length) -> bool:
        return self._compare(other) > 0

    def __neg__(self) -> Length:
        output = c_void_p()
        _lib.opensolid_Length_neg(ctypes.byref(self.__ptr__), ctypes.byref(output))
        return Length(ptr=output)

    @overload
    def __add__(self, rhs: Length) -> Length:
        pass

    @overload
    def __add__(self, rhs: Range_Meters) -> Range_Meters:
        pass

    @overload
    def __add__(self, rhs: Curve1d_Meters) -> Curve1d_Meters:
        pass

    def __add__(self, rhs):
        match rhs:
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Length_add_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case Range_Meters():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Length_add_Length_RangeMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case Curve1d_Meters():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Length_add_Length_Curve1dMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Meters(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: Length) -> Length:
        pass

    @overload
    def __sub__(self, rhs: Range_Meters) -> Range_Meters:
        pass

    @overload
    def __sub__(self, rhs: Curve1d_Meters) -> Curve1d_Meters:
        pass

    def __sub__(self, rhs):
        match rhs:
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Length_sub_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case Range_Meters():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Length_sub_Length_RangeMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case Curve1d_Meters():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Length_sub_Length_Curve1dMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Meters(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __mul__(self, rhs: float) -> Length:
        pass

    @overload
    def __mul__(self, rhs: Range_Unitless) -> Range_Meters:
        pass

    @overload
    def __mul__(self, rhs: Curve1d_Unitless) -> Curve1d_Meters:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case Range_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case Curve1d_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Meters(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Length:
        pass

    @overload
    def __truediv__(self, rhs: Length) -> float:
        pass

    @overload
    def __truediv__(self, rhs: Range_Unitless) -> Range_Meters:
        pass

    @overload
    def __truediv__(self, rhs: Range_Meters) -> Range_Unitless:
        pass

    @overload
    def __truediv__(self, rhs: Curve1d_Unitless) -> Curve1d_Meters:
        pass

    @overload
    def __truediv__(self, rhs: Curve1d_Meters) -> Curve1d_Unitless:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Length_div_Length_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_double()
                _lib.opensolid_Length_div_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return output.value
            case Range_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Length_div_Length_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case Range_Meters():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Length_div_Length_RangeMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case Curve1d_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Length_div_Length_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Meters(ptr=output)
            case Curve1d_Meters():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Length_div_Length_Curve1dMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case _:
                return NotImplemented

    def __floordiv__(self, rhs: Length) -> int:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
        output = c_int64()
        _lib.opensolid_Length_floorDiv_Length_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def __mod__(self, rhs: Length) -> Length:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
        output = c_void_p()
        _lib.opensolid_Length_mod_Length_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def __rmul__(self, lhs: float) -> Length:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Length_mul_Float_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)


class Angle:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    zero: Angle = None  # type: ignore[assignment]

    @staticmethod
    def radians(value: float) -> Angle:
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Angle_radians_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    @staticmethod
    def degrees(value: float) -> Angle:
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Angle_degrees_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    @staticmethod
    def turns(value: float) -> Angle:
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Angle_turns_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    def in_radians(self) -> float:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Angle_inRadians(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_degrees(self) -> float:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Angle_inDegrees(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_turns(self) -> float:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Angle_inTurns(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Angle):
            inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, other.__ptr__)
            output = c_int64()
            _lib.opensolid_Angle_eq(ctypes.byref(inputs), ctypes.byref(output))
            return bool(output.value)
        return False

    def _compare(self, other: Angle) -> int:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, other.__ptr__)
        output = c_int64()
        _lib.opensolid_Angle_compare(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def __lt__(self, other: Angle) -> bool:
        return self._compare(other) < 0

    def __le__(self, other: Angle) -> bool:
        return self._compare(other) <= 0

    def __ge__(self, other: Angle) -> bool:
        return self._compare(other) >= 0

    def __gt__(self, other: Angle) -> bool:
        return self._compare(other) > 0

    def __neg__(self) -> Angle:
        output = c_void_p()
        _lib.opensolid_Angle_neg(ctypes.byref(self.__ptr__), ctypes.byref(output))
        return Angle(ptr=output)

    @overload
    def __add__(self, rhs: Angle) -> Angle:
        pass

    @overload
    def __add__(self, rhs: Range_Radians) -> Range_Radians:
        pass

    @overload
    def __add__(self, rhs: Curve1d_Radians) -> Curve1d_Radians:
        pass

    def __add__(self, rhs):
        match rhs:
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Angle_add_Angle_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Angle(ptr=output)
            case Range_Radians():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Angle_add_Angle_RangeRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
            case Curve1d_Radians():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Angle_add_Angle_Curve1dRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Radians(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: Angle) -> Angle:
        pass

    @overload
    def __sub__(self, rhs: Range_Radians) -> Range_Radians:
        pass

    @overload
    def __sub__(self, rhs: Curve1d_Radians) -> Curve1d_Radians:
        pass

    def __sub__(self, rhs):
        match rhs:
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Angle_sub_Angle_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Angle(ptr=output)
            case Range_Radians():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Angle_sub_Angle_RangeRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
            case Curve1d_Radians():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Angle_sub_Angle_Curve1dRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Radians(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __mul__(self, rhs: float) -> Angle:
        pass

    @overload
    def __mul__(self, rhs: Range_Unitless) -> Range_Radians:
        pass

    @overload
    def __mul__(self, rhs: Curve1d_Unitless) -> Curve1d_Radians:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Angle_mul_Angle_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Angle(ptr=output)
            case Range_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Angle_mul_Angle_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
            case Curve1d_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Angle_mul_Angle_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Radians(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Angle:
        pass

    @overload
    def __truediv__(self, rhs: Angle) -> float:
        pass

    @overload
    def __truediv__(self, rhs: Range_Unitless) -> Range_Radians:
        pass

    @overload
    def __truediv__(self, rhs: Range_Radians) -> Range_Unitless:
        pass

    @overload
    def __truediv__(self, rhs: Curve1d_Unitless) -> Curve1d_Radians:
        pass

    @overload
    def __truediv__(self, rhs: Curve1d_Radians) -> Curve1d_Unitless:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Angle_div_Angle_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Angle(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_double()
                _lib.opensolid_Angle_div_Angle_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return output.value
            case Range_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Angle_div_Angle_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
            case Range_Radians():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Angle_div_Angle_RangeRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case Curve1d_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Angle_div_Angle_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Radians(ptr=output)
            case Curve1d_Radians():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Angle_div_Angle_Curve1dRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case _:
                return NotImplemented

    def __floordiv__(self, rhs: Angle) -> int:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
        output = c_int64()
        _lib.opensolid_Angle_floorDiv_Angle_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def __mod__(self, rhs: Angle) -> Angle:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
        output = c_void_p()
        _lib.opensolid_Angle_mod_Angle_Angle(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    def __rmul__(self, lhs: float) -> Angle:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Angle_mul_Float_Angle(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)


class Range:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    unit: Range_Unitless = None  # type: ignore[assignment]

    @overload
    @staticmethod
    def constant(value: Length) -> Range_Meters:
        pass

    @overload
    @staticmethod
    def constant(value: Angle) -> Range_Radians:
        pass

    @overload
    @staticmethod
    def constant(value: float) -> Range_Unitless:
        pass

    @staticmethod
    def constant(*args, **keywords):
        match (args, keywords):
            case ([Length() as value], {}) | ([], {"value": Length() as value}):
                inputs = value.__ptr__
                output = c_void_p()
                _lib.opensolid_Range_constant_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case ([Angle() as value], {}) | ([], {"value": Angle() as value}):
                inputs = value.__ptr__
                output = c_void_p()
                _lib.opensolid_Range_constant_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
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
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def from_endpoints(a: Length, b: Length) -> Range_Meters:
        pass

    @overload
    @staticmethod
    def from_endpoints(a: Angle, b: Angle) -> Range_Radians:
        pass

    @overload
    @staticmethod
    def from_endpoints(a: float, b: float) -> Range_Unitless:
        pass

    @staticmethod
    def from_endpoints(*args, **keywords):
        match (args, keywords):
            case (
                ([Length() as a, Length() as b], {})
                | ([Length() as a], {"b": Length() as b})
                | ([], {"a": Length() as a, "b": Length() as b})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(a.__ptr__, b.__ptr__)
                output = c_void_p()
                _lib.opensolid_Range_fromEndpoints_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case (
                ([Angle() as a, Angle() as b], {})
                | ([Angle() as a], {"b": Angle() as b})
                | ([], {"a": Angle() as a, "b": Angle() as b})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(a.__ptr__, b.__ptr__)
                output = c_void_p()
                _lib.opensolid_Range_fromEndpoints_Angle_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
            case (
                ([float() | int() as a, float() | int() as b], {})
                | ([float() | int() as a], {"b": float() | int() as b})
                | ([], {"a": float() | int() as a, "b": float() | int() as b})
            ):
                inputs = _Tuple2_c_double_c_double(a, b)
                output = c_void_p()
                _lib.opensolid_Range_fromEndpoints_Float_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @staticmethod
    def meters(a: float, b: float) -> Range_Meters:
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_Range_meters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Meters(ptr=output)

    @staticmethod
    def centimeters(a: float, b: float) -> Range_Meters:
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_Range_centimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Meters(ptr=output)

    @staticmethod
    def millimeters(a: float, b: float) -> Range_Meters:
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_Range_millimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Meters(ptr=output)

    @staticmethod
    def inches(a: float, b: float) -> Range_Meters:
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_Range_inches_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Meters(ptr=output)

    @overload
    @staticmethod
    def aggregate(a: Range_Meters, b: Range_Meters, c: Range_Meters) -> Range_Meters:
        pass

    @overload
    @staticmethod
    def aggregate(
        a: Range_Radians, b: Range_Radians, c: Range_Radians
    ) -> Range_Radians:
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
    def aggregate(a: Range_Radians, b: Range_Radians) -> Range_Radians:
        pass

    @overload
    @staticmethod
    def aggregate(a: Range_Unitless, b: Range_Unitless) -> Range_Unitless:
        pass

    @staticmethod
    def aggregate(*args, **keywords):
        match (args, keywords):
            case (
                ([Range_Meters() as a, Range_Meters() as b, Range_Meters() as c], {})
                | (
                    [Range_Meters() as a, Range_Meters() as b],
                    {"c": Range_Meters() as c},
                )
                | (
                    [Range_Meters() as a],
                    {"b": Range_Meters() as b, "c": Range_Meters() as c},
                )
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
            case (
                ([Range_Radians() as a, Range_Radians() as b, Range_Radians() as c], {})
                | (
                    [Range_Radians() as a, Range_Radians() as b],
                    {"c": Range_Radians() as c},
                )
                | (
                    [Range_Radians() as a],
                    {"b": Range_Radians() as b, "c": Range_Radians() as c},
                )
                | (
                    [],
                    {
                        "a": Range_Radians() as a,
                        "b": Range_Radians() as b,
                        "c": Range_Radians() as c,
                    },
                )
            ):
                inputs = _Tuple3_c_void_p_c_void_p_c_void_p(
                    a.__ptr__, b.__ptr__, c.__ptr__
                )
                output = c_void_p()
                _lib.opensolid_Range_aggregate_RangeRadians_RangeRadians_RangeRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
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
                    [Range_Unitless() as a, Range_Unitless() as b],
                    {"c": Range_Unitless() as c},
                )
                | (
                    [Range_Unitless() as a],
                    {"b": Range_Unitless() as b, "c": Range_Unitless() as c},
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
                | ([Range_Meters() as a], {"b": Range_Meters() as b})
                | ([], {"a": Range_Meters() as a, "b": Range_Meters() as b})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(a.__ptr__, b.__ptr__)
                output = c_void_p()
                _lib.opensolid_Range_aggregate_RangeMeters_RangeMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case (
                ([Range_Radians() as a, Range_Radians() as b], {})
                | ([Range_Radians() as a], {"b": Range_Radians() as b})
                | ([], {"a": Range_Radians() as a, "b": Range_Radians() as b})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(a.__ptr__, b.__ptr__)
                output = c_void_p()
                _lib.opensolid_Range_aggregate_RangeRadians_RangeRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
            case (
                ([Range_Unitless() as a, Range_Unitless() as b], {})
                | ([Range_Unitless() as a], {"b": Range_Unitless() as b})
                | ([], {"a": Range_Unitless() as a, "b": Range_Unitless() as b})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(a.__ptr__, b.__ptr__)
                output = c_void_p()
                _lib.opensolid_Range_aggregate_RangeUnitless_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
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

    @overload
    def __contains__(self, other: Range_Unitless) -> bool:
        pass

    @overload
    def __contains__(self, value: float) -> bool:
        pass

    def __contains__(self, *args, **keywords):
        match (args, keywords):
            case (
                ([Range_Unitless() as other], {})
                | ([], {"other": Range_Unitless() as other})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
                output = c_int64()
                _lib.opensolid_RangeUnitless_contains_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return bool(output.value)
            case (
                ([float() | int() as value], {})
                | ([], {"value": float() | int() as value})
            ):
                inputs = _Tuple2_c_double_c_void_p(value, self.__ptr__)
                output = c_int64()
                _lib.opensolid_RangeUnitless_contains_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return bool(output.value)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

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

    def __add__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_add_RangeUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case Range_Unitless():
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

    def __sub__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_sub_RangeUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case Range_Unitless():
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

    @overload
    def __mul__(self, rhs: Angle) -> Range_Radians:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_mul_RangeUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case Range_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_mul_RangeUnitless_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_mul_RangeUnitless_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_mul_RangeUnitless_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Range_Unitless:
        pass

    @overload
    def __truediv__(self, rhs: Range_Unitless) -> Range_Unitless:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_RangeUnitless_div_RangeUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case Range_Unitless():
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

    def __rmul__(self, lhs: float) -> Range_Unitless:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_RangeUnitless_mul_Float_RangeUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Unitless(ptr=output)

    def __rtruediv__(self, lhs: float) -> Range_Unitless:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_RangeUnitless_div_Float_RangeUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Unitless(ptr=output)


class Range_Radians:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def endpoints(self) -> tuple[Angle, Angle]:
        inputs = self.__ptr__
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_RangeRadians_endpoints(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (Angle(ptr=c_void_p(output.field0)), Angle(ptr=c_void_p(output.field1)))

    def intersection(self, other: Range_Radians) -> Range_Radians | None:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = _Maybe_c_void_p()
        _lib.opensolid_RangeRadians_intersection_RangeRadians(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            Range_Radians(ptr=c_void_p(output.field1)) if output.field0 == 0 else None
        )

    @overload
    def __contains__(self, other: Range_Radians) -> bool:
        pass

    @overload
    def __contains__(self, value: Angle) -> bool:
        pass

    def __contains__(self, *args, **keywords):
        match (args, keywords):
            case (
                ([Range_Radians() as other], {})
                | ([], {"other": Range_Radians() as other})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
                output = c_int64()
                _lib.opensolid_RangeRadians_contains_RangeRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return bool(output.value)
            case ([Angle() as value], {}) | ([], {"value": Angle() as value}):
                inputs = _Tuple2_c_void_p_c_void_p(value.__ptr__, self.__ptr__)
                output = c_int64()
                _lib.opensolid_RangeRadians_contains_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return bool(output.value)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    def __neg__(self) -> Range_Radians:
        output = c_void_p()
        _lib.opensolid_RangeRadians_neg(
            ctypes.byref(self.__ptr__), ctypes.byref(output)
        )
        return Range_Radians(ptr=output)

    @overload
    def __add__(self, rhs: Range_Radians) -> Range_Radians:
        pass

    @overload
    def __add__(self, rhs: Angle) -> Range_Radians:
        pass

    def __add__(self, rhs):
        match rhs:
            case Range_Radians():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeRadians_add_RangeRadians_RangeRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeRadians_add_RangeRadians_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: Range_Radians) -> Range_Radians:
        pass

    @overload
    def __sub__(self, rhs: Angle) -> Range_Radians:
        pass

    def __sub__(self, rhs):
        match rhs:
            case Range_Radians():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeRadians_sub_RangeRadians_RangeRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeRadians_sub_RangeRadians_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
            case _:
                return NotImplemented

    def __mul__(self, rhs: float) -> Range_Radians:
        inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
        output = c_void_p()
        _lib.opensolid_RangeRadians_mul_RangeRadians_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Radians(ptr=output)

    @overload
    def __truediv__(self, rhs: float) -> Range_Radians:
        pass

    @overload
    def __truediv__(self, rhs: Range_Radians) -> Range_Unitless:
        pass

    @overload
    def __truediv__(self, rhs: Range_Unitless) -> Range_Radians:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_RangeRadians_div_RangeRadians_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
            case Range_Radians():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeRadians_div_RangeRadians_RangeRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case Range_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeRadians_div_RangeRadians_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Radians(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> Range_Radians:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_RangeRadians_mul_Float_RangeRadians(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Radians(ptr=output)


class Range_Meters:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def endpoints(self) -> tuple[Length, Length]:
        inputs = self.__ptr__
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_RangeMeters_endpoints(ctypes.byref(inputs), ctypes.byref(output))
        return (
            Length(ptr=c_void_p(output.field0)),
            Length(ptr=c_void_p(output.field1)),
        )

    def intersection(self, other: Range_Meters) -> Range_Meters | None:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = _Maybe_c_void_p()
        _lib.opensolid_RangeMeters_intersection_RangeMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Meters(ptr=c_void_p(output.field1)) if output.field0 == 0 else None

    @overload
    def __contains__(self, other: Range_Meters) -> bool:
        pass

    @overload
    def __contains__(self, value: Length) -> bool:
        pass

    def __contains__(self, *args, **keywords):
        match (args, keywords):
            case (
                ([Range_Meters() as other], {})
                | ([], {"other": Range_Meters() as other})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
                output = c_int64()
                _lib.opensolid_RangeMeters_contains_RangeMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return bool(output.value)
            case ([Length() as value], {}) | ([], {"value": Length() as value}):
                inputs = _Tuple2_c_void_p_c_void_p(value.__ptr__, self.__ptr__)
                output = c_int64()
                _lib.opensolid_RangeMeters_contains_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return bool(output.value)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    def __neg__(self) -> Range_Meters:
        output = c_void_p()
        _lib.opensolid_RangeMeters_neg(ctypes.byref(self.__ptr__), ctypes.byref(output))
        return Range_Meters(ptr=output)

    @overload
    def __add__(self, rhs: Range_Meters) -> Range_Meters:
        pass

    @overload
    def __add__(self, rhs: Length) -> Range_Meters:
        pass

    def __add__(self, rhs):
        match rhs:
            case Range_Meters():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeMeters_add_RangeMeters_RangeMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeMeters_add_RangeMeters_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: Range_Meters) -> Range_Meters:
        pass

    @overload
    def __sub__(self, rhs: Length) -> Range_Meters:
        pass

    def __sub__(self, rhs):
        match rhs:
            case Range_Meters():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeMeters_sub_RangeMeters_RangeMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeMeters_sub_RangeMeters_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case _:
                return NotImplemented

    def __mul__(self, rhs: float) -> Range_Meters:
        inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
        output = c_void_p()
        _lib.opensolid_RangeMeters_mul_RangeMeters_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Meters(ptr=output)

    @overload
    def __truediv__(self, rhs: float) -> Range_Meters:
        pass

    @overload
    def __truediv__(self, rhs: Range_Meters) -> Range_Unitless:
        pass

    @overload
    def __truediv__(self, rhs: Range_Unitless) -> Range_Meters:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_RangeMeters_div_RangeMeters_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case Range_Meters():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeMeters_div_RangeMeters_RangeMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Unitless(ptr=output)
            case Range_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_RangeMeters_div_RangeMeters_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range_Meters(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> Range_Meters:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_RangeMeters_mul_Float_RangeMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Meters(ptr=output)


class Color:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    red: Color = None  # type: ignore[assignment]
    dark_red: Color = None  # type: ignore[assignment]
    light_orange: Color = None  # type: ignore[assignment]
    orange: Color = None  # type: ignore[assignment]
    dark_orange: Color = None  # type: ignore[assignment]
    light_yellow: Color = None  # type: ignore[assignment]
    yellow: Color = None  # type: ignore[assignment]
    dark_yellow: Color = None  # type: ignore[assignment]
    light_green: Color = None  # type: ignore[assignment]
    green: Color = None  # type: ignore[assignment]
    dark_green: Color = None  # type: ignore[assignment]
    light_blue: Color = None  # type: ignore[assignment]
    blue: Color = None  # type: ignore[assignment]
    dark_blue: Color = None  # type: ignore[assignment]
    light_purple: Color = None  # type: ignore[assignment]
    purple: Color = None  # type: ignore[assignment]
    dark_purple: Color = None  # type: ignore[assignment]
    light_brown: Color = None  # type: ignore[assignment]
    brown: Color = None  # type: ignore[assignment]
    dark_brown: Color = None  # type: ignore[assignment]
    black: Color = None  # type: ignore[assignment]
    white: Color = None  # type: ignore[assignment]
    light_grey: Color = None  # type: ignore[assignment]
    grey: Color = None  # type: ignore[assignment]
    dark_grey: Color = None  # type: ignore[assignment]
    light_gray: Color = None  # type: ignore[assignment]
    gray: Color = None  # type: ignore[assignment]
    dark_gray: Color = None  # type: ignore[assignment]
    light_charcoal: Color = None  # type: ignore[assignment]
    charcoal: Color = None  # type: ignore[assignment]
    dark_charcoal: Color = None  # type: ignore[assignment]

    @staticmethod
    def rgb(red: float, green: float, blue: float) -> Color:
        inputs = _Tuple3_c_double_c_double_c_double(red, green, blue)
        output = c_void_p()
        _lib.opensolid_Color_rgb_Float_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Color(ptr=output)

    @staticmethod
    def rgb255(red: int, green: int, blue: int) -> Color:
        inputs = _Tuple3_c_int64_c_int64_c_int64(red, green, blue)
        output = c_void_p()
        _lib.opensolid_Color_rgb255_Int_Int_Int(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Color(ptr=output)

    @staticmethod
    def hsl(hue: Angle, saturation: float, lightness: float) -> Color:
        inputs = _Tuple3_c_void_p_c_double_c_double(hue.__ptr__, saturation, lightness)
        output = c_void_p()
        _lib.opensolid_Color_hsl_Angle_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Color(ptr=output)

    @staticmethod
    def from_hex(hex_string: str) -> Color:
        inputs = _str_to_text(hex_string)
        output = c_void_p()
        _lib.opensolid_Color_fromHex_Text(ctypes.byref(inputs), ctypes.byref(output))
        return Color(ptr=output)

    def to_hex(self) -> str:
        inputs = self.__ptr__
        output = _Text()
        _lib.opensolid_Color_toHex(ctypes.byref(inputs), ctypes.byref(output))
        return _text_to_str(output)

    def components(self) -> tuple[float, float, float]:
        inputs = self.__ptr__
        output = _Tuple3_c_double_c_double_c_double()
        _lib.opensolid_Color_components(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1, output.field2)

    def components255(self) -> tuple[int, int, int]:
        inputs = self.__ptr__
        output = _Tuple3_c_int64_c_int64_c_int64()
        _lib.opensolid_Color_components255(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1, output.field2)


class Vector2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    zero: Vector2d_Meters = None  # type: ignore[assignment]

    @staticmethod
    def unit(direction: Direction2d) -> Vector2d_Unitless:
        inputs = direction.__ptr__
        output = c_void_p()
        _lib.opensolid_Vector2d_unit_Direction2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Unitless(ptr=output)

    @staticmethod
    def meters(x_component: float, y_component: float) -> Vector2d_Meters:
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Vector2d_meters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Meters(ptr=output)

    @staticmethod
    def centimeters(x_component: float, y_component: float) -> Vector2d_Meters:
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Vector2d_centimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Meters(ptr=output)

    @staticmethod
    def millimeters(x_component: float, y_component: float) -> Vector2d_Meters:
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Vector2d_millimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Meters(ptr=output)

    @staticmethod
    def inches(x_component: float, y_component: float) -> Vector2d_Meters:
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Vector2d_inches_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Meters(ptr=output)

    @overload
    @staticmethod
    def xy(x_component: Length, y_component: Length) -> Vector2d_Meters:
        pass

    @overload
    @staticmethod
    def xy(x_component: float, y_component: float) -> Vector2d_Unitless:
        pass

    @staticmethod
    def xy(*args, **keywords):
        match (args, keywords):
            case (
                ([Length() as x_component, Length() as y_component], {})
                | ([Length() as x_component], {"y_component": Length() as y_component})
                | (
                    [],
                    {
                        "x_component": Length() as x_component,
                        "y_component": Length() as y_component,
                    },
                )
            ):
                inputs = _Tuple2_c_void_p_c_void_p(
                    x_component.__ptr__, y_component.__ptr__
                )
                output = c_void_p()
                _lib.opensolid_Vector2d_xy_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Meters(ptr=output)
            case (
                ([float() | int() as x_component, float() | int() as y_component], {})
                | (
                    [float() | int() as x_component],
                    {"y_component": float() | int() as y_component},
                )
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
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def x(x_component: Length) -> Vector2d_Meters:
        pass

    @overload
    @staticmethod
    def x(x_component: float) -> Vector2d_Unitless:
        pass

    @staticmethod
    def x(*args, **keywords):
        match (args, keywords):
            case (
                ([Length() as x_component], {})
                | ([], {"x_component": Length() as x_component})
            ):
                inputs = x_component.__ptr__
                output = c_void_p()
                _lib.opensolid_Vector2d_x_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Meters(ptr=output)
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
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def y(y_component: Length) -> Vector2d_Meters:
        pass

    @overload
    @staticmethod
    def y(y_component: float) -> Vector2d_Unitless:
        pass

    @staticmethod
    def y(*args, **keywords):
        match (args, keywords):
            case (
                ([Length() as y_component], {})
                | ([], {"y_component": Length() as y_component})
            ):
                inputs = y_component.__ptr__
                output = c_void_p()
                _lib.opensolid_Vector2d_y_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Meters(ptr=output)
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
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def from_components(components: tuple[Length, Length]) -> Vector2d_Meters:
        pass

    @overload
    @staticmethod
    def from_components(components: tuple[float, float]) -> Vector2d_Unitless:
        pass

    @staticmethod
    def from_components(*args, **keywords):
        match (args, keywords):
            case (
                ([(Length(), Length()) as components], {})
                | ([], {"components": (Length(), Length()) as components})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(
                    components[0].__ptr__, components[1].__ptr__
                )
                output = c_void_p()
                _lib.opensolid_Vector2d_fromComponents_Tuple2LengthLength(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Meters(ptr=output)
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

    def x_component(self) -> float:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Vector2dUnitless_xComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def y_component(self) -> float:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Vector2dUnitless_yComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

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

    def __neg__(self) -> Vector2d_Unitless:
        output = c_void_p()
        _lib.opensolid_Vector2dUnitless_neg(
            ctypes.byref(self.__ptr__), ctypes.byref(output)
        )
        return Vector2d_Unitless(ptr=output)

    def __add__(self, rhs: Vector2d_Unitless) -> Vector2d_Unitless:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
        output = c_void_p()
        _lib.opensolid_Vector2dUnitless_add_Vector2dUnitless_Vector2dUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Unitless(ptr=output)

    def __sub__(self, rhs: Vector2d_Unitless) -> Vector2d_Unitless:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
        output = c_void_p()
        _lib.opensolid_Vector2dUnitless_sub_Vector2dUnitless_Vector2dUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Unitless(ptr=output)

    @overload
    def __mul__(self, rhs: float) -> Vector2d_Unitless:
        pass

    @overload
    def __mul__(self, rhs: Length) -> Vector2d_Meters:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Vector2dUnitless_mul_Vector2dUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Unitless(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Vector2dUnitless_mul_Vector2dUnitless_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Meters(ptr=output)
            case _:
                return NotImplemented

    def __truediv__(self, rhs: float) -> Vector2d_Unitless:
        inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
        output = c_void_p()
        _lib.opensolid_Vector2dUnitless_div_Vector2dUnitless_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Unitless(ptr=output)

    def __rmul__(self, lhs: float) -> Vector2d_Unitless:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Vector2dUnitless_mul_Float_Vector2dUnitless(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Unitless(ptr=output)


class Vector2d_Meters:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def components(self) -> tuple[Length, Length]:
        inputs = self.__ptr__
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_Vector2dMeters_components(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            Length(ptr=c_void_p(output.field0)),
            Length(ptr=c_void_p(output.field1)),
        )

    def x_component(self) -> Length:
        inputs = self.__ptr__
        output = c_void_p()
        _lib.opensolid_Vector2dMeters_xComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def y_component(self) -> Length:
        inputs = self.__ptr__
        output = c_void_p()
        _lib.opensolid_Vector2dMeters_yComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def direction(self) -> Direction2d:
        inputs = _Tuple2_c_void_p_c_void_p(_length_tolerance().__ptr__, self.__ptr__)
        output = _Result_c_void_p()
        _lib.opensolid_Vector2dMeters_direction(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            Direction2d(ptr=c_void_p(output.field2))
            if output.field0 == 0
            else _error(output)
        )

    def __neg__(self) -> Vector2d_Meters:
        output = c_void_p()
        _lib.opensolid_Vector2dMeters_neg(
            ctypes.byref(self.__ptr__), ctypes.byref(output)
        )
        return Vector2d_Meters(ptr=output)

    def __add__(self, rhs: Vector2d_Meters) -> Vector2d_Meters:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
        output = c_void_p()
        _lib.opensolid_Vector2dMeters_add_Vector2dMeters_Vector2dMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Meters(ptr=output)

    def __sub__(self, rhs: Vector2d_Meters) -> Vector2d_Meters:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
        output = c_void_p()
        _lib.opensolid_Vector2dMeters_sub_Vector2dMeters_Vector2dMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Meters(ptr=output)

    def __mul__(self, rhs: float) -> Vector2d_Meters:
        inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
        output = c_void_p()
        _lib.opensolid_Vector2dMeters_mul_Vector2dMeters_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Meters(ptr=output)

    @overload
    def __truediv__(self, rhs: float) -> Vector2d_Meters:
        pass

    @overload
    def __truediv__(self, rhs: Length) -> Vector2d_Unitless:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Vector2dMeters_div_Vector2dMeters_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Meters(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Vector2dMeters_div_Vector2dMeters_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d_Unitless(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> Vector2d_Meters:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Vector2dMeters_mul_Float_Vector2dMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d_Meters(ptr=output)


class Direction2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    x: Direction2d = None  # type: ignore[assignment]
    y: Direction2d = None  # type: ignore[assignment]
    positive_x: Direction2d = None  # type: ignore[assignment]
    positive_y: Direction2d = None  # type: ignore[assignment]
    negative_x: Direction2d = None  # type: ignore[assignment]
    negative_y: Direction2d = None  # type: ignore[assignment]

    @staticmethod
    def from_angle(angle: Angle) -> Direction2d:
        inputs = angle.__ptr__
        output = c_void_p()
        _lib.opensolid_Direction2d_fromAngle_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Direction2d(ptr=output)

    def to_angle(self) -> Angle:
        inputs = self.__ptr__
        output = c_void_p()
        _lib.opensolid_Direction2d_toAngle(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    def components(self) -> tuple[float, float]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_Direction2d_components(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (output.field0, output.field1)

    def x_component(self) -> float:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Direction2d_xComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def y_component(self) -> float:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Direction2d_yComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def __neg__(self) -> Direction2d:
        output = c_void_p()
        _lib.opensolid_Direction2d_neg(ctypes.byref(self.__ptr__), ctypes.byref(output))
        return Direction2d(ptr=output)


class Point2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    origin: Point2d_Meters = None  # type: ignore[assignment]

    @overload
    @staticmethod
    def xy(x_coordinate: Length, y_coordinate: Length) -> Point2d_Meters:
        pass

    @overload
    @staticmethod
    def xy(x_coordinate: float, y_coordinate: float) -> Point2d_Unitless:
        pass

    @staticmethod
    def xy(*args, **keywords):
        match (args, keywords):
            case (
                ([Length() as x_coordinate, Length() as y_coordinate], {})
                | (
                    [Length() as x_coordinate],
                    {"y_coordinate": Length() as y_coordinate},
                )
                | (
                    [],
                    {
                        "x_coordinate": Length() as x_coordinate,
                        "y_coordinate": Length() as y_coordinate,
                    },
                )
            ):
                inputs = _Tuple2_c_void_p_c_void_p(
                    x_coordinate.__ptr__, y_coordinate.__ptr__
                )
                output = c_void_p()
                _lib.opensolid_Point2d_xy_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Point2d_Meters(ptr=output)
            case (
                ([float() | int() as x_coordinate, float() | int() as y_coordinate], {})
                | (
                    [float() | int() as x_coordinate],
                    {"y_coordinate": float() | int() as y_coordinate},
                )
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
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def x(x_coordinate: Length) -> Point2d_Meters:
        pass

    @overload
    @staticmethod
    def x(x_coordinate: float) -> Point2d_Unitless:
        pass

    @staticmethod
    def x(*args, **keywords):
        match (args, keywords):
            case (
                ([Length() as x_coordinate], {})
                | ([], {"x_coordinate": Length() as x_coordinate})
            ):
                inputs = x_coordinate.__ptr__
                output = c_void_p()
                _lib.opensolid_Point2d_x_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Point2d_Meters(ptr=output)
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
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def y(y_coordinate: Length) -> Point2d_Meters:
        pass

    @overload
    @staticmethod
    def y(y_coordinate: float) -> Point2d_Unitless:
        pass

    @staticmethod
    def y(*args, **keywords):
        match (args, keywords):
            case (
                ([Length() as y_coordinate], {})
                | ([], {"y_coordinate": Length() as y_coordinate})
            ):
                inputs = y_coordinate.__ptr__
                output = c_void_p()
                _lib.opensolid_Point2d_y_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Point2d_Meters(ptr=output)
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
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @staticmethod
    def meters(x_coordinate: float, y_coordinate: float) -> Point2d_Meters:
        inputs = _Tuple2_c_double_c_double(x_coordinate, y_coordinate)
        output = c_void_p()
        _lib.opensolid_Point2d_meters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d_Meters(ptr=output)

    @staticmethod
    def centimeters(x_coordinate: float, y_coordinate: float) -> Point2d_Meters:
        inputs = _Tuple2_c_double_c_double(x_coordinate, y_coordinate)
        output = c_void_p()
        _lib.opensolid_Point2d_centimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d_Meters(ptr=output)

    @staticmethod
    def millimeters(x_coordinate: float, y_coordinate: float) -> Point2d_Meters:
        inputs = _Tuple2_c_double_c_double(x_coordinate, y_coordinate)
        output = c_void_p()
        _lib.opensolid_Point2d_millimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d_Meters(ptr=output)

    @staticmethod
    def inches(x_coordinate: float, y_coordinate: float) -> Point2d_Meters:
        inputs = _Tuple2_c_double_c_double(x_coordinate, y_coordinate)
        output = c_void_p()
        _lib.opensolid_Point2d_inches_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d_Meters(ptr=output)

    @overload
    @staticmethod
    def from_coordinates(coordinates: tuple[Length, Length]) -> Point2d_Meters:
        pass

    @overload
    @staticmethod
    def from_coordinates(coordinates: tuple[float, float]) -> Point2d_Unitless:
        pass

    @staticmethod
    def from_coordinates(*args, **keywords):
        match (args, keywords):
            case (
                ([(Length(), Length()) as coordinates], {})
                | ([], {"coordinates": (Length(), Length()) as coordinates})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(
                    coordinates[0].__ptr__, coordinates[1].__ptr__
                )
                output = c_void_p()
                _lib.opensolid_Point2d_fromCoordinates_Tuple2LengthLength(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Point2d_Meters(ptr=output)
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

    def x_coordinate(self) -> float:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Point2dUnitless_xCoordinate(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def y_coordinate(self) -> float:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_Point2dUnitless_yCoordinate(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

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
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_Point2dMeters_coordinates(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            Length(ptr=c_void_p(output.field0)),
            Length(ptr=c_void_p(output.field1)),
        )

    def x_coordinate(self) -> Length:
        inputs = self.__ptr__
        output = c_void_p()
        _lib.opensolid_Point2dMeters_xCoordinate(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def y_coordinate(self) -> Length:
        inputs = self.__ptr__
        output = c_void_p()
        _lib.opensolid_Point2dMeters_yCoordinate(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def distance_to(self, other: Point2d_Meters) -> Length:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Point2dMeters_distanceTo_Point2dMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def midpoint(self, other: Point2d_Meters) -> Point2d_Meters:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Point2dMeters_midpoint_Point2dMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d_Meters(ptr=output)


class Bounds2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    @overload
    @staticmethod
    def xy(x_coordinate: Range_Meters, y_coordinate: Range_Meters) -> Bounds2d_Meters:
        pass

    @overload
    @staticmethod
    def xy(
        x_coordinate: Range_Unitless, y_coordinate: Range_Unitless
    ) -> Bounds2d_Unitless:
        pass

    @staticmethod
    def xy(*args, **keywords):
        match (args, keywords):
            case (
                ([Range_Meters() as x_coordinate, Range_Meters() as y_coordinate], {})
                | (
                    [Range_Meters() as x_coordinate],
                    {"y_coordinate": Range_Meters() as y_coordinate},
                )
                | (
                    [],
                    {
                        "x_coordinate": Range_Meters() as x_coordinate,
                        "y_coordinate": Range_Meters() as y_coordinate,
                    },
                )
            ):
                inputs = _Tuple2_c_void_p_c_void_p(
                    x_coordinate.__ptr__, y_coordinate.__ptr__
                )
                output = c_void_p()
                _lib.opensolid_Bounds2d_xy_RangeMeters_RangeMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Bounds2d_Meters(ptr=output)
            case (
                (
                    [
                        Range_Unitless() as x_coordinate,
                        Range_Unitless() as y_coordinate,
                    ],
                    {},
                )
                | (
                    [Range_Unitless() as x_coordinate],
                    {"y_coordinate": Range_Unitless() as y_coordinate},
                )
                | (
                    [],
                    {
                        "x_coordinate": Range_Unitless() as x_coordinate,
                        "y_coordinate": Range_Unitless() as y_coordinate,
                    },
                )
            ):
                inputs = _Tuple2_c_void_p_c_void_p(
                    x_coordinate.__ptr__, y_coordinate.__ptr__
                )
                output = c_void_p()
                _lib.opensolid_Bounds2d_xy_RangeUnitless_RangeUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Bounds2d_Unitless(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def constant(point: Point2d_Meters) -> Bounds2d_Meters:
        pass

    @overload
    @staticmethod
    def constant(point: Point2d_Unitless) -> Bounds2d_Unitless:
        pass

    @staticmethod
    def constant(*args, **keywords):
        match (args, keywords):
            case (
                ([Point2d_Meters() as point], {})
                | ([], {"point": Point2d_Meters() as point})
            ):
                inputs = point.__ptr__
                output = c_void_p()
                _lib.opensolid_Bounds2d_constant_Point2dMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Bounds2d_Meters(ptr=output)
            case (
                ([Point2d_Unitless() as point], {})
                | ([], {"point": Point2d_Unitless() as point})
            ):
                inputs = point.__ptr__
                output = c_void_p()
                _lib.opensolid_Bounds2d_constant_Point2dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Bounds2d_Unitless(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def hull(
        p1: Point2d_Meters, p2: Point2d_Meters, p3: Point2d_Meters, p4: Point2d_Meters
    ) -> Bounds2d_Meters:
        pass

    @overload
    @staticmethod
    def hull(
        p1: Point2d_Unitless,
        p2: Point2d_Unitless,
        p3: Point2d_Unitless,
        p4: Point2d_Unitless,
    ) -> Bounds2d_Unitless:
        pass

    @overload
    @staticmethod
    def hull(
        p1: Point2d_Meters, p2: Point2d_Meters, p3: Point2d_Meters
    ) -> Bounds2d_Meters:
        pass

    @overload
    @staticmethod
    def hull(
        p1: Point2d_Unitless, p2: Point2d_Unitless, p3: Point2d_Unitless
    ) -> Bounds2d_Unitless:
        pass

    @overload
    @staticmethod
    def hull(p1: Point2d_Meters, p2: Point2d_Meters) -> Bounds2d_Meters:
        pass

    @overload
    @staticmethod
    def hull(p1: Point2d_Unitless, p2: Point2d_Unitless) -> Bounds2d_Unitless:
        pass

    @staticmethod
    def hull(*args, **keywords):
        match (args, keywords):
            case (
                (
                    [
                        Point2d_Meters() as p1,
                        Point2d_Meters() as p2,
                        Point2d_Meters() as p3,
                        Point2d_Meters() as p4,
                    ],
                    {},
                )
                | (
                    [
                        Point2d_Meters() as p1,
                        Point2d_Meters() as p2,
                        Point2d_Meters() as p3,
                    ],
                    {"p4": Point2d_Meters() as p4},
                )
                | (
                    [Point2d_Meters() as p1, Point2d_Meters() as p2],
                    {"p3": Point2d_Meters() as p3, "p4": Point2d_Meters() as p4},
                )
                | (
                    [Point2d_Meters() as p1],
                    {
                        "p2": Point2d_Meters() as p2,
                        "p3": Point2d_Meters() as p3,
                        "p4": Point2d_Meters() as p4,
                    },
                )
                | (
                    [],
                    {
                        "p1": Point2d_Meters() as p1,
                        "p2": Point2d_Meters() as p2,
                        "p3": Point2d_Meters() as p3,
                        "p4": Point2d_Meters() as p4,
                    },
                )
            ):
                inputs = _Tuple4_c_void_p_c_void_p_c_void_p_c_void_p(
                    p1.__ptr__, p2.__ptr__, p3.__ptr__, p4.__ptr__
                )
                output = c_void_p()
                _lib.opensolid_Bounds2d_hull_Point2dMeters_Point2dMeters_Point2dMeters_Point2dMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Bounds2d_Meters(ptr=output)
            case (
                (
                    [
                        Point2d_Unitless() as p1,
                        Point2d_Unitless() as p2,
                        Point2d_Unitless() as p3,
                        Point2d_Unitless() as p4,
                    ],
                    {},
                )
                | (
                    [
                        Point2d_Unitless() as p1,
                        Point2d_Unitless() as p2,
                        Point2d_Unitless() as p3,
                    ],
                    {"p4": Point2d_Unitless() as p4},
                )
                | (
                    [Point2d_Unitless() as p1, Point2d_Unitless() as p2],
                    {"p3": Point2d_Unitless() as p3, "p4": Point2d_Unitless() as p4},
                )
                | (
                    [Point2d_Unitless() as p1],
                    {
                        "p2": Point2d_Unitless() as p2,
                        "p3": Point2d_Unitless() as p3,
                        "p4": Point2d_Unitless() as p4,
                    },
                )
                | (
                    [],
                    {
                        "p1": Point2d_Unitless() as p1,
                        "p2": Point2d_Unitless() as p2,
                        "p3": Point2d_Unitless() as p3,
                        "p4": Point2d_Unitless() as p4,
                    },
                )
            ):
                inputs = _Tuple4_c_void_p_c_void_p_c_void_p_c_void_p(
                    p1.__ptr__, p2.__ptr__, p3.__ptr__, p4.__ptr__
                )
                output = c_void_p()
                _lib.opensolid_Bounds2d_hull_Point2dUnitless_Point2dUnitless_Point2dUnitless_Point2dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Bounds2d_Unitless(ptr=output)
            case (
                (
                    [
                        Point2d_Meters() as p1,
                        Point2d_Meters() as p2,
                        Point2d_Meters() as p3,
                    ],
                    {},
                )
                | (
                    [Point2d_Meters() as p1, Point2d_Meters() as p2],
                    {"p3": Point2d_Meters() as p3},
                )
                | (
                    [Point2d_Meters() as p1],
                    {"p2": Point2d_Meters() as p2, "p3": Point2d_Meters() as p3},
                )
                | (
                    [],
                    {
                        "p1": Point2d_Meters() as p1,
                        "p2": Point2d_Meters() as p2,
                        "p3": Point2d_Meters() as p3,
                    },
                )
            ):
                inputs = _Tuple3_c_void_p_c_void_p_c_void_p(
                    p1.__ptr__, p2.__ptr__, p3.__ptr__
                )
                output = c_void_p()
                _lib.opensolid_Bounds2d_hull_Point2dMeters_Point2dMeters_Point2dMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Bounds2d_Meters(ptr=output)
            case (
                (
                    [
                        Point2d_Unitless() as p1,
                        Point2d_Unitless() as p2,
                        Point2d_Unitless() as p3,
                    ],
                    {},
                )
                | (
                    [Point2d_Unitless() as p1, Point2d_Unitless() as p2],
                    {"p3": Point2d_Unitless() as p3},
                )
                | (
                    [Point2d_Unitless() as p1],
                    {"p2": Point2d_Unitless() as p2, "p3": Point2d_Unitless() as p3},
                )
                | (
                    [],
                    {
                        "p1": Point2d_Unitless() as p1,
                        "p2": Point2d_Unitless() as p2,
                        "p3": Point2d_Unitless() as p3,
                    },
                )
            ):
                inputs = _Tuple3_c_void_p_c_void_p_c_void_p(
                    p1.__ptr__, p2.__ptr__, p3.__ptr__
                )
                output = c_void_p()
                _lib.opensolid_Bounds2d_hull_Point2dUnitless_Point2dUnitless_Point2dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Bounds2d_Unitless(ptr=output)
            case (
                ([Point2d_Meters() as p1, Point2d_Meters() as p2], {})
                | ([Point2d_Meters() as p1], {"p2": Point2d_Meters() as p2})
                | ([], {"p1": Point2d_Meters() as p1, "p2": Point2d_Meters() as p2})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(p1.__ptr__, p2.__ptr__)
                output = c_void_p()
                _lib.opensolid_Bounds2d_hull_Point2dMeters_Point2dMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Bounds2d_Meters(ptr=output)
            case (
                ([Point2d_Unitless() as p1, Point2d_Unitless() as p2], {})
                | ([Point2d_Unitless() as p1], {"p2": Point2d_Unitless() as p2})
                | ([], {"p1": Point2d_Unitless() as p1, "p2": Point2d_Unitless() as p2})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(p1.__ptr__, p2.__ptr__)
                output = c_void_p()
                _lib.opensolid_Bounds2d_hull_Point2dUnitless_Point2dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Bounds2d_Unitless(ptr=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)


class Bounds2d_Unitless:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def coordinates(self) -> tuple[Range_Unitless, Range_Unitless]:
        inputs = self.__ptr__
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_Bounds2dUnitless_coordinates(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            Range_Unitless(ptr=c_void_p(output.field0)),
            Range_Unitless(ptr=c_void_p(output.field1)),
        )

    def x_coordinate(self) -> Range_Unitless:
        inputs = self.__ptr__
        output = c_void_p()
        _lib.opensolid_Bounds2dUnitless_xCoordinate(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Unitless(ptr=output)

    def y_coordinate(self) -> Range_Unitless:
        inputs = self.__ptr__
        output = c_void_p()
        _lib.opensolid_Bounds2dUnitless_yCoordinate(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Unitless(ptr=output)


class Bounds2d_Meters:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def coordinates(self) -> tuple[Range_Meters, Range_Meters]:
        inputs = self.__ptr__
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_Bounds2dMeters_coordinates(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            Range_Meters(ptr=c_void_p(output.field0)),
            Range_Meters(ptr=c_void_p(output.field1)),
        )

    def x_coordinate(self) -> Range_Meters:
        inputs = self.__ptr__
        output = c_void_p()
        _lib.opensolid_Bounds2dMeters_xCoordinate(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Meters(ptr=output)

    def y_coordinate(self) -> Range_Meters:
        inputs = self.__ptr__
        output = c_void_p()
        _lib.opensolid_Bounds2dMeters_yCoordinate(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range_Meters(ptr=output)


class Curve1d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    t: Curve1d_Unitless = None  # type: ignore[assignment]

    @staticmethod
    def sin(curve: Curve1d_Radians) -> Curve1d_Unitless:
        inputs = curve.__ptr__
        output = c_void_p()
        _lib.opensolid_Curve1d_sin_Curve1dRadians(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Unitless(ptr=output)

    @staticmethod
    def cos(curve: Curve1d_Radians) -> Curve1d_Unitless:
        inputs = curve.__ptr__
        output = c_void_p()
        _lib.opensolid_Curve1d_cos_Curve1dRadians(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Unitless(ptr=output)

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

    def __add__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_add_Curve1dUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case Curve1d_Unitless():
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

    def __sub__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_sub_Curve1dUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case Curve1d_Unitless():
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

    @overload
    def __mul__(self, rhs: Length) -> Curve1d_Meters:
        pass

    @overload
    def __mul__(self, rhs: Angle) -> Curve1d_Radians:
        pass

    @overload
    def __mul__(self, rhs: Curve1d_Meters) -> Curve1d_Meters:
        pass

    @overload
    def __mul__(self, rhs: Curve1d_Radians) -> Curve1d_Radians:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_mul_Curve1dUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case Curve1d_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_mul_Curve1dUnitless_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_mul_Curve1dUnitless_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Meters(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_mul_Curve1dUnitless_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Radians(ptr=output)
            case Curve1d_Meters():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_mul_Curve1dUnitless_Curve1dMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Meters(ptr=output)
            case Curve1d_Radians():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_mul_Curve1dUnitless_Curve1dRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Radians(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Curve1d_Unitless:
        pass

    @overload
    def __truediv__(self, rhs: Curve1d_Unitless) -> Curve1d_Unitless:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Curve1dUnitless_div_Curve1dUnitless_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case Curve1d_Unitless():
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
        output = c_void_p()
        _lib.opensolid_Curve1dRadians_evaluate_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Angle(ptr=output)

    def zeros(self) -> list[Curve1d.Root]:
        inputs = _Tuple2_c_void_p_c_void_p(_angle_tolerance().__ptr__, self.__ptr__)
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

    def __neg__(self) -> Curve1d_Radians:
        output = c_void_p()
        _lib.opensolid_Curve1dRadians_neg(
            ctypes.byref(self.__ptr__), ctypes.byref(output)
        )
        return Curve1d_Radians(ptr=output)

    def __add__(self, rhs: Curve1d_Radians) -> Curve1d_Radians:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
        output = c_void_p()
        _lib.opensolid_Curve1dRadians_add_Curve1dRadians_Curve1dRadians(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Radians(ptr=output)

    def __sub__(self, rhs: Curve1d_Radians) -> Curve1d_Radians:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
        output = c_void_p()
        _lib.opensolid_Curve1dRadians_sub_Curve1dRadians_Curve1dRadians(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Radians(ptr=output)

    @overload
    def __mul__(self, rhs: float) -> Curve1d_Radians:
        pass

    @overload
    def __mul__(self, rhs: Curve1d_Unitless) -> Curve1d_Radians:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Curve1dRadians_mul_Curve1dRadians_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Radians(ptr=output)
            case Curve1d_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dRadians_mul_Curve1dRadians_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Radians(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Curve1d_Radians:
        pass

    @overload
    def __truediv__(self, rhs: Curve1d_Radians) -> Curve1d_Unitless:
        pass

    @overload
    def __truediv__(self, rhs: Angle) -> Curve1d_Unitless:
        pass

    @overload
    def __truediv__(self, rhs: Curve1d_Unitless) -> Curve1d_Radians:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Curve1dRadians_div_Curve1dRadians_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Radians(ptr=output)
            case Curve1d_Radians():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dRadians_div_Curve1dRadians_Curve1dRadians(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dRadians_div_Curve1dRadians_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case Curve1d_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dRadians_div_Curve1dRadians_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Radians(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> Curve1d_Radians:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Curve1dRadians_mul_Float_Curve1dRadians(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Radians(ptr=output)


class Curve1d_Meters:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    def evaluate(self, parameter_value: float) -> Length:
        inputs = _Tuple2_c_double_c_void_p(parameter_value, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Curve1dMeters_evaluate_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def zeros(self) -> list[Curve1d.Root]:
        inputs = _Tuple2_c_void_p_c_void_p(_length_tolerance().__ptr__, self.__ptr__)
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

    def __neg__(self) -> Curve1d_Meters:
        output = c_void_p()
        _lib.opensolid_Curve1dMeters_neg(
            ctypes.byref(self.__ptr__), ctypes.byref(output)
        )
        return Curve1d_Meters(ptr=output)

    def __add__(self, rhs: Curve1d_Meters) -> Curve1d_Meters:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
        output = c_void_p()
        _lib.opensolid_Curve1dMeters_add_Curve1dMeters_Curve1dMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Meters(ptr=output)

    def __sub__(self, rhs: Curve1d_Meters) -> Curve1d_Meters:
        inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
        output = c_void_p()
        _lib.opensolid_Curve1dMeters_sub_Curve1dMeters_Curve1dMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Meters(ptr=output)

    @overload
    def __mul__(self, rhs: float) -> Curve1d_Meters:
        pass

    @overload
    def __mul__(self, rhs: Curve1d_Unitless) -> Curve1d_Meters:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Curve1dMeters_mul_Curve1dMeters_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Meters(ptr=output)
            case Curve1d_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dMeters_mul_Curve1dMeters_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Meters(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Curve1d_Meters:
        pass

    @overload
    def __truediv__(self, rhs: Curve1d_Meters) -> Curve1d_Unitless:
        pass

    @overload
    def __truediv__(self, rhs: Length) -> Curve1d_Unitless:
        pass

    @overload
    def __truediv__(self, rhs: Curve1d_Unitless) -> Curve1d_Meters:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self.__ptr__, rhs)
                output = c_void_p()
                _lib.opensolid_Curve1dMeters_div_Curve1dMeters_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Meters(ptr=output)
            case Curve1d_Meters():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dMeters_div_Curve1dMeters_Curve1dMeters(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dMeters_div_Curve1dMeters_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Unitless(ptr=output)
            case Curve1d_Unitless():
                inputs = _Tuple2_c_void_p_c_void_p(self.__ptr__, rhs.__ptr__)
                output = c_void_p()
                _lib.opensolid_Curve1dMeters_div_Curve1dMeters_Curve1dUnitless(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve1d_Meters(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> Curve1d_Meters:
        inputs = _Tuple2_c_double_c_void_p(lhs, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_Curve1dMeters_mul_Float_Curve1dMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Curve1d_Meters(ptr=output)


class Drawing2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self.__ptr__ = ptr

    black_stroke: Drawing2d.Attribute = None  # type: ignore[assignment]
    no_fill: Drawing2d.Attribute = None  # type: ignore[assignment]

    @staticmethod
    def to_svg(view_box: Bounds2d_Meters, entities: list[Drawing2d.Entity]) -> str:
        inputs = _Tuple2_c_void_p_List_c_void_p(
            view_box.__ptr__,
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(entities))(*[item.__ptr__ for item in entities]),
            ),
        )
        output = _Text()
        _lib.opensolid_Drawing2d_toSVG_Bounds2dMeters_ListDrawing2dEntity(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return _text_to_str(output)

    @staticmethod
    def polygon(
        attributes: list[Drawing2d.Attribute], vertices: list[Point2d_Meters]
    ) -> Drawing2d.Entity:
        inputs = _Tuple2_List_c_void_p_List_c_void_p(
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(attributes))(*[item.__ptr__ for item in attributes]),
            ),
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(vertices))(*[item.__ptr__ for item in vertices]),
            ),
        )
        output = c_void_p()
        _lib.opensolid_Drawing2d_polygon_ListDrawing2dAttribute_ListPoint2dMeters(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Drawing2d.Entity(ptr=output)

    @staticmethod
    def circle(
        attributes: list[Drawing2d.Attribute],
        center_point: Point2d_Meters,
        radius: Length,
    ) -> Drawing2d.Entity:
        inputs = _Tuple3_List_c_void_p_c_void_p_c_void_p(
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(attributes))(*[item.__ptr__ for item in attributes]),
            ),
            center_point.__ptr__,
            radius.__ptr__,
        )
        output = c_void_p()
        _lib.opensolid_Drawing2d_circle_ListDrawing2dAttribute_Point2dMeters_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Drawing2d.Entity(ptr=output)

    @staticmethod
    def stroke_color(color: Color) -> Drawing2d.Attribute:
        inputs = color.__ptr__
        output = c_void_p()
        _lib.opensolid_Drawing2d_strokeColor_Color(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Drawing2d.Attribute(ptr=output)

    @staticmethod
    def fill_color(color: Color) -> Drawing2d.Attribute:
        inputs = color.__ptr__
        output = c_void_p()
        _lib.opensolid_Drawing2d_fillColor_Color(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Drawing2d.Attribute(ptr=output)

    class Entity:
        def __init__(self, *, ptr: c_void_p) -> None:
            self.__ptr__ = ptr

    class Attribute:
        def __init__(self, *, ptr: c_void_p) -> None:
            self.__ptr__ = ptr


def _length_zero() -> Length:
    output = c_void_p()
    _lib.opensolid_Length_zero(c_void_p(), ctypes.byref(output))
    return Length(ptr=output)


Length.zero = _length_zero()


def _angle_zero() -> Angle:
    output = c_void_p()
    _lib.opensolid_Angle_zero(c_void_p(), ctypes.byref(output))
    return Angle(ptr=output)


Angle.zero = _angle_zero()


def _range_unit() -> Range_Unitless:
    output = c_void_p()
    _lib.opensolid_Range_unit(c_void_p(), ctypes.byref(output))
    return Range_Unitless(ptr=output)


Range.unit = _range_unit()


def _color_red() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_red(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.red = _color_red()


def _color_dark_red() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_darkRed(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.dark_red = _color_dark_red()


def _color_light_orange() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_lightOrange(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.light_orange = _color_light_orange()


def _color_orange() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_orange(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.orange = _color_orange()


def _color_dark_orange() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_darkOrange(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.dark_orange = _color_dark_orange()


def _color_light_yellow() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_lightYellow(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.light_yellow = _color_light_yellow()


def _color_yellow() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_yellow(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.yellow = _color_yellow()


def _color_dark_yellow() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_darkYellow(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.dark_yellow = _color_dark_yellow()


def _color_light_green() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_lightGreen(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.light_green = _color_light_green()


def _color_green() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_green(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.green = _color_green()


def _color_dark_green() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_darkGreen(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.dark_green = _color_dark_green()


def _color_light_blue() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_lightBlue(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.light_blue = _color_light_blue()


def _color_blue() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_blue(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.blue = _color_blue()


def _color_dark_blue() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_darkBlue(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.dark_blue = _color_dark_blue()


def _color_light_purple() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_lightPurple(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.light_purple = _color_light_purple()


def _color_purple() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_purple(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.purple = _color_purple()


def _color_dark_purple() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_darkPurple(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.dark_purple = _color_dark_purple()


def _color_light_brown() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_lightBrown(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.light_brown = _color_light_brown()


def _color_brown() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_brown(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.brown = _color_brown()


def _color_dark_brown() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_darkBrown(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.dark_brown = _color_dark_brown()


def _color_black() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_black(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.black = _color_black()


def _color_white() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_white(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.white = _color_white()


def _color_light_grey() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_lightGrey(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.light_grey = _color_light_grey()


def _color_grey() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_grey(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.grey = _color_grey()


def _color_dark_grey() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_darkGrey(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.dark_grey = _color_dark_grey()


def _color_light_gray() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_lightGray(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.light_gray = _color_light_gray()


def _color_gray() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_gray(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.gray = _color_gray()


def _color_dark_gray() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_darkGray(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.dark_gray = _color_dark_gray()


def _color_light_charcoal() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_lightCharcoal(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.light_charcoal = _color_light_charcoal()


def _color_charcoal() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_charcoal(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.charcoal = _color_charcoal()


def _color_dark_charcoal() -> Color:
    output = c_void_p()
    _lib.opensolid_Color_darkCharcoal(c_void_p(), ctypes.byref(output))
    return Color(ptr=output)


Color.dark_charcoal = _color_dark_charcoal()


def _vector2d_zero() -> Vector2d_Meters:
    output = c_void_p()
    _lib.opensolid_Vector2d_zero(c_void_p(), ctypes.byref(output))
    return Vector2d_Meters(ptr=output)


Vector2d.zero = _vector2d_zero()


def _direction2d_x() -> Direction2d:
    output = c_void_p()
    _lib.opensolid_Direction2d_x(c_void_p(), ctypes.byref(output))
    return Direction2d(ptr=output)


Direction2d.x = _direction2d_x()


def _direction2d_y() -> Direction2d:
    output = c_void_p()
    _lib.opensolid_Direction2d_y(c_void_p(), ctypes.byref(output))
    return Direction2d(ptr=output)


Direction2d.y = _direction2d_y()


def _direction2d_positive_x() -> Direction2d:
    output = c_void_p()
    _lib.opensolid_Direction2d_positiveX(c_void_p(), ctypes.byref(output))
    return Direction2d(ptr=output)


Direction2d.positive_x = _direction2d_positive_x()


def _direction2d_positive_y() -> Direction2d:
    output = c_void_p()
    _lib.opensolid_Direction2d_positiveY(c_void_p(), ctypes.byref(output))
    return Direction2d(ptr=output)


Direction2d.positive_y = _direction2d_positive_y()


def _direction2d_negative_x() -> Direction2d:
    output = c_void_p()
    _lib.opensolid_Direction2d_negativeX(c_void_p(), ctypes.byref(output))
    return Direction2d(ptr=output)


Direction2d.negative_x = _direction2d_negative_x()


def _direction2d_negative_y() -> Direction2d:
    output = c_void_p()
    _lib.opensolid_Direction2d_negativeY(c_void_p(), ctypes.byref(output))
    return Direction2d(ptr=output)


Direction2d.negative_y = _direction2d_negative_y()


def _point2d_origin() -> Point2d_Meters:
    output = c_void_p()
    _lib.opensolid_Point2d_origin(c_void_p(), ctypes.byref(output))
    return Point2d_Meters(ptr=output)


Point2d.origin = _point2d_origin()


def _curve1d_t() -> Curve1d_Unitless:
    output = c_void_p()
    _lib.opensolid_Curve1d_t(c_void_p(), ctypes.byref(output))
    return Curve1d_Unitless(ptr=output)


Curve1d.t = _curve1d_t()


def _drawing2d_black_stroke() -> Drawing2d.Attribute:
    output = c_void_p()
    _lib.opensolid_Drawing2d_blackStroke(c_void_p(), ctypes.byref(output))
    return Drawing2d.Attribute(ptr=output)


Drawing2d.black_stroke = _drawing2d_black_stroke()


def _drawing2d_no_fill() -> Drawing2d.Attribute:
    output = c_void_p()
    _lib.opensolid_Drawing2d_noFill(c_void_p(), ctypes.byref(output))
    return Drawing2d.Attribute(ptr=output)


Drawing2d.no_fill = _drawing2d_no_fill()
