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


def _load_library() -> CDLL:
    """Load the native library from the same directory as __init__.py."""
    match platform.system():
        case "Windows":
            lib_file_name = "opensolid-ffi.dll"
        case "Darwin":
            lib_file_name = "libopensolid-ffi.dylib"
        case "Linux":
            lib_file_name = "libopensolid-ffi.so"
        case unsupported_system:
            raise OSError(unsupported_system + " is not yet supported")
    self_dir = Path(__file__).parent
    lib_path = self_dir / lib_file_name
    return ctypes.cdll.LoadLibrary(str(lib_path))


_lib: CDLL = _load_library()

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


def _error(message: str) -> Any:  # noqa: ANN401
    raise Error(message)


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
        message = 'No float tolerance set, please set one using "with Tolerance(...)"'
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
        message = 'No length tolerance set, please set one using "with Tolerance(...)"'
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
        message = 'No angle tolerance set, please set one using "with Tolerance(...)"'
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


class _List_c_double(Structure):
    _fields_ = [("field0", c_int64), ("field1", POINTER(c_double))]


class Length:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

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
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Length_inMeters(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_centimeters(self) -> float:
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Length_inCentimeters(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_millimeters(self) -> float:
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Length_inMillimeters(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_inches(self) -> float:
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Length_inInches(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Length):
            inputs = _Tuple2_c_void_p_c_void_p(self._ptr, other._ptr)
            output = c_int64()
            _lib.opensolid_Length_eq(ctypes.byref(inputs), ctypes.byref(output))
            return bool(output.value)
        return False

    def _compare(self, other: Length) -> int:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, other._ptr)
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
        _lib.opensolid_Length_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Length(ptr=output)

    @overload
    def __add__(self, rhs: Length) -> Length:
        pass

    @overload
    def __add__(self, rhs: LengthRange) -> LengthRange:
        pass

    @overload
    def __add__(self, rhs: LengthCurve) -> LengthCurve:
        pass

    def __add__(self, rhs):
        match rhs:
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_add_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case LengthRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_add_Length_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case LengthCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_add_Length_LengthCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: Length) -> Length:
        pass

    @overload
    def __sub__(self, rhs: LengthRange) -> LengthRange:
        pass

    @overload
    def __sub__(self, rhs: LengthCurve) -> LengthCurve:
        pass

    def __sub__(self, rhs):
        match rhs:
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_sub_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case LengthRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_sub_Length_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case LengthCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_sub_Length_LengthCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __mul__(self, rhs: float) -> Length:
        pass

    @overload
    def __mul__(self, rhs: Range) -> LengthRange:
        pass

    @overload
    def __mul__(self, rhs: Curve) -> LengthCurve:
        pass

    @overload
    def __mul__(self, rhs: Direction2d) -> Displacement2d:
        pass

    @overload
    def __mul__(self, rhs: Vector2d) -> Displacement2d:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case Direction2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_Direction2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Displacement2d(ptr=output)
            case Vector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_Vector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Displacement2d(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Length:
        pass

    @overload
    def __truediv__(self, rhs: Length) -> float:
        pass

    @overload
    def __truediv__(self, rhs: Range) -> LengthRange:
        pass

    @overload
    def __truediv__(self, rhs: LengthRange) -> Range:
        pass

    @overload
    def __truediv__(self, rhs: Curve) -> LengthCurve:
        pass

    @overload
    def __truediv__(self, rhs: LengthCurve) -> Curve:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Length_div_Length_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_double()
                _lib.opensolid_Length_div_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return output.value
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_div_Length_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case LengthRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_div_Length_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_div_Length_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case LengthCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_div_Length_LengthCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case _:
                return NotImplemented

    def __floordiv__(self, rhs: Length) -> int:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_int64()
        _lib.opensolid_Length_floorDiv_Length_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def __mod__(self, rhs: Length) -> Length:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Length_mod_Length_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def __rmul__(self, lhs: float) -> Length:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Length_mul_Float_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def __repr__(self) -> str:
        if self == Length.zero:
            return "Length.zero"
        return "Length.meters(" + str(self.in_meters()) + ")"


class Angle:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

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
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Angle_inRadians(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_degrees(self) -> float:
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Angle_inDegrees(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_turns(self) -> float:
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Angle_inTurns(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Angle):
            inputs = _Tuple2_c_void_p_c_void_p(self._ptr, other._ptr)
            output = c_int64()
            _lib.opensolid_Angle_eq(ctypes.byref(inputs), ctypes.byref(output))
            return bool(output.value)
        return False

    def _compare(self, other: Angle) -> int:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, other._ptr)
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
        _lib.opensolid_Angle_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Angle(ptr=output)

    @overload
    def __add__(self, rhs: Angle) -> Angle:
        pass

    @overload
    def __add__(self, rhs: AngleRange) -> AngleRange:
        pass

    @overload
    def __add__(self, rhs: AngleCurve) -> AngleCurve:
        pass

    def __add__(self, rhs):
        match rhs:
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Angle_add_Angle_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Angle(ptr=output)
            case AngleRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Angle_add_Angle_AngleRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleRange(ptr=output)
            case AngleCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Angle_add_Angle_AngleCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: Angle) -> Angle:
        pass

    @overload
    def __sub__(self, rhs: AngleRange) -> AngleRange:
        pass

    @overload
    def __sub__(self, rhs: AngleCurve) -> AngleCurve:
        pass

    def __sub__(self, rhs):
        match rhs:
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Angle_sub_Angle_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Angle(ptr=output)
            case AngleRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Angle_sub_Angle_AngleRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleRange(ptr=output)
            case AngleCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Angle_sub_Angle_AngleCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __mul__(self, rhs: float) -> Angle:
        pass

    @overload
    def __mul__(self, rhs: Range) -> AngleRange:
        pass

    @overload
    def __mul__(self, rhs: Curve) -> AngleCurve:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Angle_mul_Angle_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Angle(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Angle_mul_Angle_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleRange(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Angle_mul_Angle_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Angle:
        pass

    @overload
    def __truediv__(self, rhs: Angle) -> float:
        pass

    @overload
    def __truediv__(self, rhs: Range) -> AngleRange:
        pass

    @overload
    def __truediv__(self, rhs: AngleRange) -> Range:
        pass

    @overload
    def __truediv__(self, rhs: Curve) -> AngleCurve:
        pass

    @overload
    def __truediv__(self, rhs: AngleCurve) -> Curve:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Angle_div_Angle_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Angle(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_double()
                _lib.opensolid_Angle_div_Angle_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return output.value
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Angle_div_Angle_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleRange(ptr=output)
            case AngleRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Angle_div_Angle_AngleRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Angle_div_Angle_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case AngleCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Angle_div_Angle_AngleCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case _:
                return NotImplemented

    def __floordiv__(self, rhs: Angle) -> int:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_int64()
        _lib.opensolid_Angle_floorDiv_Angle_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def __mod__(self, rhs: Angle) -> Angle:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Angle_mod_Angle_Angle(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    def __rmul__(self, lhs: float) -> Angle:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Angle_mul_Float_Angle(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    def __repr__(self) -> str:
        if self == Angle.zero:
            return "Angle.zero"
        return "Angle.degrees(" + str(self.in_degrees()) + ")"


class Range:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    unit: Range = None  # type: ignore[assignment]

    @staticmethod
    def constant(value: float) -> Range:
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Range_constant_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    @staticmethod
    def from_endpoints(a: float, b: float) -> Range:
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_Range_fromEndpoints_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range(ptr=output)

    @staticmethod
    def hull(values: list[float]) -> Range:
        inputs = (
            _list_argument(
                _List_c_double,
                (c_double * len(values))(*[c_double(item) for item in values]),
            )
            if values
            else _error("List is empty")
        )
        output = c_void_p()
        _lib.opensolid_Range_hull_NonEmptyFloat(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range(ptr=output)

    @staticmethod
    def aggregate(ranges: list[Range]) -> Range:
        inputs = (
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(ranges))(*[item._ptr for item in ranges]),
            )
            if ranges
            else _error("List is empty")
        )
        output = c_void_p()
        _lib.opensolid_Range_aggregate_NonEmptyRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range(ptr=output)

    def endpoints(self) -> tuple[float, float]:
        inputs = self._ptr
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_Range_endpoints(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1)

    def intersection(self, other: Range) -> Range | None:
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = _Maybe_c_void_p()
        _lib.opensolid_Range_intersection_Range(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range(ptr=c_void_p(output.field1)) if output.field0 == 0 else None

    @overload
    def __contains__(self, other: Range) -> bool:
        pass

    @overload
    def __contains__(self, value: float) -> bool:
        pass

    def __contains__(self, *args, **keywords):
        match (args, keywords):
            case ([Range() as other], {}) | ([], {"other": Range() as other}):
                inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
                output = c_int64()
                _lib.opensolid_Range_contains_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return bool(output.value)
            case (
                ([float() | int() as value], {})
                | ([], {"value": float() | int() as value})
            ):
                inputs = _Tuple2_c_double_c_void_p(value, self._ptr)
                output = c_int64()
                _lib.opensolid_Range_contains_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return bool(output.value)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    def __neg__(self) -> Range:
        output = c_void_p()
        _lib.opensolid_Range_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Range(ptr=output)

    @overload
    def __add__(self, rhs: float) -> Range:
        pass

    @overload
    def __add__(self, rhs: Range) -> Range:
        pass

    def __add__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Range_add_Range_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Range_add_Range_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: float) -> Range:
        pass

    @overload
    def __sub__(self, rhs: Range) -> Range:
        pass

    def __sub__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Range_sub_Range_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Range_sub_Range_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __mul__(self, rhs: float) -> Range:
        pass

    @overload
    def __mul__(self, rhs: Range) -> Range:
        pass

    @overload
    def __mul__(self, rhs: Length) -> LengthRange:
        pass

    @overload
    def __mul__(self, rhs: Angle) -> AngleRange:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Range_mul_Range_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Range_mul_Range_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Range_mul_Range_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Range_mul_Range_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleRange(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Range:
        pass

    @overload
    def __truediv__(self, rhs: Range) -> Range:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Range_div_Range_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Range_div_Range_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case _:
                return NotImplemented

    def __radd__(self, lhs: float) -> Range:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Range_add_Float_Range(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    def __rsub__(self, lhs: float) -> Range:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Range_sub_Float_Range(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    def __rmul__(self, lhs: float) -> Range:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Range_mul_Float_Range(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    def __rtruediv__(self, lhs: float) -> Range:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Range_div_Float_Range(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    def __repr__(self) -> str:
        low, high = self.endpoints()
        return "Range.from_endpoints(" + str(low) + "," + str(high) + ")"


class LengthRange:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    @staticmethod
    def constant(value: Length) -> LengthRange:
        inputs = value._ptr
        output = c_void_p()
        _lib.opensolid_LengthRange_constant_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @staticmethod
    def from_endpoints(a: Length, b: Length) -> LengthRange:
        inputs = _Tuple2_c_void_p_c_void_p(a._ptr, b._ptr)
        output = c_void_p()
        _lib.opensolid_LengthRange_fromEndpoints_Length_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @staticmethod
    def meters(a: float, b: float) -> LengthRange:
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_LengthRange_meters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @staticmethod
    def centimeters(a: float, b: float) -> LengthRange:
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_LengthRange_centimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @staticmethod
    def millimeters(a: float, b: float) -> LengthRange:
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_LengthRange_millimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @staticmethod
    def inches(a: float, b: float) -> LengthRange:
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_LengthRange_inches_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @staticmethod
    def hull(values: list[Length]) -> LengthRange:
        inputs = (
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(values))(*[item._ptr for item in values]),
            )
            if values
            else _error("List is empty")
        )
        output = c_void_p()
        _lib.opensolid_LengthRange_hull_NonEmptyLength(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @staticmethod
    def aggregate(ranges: list[LengthRange]) -> LengthRange:
        inputs = (
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(ranges))(*[item._ptr for item in ranges]),
            )
            if ranges
            else _error("List is empty")
        )
        output = c_void_p()
        _lib.opensolid_LengthRange_aggregate_NonEmptyLengthRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    def endpoints(self) -> tuple[Length, Length]:
        inputs = self._ptr
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_LengthRange_endpoints(ctypes.byref(inputs), ctypes.byref(output))
        return (
            Length(ptr=c_void_p(output.field0)),
            Length(ptr=c_void_p(output.field1)),
        )

    def intersection(self, other: LengthRange) -> LengthRange | None:
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = _Maybe_c_void_p()
        _lib.opensolid_LengthRange_intersection_LengthRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=c_void_p(output.field1)) if output.field0 == 0 else None

    @overload
    def __contains__(self, other: LengthRange) -> bool:
        pass

    @overload
    def __contains__(self, value: Length) -> bool:
        pass

    def __contains__(self, *args, **keywords):
        match (args, keywords):
            case (
                ([LengthRange() as other], {})
                | ([], {"other": LengthRange() as other})
            ):
                inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
                output = c_int64()
                _lib.opensolid_LengthRange_contains_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return bool(output.value)
            case ([Length() as value], {}) | ([], {"value": Length() as value}):
                inputs = _Tuple2_c_void_p_c_void_p(value._ptr, self._ptr)
                output = c_int64()
                _lib.opensolid_LengthRange_contains_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return bool(output.value)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    def __neg__(self) -> LengthRange:
        output = c_void_p()
        _lib.opensolid_LengthRange_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return LengthRange(ptr=output)

    @overload
    def __add__(self, rhs: LengthRange) -> LengthRange:
        pass

    @overload
    def __add__(self, rhs: Length) -> LengthRange:
        pass

    def __add__(self, rhs):
        match rhs:
            case LengthRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthRange_add_LengthRange_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthRange_add_LengthRange_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: LengthRange) -> LengthRange:
        pass

    @overload
    def __sub__(self, rhs: Length) -> LengthRange:
        pass

    def __sub__(self, rhs):
        match rhs:
            case LengthRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthRange_sub_LengthRange_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthRange_sub_LengthRange_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case _:
                return NotImplemented

    def __mul__(self, rhs: float) -> LengthRange:
        inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
        output = c_void_p()
        _lib.opensolid_LengthRange_mul_LengthRange_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @overload
    def __truediv__(self, rhs: float) -> LengthRange:
        pass

    @overload
    def __truediv__(self, rhs: LengthRange) -> Range:
        pass

    @overload
    def __truediv__(self, rhs: Range) -> LengthRange:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_LengthRange_div_LengthRange_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case LengthRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthRange_div_LengthRange_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthRange_div_LengthRange_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> LengthRange:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_LengthRange_mul_Float_LengthRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    def __repr__(self) -> str:
        low, high = self.endpoints()
        return (
            "LengthRange.meters("
            + str(low.in_meters())
            + ","
            + str(high.in_meters())
            + ")"
        )


class AngleRange:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    @staticmethod
    def constant(value: Angle) -> AngleRange:
        inputs = value._ptr
        output = c_void_p()
        _lib.opensolid_AngleRange_constant_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    @staticmethod
    def from_endpoints(a: Angle, b: Angle) -> AngleRange:
        inputs = _Tuple2_c_void_p_c_void_p(a._ptr, b._ptr)
        output = c_void_p()
        _lib.opensolid_AngleRange_fromEndpoints_Angle_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    @staticmethod
    def radians(a: float, b: float) -> AngleRange:
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_AngleRange_radians_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    @staticmethod
    def degrees(a: float, b: float) -> AngleRange:
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_AngleRange_degrees_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    @staticmethod
    def turns(a: float, b: float) -> AngleRange:
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_AngleRange_turns_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    @staticmethod
    def hull(values: list[Angle]) -> AngleRange:
        inputs = (
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(values))(*[item._ptr for item in values]),
            )
            if values
            else _error("List is empty")
        )
        output = c_void_p()
        _lib.opensolid_AngleRange_hull_NonEmptyAngle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    @staticmethod
    def aggregate(ranges: list[AngleRange]) -> AngleRange:
        inputs = (
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(ranges))(*[item._ptr for item in ranges]),
            )
            if ranges
            else _error("List is empty")
        )
        output = c_void_p()
        _lib.opensolid_AngleRange_aggregate_NonEmptyAngleRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    def endpoints(self) -> tuple[Angle, Angle]:
        inputs = self._ptr
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_AngleRange_endpoints(ctypes.byref(inputs), ctypes.byref(output))
        return (Angle(ptr=c_void_p(output.field0)), Angle(ptr=c_void_p(output.field1)))

    def intersection(self, other: AngleRange) -> AngleRange | None:
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = _Maybe_c_void_p()
        _lib.opensolid_AngleRange_intersection_AngleRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=c_void_p(output.field1)) if output.field0 == 0 else None

    @overload
    def __contains__(self, other: AngleRange) -> bool:
        pass

    @overload
    def __contains__(self, value: Angle) -> bool:
        pass

    def __contains__(self, *args, **keywords):
        match (args, keywords):
            case ([AngleRange() as other], {}) | ([], {"other": AngleRange() as other}):
                inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
                output = c_int64()
                _lib.opensolid_AngleRange_contains_AngleRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return bool(output.value)
            case ([Angle() as value], {}) | ([], {"value": Angle() as value}):
                inputs = _Tuple2_c_void_p_c_void_p(value._ptr, self._ptr)
                output = c_int64()
                _lib.opensolid_AngleRange_contains_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return bool(output.value)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    def __neg__(self) -> AngleRange:
        output = c_void_p()
        _lib.opensolid_AngleRange_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return AngleRange(ptr=output)

    @overload
    def __add__(self, rhs: AngleRange) -> AngleRange:
        pass

    @overload
    def __add__(self, rhs: Angle) -> AngleRange:
        pass

    def __add__(self, rhs):
        match rhs:
            case AngleRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleRange_add_AngleRange_AngleRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleRange(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleRange_add_AngleRange_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleRange(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: AngleRange) -> AngleRange:
        pass

    @overload
    def __sub__(self, rhs: Angle) -> AngleRange:
        pass

    def __sub__(self, rhs):
        match rhs:
            case AngleRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleRange_sub_AngleRange_AngleRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleRange(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleRange_sub_AngleRange_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleRange(ptr=output)
            case _:
                return NotImplemented

    def __mul__(self, rhs: float) -> AngleRange:
        inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
        output = c_void_p()
        _lib.opensolid_AngleRange_mul_AngleRange_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    @overload
    def __truediv__(self, rhs: float) -> AngleRange:
        pass

    @overload
    def __truediv__(self, rhs: AngleRange) -> Range:
        pass

    @overload
    def __truediv__(self, rhs: Range) -> AngleRange:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_AngleRange_div_AngleRange_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleRange(ptr=output)
            case AngleRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleRange_div_AngleRange_AngleRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleRange_div_AngleRange_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleRange(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> AngleRange:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_AngleRange_mul_Float_AngleRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    def __repr__(self) -> str:
        low, high = self.endpoints()
        return (
            "AngleRange.degrees("
            + str(low.in_degrees())
            + ","
            + str(high.in_degrees())
            + ")"
        )


class Color:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

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
    def rgb_255(red: int, green: int, blue: int) -> Color:
        inputs = _Tuple3_c_int64_c_int64_c_int64(red, green, blue)
        output = c_void_p()
        _lib.opensolid_Color_rgb255_Int_Int_Int(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Color(ptr=output)

    @staticmethod
    def hsl(hue: Angle, saturation: float, lightness: float) -> Color:
        inputs = _Tuple3_c_void_p_c_double_c_double(hue._ptr, saturation, lightness)
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
        inputs = self._ptr
        output = _Text()
        _lib.opensolid_Color_toHex(ctypes.byref(inputs), ctypes.byref(output))
        return _text_to_str(output)

    def components(self) -> tuple[float, float, float]:
        inputs = self._ptr
        output = _Tuple3_c_double_c_double_c_double()
        _lib.opensolid_Color_components(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1, output.field2)

    def components_255(self) -> tuple[int, int, int]:
        inputs = self._ptr
        output = _Tuple3_c_int64_c_int64_c_int64()
        _lib.opensolid_Color_components255(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1, output.field2)

    def __repr__(self) -> str:
        r, g, b = self.components_255()
        return "Color.rgb_255(" + str(r) + "," + str(g) + "," + str(b) + ")"


class Vector2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    zero: Vector2d = None  # type: ignore[assignment]

    @staticmethod
    def unit(direction: Direction2d) -> Vector2d:
        inputs = direction._ptr
        output = c_void_p()
        _lib.opensolid_Vector2d_unit_Direction2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    @staticmethod
    def xy(x_component: float, y_component: float) -> Vector2d:
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Vector2d_xy_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    @staticmethod
    def y(y_component: float) -> Vector2d:
        inputs = c_double(y_component)
        output = c_void_p()
        _lib.opensolid_Vector2d_y_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Vector2d(ptr=output)

    @staticmethod
    def x(x_component: float) -> Vector2d:
        inputs = c_double(x_component)
        output = c_void_p()
        _lib.opensolid_Vector2d_x_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Vector2d(ptr=output)

    @staticmethod
    def from_components(components: tuple[float, float]) -> Vector2d:
        inputs = _Tuple2_c_double_c_double(components[0], components[1])
        output = c_void_p()
        _lib.opensolid_Vector2d_fromComponents_Tuple2FloatFloat(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    def components(self) -> tuple[float, float]:
        inputs = self._ptr
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_Vector2d_components(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1)

    def x_component(self) -> float:
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Vector2d_xComponent(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def y_component(self) -> float:
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Vector2d_yComponent(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def direction(self) -> Direction2d:
        inputs = _Tuple2_c_double_c_void_p(_float_tolerance(), self._ptr)
        output = _Result_c_void_p()
        _lib.opensolid_Vector2d_direction(ctypes.byref(inputs), ctypes.byref(output))
        return (
            Direction2d(ptr=c_void_p(output.field2))
            if output.field0 == 0
            else _error(_text_to_str(output.field1))
        )

    def __neg__(self) -> Vector2d:
        output = c_void_p()
        _lib.opensolid_Vector2d_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Vector2d(ptr=output)

    def __add__(self, rhs: Vector2d) -> Vector2d:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Vector2d_add_Vector2d_Vector2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    def __sub__(self, rhs: Vector2d) -> Vector2d:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Vector2d_sub_Vector2d_Vector2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    @overload
    def __mul__(self, rhs: float) -> Vector2d:
        pass

    @overload
    def __mul__(self, rhs: Length) -> Displacement2d:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Vector2d_mul_Vector2d_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Vector2d_mul_Vector2d_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Displacement2d(ptr=output)
            case _:
                return NotImplemented

    def __truediv__(self, rhs: float) -> Vector2d:
        inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
        output = c_void_p()
        _lib.opensolid_Vector2d_div_Vector2d_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    def __rmul__(self, lhs: float) -> Vector2d:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Vector2d_mul_Float_Vector2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    def __repr__(self) -> str:
        x, y = self.components()
        return "Vector2d.xy(" + str(x) + "," + str(y) + ")"


class Displacement2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    zero: Displacement2d = None  # type: ignore[assignment]

    @staticmethod
    def xy(x_component: Length, y_component: Length) -> Displacement2d:
        inputs = _Tuple2_c_void_p_c_void_p(x_component._ptr, y_component._ptr)
        output = c_void_p()
        _lib.opensolid_Displacement2d_xy_Length_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def x(x_component: Length) -> Displacement2d:
        inputs = x_component._ptr
        output = c_void_p()
        _lib.opensolid_Displacement2d_x_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def y(y_component: Length) -> Displacement2d:
        inputs = y_component._ptr
        output = c_void_p()
        _lib.opensolid_Displacement2d_y_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def meters(x_component: float, y_component: float) -> Displacement2d:
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Displacement2d_meters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def centimeters(x_component: float, y_component: float) -> Displacement2d:
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Displacement2d_centimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def millimeters(x_component: float, y_component: float) -> Displacement2d:
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Displacement2d_millimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def inches(x_component: float, y_component: float) -> Displacement2d:
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Displacement2d_inches_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def from_components(components: tuple[Length, Length]) -> Displacement2d:
        inputs = _Tuple2_c_void_p_c_void_p(components[0]._ptr, components[1]._ptr)
        output = c_void_p()
        _lib.opensolid_Displacement2d_fromComponents_Tuple2LengthLength(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    def components(self) -> tuple[Length, Length]:
        inputs = self._ptr
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_Displacement2d_components(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            Length(ptr=c_void_p(output.field0)),
            Length(ptr=c_void_p(output.field1)),
        )

    def x_component(self) -> Length:
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Displacement2d_xComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def y_component(self) -> Length:
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Displacement2d_yComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def direction(self) -> Direction2d:
        inputs = _Tuple2_c_void_p_c_void_p(_length_tolerance()._ptr, self._ptr)
        output = _Result_c_void_p()
        _lib.opensolid_Displacement2d_direction(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            Direction2d(ptr=c_void_p(output.field2))
            if output.field0 == 0
            else _error(_text_to_str(output.field1))
        )

    def __neg__(self) -> Displacement2d:
        output = c_void_p()
        _lib.opensolid_Displacement2d_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Displacement2d(ptr=output)

    def __add__(self, rhs: Displacement2d) -> Displacement2d:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Displacement2d_add_Displacement2d_Displacement2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    def __sub__(self, rhs: Displacement2d) -> Displacement2d:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Displacement2d_sub_Displacement2d_Displacement2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    def __mul__(self, rhs: float) -> Displacement2d:
        inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
        output = c_void_p()
        _lib.opensolid_Displacement2d_mul_Displacement2d_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @overload
    def __truediv__(self, rhs: float) -> Displacement2d:
        pass

    @overload
    def __truediv__(self, rhs: Length) -> Vector2d:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Displacement2d_div_Displacement2d_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Displacement2d(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Displacement2d_div_Displacement2d_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> Displacement2d:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Displacement2d_mul_Float_Displacement2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    def __repr__(self) -> str:
        x, y = self.components()
        return (
            "Displacement2d.meters("
            + str(x.in_meters())
            + ","
            + str(y.in_meters())
            + ")"
        )


class Direction2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    x: Direction2d = None  # type: ignore[assignment]
    y: Direction2d = None  # type: ignore[assignment]
    positive_x: Direction2d = None  # type: ignore[assignment]
    positive_y: Direction2d = None  # type: ignore[assignment]
    negative_x: Direction2d = None  # type: ignore[assignment]
    negative_y: Direction2d = None  # type: ignore[assignment]

    @staticmethod
    def from_angle(angle: Angle) -> Direction2d:
        inputs = angle._ptr
        output = c_void_p()
        _lib.opensolid_Direction2d_fromAngle_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Direction2d(ptr=output)

    @staticmethod
    def degrees(value: float) -> Direction2d:
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Direction2d_degrees_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Direction2d(ptr=output)

    @staticmethod
    def radians(value: float) -> Direction2d:
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Direction2d_radians_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Direction2d(ptr=output)

    def to_angle(self) -> Angle:
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Direction2d_toAngle(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    def components(self) -> tuple[float, float]:
        inputs = self._ptr
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_Direction2d_components(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (output.field0, output.field1)

    def x_component(self) -> float:
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Direction2d_xComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def y_component(self) -> float:
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Direction2d_yComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def __neg__(self) -> Direction2d:
        output = c_void_p()
        _lib.opensolid_Direction2d_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Direction2d(ptr=output)

    @overload
    def __mul__(self, rhs: float) -> Vector2d:
        pass

    @overload
    def __mul__(self, rhs: Length) -> Displacement2d:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Direction2d_mul_Direction2d_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Direction2d_mul_Direction2d_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Displacement2d(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> Vector2d:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Direction2d_mul_Float_Direction2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    def __repr__(self) -> str:
        return "Direction2d.degrees(" + str(self.to_angle().in_degrees()) + ")"


class Point2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    origin: Point2d = None  # type: ignore[assignment]

    @staticmethod
    def xy(x_coordinate: Length, y_coordinate: Length) -> Point2d:
        inputs = _Tuple2_c_void_p_c_void_p(x_coordinate._ptr, y_coordinate._ptr)
        output = c_void_p()
        _lib.opensolid_Point2d_xy_Length_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    @staticmethod
    def x(x_coordinate: Length) -> Point2d:
        inputs = x_coordinate._ptr
        output = c_void_p()
        _lib.opensolid_Point2d_x_Length(ctypes.byref(inputs), ctypes.byref(output))
        return Point2d(ptr=output)

    @staticmethod
    def y(y_coordinate: Length) -> Point2d:
        inputs = y_coordinate._ptr
        output = c_void_p()
        _lib.opensolid_Point2d_y_Length(ctypes.byref(inputs), ctypes.byref(output))
        return Point2d(ptr=output)

    @staticmethod
    def meters(x_coordinate: float, y_coordinate: float) -> Point2d:
        inputs = _Tuple2_c_double_c_double(x_coordinate, y_coordinate)
        output = c_void_p()
        _lib.opensolid_Point2d_meters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    @staticmethod
    def centimeters(x_coordinate: float, y_coordinate: float) -> Point2d:
        inputs = _Tuple2_c_double_c_double(x_coordinate, y_coordinate)
        output = c_void_p()
        _lib.opensolid_Point2d_centimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    @staticmethod
    def millimeters(x_coordinate: float, y_coordinate: float) -> Point2d:
        inputs = _Tuple2_c_double_c_double(x_coordinate, y_coordinate)
        output = c_void_p()
        _lib.opensolid_Point2d_millimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    @staticmethod
    def inches(x_coordinate: float, y_coordinate: float) -> Point2d:
        inputs = _Tuple2_c_double_c_double(x_coordinate, y_coordinate)
        output = c_void_p()
        _lib.opensolid_Point2d_inches_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    @staticmethod
    def from_coordinates(coordinates: tuple[Length, Length]) -> Point2d:
        inputs = _Tuple2_c_void_p_c_void_p(coordinates[0]._ptr, coordinates[1]._ptr)
        output = c_void_p()
        _lib.opensolid_Point2d_fromCoordinates_Tuple2LengthLength(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    def coordinates(self) -> tuple[Length, Length]:
        inputs = self._ptr
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_Point2d_coordinates(ctypes.byref(inputs), ctypes.byref(output))
        return (
            Length(ptr=c_void_p(output.field0)),
            Length(ptr=c_void_p(output.field1)),
        )

    def x_coordinate(self) -> Length:
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Point2d_xCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return Length(ptr=output)

    def y_coordinate(self) -> Length:
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Point2d_yCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return Length(ptr=output)

    def distance_to(self, other: Point2d) -> Length:
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = c_void_p()
        _lib.opensolid_Point2d_distanceTo_Point2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def midpoint(self, other: Point2d) -> Point2d:
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = c_void_p()
        _lib.opensolid_Point2d_midpoint_Point2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    @overload
    def __sub__(self, rhs: Point2d) -> Displacement2d:
        pass

    @overload
    def __sub__(self, rhs: Displacement2d) -> Point2d:
        pass

    def __sub__(self, rhs):
        match rhs:
            case Point2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Point2d_sub_Point2d_Point2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Displacement2d(ptr=output)
            case Displacement2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Point2d_sub_Point2d_Displacement2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Point2d(ptr=output)
            case _:
                return NotImplemented

    def __add__(self, rhs: Displacement2d) -> Point2d:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Point2d_add_Point2d_Displacement2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    def __repr__(self) -> str:
        x, y = self.coordinates()
        return "Point2d.meters(" + str(x.in_meters()) + "," + str(y.in_meters()) + ")"


class UvPoint:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    origin: UvPoint = None  # type: ignore[assignment]

    @staticmethod
    def uv(u_coordinate: float, v_coordinate: float) -> UvPoint:
        inputs = _Tuple2_c_double_c_double(u_coordinate, v_coordinate)
        output = c_void_p()
        _lib.opensolid_UvPoint_uv_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvPoint(ptr=output)

    @staticmethod
    def u(u_coordinate: float) -> UvPoint:
        inputs = c_double(u_coordinate)
        output = c_void_p()
        _lib.opensolid_UvPoint_u_Float(ctypes.byref(inputs), ctypes.byref(output))
        return UvPoint(ptr=output)

    @staticmethod
    def v(v_coordinate: float) -> UvPoint:
        inputs = c_double(v_coordinate)
        output = c_void_p()
        _lib.opensolid_UvPoint_v_Float(ctypes.byref(inputs), ctypes.byref(output))
        return UvPoint(ptr=output)

    @staticmethod
    def from_coordinates(coordinates: tuple[float, float]) -> UvPoint:
        inputs = _Tuple2_c_double_c_double(coordinates[0], coordinates[1])
        output = c_void_p()
        _lib.opensolid_UvPoint_fromCoordinates_Tuple2FloatFloat(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvPoint(ptr=output)

    def coordinates(self) -> tuple[float, float]:
        inputs = self._ptr
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_UvPoint_coordinates(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1)

    def u_coordinate(self) -> float:
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_UvPoint_uCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def v_coordinate(self) -> float:
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_UvPoint_vCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def distance_to(self, other: UvPoint) -> float:
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = c_double()
        _lib.opensolid_UvPoint_distanceTo_UvPoint(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def midpoint(self, other: UvPoint) -> UvPoint:
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = c_void_p()
        _lib.opensolid_UvPoint_midpoint_UvPoint(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvPoint(ptr=output)

    @overload
    def __sub__(self, rhs: UvPoint) -> Vector2d:
        pass

    @overload
    def __sub__(self, rhs: Vector2d) -> UvPoint:
        pass

    def __sub__(self, rhs):
        match rhs:
            case UvPoint():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_UvPoint_sub_UvPoint_UvPoint(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d(ptr=output)
            case Vector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_UvPoint_sub_UvPoint_Vector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return UvPoint(ptr=output)
            case _:
                return NotImplemented

    def __add__(self, rhs: Vector2d) -> UvPoint:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_UvPoint_add_UvPoint_Vector2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvPoint(ptr=output)

    def __repr__(self) -> str:
        x, y = self.coordinates()
        return "UvPoint.uv(" + str(x) + "," + str(y) + ")"


class Bounds2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    @staticmethod
    def xy(x_coordinate: LengthRange, y_coordinate: LengthRange) -> Bounds2d:
        inputs = _Tuple2_c_void_p_c_void_p(x_coordinate._ptr, y_coordinate._ptr)
        output = c_void_p()
        _lib.opensolid_Bounds2d_xy_LengthRange_LengthRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Bounds2d(ptr=output)

    @staticmethod
    def constant(point: Point2d) -> Bounds2d:
        inputs = point._ptr
        output = c_void_p()
        _lib.opensolid_Bounds2d_constant_Point2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Bounds2d(ptr=output)

    @staticmethod
    def from_corners(p1: Point2d, p2: Point2d) -> Bounds2d:
        inputs = _Tuple2_c_void_p_c_void_p(p1._ptr, p2._ptr)
        output = c_void_p()
        _lib.opensolid_Bounds2d_fromCorners_Point2d_Point2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Bounds2d(ptr=output)

    @staticmethod
    def hull(points: list[Point2d]) -> Bounds2d:
        inputs = (
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(points))(*[item._ptr for item in points]),
            )
            if points
            else _error("List is empty")
        )
        output = c_void_p()
        _lib.opensolid_Bounds2d_hull_NonEmptyPoint2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Bounds2d(ptr=output)

    @staticmethod
    def aggregate(bounds: list[Bounds2d]) -> Bounds2d:
        inputs = (
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(bounds))(*[item._ptr for item in bounds]),
            )
            if bounds
            else _error("List is empty")
        )
        output = c_void_p()
        _lib.opensolid_Bounds2d_aggregate_NonEmptyBounds2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Bounds2d(ptr=output)

    def coordinates(self) -> tuple[LengthRange, LengthRange]:
        inputs = self._ptr
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_Bounds2d_coordinates(ctypes.byref(inputs), ctypes.byref(output))
        return (
            LengthRange(ptr=c_void_p(output.field0)),
            LengthRange(ptr=c_void_p(output.field1)),
        )

    def x_coordinate(self) -> LengthRange:
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Bounds2d_xCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return LengthRange(ptr=output)

    def y_coordinate(self) -> LengthRange:
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Bounds2d_yCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return LengthRange(ptr=output)

    def __repr__(self) -> str:
        x, y = self.coordinates()
        return "Bounds2d.xy(" + repr(x) + "," + repr(y) + ")"


class UvBounds:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    @staticmethod
    def uv(u_coordinate: Range, v_coordinate: Range) -> UvBounds:
        inputs = _Tuple2_c_void_p_c_void_p(u_coordinate._ptr, v_coordinate._ptr)
        output = c_void_p()
        _lib.opensolid_UvBounds_uv_Range_Range(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvBounds(ptr=output)

    @staticmethod
    def constant(point: UvPoint) -> UvBounds:
        inputs = point._ptr
        output = c_void_p()
        _lib.opensolid_UvBounds_constant_UvPoint(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvBounds(ptr=output)

    @staticmethod
    def from_corners(p1: UvPoint, p2: UvPoint) -> UvBounds:
        inputs = _Tuple2_c_void_p_c_void_p(p1._ptr, p2._ptr)
        output = c_void_p()
        _lib.opensolid_UvBounds_fromCorners_UvPoint_UvPoint(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvBounds(ptr=output)

    @staticmethod
    def hull(points: list[UvPoint]) -> UvBounds:
        inputs = (
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(points))(*[item._ptr for item in points]),
            )
            if points
            else _error("List is empty")
        )
        output = c_void_p()
        _lib.opensolid_UvBounds_hull_NonEmptyUvPoint(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvBounds(ptr=output)

    @staticmethod
    def aggregate(bounds: list[UvBounds]) -> UvBounds:
        inputs = (
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(bounds))(*[item._ptr for item in bounds]),
            )
            if bounds
            else _error("List is empty")
        )
        output = c_void_p()
        _lib.opensolid_UvBounds_aggregate_NonEmptyUvBounds(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvBounds(ptr=output)

    def coordinates(self) -> tuple[Range, Range]:
        inputs = self._ptr
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_UvBounds_coordinates(ctypes.byref(inputs), ctypes.byref(output))
        return (Range(ptr=c_void_p(output.field0)), Range(ptr=c_void_p(output.field1)))

    def u_coordinate(self) -> Range:
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_UvBounds_uCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    def v_coordinate(self) -> Range:
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_UvBounds_vCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    def __repr__(self) -> str:
        u, v = self.coordinates()
        return "UvBounds.uv(" + repr(u) + "," + repr(v) + ")"


class Curve:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    t: Curve = None  # type: ignore[assignment]

    def squared(self) -> Curve:
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Curve_squared(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def sqrt(self) -> Curve:
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Curve_sqrt(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def evaluate(self, parameter_value: float) -> float:
        inputs = _Tuple2_c_double_c_void_p(parameter_value, self._ptr)
        output = c_double()
        _lib.opensolid_Curve_evaluate_Float(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def zeros(self) -> list[CurveZero]:
        inputs = _Tuple2_c_double_c_void_p(_float_tolerance(), self._ptr)
        output = _Result_List_c_void_p()
        _lib.opensolid_Curve_zeros(ctypes.byref(inputs), ctypes.byref(output))
        return (
            [
                CurveZero(ptr=c_void_p(item))
                for item in [
                    output.field2.field1[index] for index in range(output.field2.field0)
                ]
            ]
            if output.field0 == 0
            else _error(_text_to_str(output.field1))
        )

    def __neg__(self) -> Curve:
        output = c_void_p()
        _lib.opensolid_Curve_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Curve(ptr=output)

    @overload
    def __add__(self, rhs: float) -> Curve:
        pass

    @overload
    def __add__(self, rhs: Curve) -> Curve:
        pass

    def __add__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Curve_add_Curve_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Curve_add_Curve_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: float) -> Curve:
        pass

    @overload
    def __sub__(self, rhs: Curve) -> Curve:
        pass

    def __sub__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Curve_sub_Curve_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Curve_sub_Curve_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __mul__(self, rhs: float) -> Curve:
        pass

    @overload
    def __mul__(self, rhs: Curve) -> Curve:
        pass

    @overload
    def __mul__(self, rhs: Length) -> LengthCurve:
        pass

    @overload
    def __mul__(self, rhs: Angle) -> AngleCurve:
        pass

    @overload
    def __mul__(self, rhs: LengthCurve) -> LengthCurve:
        pass

    @overload
    def __mul__(self, rhs: AngleCurve) -> AngleCurve:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Curve_mul_Curve_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Curve_mul_Curve_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Curve_mul_Curve_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Curve_mul_Curve_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case LengthCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Curve_mul_Curve_LengthCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case AngleCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Curve_mul_Curve_AngleCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Curve:
        pass

    @overload
    def __truediv__(self, rhs: Curve) -> Curve:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Curve_div_Curve_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Curve_div_Curve_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case _:
                return NotImplemented

    def __radd__(self, lhs: float) -> Curve:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Curve_add_Float_Curve(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def __rsub__(self, lhs: float) -> Curve:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Curve_sub_Float_Curve(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def __rmul__(self, lhs: float) -> Curve:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Curve_mul_Float_Curve(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def __rtruediv__(self, lhs: float) -> Curve:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Curve_div_Float_Curve(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)


class CurveZero:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def location(self) -> float:
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_CurveZero_location(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def order(self) -> int:
        inputs = self._ptr
        output = c_int64()
        _lib.opensolid_CurveZero_order(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def sign(self) -> int:
        inputs = self._ptr
        output = c_int64()
        _lib.opensolid_CurveZero_sign(ctypes.byref(inputs), ctypes.byref(output))
        return output.value


class LengthCurve:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def evaluate(self, parameter_value: float) -> Length:
        inputs = _Tuple2_c_double_c_void_p(parameter_value, self._ptr)
        output = c_void_p()
        _lib.opensolid_LengthCurve_evaluate_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def zeros(self) -> list[CurveZero]:
        inputs = _Tuple2_c_void_p_c_void_p(_length_tolerance()._ptr, self._ptr)
        output = _Result_List_c_void_p()
        _lib.opensolid_LengthCurve_zeros(ctypes.byref(inputs), ctypes.byref(output))
        return (
            [
                CurveZero(ptr=c_void_p(item))
                for item in [
                    output.field2.field1[index] for index in range(output.field2.field0)
                ]
            ]
            if output.field0 == 0
            else _error(_text_to_str(output.field1))
        )

    def __neg__(self) -> LengthCurve:
        output = c_void_p()
        _lib.opensolid_LengthCurve_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return LengthCurve(ptr=output)

    def __add__(self, rhs: LengthCurve) -> LengthCurve:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_LengthCurve_add_LengthCurve_LengthCurve(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthCurve(ptr=output)

    def __sub__(self, rhs: LengthCurve) -> LengthCurve:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_LengthCurve_sub_LengthCurve_LengthCurve(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthCurve(ptr=output)

    @overload
    def __mul__(self, rhs: float) -> LengthCurve:
        pass

    @overload
    def __mul__(self, rhs: Curve) -> LengthCurve:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_LengthCurve_mul_LengthCurve_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthCurve_mul_LengthCurve_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> LengthCurve:
        pass

    @overload
    def __truediv__(self, rhs: LengthCurve) -> Curve:
        pass

    @overload
    def __truediv__(self, rhs: Length) -> Curve:
        pass

    @overload
    def __truediv__(self, rhs: Curve) -> LengthCurve:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_LengthCurve_div_LengthCurve_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case LengthCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthCurve_div_LengthCurve_LengthCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthCurve_div_LengthCurve_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthCurve_div_LengthCurve_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> LengthCurve:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_LengthCurve_mul_Float_LengthCurve(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthCurve(ptr=output)


class AngleCurve:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def sin(self) -> Curve:
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_AngleCurve_sin(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def cos(self) -> Curve:
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_AngleCurve_cos(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def evaluate(self, parameter_value: float) -> Angle:
        inputs = _Tuple2_c_double_c_void_p(parameter_value, self._ptr)
        output = c_void_p()
        _lib.opensolid_AngleCurve_evaluate_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Angle(ptr=output)

    def zeros(self) -> list[CurveZero]:
        inputs = _Tuple2_c_void_p_c_void_p(_angle_tolerance()._ptr, self._ptr)
        output = _Result_List_c_void_p()
        _lib.opensolid_AngleCurve_zeros(ctypes.byref(inputs), ctypes.byref(output))
        return (
            [
                CurveZero(ptr=c_void_p(item))
                for item in [
                    output.field2.field1[index] for index in range(output.field2.field0)
                ]
            ]
            if output.field0 == 0
            else _error(_text_to_str(output.field1))
        )

    def __neg__(self) -> AngleCurve:
        output = c_void_p()
        _lib.opensolid_AngleCurve_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return AngleCurve(ptr=output)

    def __add__(self, rhs: AngleCurve) -> AngleCurve:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_AngleCurve_add_AngleCurve_AngleCurve(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleCurve(ptr=output)

    def __sub__(self, rhs: AngleCurve) -> AngleCurve:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_AngleCurve_sub_AngleCurve_AngleCurve(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleCurve(ptr=output)

    @overload
    def __mul__(self, rhs: float) -> AngleCurve:
        pass

    @overload
    def __mul__(self, rhs: Curve) -> AngleCurve:
        pass

    def __mul__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_AngleCurve_mul_AngleCurve_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleCurve_mul_AngleCurve_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> AngleCurve:
        pass

    @overload
    def __truediv__(self, rhs: AngleCurve) -> Curve:
        pass

    @overload
    def __truediv__(self, rhs: Angle) -> Curve:
        pass

    @overload
    def __truediv__(self, rhs: Curve) -> AngleCurve:
        pass

    def __truediv__(self, rhs):
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_AngleCurve_div_AngleCurve_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case AngleCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleCurve_div_AngleCurve_AngleCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleCurve_div_AngleCurve_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleCurve_div_AngleCurve_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> AngleCurve:
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_AngleCurve_mul_Float_AngleCurve(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleCurve(ptr=output)


class Drawing2d:
    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    black_stroke: Drawing2d.Attribute = None  # type: ignore[assignment]
    no_fill: Drawing2d.Attribute = None  # type: ignore[assignment]

    @staticmethod
    def to_svg(view_box: Bounds2d, entities: list[Drawing2d.Entity]) -> str:
        inputs = _Tuple2_c_void_p_List_c_void_p(
            view_box._ptr,
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(entities))(*[item._ptr for item in entities]),
            ),
        )
        output = _Text()
        _lib.opensolid_Drawing2d_toSVG_Bounds2d_ListDrawing2dEntity(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return _text_to_str(output)

    @staticmethod
    def polygon(
        attributes: list[Drawing2d.Attribute], vertices: list[Point2d]
    ) -> Drawing2d.Entity:
        inputs = _Tuple2_List_c_void_p_List_c_void_p(
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(attributes))(*[item._ptr for item in attributes]),
            ),
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(vertices))(*[item._ptr for item in vertices]),
            ),
        )
        output = c_void_p()
        _lib.opensolid_Drawing2d_polygon_ListDrawing2dAttribute_ListPoint2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Drawing2d.Entity(ptr=output)

    @staticmethod
    def circle(
        attributes: list[Drawing2d.Attribute], center_point: Point2d, radius: Length
    ) -> Drawing2d.Entity:
        inputs = _Tuple3_List_c_void_p_c_void_p_c_void_p(
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(attributes))(*[item._ptr for item in attributes]),
            ),
            center_point._ptr,
            radius._ptr,
        )
        output = c_void_p()
        _lib.opensolid_Drawing2d_circle_ListDrawing2dAttribute_Point2d_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Drawing2d.Entity(ptr=output)

    @staticmethod
    def stroke_color(color: Color) -> Drawing2d.Attribute:
        inputs = color._ptr
        output = c_void_p()
        _lib.opensolid_Drawing2d_strokeColor_Color(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Drawing2d.Attribute(ptr=output)

    @staticmethod
    def fill_color(color: Color) -> Drawing2d.Attribute:
        inputs = color._ptr
        output = c_void_p()
        _lib.opensolid_Drawing2d_fillColor_Color(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Drawing2d.Attribute(ptr=output)

    class Entity:
        def __init__(self, *, ptr: c_void_p) -> None:
            self._ptr = ptr

    class Attribute:
        def __init__(self, *, ptr: c_void_p) -> None:
            self._ptr = ptr


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


def _range_unit() -> Range:
    output = c_void_p()
    _lib.opensolid_Range_unit(c_void_p(), ctypes.byref(output))
    return Range(ptr=output)


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


def _vector2d_zero() -> Vector2d:
    output = c_void_p()
    _lib.opensolid_Vector2d_zero(c_void_p(), ctypes.byref(output))
    return Vector2d(ptr=output)


Vector2d.zero = _vector2d_zero()


def _displacement2d_zero() -> Displacement2d:
    output = c_void_p()
    _lib.opensolid_Displacement2d_zero(c_void_p(), ctypes.byref(output))
    return Displacement2d(ptr=output)


Displacement2d.zero = _displacement2d_zero()


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


def _point2d_origin() -> Point2d:
    output = c_void_p()
    _lib.opensolid_Point2d_origin(c_void_p(), ctypes.byref(output))
    return Point2d(ptr=output)


Point2d.origin = _point2d_origin()


def _uvpoint_origin() -> UvPoint:
    output = c_void_p()
    _lib.opensolid_UvPoint_origin(c_void_p(), ctypes.byref(output))
    return UvPoint(ptr=output)


UvPoint.origin = _uvpoint_origin()


def _curve_t() -> Curve:
    output = c_void_p()
    _lib.opensolid_Curve_t(c_void_p(), ctypes.byref(output))
    return Curve(ptr=output)


Curve.t = _curve_t()


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
