"""A collection of classes for 2D/3D geometric modelling."""

from __future__ import annotations

import ctypes
import platform
import threading
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
    """An error that may be thrown by OpenSolid functions."""


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


class _Tolerance(threading.local):
    value: float | Length | Area | Angle | None = None


class Tolerance:
    """Manages a thread-local tolerance value.

    Many functions in OpenSolid require a tolerance to be set.
    You should generally choose a value that is
    much smaller than any meaningful size/dimension in the geometry you're modelling,
    but significantly *larger* than any expected numerical roundoff that might occur.
    A good default choice is roughly one-billionth of the overall size of your geometry;
    for 'human-scale' things (say, from an earring up to a house)
    that means that one nanometer is a reasonable value to use.

    Passing a tolerance into every function that needed one would get very verbose,
    and it's very common to choose a single tolerance value and use it throughout a project.
    However, it's also occasionally necessary to set a different tolerance for some code.
    This class allows managing tolerances using Python's ``with`` statement, e.g.::

        with Tolerance(Length.nanometer):
            do_something()
            do_something_else()
            with Tolerance(Angle.degrees(0.001)):
                compare_two_angles()
            do_more_things()

    In the above code, the ``Length.nanometer`` tolerance value
    will be used for ``do_something()`` and ``do_something_else()``
    (and any functions they call).
    The ``Angle.degrees(0.001))`` tolerance value
    will then be used for ``compare_two_angles()``,
    and then the ``Length.nanometer`` tolerance value will be restored
    and used for ``do_more_things()``.
    """

    _value: float | Length | Area | Angle | None = None
    _saved: float | Length | Area | Angle | None = None

    def __init__(self, value: float | Length | Area | Angle | None) -> None:
        self._value = value

    def __enter__(self) -> None:
        """Set the given tolerance as the currently active one."""
        self._saved = _Tolerance.value
        _Tolerance.value = self._value

    def __exit__(
        self, _exception_type: object, _exception_value: object, _traceback: object
    ) -> None:
        """Restore the previous tolerance as the currently active one."""
        _Tolerance.value = self._saved
        self._saved = None

    @staticmethod
    def current() -> float | Length | Area | Angle | None:
        """Get the current tolerance value."""
        return _Tolerance.value


def _float_tolerance() -> float:
    if isinstance(_Tolerance.value, float):
        return _Tolerance.value
    if _Tolerance.value is None:
        message = 'No float tolerance set, please set one using "with Tolerance(...)"'
        raise TypeError(message)
    message = (
        "Expected a tolerance of type float but current tolerance is of type "
        + type(_Tolerance.value).__name__
    )
    raise TypeError(message)


def _length_tolerance() -> Length:
    if isinstance(_Tolerance.value, Length):
        return _Tolerance.value
    if _Tolerance.value is None:
        message = 'No length tolerance set, please set one using "with Tolerance(...)"'
        raise TypeError(message)
    message = (
        "Expected a tolerance of type Length but current tolerance is of type "
        + type(_Tolerance.value).__name__
    )
    raise TypeError(message)


def _area_tolerance() -> Area:
    if isinstance(_Tolerance.value, Area):
        return _Tolerance.value
    if _Tolerance.value is None:
        message = 'No area tolerance set, please set one using "with Tolerance(...)"'
        raise TypeError(message)
    message = (
        "Expected a tolerance of type Area but current tolerance is of type "
        + type(_Tolerance.value).__name__
    )
    raise TypeError(message)


def _angle_tolerance() -> Angle:
    if isinstance(_Tolerance.value, Angle):
        return _Tolerance.value
    if _Tolerance.value is None:
        message = 'No angle tolerance set, please set one using "with Tolerance(...)"'
        raise TypeError(message)
    message = (
        "Expected a tolerance of type Angle but current tolerance is of type "
        + type(_Tolerance.value).__name__
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
    """A length in millimeters, meters, inches etc.

    Represented internally as a value in meters.
    """

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    zero: Length = None  # type: ignore[assignment]
    """The zero value."""

    meter: Length = None  # type: ignore[assignment]
    """One meter."""

    centimeter: Length = None  # type: ignore[assignment]
    """One centimeter."""

    millimeter: Length = None  # type: ignore[assignment]
    """One millimeter."""

    micrometer: Length = None  # type: ignore[assignment]
    """One micrometer."""

    nanometer: Length = None  # type: ignore[assignment]
    """One nanometer."""

    inch: Length = None  # type: ignore[assignment]
    """One inch."""

    pixel: Length = None  # type: ignore[assignment]
    """One CSS pixel, equal to 1/96 of an inch."""

    @staticmethod
    def meters(value: float) -> Length:
        """Construct a length from a number of meters."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Length_meters_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Length(ptr=output)

    @staticmethod
    def centimeters(value: float) -> Length:
        """Construct a length from a number of centimeters."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Length_centimeters_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    @staticmethod
    def millimeters(value: float) -> Length:
        """Construct a length value from a number of millimeters."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Length_millimeters_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    @staticmethod
    def micrometers(value: float) -> Length:
        """Construct a length from a number of micrometers."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Length_micrometers_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    @staticmethod
    def nanometers(value: float) -> Length:
        """Construct a length from a number of nanometers."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Length_nanometers_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    @staticmethod
    def inches(value: float) -> Length:
        """Construct a length from a number of inches."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Length_inches_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Length(ptr=output)

    @staticmethod
    def pixels(value: float) -> Length:
        """Construct a length from a number of CSS pixels."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Length_pixels_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Length(ptr=output)

    def in_meters(self) -> float:
        """Convert a length to a number of meters."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Length_inMeters(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_centimeters(self) -> float:
        """Convert a length to a number of centimeters."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Length_inCentimeters(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_millimeters(self) -> float:
        """Convert a length to a number of millimeters."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Length_inMillimeters(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_micrometers(self) -> float:
        """Convert a length to a number of micrometers."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Length_inMicrometers(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_nanometers(self) -> float:
        """Convert a length to a number of nanometers."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Length_inNanometers(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_inches(self) -> float:
        """Convert a length to a number of inches."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Length_inInches(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_pixels(self) -> float:
        """Convert a length into a number of CSS pixels."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Length_inPixels(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def is_zero(self) -> bool:
        """Check if a length is zero, within the current tolerance."""
        inputs = _Tuple2_c_void_p_c_void_p(_length_tolerance()._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_Length_isZero(ctypes.byref(inputs), ctypes.byref(output))
        return bool(output.value)

    def __eq__(self, other: object) -> bool:
        """Return ``self == other``.

        Note that this is an *exact* comparison; for a tolerant comparison
        (one which will return true if two values are *almost* equal)
        you'll likely want to use an ``is_zero()`` method instead.
        """
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
        """Return ``self < other``."""
        return self._compare(other) < 0

    def __le__(self, other: Length) -> bool:
        """Return ``self <= other``."""
        return self._compare(other) <= 0

    def __ge__(self, other: Length) -> bool:
        """Return ``self >= other``."""
        return self._compare(other) >= 0

    def __gt__(self, other: Length) -> bool:
        """Return ``self > other``."""
        return self._compare(other) > 0

    def __neg__(self) -> Length:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_Length_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Length(ptr=output)

    def __abs__(self) -> Length:
        """Return ``abs(self)``."""
        output = c_void_p()
        _lib.opensolid_Length_abs(ctypes.byref(self._ptr), ctypes.byref(output))
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
        """Return ``self + rhs``."""
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
        """Return ``self - rhs``."""
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
    def __mul__(self, rhs: Length) -> Area:
        pass

    @overload
    def __mul__(self, rhs: Range) -> LengthRange:
        pass

    @overload
    def __mul__(self, rhs: LengthRange) -> AreaRange:
        pass

    @overload
    def __mul__(self, rhs: Curve) -> LengthCurve:
        pass

    @overload
    def __mul__(self, rhs: LengthCurve) -> AreaCurve:
        pass

    @overload
    def __mul__(self, rhs: Direction2d) -> Displacement2d:
        pass

    @overload
    def __mul__(self, rhs: Vector2d) -> Displacement2d:
        pass

    @overload
    def __mul__(self, rhs: Displacement2d) -> AreaVector2d:
        pass

    def __mul__(self, rhs):
        """Return ``self * rhs``."""
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case LengthRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case LengthCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_LengthCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaCurve(ptr=output)
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
            case Displacement2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Length_mul_Length_Displacement2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaVector2d(ptr=output)
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
        """Return ``self / rhs``."""
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
        """Return ``self // rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_int64()
        _lib.opensolid_Length_floorDiv_Length_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def __mod__(self, rhs: Length) -> Length:
        """Return ``self % rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Length_mod_Length_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def __rmul__(self, lhs: float) -> Length:
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Length_mul_Float_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        return "Length.meters(" + str(self.in_meters()) + ")"


class Area:
    """An area in square meters, square inches etc.

    Represented internally as a value in square meters.
    """

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    zero: Area = None  # type: ignore[assignment]
    """The zero value."""

    square_meter: Area = None  # type: ignore[assignment]
    """One square meter."""

    square_inch: Area = None  # type: ignore[assignment]
    """One square inch."""

    @staticmethod
    def square_meters(value: float) -> Area:
        """Construct an area from a number of square meters."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Area_squareMeters_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Area(ptr=output)

    @staticmethod
    def square_inches(value: float) -> Area:
        """Construct an area from a number of square inches."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Area_squareInches_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Area(ptr=output)

    def in_square_meters(self) -> float:
        """Convert an area to a number of square meters."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Area_inSquareMeters(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_square_inches(self) -> float:
        """Convert an area to a number of square inches."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Area_inSquareInches(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def is_zero(self) -> bool:
        """Check if an area is zero, within the current tolerance."""
        inputs = _Tuple2_c_void_p_c_void_p(_area_tolerance()._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_Area_isZero(ctypes.byref(inputs), ctypes.byref(output))
        return bool(output.value)

    def __eq__(self, other: object) -> bool:
        """Return ``self == other``.

        Note that this is an *exact* comparison; for a tolerant comparison
        (one which will return true if two values are *almost* equal)
        you'll likely want to use an ``is_zero()`` method instead.
        """
        if isinstance(other, Area):
            inputs = _Tuple2_c_void_p_c_void_p(self._ptr, other._ptr)
            output = c_int64()
            _lib.opensolid_Area_eq(ctypes.byref(inputs), ctypes.byref(output))
            return bool(output.value)
        return False

    def _compare(self, other: Area) -> int:
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, other._ptr)
        output = c_int64()
        _lib.opensolid_Area_compare(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def __lt__(self, other: Area) -> bool:
        """Return ``self < other``."""
        return self._compare(other) < 0

    def __le__(self, other: Area) -> bool:
        """Return ``self <= other``."""
        return self._compare(other) <= 0

    def __ge__(self, other: Area) -> bool:
        """Return ``self >= other``."""
        return self._compare(other) >= 0

    def __gt__(self, other: Area) -> bool:
        """Return ``self > other``."""
        return self._compare(other) > 0

    def __neg__(self) -> Area:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_Area_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Area(ptr=output)

    def __abs__(self) -> Area:
        """Return ``abs(self)``."""
        output = c_void_p()
        _lib.opensolid_Area_abs(ctypes.byref(self._ptr), ctypes.byref(output))
        return Area(ptr=output)

    @overload
    def __add__(self, rhs: Area) -> Area:
        pass

    @overload
    def __add__(self, rhs: AreaRange) -> AreaRange:
        pass

    @overload
    def __add__(self, rhs: AreaCurve) -> AreaCurve:
        pass

    def __add__(self, rhs):
        """Return ``self + rhs``."""
        match rhs:
            case Area():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_add_Area_Area(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case AreaRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_add_Area_AreaRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case AreaCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_add_Area_AreaCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: Area) -> Area:
        pass

    @overload
    def __sub__(self, rhs: AreaRange) -> AreaRange:
        pass

    @overload
    def __sub__(self, rhs: AreaCurve) -> AreaCurve:
        pass

    def __sub__(self, rhs):
        """Return ``self - rhs``."""
        match rhs:
            case Area():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_sub_Area_Area(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case AreaRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_sub_Area_AreaRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case AreaCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_sub_Area_AreaCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __mul__(self, rhs: float) -> Area:
        pass

    @overload
    def __mul__(self, rhs: Range) -> AreaRange:
        pass

    @overload
    def __mul__(self, rhs: Curve) -> AreaCurve:
        pass

    @overload
    def __mul__(self, rhs: Direction2d) -> AreaVector2d:
        pass

    @overload
    def __mul__(self, rhs: Vector2d) -> AreaVector2d:
        pass

    def __mul__(self, rhs):
        """Return ``self * rhs``."""
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Area_mul_Area_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_mul_Area_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_mul_Area_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaCurve(ptr=output)
            case Direction2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_mul_Area_Direction2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaVector2d(ptr=output)
            case Vector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_mul_Area_Vector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaVector2d(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Area:
        pass

    @overload
    def __truediv__(self, rhs: Area) -> float:
        pass

    @overload
    def __truediv__(self, rhs: Length) -> Length:
        pass

    @overload
    def __truediv__(self, rhs: Range) -> AreaRange:
        pass

    @overload
    def __truediv__(self, rhs: LengthRange) -> LengthRange:
        pass

    @overload
    def __truediv__(self, rhs: AreaRange) -> Range:
        pass

    @overload
    def __truediv__(self, rhs: Curve) -> AreaCurve:
        pass

    @overload
    def __truediv__(self, rhs: LengthCurve) -> LengthCurve:
        pass

    @overload
    def __truediv__(self, rhs: AreaCurve) -> Curve:
        pass

    def __truediv__(self, rhs):
        """Return ``self / rhs``."""
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Area_div_Area_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case Area():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_double()
                _lib.opensolid_Area_div_Area_Area(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return output.value
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_div_Area_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_div_Area_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case LengthRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_div_Area_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case AreaRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_div_Area_AreaRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_div_Area_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaCurve(ptr=output)
            case LengthCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_div_Area_LengthCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case AreaCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Area_div_Area_AreaCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case _:
                return NotImplemented

    def __floordiv__(self, rhs: Area) -> int:
        """Return ``self // rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_int64()
        _lib.opensolid_Area_floorDiv_Area_Area(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def __mod__(self, rhs: Area) -> Area:
        """Return ``self % rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Area_mod_Area_Area(ctypes.byref(inputs), ctypes.byref(output))
        return Area(ptr=output)

    def __rmul__(self, lhs: float) -> Area:
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Area_mul_Float_Area(ctypes.byref(inputs), ctypes.byref(output))
        return Area(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        return "Area.square_meters(" + str(self.in_square_meters()) + ")"


class Angle:
    """An angle in degrees, radians, turns etc.

    Represented internally as a value in radians.
    """

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    zero: Angle = None  # type: ignore[assignment]
    """The zero value."""

    golden_angle: Angle = None  # type: ignore[assignment]
    """The [golden angle](https://en.wikipedia.org/wiki/Golden_angle)."""

    radian: Angle = None  # type: ignore[assignment]
    """One radian."""

    degree: Angle = None  # type: ignore[assignment]
    """One degree."""

    full_turn: Angle = None  # type: ignore[assignment]
    """One full turn, or 360 degrees."""

    half_turn: Angle = None  # type: ignore[assignment]
    """One half turn, or 180 degrees."""

    quarter_turn: Angle = None  # type: ignore[assignment]
    """One quarter turn, or 90 degrees."""

    pi: Angle = None  # type: ignore[assignment]
    """π radians, or 180 degrees."""

    two_pi: Angle = None  # type: ignore[assignment]
    """2π radians, or 360 degrees."""

    @staticmethod
    def radians(value: float) -> Angle:
        """Construct an angle from a number of radians."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Angle_radians_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    @staticmethod
    def degrees(value: float) -> Angle:
        """Construct an angle from a number of degrees."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Angle_degrees_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    @staticmethod
    def turns(value: float) -> Angle:
        """Construct an angle from a number of turns.

        One turn is equal to 360 degrees.
        """
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Angle_turns_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    @staticmethod
    def acos(value: float) -> Angle:
        """Compute the inverse cosine of a value."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Angle_acos_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    @staticmethod
    def asin(value: float) -> Angle:
        """Compute the inverse sine of a value."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Angle_asin_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    @staticmethod
    def atan(value: float) -> Angle:
        """Compute the inverse tangent of a value."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Angle_atan_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    def in_radians(self) -> float:
        """Convert an angle to a number of radians."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Angle_inRadians(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_degrees(self) -> float:
        """Convert an angle to a number of degrees."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Angle_inDegrees(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def in_turns(self) -> float:
        """Convert an angle to a number of turns.

        One turn is equal to 360 degrees.
        """
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Angle_inTurns(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def is_zero(self) -> bool:
        """Check if an angle is zero, within the current tolerance."""
        inputs = _Tuple2_c_void_p_c_void_p(_angle_tolerance()._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_Angle_isZero(ctypes.byref(inputs), ctypes.byref(output))
        return bool(output.value)

    def sin(self) -> float:
        """Compute the sine of an angle."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Angle_sin(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def cos(self) -> float:
        """Compute the cosine of an angle."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Angle_cos(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def tan(self) -> float:
        """Compute the tangent of an angle."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Angle_tan(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def __eq__(self, other: object) -> bool:
        """Return ``self == other``.

        Note that this is an *exact* comparison; for a tolerant comparison
        (one which will return true if two values are *almost* equal)
        you'll likely want to use an ``is_zero()`` method instead.
        """
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
        """Return ``self < other``."""
        return self._compare(other) < 0

    def __le__(self, other: Angle) -> bool:
        """Return ``self <= other``."""
        return self._compare(other) <= 0

    def __ge__(self, other: Angle) -> bool:
        """Return ``self >= other``."""
        return self._compare(other) >= 0

    def __gt__(self, other: Angle) -> bool:
        """Return ``self > other``."""
        return self._compare(other) > 0

    def __neg__(self) -> Angle:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_Angle_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Angle(ptr=output)

    def __abs__(self) -> Angle:
        """Return ``abs(self)``."""
        output = c_void_p()
        _lib.opensolid_Angle_abs(ctypes.byref(self._ptr), ctypes.byref(output))
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
        """Return ``self + rhs``."""
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
        """Return ``self - rhs``."""
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
        """Return ``self * rhs``."""
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
        """Return ``self / rhs``."""
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
        """Return ``self // rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_int64()
        _lib.opensolid_Angle_floorDiv_Angle_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def __mod__(self, rhs: Angle) -> Angle:
        """Return ``self % rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Angle_mod_Angle_Angle(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    def __rmul__(self, lhs: float) -> Angle:
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Angle_mul_Float_Angle(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        return "Angle.degrees(" + str(self.in_degrees()) + ")"


class Range:
    """A range of unitless values, with a lower bound and upper bound."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    unit: Range = None  # type: ignore[assignment]
    """The range with endoints [0,1]."""

    @staticmethod
    def constant(value: float) -> Range:
        """Construct a zero-width range containing a single value."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Range_constant_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    @staticmethod
    def from_endpoints(a: float, b: float) -> Range:
        """Construct a range from its lower and upper bounds.

        The order of the two arguments does not matter;
        the minimum of the two will be used as the lower bound of the range
        and the maximum will be used as the upper bound.
        """
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_Range_fromEndpoints_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range(ptr=output)

    @staticmethod
    def hull(values: list[float]) -> Range:
        """Build a range containing all values in the given non-empty list."""
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
        """Build a range containing all ranges in the given non-empty list."""
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
        """Get the lower and upper bounds of a range."""
        inputs = self._ptr
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_Range_endpoints(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1)

    def lower_bound(self) -> float:
        """Get the lower bound of a range."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Range_lowerBound(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def upper_bound(self) -> float:
        """Get the upper bound of a range."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Range_upperBound(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def intersection(self, other: Range) -> Range | None:
        """Attempt to find the intersection of two ranges."""
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = _Maybe_c_void_p()
        _lib.opensolid_Range_intersection_Range(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Range(ptr=c_void_p(output.field1)) if output.field0 == 0 else None

    def includes(self, value: float) -> bool:
        """Check if a given value is included in a range.

        Note that this does *not* use a tolerance, so use with care -
        for example, a value *just* outside the range (due to numerical roundoff)
        will be reported as not included.
        """
        inputs = _Tuple2_c_double_c_void_p(value, self._ptr)
        output = c_int64()
        _lib.opensolid_Range_includes_Float(ctypes.byref(inputs), ctypes.byref(output))
        return bool(output.value)

    def contains(self, other: Range) -> bool:
        """Check if one range contains another.

        Note that this does *not* use a tolerance, so use with care -
        for example, a range that extends *just* outside another range (due to numerical
        roundoff) will be reported as not contained by that range.
        """
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_Range_contains_Range(ctypes.byref(inputs), ctypes.byref(output))
        return bool(output.value)

    def __neg__(self) -> Range:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_Range_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Range(ptr=output)

    def __abs__(self) -> Range:
        """Return ``abs(self)``."""
        output = c_void_p()
        _lib.opensolid_Range_abs(ctypes.byref(self._ptr), ctypes.byref(output))
        return Range(ptr=output)

    @overload
    def __add__(self, rhs: float) -> Range:
        pass

    @overload
    def __add__(self, rhs: Range) -> Range:
        pass

    def __add__(self, rhs):
        """Return ``self + rhs``."""
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
        """Return ``self - rhs``."""
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

    @overload
    def __mul__(self, rhs: LengthRange) -> LengthRange:
        pass

    @overload
    def __mul__(self, rhs: AreaRange) -> AreaRange:
        pass

    def __mul__(self, rhs):
        """Return ``self * rhs``."""
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
            case LengthRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Range_mul_Range_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case AreaRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Range_mul_Range_AreaRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Range:
        pass

    @overload
    def __truediv__(self, rhs: Range) -> Range:
        pass

    def __truediv__(self, rhs):
        """Return ``self / rhs``."""
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
        """Return ``lhs + self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Range_add_Float_Range(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    def __rsub__(self, lhs: float) -> Range:
        """Return ``lhs - self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Range_sub_Float_Range(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    def __rmul__(self, lhs: float) -> Range:
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Range_mul_Float_Range(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    def __rtruediv__(self, lhs: float) -> Range:
        """Return ``lhs / self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Range_div_Float_Range(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        low, high = self.endpoints()
        return "Range.from_endpoints(" + str(low) + "," + str(high) + ")"


class LengthRange:
    """A range of length values, with a lower bound and upper bound."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    @staticmethod
    def constant(value: Length) -> LengthRange:
        """Construct a zero-width range containing a single value."""
        inputs = value._ptr
        output = c_void_p()
        _lib.opensolid_LengthRange_constant_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @staticmethod
    def from_endpoints(a: Length, b: Length) -> LengthRange:
        """Construct a range from its lower and upper bounds.

        The order of the two arguments does not matter;
        the minimum of the two will be used as the lower bound of the range
        and the maximum will be used as the upper bound.
        """
        inputs = _Tuple2_c_void_p_c_void_p(a._ptr, b._ptr)
        output = c_void_p()
        _lib.opensolid_LengthRange_fromEndpoints_Length_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @staticmethod
    def meters(a: float, b: float) -> LengthRange:
        """Construct a length range from lower and upper bounds given in meters."""
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_LengthRange_meters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @staticmethod
    def centimeters(a: float, b: float) -> LengthRange:
        """Construct a length range from lower and upper bounds given in centimeters."""
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_LengthRange_centimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @staticmethod
    def millimeters(a: float, b: float) -> LengthRange:
        """Construct a length range from lower and upper bounds given in millimeters."""
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_LengthRange_millimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @staticmethod
    def inches(a: float, b: float) -> LengthRange:
        """Construct a length range from lower and upper bounds given in inches."""
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_LengthRange_inches_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    @staticmethod
    def hull(values: list[Length]) -> LengthRange:
        """Build a range containing all values in the given non-empty list."""
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
        """Build a range containing all ranges in the given non-empty list."""
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
        """Get the lower and upper bounds of a range."""
        inputs = self._ptr
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_LengthRange_endpoints(ctypes.byref(inputs), ctypes.byref(output))
        return (
            Length(ptr=c_void_p(output.field0)),
            Length(ptr=c_void_p(output.field1)),
        )

    def intersection(self, other: LengthRange) -> LengthRange | None:
        """Attempt to find the intersection of two ranges."""
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = _Maybe_c_void_p()
        _lib.opensolid_LengthRange_intersection_LengthRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=c_void_p(output.field1)) if output.field0 == 0 else None

    def includes(self, value: Length) -> bool:
        """Check if a given value is included in a range.

        Note that this does *not* use a tolerance, so use with care -
        for example, a value *just* outside the range (due to numerical roundoff)
        will be reported as not included.
        """
        inputs = _Tuple2_c_void_p_c_void_p(value._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_LengthRange_includes_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return bool(output.value)

    def contains(self, other: LengthRange) -> bool:
        """Check if one range contains another.

        Note that this does *not* use a tolerance, so use with care -
        for example, a range that extends *just* outside another range (due to numerical
        roundoff) will be reported as not contained by that range.
        """
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_LengthRange_contains_LengthRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return bool(output.value)

    def __neg__(self) -> LengthRange:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_LengthRange_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return LengthRange(ptr=output)

    def __abs__(self) -> LengthRange:
        """Return ``abs(self)``."""
        output = c_void_p()
        _lib.opensolid_LengthRange_abs(ctypes.byref(self._ptr), ctypes.byref(output))
        return LengthRange(ptr=output)

    @overload
    def __add__(self, rhs: LengthRange) -> LengthRange:
        pass

    @overload
    def __add__(self, rhs: Length) -> LengthRange:
        pass

    def __add__(self, rhs):
        """Return ``self + rhs``."""
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
        """Return ``self - rhs``."""
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

    @overload
    def __mul__(self, rhs: float) -> LengthRange:
        pass

    @overload
    def __mul__(self, rhs: LengthRange) -> AreaRange:
        pass

    @overload
    def __mul__(self, rhs: Length) -> AreaRange:
        pass

    @overload
    def __mul__(self, rhs: Range) -> LengthRange:
        pass

    def __mul__(self, rhs):
        """Return ``self * rhs``."""
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_LengthRange_mul_LengthRange_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case LengthRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthRange_mul_LengthRange_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthRange_mul_LengthRange_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthRange_mul_LengthRange_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> LengthRange:
        pass

    @overload
    def __truediv__(self, rhs: LengthRange) -> Range:
        pass

    @overload
    def __truediv__(self, rhs: Length) -> Range:
        pass

    @overload
    def __truediv__(self, rhs: Range) -> LengthRange:
        pass

    def __truediv__(self, rhs):
        """Return ``self / rhs``."""
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
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthRange_div_LengthRange_Length(
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
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_LengthRange_mul_Float_LengthRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthRange(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        low, high = self.endpoints()
        return (
            "LengthRange.meters("
            + str(low.in_meters())
            + ","
            + str(high.in_meters())
            + ")"
        )


class AreaRange:
    """A range of area values, with a lower bound and upper bound."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    @staticmethod
    def constant(value: Area) -> AreaRange:
        """Construct a zero-width range containing a single value."""
        inputs = value._ptr
        output = c_void_p()
        _lib.opensolid_AreaRange_constant_Area(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaRange(ptr=output)

    @staticmethod
    def square_meters(a: float, b: float) -> AreaRange:
        """Construct an area range from lower and upper bounds given in square meters."""
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_AreaRange_squareMeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaRange(ptr=output)

    @staticmethod
    def from_endpoints(a: Area, b: Area) -> AreaRange:
        """Construct a range from its lower and upper bounds.

        The order of the two arguments does not matter;
        the minimum of the two will be used as the lower bound of the range
        and the maximum will be used as the upper bound.
        """
        inputs = _Tuple2_c_void_p_c_void_p(a._ptr, b._ptr)
        output = c_void_p()
        _lib.opensolid_AreaRange_fromEndpoints_Area_Area(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaRange(ptr=output)

    @staticmethod
    def hull(values: list[Area]) -> AreaRange:
        """Build a range containing all values in the given non-empty list."""
        inputs = (
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(values))(*[item._ptr for item in values]),
            )
            if values
            else _error("List is empty")
        )
        output = c_void_p()
        _lib.opensolid_AreaRange_hull_NonEmptyArea(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaRange(ptr=output)

    @staticmethod
    def aggregate(ranges: list[AreaRange]) -> AreaRange:
        """Build a range containing all ranges in the given non-empty list."""
        inputs = (
            _list_argument(
                _List_c_void_p,
                (c_void_p * len(ranges))(*[item._ptr for item in ranges]),
            )
            if ranges
            else _error("List is empty")
        )
        output = c_void_p()
        _lib.opensolid_AreaRange_aggregate_NonEmptyAreaRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaRange(ptr=output)

    def endpoints(self) -> tuple[Area, Area]:
        """Get the lower and upper bounds of a range."""
        inputs = self._ptr
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_AreaRange_endpoints(ctypes.byref(inputs), ctypes.byref(output))
        return (Area(ptr=c_void_p(output.field0)), Area(ptr=c_void_p(output.field1)))

    def intersection(self, other: AreaRange) -> AreaRange | None:
        """Attempt to find the intersection of two ranges."""
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = _Maybe_c_void_p()
        _lib.opensolid_AreaRange_intersection_AreaRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaRange(ptr=c_void_p(output.field1)) if output.field0 == 0 else None

    def includes(self, value: Area) -> bool:
        """Check if a given value is included in a range.

        Note that this does *not* use a tolerance, so use with care -
        for example, a value *just* outside the range (due to numerical roundoff)
        will be reported as not included.
        """
        inputs = _Tuple2_c_void_p_c_void_p(value._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_AreaRange_includes_Area(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return bool(output.value)

    def contains(self, other: AreaRange) -> bool:
        """Check if one range contains another.

        Note that this does *not* use a tolerance, so use with care -
        for example, a range that extends *just* outside another range (due to numerical
        roundoff) will be reported as not contained by that range.
        """
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_AreaRange_contains_AreaRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return bool(output.value)

    def __neg__(self) -> AreaRange:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_AreaRange_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return AreaRange(ptr=output)

    def __abs__(self) -> AreaRange:
        """Return ``abs(self)``."""
        output = c_void_p()
        _lib.opensolid_AreaRange_abs(ctypes.byref(self._ptr), ctypes.byref(output))
        return AreaRange(ptr=output)

    @overload
    def __add__(self, rhs: AreaRange) -> AreaRange:
        pass

    @overload
    def __add__(self, rhs: Area) -> AreaRange:
        pass

    def __add__(self, rhs):
        """Return ``self + rhs``."""
        match rhs:
            case AreaRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaRange_add_AreaRange_AreaRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case Area():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaRange_add_AreaRange_Area(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: AreaRange) -> AreaRange:
        pass

    @overload
    def __sub__(self, rhs: Area) -> AreaRange:
        pass

    def __sub__(self, rhs):
        """Return ``self - rhs``."""
        match rhs:
            case AreaRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaRange_sub_AreaRange_AreaRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case Area():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaRange_sub_AreaRange_Area(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __mul__(self, rhs: float) -> AreaRange:
        pass

    @overload
    def __mul__(self, rhs: Range) -> AreaRange:
        pass

    def __mul__(self, rhs):
        """Return ``self * rhs``."""
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_AreaRange_mul_AreaRange_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaRange_mul_AreaRange_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> AreaRange:
        pass

    @overload
    def __truediv__(self, rhs: AreaRange) -> Range:
        pass

    @overload
    def __truediv__(self, rhs: Length) -> LengthRange:
        pass

    @overload
    def __truediv__(self, rhs: Area) -> Range:
        pass

    @overload
    def __truediv__(self, rhs: Range) -> AreaRange:
        pass

    @overload
    def __truediv__(self, rhs: LengthRange) -> LengthRange:
        pass

    def __truediv__(self, rhs):
        """Return ``self / rhs``."""
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_AreaRange_div_AreaRange_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case AreaRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaRange_div_AreaRange_AreaRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaRange_div_AreaRange_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case Area():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaRange_div_AreaRange_Area(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Range(ptr=output)
            case Range():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaRange_div_AreaRange_Range(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaRange(ptr=output)
            case LengthRange():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaRange_div_AreaRange_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> AreaRange:
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_AreaRange_mul_Float_AreaRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaRange(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        low, high = self.endpoints()
        return (
            "AreaRange.square_meters("
            + str(low.in_square_meters())
            + ","
            + str(high.in_square_meters())
            + ")"
        )


class AngleRange:
    """A range of angle values, with a lower bound and upper bound."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    @staticmethod
    def constant(value: Angle) -> AngleRange:
        """Construct a zero-width range containing a single value."""
        inputs = value._ptr
        output = c_void_p()
        _lib.opensolid_AngleRange_constant_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    @staticmethod
    def from_endpoints(a: Angle, b: Angle) -> AngleRange:
        """Construct a range from its lower and upper bounds.

        The order of the two arguments does not matter;
        the minimum of the two will be used as the lower bound of the range
        and the maximum will be used as the upper bound.
        """
        inputs = _Tuple2_c_void_p_c_void_p(a._ptr, b._ptr)
        output = c_void_p()
        _lib.opensolid_AngleRange_fromEndpoints_Angle_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    @staticmethod
    def radians(a: float, b: float) -> AngleRange:
        """Construct an angle range from lower and upper bounds given in radians."""
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_AngleRange_radians_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    @staticmethod
    def degrees(a: float, b: float) -> AngleRange:
        """Construct an angle range from lower and upper bounds given in degrees."""
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_AngleRange_degrees_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    @staticmethod
    def turns(a: float, b: float) -> AngleRange:
        """Construct an angle range from lower and upper bounds given in turns."""
        inputs = _Tuple2_c_double_c_double(a, b)
        output = c_void_p()
        _lib.opensolid_AngleRange_turns_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    @staticmethod
    def hull(values: list[Angle]) -> AngleRange:
        """Build a range containing all values in the given non-empty list."""
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
        """Build a range containing all ranges in the given non-empty list."""
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
        """Get the lower and upper bounds of a range."""
        inputs = self._ptr
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_AngleRange_endpoints(ctypes.byref(inputs), ctypes.byref(output))
        return (Angle(ptr=c_void_p(output.field0)), Angle(ptr=c_void_p(output.field1)))

    def intersection(self, other: AngleRange) -> AngleRange | None:
        """Attempt to find the intersection of two ranges."""
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = _Maybe_c_void_p()
        _lib.opensolid_AngleRange_intersection_AngleRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=c_void_p(output.field1)) if output.field0 == 0 else None

    def includes(self, value: Angle) -> bool:
        """Check if a given value is included in a range.

        Note that this does *not* use a tolerance, so use with care -
        for example, a value *just* outside the range (due to numerical roundoff)
        will be reported as not included.
        """
        inputs = _Tuple2_c_void_p_c_void_p(value._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_AngleRange_includes_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return bool(output.value)

    def contains(self, other: AngleRange) -> bool:
        """Check if one range contains another.

        Note that this does *not* use a tolerance, so use with care -
        for example, a range that extends *just* outside another range (due to numerical
        roundoff) will be reported as not contained by that range.
        """
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_AngleRange_contains_AngleRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return bool(output.value)

    def __neg__(self) -> AngleRange:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_AngleRange_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return AngleRange(ptr=output)

    def __abs__(self) -> AngleRange:
        """Return ``abs(self)``."""
        output = c_void_p()
        _lib.opensolid_AngleRange_abs(ctypes.byref(self._ptr), ctypes.byref(output))
        return AngleRange(ptr=output)

    @overload
    def __add__(self, rhs: AngleRange) -> AngleRange:
        pass

    @overload
    def __add__(self, rhs: Angle) -> AngleRange:
        pass

    def __add__(self, rhs):
        """Return ``self + rhs``."""
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
        """Return ``self - rhs``."""
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
        """Return ``self * rhs``."""
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
        """Return ``self / rhs``."""
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
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_AngleRange_mul_Float_AngleRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleRange(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        low, high = self.endpoints()
        return (
            "AngleRange.degrees("
            + str(low.in_degrees())
            + ","
            + str(high.in_degrees())
            + ")"
        )


class Color:
    """An RGB color value."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    red: Color = None  # type: ignore[assignment]
    """Scarlet Red from the Tango icon theme."""

    dark_red: Color = None  # type: ignore[assignment]
    """Dark Scarlet Red from the Tango icon theme."""

    light_orange: Color = None  # type: ignore[assignment]
    """Light Orange from the Tango icon theme."""

    orange: Color = None  # type: ignore[assignment]
    """Orange from the Tango icon theme."""

    dark_orange: Color = None  # type: ignore[assignment]
    """Dark Orange from the Tango icon theme."""

    light_yellow: Color = None  # type: ignore[assignment]
    """Light Butter from the Tango icon theme."""

    yellow: Color = None  # type: ignore[assignment]
    """Butter from the Tango icon theme."""

    dark_yellow: Color = None  # type: ignore[assignment]
    """Dark Butter from the Tango icon theme."""

    light_green: Color = None  # type: ignore[assignment]
    """Light Chameleon from the Tango icon theme."""

    green: Color = None  # type: ignore[assignment]
    """Chameleon from the Tango icon theme."""

    dark_green: Color = None  # type: ignore[assignment]
    """Dark Chameleon from the Tango icon theme."""

    light_blue: Color = None  # type: ignore[assignment]
    """Light Sky Blue from the Tango icon theme."""

    blue: Color = None  # type: ignore[assignment]
    """Sky Blue from the Tango icon theme."""

    dark_blue: Color = None  # type: ignore[assignment]
    """Dark Sky Blue from the Tango icon theme."""

    light_purple: Color = None  # type: ignore[assignment]
    """Light Plum from the Tango icon theme."""

    purple: Color = None  # type: ignore[assignment]
    """Plum from the Tango icon theme."""

    dark_purple: Color = None  # type: ignore[assignment]
    """Dark Plum from the Tango icon theme."""

    light_brown: Color = None  # type: ignore[assignment]
    """Light Chocolate from the Tango icon theme."""

    brown: Color = None  # type: ignore[assignment]
    """Chocolate from the Tango icon theme."""

    dark_brown: Color = None  # type: ignore[assignment]
    """Dark Chocolate from the Tango icon theme."""

    black: Color = None  # type: ignore[assignment]
    """Black."""

    white: Color = None  # type: ignore[assignment]
    """White."""

    light_grey: Color = None  # type: ignore[assignment]
    """Aluminium 1/6 from the Tango icon theme."""

    grey: Color = None  # type: ignore[assignment]
    """Aluminium 2/6 from the Tango icon theme."""

    dark_grey: Color = None  # type: ignore[assignment]
    """Aluminium 3/6 from the Tango icon theme."""

    light_gray: Color = None  # type: ignore[assignment]
    """Aluminium 1/6 from the Tango icon theme."""

    gray: Color = None  # type: ignore[assignment]
    """Aluminium 2/6 from the Tango icon theme."""

    dark_gray: Color = None  # type: ignore[assignment]
    """Aluminium 3/6 from the Tango icon theme."""

    light_charcoal: Color = None  # type: ignore[assignment]
    """Aluminium 4/6 from the Tango icon theme."""

    charcoal: Color = None  # type: ignore[assignment]
    """Aluminium 5/6 from the Tango icon theme."""

    dark_charcoal: Color = None  # type: ignore[assignment]
    """Aluminium 6/6 from the Tango icon theme."""

    @staticmethod
    def rgb(red: float, green: float, blue: float) -> Color:
        """Construct a color from its RGB components, in the range [0,1]."""
        inputs = _Tuple3_c_double_c_double_c_double(red, green, blue)
        output = c_void_p()
        _lib.opensolid_Color_rgb_Float_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Color(ptr=output)

    @staticmethod
    def rgb_255(red: int, green: int, blue: int) -> Color:
        """Construct a color from its RGB components, in the range [0,255]."""
        inputs = _Tuple3_c_int64_c_int64_c_int64(red, green, blue)
        output = c_void_p()
        _lib.opensolid_Color_rgb255_Int_Int_Int(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Color(ptr=output)

    @staticmethod
    def hsl(hue: Angle, saturation: float, lightness: float) -> Color:
        """Construct a color from its hue, saturation and lightness values."""
        inputs = _Tuple3_c_void_p_c_double_c_double(hue._ptr, saturation, lightness)
        output = c_void_p()
        _lib.opensolid_Color_hsl_Angle_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Color(ptr=output)

    @staticmethod
    def from_hex(hex_string: str) -> Color:
        """Construct a color from a hex string such as '#f3f3f3' or 'f3f3f3'."""
        inputs = _str_to_text(hex_string)
        output = c_void_p()
        _lib.opensolid_Color_fromHex_Text(ctypes.byref(inputs), ctypes.byref(output))
        return Color(ptr=output)

    def to_hex(self) -> str:
        """Convert a color to a hex string such as '#f3f3f3'."""
        inputs = self._ptr
        output = _Text()
        _lib.opensolid_Color_toHex(ctypes.byref(inputs), ctypes.byref(output))
        return _text_to_str(output)

    def components(self) -> tuple[float, float, float]:
        """Get the RGB components of a color as values in the range [0,1]."""
        inputs = self._ptr
        output = _Tuple3_c_double_c_double_c_double()
        _lib.opensolid_Color_components(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1, output.field2)

    def components_255(self) -> tuple[int, int, int]:
        """Get the RGB components of a color as values in the range [0,255]."""
        inputs = self._ptr
        output = _Tuple3_c_int64_c_int64_c_int64()
        _lib.opensolid_Color_components255(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1, output.field2)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        r, g, b = self.components_255()
        return "Color.rgb_255(" + str(r) + "," + str(g) + "," + str(b) + ")"


class Vector2d:
    """A unitless vector in 2D."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    zero: Vector2d = None  # type: ignore[assignment]
    """The zero vector."""

    @staticmethod
    def unit(direction: Direction2d) -> Vector2d:
        """Construct a unit vector in the given direction."""
        inputs = direction._ptr
        output = c_void_p()
        _lib.opensolid_Vector2d_unit_Direction2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    @staticmethod
    def xy(x_component: float, y_component: float) -> Vector2d:
        """Construct a vector from its X and Y components."""
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Vector2d_xy_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    @staticmethod
    def y(y_component: float) -> Vector2d:
        """Construct a vector from just a Y component.

        The X component will be set to zero.
        """
        inputs = c_double(y_component)
        output = c_void_p()
        _lib.opensolid_Vector2d_y_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Vector2d(ptr=output)

    @staticmethod
    def x(x_component: float) -> Vector2d:
        """Construct a vector from just an X component.

        The Y component will be set to zero.
        """
        inputs = c_double(x_component)
        output = c_void_p()
        _lib.opensolid_Vector2d_x_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Vector2d(ptr=output)

    @staticmethod
    def polar(magnitude: float, angle: Angle) -> Vector2d:
        """Construct a vector from its magnitude (length) and angle."""
        inputs = _Tuple2_c_double_c_void_p(magnitude, angle._ptr)
        output = c_void_p()
        _lib.opensolid_Vector2d_polar_Float_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    @staticmethod
    def from_components(components: tuple[float, float]) -> Vector2d:
        """Construct a vector from a pair of X and Y components."""
        inputs = _Tuple2_c_double_c_double(components[0], components[1])
        output = c_void_p()
        _lib.opensolid_Vector2d_fromComponents_Tuple2FloatFloat(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    def components(self) -> tuple[float, float]:
        """Get the X and Y components of a vector."""
        inputs = self._ptr
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_Vector2d_components(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1)

    def x_component(self) -> float:
        """Get the X component of a vector."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Vector2d_xComponent(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def y_component(self) -> float:
        """Get the Y component of a vector."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Vector2d_yComponent(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def direction(self) -> Direction2d:
        """Attempt to get the direction of a vector.

        The current tolerance will be used to check if the vector is zero
        (and therefore does not have a direction).
        """
        inputs = _Tuple2_c_double_c_void_p(_float_tolerance(), self._ptr)
        output = _Result_c_void_p()
        _lib.opensolid_Vector2d_direction(ctypes.byref(inputs), ctypes.byref(output))
        return (
            Direction2d(ptr=c_void_p(output.field2))
            if output.field0 == 0
            else _error(_text_to_str(output.field1))
        )

    def angle(self) -> Angle:
        """Get the angle of a vector.

        The angle is measured counterclockwise from the positive X axis, so:

          * A vector in the positive X direction has an angle of zero.
          * A vector in the positive Y direction has an angle of 90 degrees.
          * A vector in the negative Y direction has an angle of -90 degrees.
          * It is not defined whether a vector exactly in the negative X direction has
            an angle of -180 or +180 degrees. (Currently it is reported as having an
            angle of +180 degrees, but this should not be relied upon.)

        The returned angle will be between -180 and +180 degrees.
        """
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Vector2d_angle(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    def is_zero(self) -> bool:
        """Check if a vector is zero, within the current tolerance."""
        inputs = _Tuple2_c_double_c_void_p(_float_tolerance(), self._ptr)
        output = c_int64()
        _lib.opensolid_Vector2d_isZero(ctypes.byref(inputs), ctypes.byref(output))
        return bool(output.value)

    def __neg__(self) -> Vector2d:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_Vector2d_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Vector2d(ptr=output)

    def __add__(self, rhs: Vector2d) -> Vector2d:
        """Return ``self + rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Vector2d_add_Vector2d_Vector2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    def __sub__(self, rhs: Vector2d) -> Vector2d:
        """Return ``self - rhs``."""
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

    @overload
    def __mul__(self, rhs: Area) -> AreaVector2d:
        pass

    def __mul__(self, rhs):
        """Return ``self * rhs``."""
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
            case Area():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Vector2d_mul_Vector2d_Area(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaVector2d(ptr=output)
            case _:
                return NotImplemented

    def __truediv__(self, rhs: float) -> Vector2d:
        """Return ``self / rhs``."""
        inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
        output = c_void_p()
        _lib.opensolid_Vector2d_div_Vector2d_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    @overload
    def dot(self, rhs: Vector2d) -> float:
        pass

    @overload
    def dot(self, rhs: Displacement2d) -> Length:
        pass

    @overload
    def dot(self, rhs: AreaVector2d) -> Area:
        pass

    @overload
    def dot(self, rhs: Direction2d) -> float:
        pass

    def dot(self, rhs):
        """Compute the dot product of two vector-like values."""
        match rhs:
            case Vector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_double()
                _lib.opensolid_Vector2d_dot_Vector2d_Vector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return output.value
            case Displacement2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Vector2d_dot_Vector2d_Displacement2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case AreaVector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Vector2d_dot_Vector2d_AreaVector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case Direction2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_double()
                _lib.opensolid_Vector2d_dot_Vector2d_Direction2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return output.value
            case _:
                return NotImplemented

    @overload
    def cross(self, rhs: Vector2d) -> float:
        pass

    @overload
    def cross(self, rhs: Displacement2d) -> Length:
        pass

    @overload
    def cross(self, rhs: AreaVector2d) -> Area:
        pass

    @overload
    def cross(self, rhs: Direction2d) -> float:
        pass

    def cross(self, rhs):
        """Compute the cross product of two vector-like values."""
        match rhs:
            case Vector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_double()
                _lib.opensolid_Vector2d_cross_Vector2d_Vector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return output.value
            case Displacement2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Vector2d_cross_Vector2d_Displacement2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case AreaVector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Vector2d_cross_Vector2d_AreaVector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case Direction2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_double()
                _lib.opensolid_Vector2d_cross_Vector2d_Direction2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return output.value
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> Vector2d:
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Vector2d_mul_Float_Vector2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        x, y = self.components()
        return "Vector2d.xy(" + str(x) + "," + str(y) + ")"


class Displacement2d:
    """A displacement vector in 2D."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    zero: Displacement2d = None  # type: ignore[assignment]
    """The zero vector."""

    @staticmethod
    def xy(x_component: Length, y_component: Length) -> Displacement2d:
        """Construct a vector from its X and Y components."""
        inputs = _Tuple2_c_void_p_c_void_p(x_component._ptr, y_component._ptr)
        output = c_void_p()
        _lib.opensolid_Displacement2d_xy_Length_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def x(x_component: Length) -> Displacement2d:
        """Construct a vector from just an X component.

        The Y component will be set to zero.
        """
        inputs = x_component._ptr
        output = c_void_p()
        _lib.opensolid_Displacement2d_x_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def y(y_component: Length) -> Displacement2d:
        """Construct a vector from just a Y component.

        The X component will be set to zero.
        """
        inputs = y_component._ptr
        output = c_void_p()
        _lib.opensolid_Displacement2d_y_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def polar(magnitude: Length, angle: Angle) -> Displacement2d:
        """Construct a vector from its magnitude (length) and angle."""
        inputs = _Tuple2_c_void_p_c_void_p(magnitude._ptr, angle._ptr)
        output = c_void_p()
        _lib.opensolid_Displacement2d_polar_Length_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def meters(x_component: float, y_component: float) -> Displacement2d:
        """Construct a vector from its X and Y components given in meters."""
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Displacement2d_meters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def centimeters(x_component: float, y_component: float) -> Displacement2d:
        """Construct a vector from its X and Y components given in centimeters."""
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Displacement2d_centimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def millimeters(x_component: float, y_component: float) -> Displacement2d:
        """Construct a vector from its X and Y components given in millimeters."""
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Displacement2d_millimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def inches(x_component: float, y_component: float) -> Displacement2d:
        """Construct a vector from its X and Y components given in inches."""
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_Displacement2d_inches_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @staticmethod
    def from_components(components: tuple[Length, Length]) -> Displacement2d:
        """Construct a vector from a pair of X and Y components."""
        inputs = _Tuple2_c_void_p_c_void_p(components[0]._ptr, components[1]._ptr)
        output = c_void_p()
        _lib.opensolid_Displacement2d_fromComponents_Tuple2LengthLength(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    def components(self) -> tuple[Length, Length]:
        """Get the X and Y components of a vector."""
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
        """Get the X component of a vector."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Displacement2d_xComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def y_component(self) -> Length:
        """Get the Y component of a vector."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Displacement2d_yComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def direction(self) -> Direction2d:
        """Attempt to get the direction of a vector.

        The current tolerance will be used to check if the vector is zero
        (and therefore does not have a direction).
        """
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

    def angle(self) -> Angle:
        """Get the angle of a vector.

        The angle is measured counterclockwise from the positive X axis, so:

          * A vector in the positive X direction has an angle of zero.
          * A vector in the positive Y direction has an angle of 90 degrees.
          * A vector in the negative Y direction has an angle of -90 degrees.
          * It is not defined whether a vector exactly in the negative X direction has
            an angle of -180 or +180 degrees. (Currently it is reported as having an
            angle of +180 degrees, but this should not be relied upon.)

        The returned angle will be between -180 and +180 degrees.
        """
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Displacement2d_angle(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    def is_zero(self) -> bool:
        """Check if a displacement is zero, within the current tolerance."""
        inputs = _Tuple2_c_void_p_c_void_p(_length_tolerance()._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_Displacement2d_isZero(ctypes.byref(inputs), ctypes.byref(output))
        return bool(output.value)

    def __neg__(self) -> Displacement2d:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_Displacement2d_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Displacement2d(ptr=output)

    def __add__(self, rhs: Displacement2d) -> Displacement2d:
        """Return ``self + rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Displacement2d_add_Displacement2d_Displacement2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    def __sub__(self, rhs: Displacement2d) -> Displacement2d:
        """Return ``self - rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Displacement2d_sub_Displacement2d_Displacement2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    @overload
    def __mul__(self, rhs: float) -> Displacement2d:
        pass

    @overload
    def __mul__(self, rhs: Length) -> AreaVector2d:
        pass

    def __mul__(self, rhs):
        """Return ``self * rhs``."""
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_Displacement2d_mul_Displacement2d_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Displacement2d(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Displacement2d_mul_Displacement2d_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaVector2d(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> Displacement2d:
        pass

    @overload
    def __truediv__(self, rhs: Length) -> Vector2d:
        pass

    def __truediv__(self, rhs):
        """Return ``self / rhs``."""
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

    @overload
    def dot(self, rhs: Displacement2d) -> Area:
        pass

    @overload
    def dot(self, rhs: Vector2d) -> Length:
        pass

    @overload
    def dot(self, rhs: Direction2d) -> Length:
        pass

    def dot(self, rhs):
        """Compute the dot product of two vector-like values."""
        match rhs:
            case Displacement2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Displacement2d_dot_Displacement2d_Displacement2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case Vector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Displacement2d_dot_Displacement2d_Vector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case Direction2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Displacement2d_dot_Displacement2d_Direction2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case _:
                return NotImplemented

    @overload
    def cross(self, rhs: Displacement2d) -> Area:
        pass

    @overload
    def cross(self, rhs: Vector2d) -> Length:
        pass

    @overload
    def cross(self, rhs: Direction2d) -> Length:
        pass

    def cross(self, rhs):
        """Compute the cross product of two vector-like values."""
        match rhs:
            case Displacement2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Displacement2d_cross_Displacement2d_Displacement2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case Vector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Displacement2d_cross_Displacement2d_Vector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case Direction2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Displacement2d_cross_Displacement2d_Direction2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> Displacement2d:
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Displacement2d_mul_Float_Displacement2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Displacement2d(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        x, y = self.components()
        return (
            "Displacement2d.meters("
            + str(x.in_meters())
            + ","
            + str(y.in_meters())
            + ")"
        )


class AreaVector2d:
    """A vector in 2D with units of area."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    zero: AreaVector2d = None  # type: ignore[assignment]
    """The zero vector."""

    @staticmethod
    def xy(x_component: Area, y_component: Area) -> AreaVector2d:
        """Construct a vector from its X and Y components."""
        inputs = _Tuple2_c_void_p_c_void_p(x_component._ptr, y_component._ptr)
        output = c_void_p()
        _lib.opensolid_AreaVector2d_xy_Area_Area(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaVector2d(ptr=output)

    @staticmethod
    def x(x_component: Area) -> AreaVector2d:
        """Construct a vector from just an X component.

        The Y component will be set to zero.
        """
        inputs = x_component._ptr
        output = c_void_p()
        _lib.opensolid_AreaVector2d_x_Area(ctypes.byref(inputs), ctypes.byref(output))
        return AreaVector2d(ptr=output)

    @staticmethod
    def y(y_component: Area) -> AreaVector2d:
        """Construct a vector from just a Y component.

        The X component will be set to zero.
        """
        inputs = y_component._ptr
        output = c_void_p()
        _lib.opensolid_AreaVector2d_y_Area(ctypes.byref(inputs), ctypes.byref(output))
        return AreaVector2d(ptr=output)

    @staticmethod
    def polar(magnitude: Area, angle: Angle) -> AreaVector2d:
        """Construct a vector from its magnitude (length) and angle."""
        inputs = _Tuple2_c_void_p_c_void_p(magnitude._ptr, angle._ptr)
        output = c_void_p()
        _lib.opensolid_AreaVector2d_polar_Area_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaVector2d(ptr=output)

    @staticmethod
    def square_meters(x_component: float, y_component: float) -> AreaVector2d:
        """Construct a vector from its X and Y components given in square meters."""
        inputs = _Tuple2_c_double_c_double(x_component, y_component)
        output = c_void_p()
        _lib.opensolid_AreaVector2d_squareMeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaVector2d(ptr=output)

    @staticmethod
    def from_components(components: tuple[Area, Area]) -> AreaVector2d:
        """Construct a vector from a pair of X and Y components."""
        inputs = _Tuple2_c_void_p_c_void_p(components[0]._ptr, components[1]._ptr)
        output = c_void_p()
        _lib.opensolid_AreaVector2d_fromComponents_Tuple2AreaArea(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaVector2d(ptr=output)

    def components(self) -> tuple[Area, Area]:
        """Get the X and Y components of a vector."""
        inputs = self._ptr
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_AreaVector2d_components(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (Area(ptr=c_void_p(output.field0)), Area(ptr=c_void_p(output.field1)))

    def x_component(self) -> Area:
        """Get the X component of a vector."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_AreaVector2d_xComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Area(ptr=output)

    def y_component(self) -> Area:
        """Get the Y component of a vector."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_AreaVector2d_yComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Area(ptr=output)

    def direction(self) -> Direction2d:
        """Attempt to get the direction of a vector.

        The current tolerance will be used to check if the vector is zero
        (and therefore does not have a direction).
        """
        inputs = _Tuple2_c_void_p_c_void_p(_area_tolerance()._ptr, self._ptr)
        output = _Result_c_void_p()
        _lib.opensolid_AreaVector2d_direction(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            Direction2d(ptr=c_void_p(output.field2))
            if output.field0 == 0
            else _error(_text_to_str(output.field1))
        )

    def angle(self) -> Angle:
        """Get the angle of a vector.

        The angle is measured counterclockwise from the positive X axis, so:

          * A vector in the positive X direction has an angle of zero.
          * A vector in the positive Y direction has an angle of 90 degrees.
          * A vector in the negative Y direction has an angle of -90 degrees.
          * It is not defined whether a vector exactly in the negative X direction has
            an angle of -180 or +180 degrees. (Currently it is reported as having an
            angle of +180 degrees, but this should not be relied upon.)

        The returned angle will be between -180 and +180 degrees.
        """
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_AreaVector2d_angle(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    def is_zero(self) -> bool:
        """Check if an area vector is zero, within the current tolerance."""
        inputs = _Tuple2_c_void_p_c_void_p(_area_tolerance()._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_AreaVector2d_isZero(ctypes.byref(inputs), ctypes.byref(output))
        return bool(output.value)

    def __neg__(self) -> AreaVector2d:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_AreaVector2d_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return AreaVector2d(ptr=output)

    def __add__(self, rhs: AreaVector2d) -> AreaVector2d:
        """Return ``self + rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_AreaVector2d_add_AreaVector2d_AreaVector2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaVector2d(ptr=output)

    def __sub__(self, rhs: AreaVector2d) -> AreaVector2d:
        """Return ``self - rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_AreaVector2d_sub_AreaVector2d_AreaVector2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaVector2d(ptr=output)

    def __mul__(self, rhs: float) -> AreaVector2d:
        """Return ``self * rhs``."""
        inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
        output = c_void_p()
        _lib.opensolid_AreaVector2d_mul_AreaVector2d_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaVector2d(ptr=output)

    @overload
    def __truediv__(self, rhs: float) -> AreaVector2d:
        pass

    @overload
    def __truediv__(self, rhs: Length) -> Displacement2d:
        pass

    @overload
    def __truediv__(self, rhs: Area) -> Vector2d:
        pass

    def __truediv__(self, rhs):
        """Return ``self / rhs``."""
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_AreaVector2d_div_AreaVector2d_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaVector2d(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaVector2d_div_AreaVector2d_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Displacement2d(ptr=output)
            case Area():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaVector2d_div_AreaVector2d_Area(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Vector2d(ptr=output)
            case _:
                return NotImplemented

    @overload
    def dot(self, rhs: Vector2d) -> Area:
        pass

    @overload
    def dot(self, rhs: Direction2d) -> Area:
        pass

    def dot(self, rhs):
        """Compute the dot product of two vector-like values."""
        match rhs:
            case Vector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaVector2d_dot_AreaVector2d_Vector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case Direction2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaVector2d_dot_AreaVector2d_Direction2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case _:
                return NotImplemented

    @overload
    def cross(self, rhs: Vector2d) -> Area:
        pass

    @overload
    def cross(self, rhs: Direction2d) -> Area:
        pass

    def cross(self, rhs):
        """Compute the cross product of two vector-like values."""
        match rhs:
            case Vector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaVector2d_cross_AreaVector2d_Vector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case Direction2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaVector2d_cross_AreaVector2d_Direction2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> AreaVector2d:
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_AreaVector2d_mul_Float_AreaVector2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaVector2d(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        x, y = self.components()
        return (
            "AreaVector2d.square_meters("
            + str(x.in_square_meters())
            + ","
            + str(y.in_square_meters())
            + ")"
        )


class Direction2d:
    """A direction in 2D.

    This is effectively a type-safe unit vector.
    """

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    x: Direction2d = None  # type: ignore[assignment]
    """Alias for 'positiveX'."""

    y: Direction2d = None  # type: ignore[assignment]
    """Alias for 'positiveY'."""

    positive_x: Direction2d = None  # type: ignore[assignment]
    """The positive X direction."""

    positive_y: Direction2d = None  # type: ignore[assignment]
    """The positive Y direction."""

    negative_x: Direction2d = None  # type: ignore[assignment]
    """The negative X direction."""

    negative_y: Direction2d = None  # type: ignore[assignment]
    """The negative Y direction."""

    @staticmethod
    def from_angle(angle: Angle) -> Direction2d:
        """Construct a direction from an angle.

        The angle is measured counterclockwise from the positive X direction, so:

          * An angle of zero corresponds to the positive X direction
          * An angle of 90 degrees corresponds to the positive Y direction
          * An angle of 180 degrees (or -180 degrees) corresponds to the negative X direction
        """
        inputs = angle._ptr
        output = c_void_p()
        _lib.opensolid_Direction2d_fromAngle_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Direction2d(ptr=output)

    @staticmethod
    def degrees(value: float) -> Direction2d:
        """Construct a direction from an angle given in degrees.

        See 'fromAngle' for details.
        """
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Direction2d_degrees_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Direction2d(ptr=output)

    @staticmethod
    def radians(value: float) -> Direction2d:
        """Construct a direction from an angle given in radians.

        See 'fromAngle' for details.
        """
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Direction2d_radians_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Direction2d(ptr=output)

    def to_angle(self) -> Angle:
        """Convert a direction to an angle.

        The angle is measured counterclockwise from the positive X direction, so:

          * The positive X direction has an angle of zero.
          * The positive Y direction has an angle of 90 degrees.
          * The negative Y direction has an angle of -90 degrees.
          * It is not defined whether the negative X direction has an angle of -180 or
            +180 degrees. (Currently it is reported as having an angle of +180 degrees,
            but this should not be relied upon.)

        The returned angle will be between -180 and +180 degrees.
        """
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Direction2d_toAngle(ctypes.byref(inputs), ctypes.byref(output))
        return Angle(ptr=output)

    def angle_to(self, other: Direction2d) -> Angle:
        """Measure the signed angle from one direction to another.

        The angle will be measured counterclockwise from the first direction to the
        second, and will always be between -180 and +180 degrees.
        """
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = c_void_p()
        _lib.opensolid_Direction2d_angleTo_Direction2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Angle(ptr=output)

    def components(self) -> tuple[float, float]:
        """Get the X and Y components of a direction."""
        inputs = self._ptr
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_Direction2d_components(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (output.field0, output.field1)

    def x_component(self) -> float:
        """Get the X component of a direction."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Direction2d_xComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def y_component(self) -> float:
        """Get the Y component of a direction."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_Direction2d_yComponent(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def __neg__(self) -> Direction2d:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_Direction2d_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return Direction2d(ptr=output)

    @overload
    def __mul__(self, rhs: float) -> Vector2d:
        pass

    @overload
    def __mul__(self, rhs: Length) -> Displacement2d:
        pass

    @overload
    def __mul__(self, rhs: Area) -> AreaVector2d:
        pass

    def __mul__(self, rhs):
        """Return ``self * rhs``."""
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
            case Area():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Direction2d_mul_Direction2d_Area(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaVector2d(ptr=output)
            case _:
                return NotImplemented

    @overload
    def dot(self, rhs: Direction2d) -> float:
        pass

    @overload
    def dot(self, rhs: Vector2d) -> float:
        pass

    @overload
    def dot(self, rhs: Displacement2d) -> Length:
        pass

    @overload
    def dot(self, rhs: AreaVector2d) -> Area:
        pass

    def dot(self, rhs):
        """Compute the dot product of two vector-like values."""
        match rhs:
            case Direction2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_double()
                _lib.opensolid_Direction2d_dot_Direction2d_Direction2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return output.value
            case Vector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_double()
                _lib.opensolid_Direction2d_dot_Direction2d_Vector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return output.value
            case Displacement2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Direction2d_dot_Direction2d_Displacement2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case AreaVector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Direction2d_dot_Direction2d_AreaVector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case _:
                return NotImplemented

    @overload
    def cross(self, rhs: Direction2d) -> float:
        pass

    @overload
    def cross(self, rhs: Vector2d) -> float:
        pass

    @overload
    def cross(self, rhs: Displacement2d) -> Length:
        pass

    @overload
    def cross(self, rhs: AreaVector2d) -> Area:
        pass

    def cross(self, rhs):
        """Compute the cross product of two vector-like values."""
        match rhs:
            case Direction2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_double()
                _lib.opensolid_Direction2d_cross_Direction2d_Direction2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return output.value
            case Vector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_double()
                _lib.opensolid_Direction2d_cross_Direction2d_Vector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return output.value
            case Displacement2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Direction2d_cross_Direction2d_Displacement2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Length(ptr=output)
            case AreaVector2d():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Direction2d_cross_Direction2d_AreaVector2d(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Area(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> Vector2d:
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Direction2d_mul_Float_Direction2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Vector2d(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        return "Direction2d.degrees(" + str(self.to_angle().in_degrees()) + ")"


class Point2d:
    """A point in 2D, defined by its X and Y coordinates."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    origin: Point2d = None  # type: ignore[assignment]
    """The point with coordinates (0,0)."""

    @staticmethod
    def xy(x_coordinate: Length, y_coordinate: Length) -> Point2d:
        """Construct a point from its X and Y coordinates."""
        inputs = _Tuple2_c_void_p_c_void_p(x_coordinate._ptr, y_coordinate._ptr)
        output = c_void_p()
        _lib.opensolid_Point2d_xy_Length_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    @staticmethod
    def x(x_coordinate: Length) -> Point2d:
        """Construct a point along the X axis, with the given X coordinate."""
        inputs = x_coordinate._ptr
        output = c_void_p()
        _lib.opensolid_Point2d_x_Length(ctypes.byref(inputs), ctypes.byref(output))
        return Point2d(ptr=output)

    @staticmethod
    def y(y_coordinate: Length) -> Point2d:
        """Construct a point along the Y axis, with the given Y coordinate."""
        inputs = y_coordinate._ptr
        output = c_void_p()
        _lib.opensolid_Point2d_y_Length(ctypes.byref(inputs), ctypes.byref(output))
        return Point2d(ptr=output)

    @staticmethod
    def meters(x_coordinate: float, y_coordinate: float) -> Point2d:
        """Construct a point from its X and Y coordinates given in meters."""
        inputs = _Tuple2_c_double_c_double(x_coordinate, y_coordinate)
        output = c_void_p()
        _lib.opensolid_Point2d_meters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    @staticmethod
    def centimeters(x_coordinate: float, y_coordinate: float) -> Point2d:
        """Construct a point from its X and Y coordinates given in centimeters."""
        inputs = _Tuple2_c_double_c_double(x_coordinate, y_coordinate)
        output = c_void_p()
        _lib.opensolid_Point2d_centimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    @staticmethod
    def millimeters(x_coordinate: float, y_coordinate: float) -> Point2d:
        """Construct a point from its X and Y coordinates given in millimeters."""
        inputs = _Tuple2_c_double_c_double(x_coordinate, y_coordinate)
        output = c_void_p()
        _lib.opensolid_Point2d_millimeters_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    @staticmethod
    def inches(x_coordinate: float, y_coordinate: float) -> Point2d:
        """Construct a point from its X and Y coordinates given in inches."""
        inputs = _Tuple2_c_double_c_double(x_coordinate, y_coordinate)
        output = c_void_p()
        _lib.opensolid_Point2d_inches_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    @staticmethod
    def from_coordinates(coordinates: tuple[Length, Length]) -> Point2d:
        """Construct a point from a pair of X and Y coordinates."""
        inputs = _Tuple2_c_void_p_c_void_p(coordinates[0]._ptr, coordinates[1]._ptr)
        output = c_void_p()
        _lib.opensolid_Point2d_fromCoordinates_Tuple2LengthLength(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    def coordinates(self) -> tuple[Length, Length]:
        """Get the X and Y coordinates of a point."""
        inputs = self._ptr
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_Point2d_coordinates(ctypes.byref(inputs), ctypes.byref(output))
        return (
            Length(ptr=c_void_p(output.field0)),
            Length(ptr=c_void_p(output.field1)),
        )

    def x_coordinate(self) -> Length:
        """Get the X coordinate of a point."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Point2d_xCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return Length(ptr=output)

    def y_coordinate(self) -> Length:
        """Get the Y coordinate of a point."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Point2d_yCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return Length(ptr=output)

    def distance_to(self, other: Point2d) -> Length:
        """Compute the distance from one point to another."""
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = c_void_p()
        _lib.opensolid_Point2d_distanceTo_Point2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def midpoint(self, other: Point2d) -> Point2d:
        """Find the midpoint between two points."""
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
        """Return ``self - rhs``."""
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
        """Return ``self + rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_Point2d_add_Point2d_Displacement2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        x, y = self.coordinates()
        return "Point2d.meters(" + str(x.in_meters()) + "," + str(y.in_meters()) + ")"


class UvPoint:
    """A point in UV parameter space."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    origin: UvPoint = None  # type: ignore[assignment]
    """The point with coordinates (0,0)."""

    @staticmethod
    def uv(u_coordinate: float, v_coordinate: float) -> UvPoint:
        """Construct a point from its X and Y coordinates."""
        inputs = _Tuple2_c_double_c_double(u_coordinate, v_coordinate)
        output = c_void_p()
        _lib.opensolid_UvPoint_uv_Float_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvPoint(ptr=output)

    @staticmethod
    def u(u_coordinate: float) -> UvPoint:
        """Construct a point along the X axis, with the given X coordinate."""
        inputs = c_double(u_coordinate)
        output = c_void_p()
        _lib.opensolid_UvPoint_u_Float(ctypes.byref(inputs), ctypes.byref(output))
        return UvPoint(ptr=output)

    @staticmethod
    def v(v_coordinate: float) -> UvPoint:
        """Construct a point along the Y axis, with the given Y coordinate."""
        inputs = c_double(v_coordinate)
        output = c_void_p()
        _lib.opensolid_UvPoint_v_Float(ctypes.byref(inputs), ctypes.byref(output))
        return UvPoint(ptr=output)

    @staticmethod
    def from_coordinates(coordinates: tuple[float, float]) -> UvPoint:
        """Construct a point from a pair of X and Y coordinates."""
        inputs = _Tuple2_c_double_c_double(coordinates[0], coordinates[1])
        output = c_void_p()
        _lib.opensolid_UvPoint_fromCoordinates_Tuple2FloatFloat(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvPoint(ptr=output)

    def coordinates(self) -> tuple[float, float]:
        """Get the U and V coordinates of a point."""
        inputs = self._ptr
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_UvPoint_coordinates(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1)

    def u_coordinate(self) -> float:
        """Get the U coordinate of a point."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_UvPoint_uCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def v_coordinate(self) -> float:
        """Get the V coordinate of a point."""
        inputs = self._ptr
        output = c_double()
        _lib.opensolid_UvPoint_vCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def distance_to(self, other: UvPoint) -> float:
        """Compute the distance from one point to another."""
        inputs = _Tuple2_c_void_p_c_void_p(other._ptr, self._ptr)
        output = c_double()
        _lib.opensolid_UvPoint_distanceTo_UvPoint(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def midpoint(self, other: UvPoint) -> UvPoint:
        """Find the midpoint between two points."""
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
        """Return ``self - rhs``."""
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
        """Return ``self + rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_UvPoint_add_UvPoint_Vector2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvPoint(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        x, y = self.coordinates()
        return "UvPoint.uv(" + str(x) + "," + str(y) + ")"


class Bounds2d:
    """A bounding box in 2D."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    @staticmethod
    def xy(x_coordinate: LengthRange, y_coordinate: LengthRange) -> Bounds2d:
        """Construct a bounding box from its X and Y coordinate ranges."""
        inputs = _Tuple2_c_void_p_c_void_p(x_coordinate._ptr, y_coordinate._ptr)
        output = c_void_p()
        _lib.opensolid_Bounds2d_xy_LengthRange_LengthRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Bounds2d(ptr=output)

    @staticmethod
    def constant(point: Point2d) -> Bounds2d:
        """Construct a zero-size bounding box containing a single point."""
        inputs = point._ptr
        output = c_void_p()
        _lib.opensolid_Bounds2d_constant_Point2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Bounds2d(ptr=output)

    @staticmethod
    def from_corners(p1: Point2d, p2: Point2d) -> Bounds2d:
        """Construct a bounding box from two corner points."""
        inputs = _Tuple2_c_void_p_c_void_p(p1._ptr, p2._ptr)
        output = c_void_p()
        _lib.opensolid_Bounds2d_fromCorners_Point2d_Point2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Bounds2d(ptr=output)

    @staticmethod
    def hull(points: list[Point2d]) -> Bounds2d:
        """Construct a bounding box containing all points in the given non-empty list."""
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
        """Construct a bounding box containing all bounding boxes in the given non-empty list."""
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
        """Get the X and Y coordinate ranges of a bounding box."""
        inputs = self._ptr
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_Bounds2d_coordinates(ctypes.byref(inputs), ctypes.byref(output))
        return (
            LengthRange(ptr=c_void_p(output.field0)),
            LengthRange(ptr=c_void_p(output.field1)),
        )

    def x_coordinate(self) -> LengthRange:
        """Get the X coordinate range of a bounding box."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Bounds2d_xCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return LengthRange(ptr=output)

    def y_coordinate(self) -> LengthRange:
        """Get the Y coordinate range of a bounding box."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Bounds2d_yCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return LengthRange(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        x, y = self.coordinates()
        return "Bounds2d.xy(" + repr(x) + "," + repr(y) + ")"


class UvBounds:
    """A bounding box in UV parameter space."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    @staticmethod
    def uv(u_coordinate: Range, v_coordinate: Range) -> UvBounds:
        """Construct a bounding box from its X and Y coordinate ranges."""
        inputs = _Tuple2_c_void_p_c_void_p(u_coordinate._ptr, v_coordinate._ptr)
        output = c_void_p()
        _lib.opensolid_UvBounds_uv_Range_Range(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvBounds(ptr=output)

    @staticmethod
    def constant(point: UvPoint) -> UvBounds:
        """Construct a zero-size bounding box containing a single point."""
        inputs = point._ptr
        output = c_void_p()
        _lib.opensolid_UvBounds_constant_UvPoint(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvBounds(ptr=output)

    @staticmethod
    def from_corners(p1: UvPoint, p2: UvPoint) -> UvBounds:
        """Construct a bounding box from two corner points."""
        inputs = _Tuple2_c_void_p_c_void_p(p1._ptr, p2._ptr)
        output = c_void_p()
        _lib.opensolid_UvBounds_fromCorners_UvPoint_UvPoint(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return UvBounds(ptr=output)

    @staticmethod
    def hull(points: list[UvPoint]) -> UvBounds:
        """Construct a bounding box containing all points in the given non-empty list."""
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
        """Construct a bounding box containing all bounding boxes in the given non-empty list."""
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
        """Get the X and Y coordinate ranges of a bounding box."""
        inputs = self._ptr
        output = _Tuple2_c_void_p_c_void_p()
        _lib.opensolid_UvBounds_coordinates(ctypes.byref(inputs), ctypes.byref(output))
        return (Range(ptr=c_void_p(output.field0)), Range(ptr=c_void_p(output.field1)))

    def u_coordinate(self) -> Range:
        """Get the X coordinate range of a bounding box."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_UvBounds_uCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    def v_coordinate(self) -> Range:
        """Get the Y coordinate range of a bounding box."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_UvBounds_vCoordinate(ctypes.byref(inputs), ctypes.byref(output))
        return Range(ptr=output)

    def __repr__(self) -> str:
        """Return a human-readable representation of this value."""
        u, v = self.coordinates()
        return "UvBounds.uv(" + repr(u) + "," + repr(v) + ")"


class Curve:
    """A parametric curve definining a unitless value in terms of a parameter value."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    zero: Curve = None  # type: ignore[assignment]
    """A curve equal to zero everywhere."""

    t: Curve = None  # type: ignore[assignment]
    """A curve parameter.

    In other words, a curve whose value is equal to its input parameter.
    When defining parametric curves, you will typically start with 'Curve.t'
    and then use arithmetic operators etc. to build up more complex curves.
    """

    @staticmethod
    def constant(value: float) -> Curve:
        """Create a curve with the given constant value."""
        inputs = c_double(value)
        output = c_void_p()
        _lib.opensolid_Curve_constant_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def squared(self) -> Curve:
        """Compute the square of a curve."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Curve_squared(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def sqrt(self) -> Curve:
        """Compute the square root of a curve."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_Curve_sqrt(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def evaluate(self, parameter_value: float) -> float:
        """Evaluate a curve at a given parameter value.

        The parameter value should be between 0 and 1.
        """
        inputs = _Tuple2_c_double_c_void_p(parameter_value, self._ptr)
        output = c_double()
        _lib.opensolid_Curve_evaluate_Float(ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def zeros(self) -> list[Curve.Zero]:
        """Find all points at which the given curve is zero.

        This includes not only points where the curve *crosses* zero,
        but also where it is *tangent* to zero.
        For example, y=x-3 crosses zero at x=3,
        while y=(x-3)^2 is tangent to zero at x=3.

        We define y=x-3 as having a zero of order 0 at x=3,
        since only the "derivative of order zero" (the curve itself)
        is zero at that point.
        Similarly, y=(x-3)^2 has a zero of order 1 at x=3,
        since the first derivative (but not the second derivative)
        is zero at that point.

        Currently, this function up to third-order zeros
        (e.g. y=x^4 has a third-order zero at x=0,
        since everything up to the third derivative is zero at x=0).

        The current tolerance is used to determine
        whether a given point should be considered a zero,
        and of what order.
        For example, the curve y=x^2-0.0001 is *exactly* zero at x=0.01 and x=-0.01.
        However, note that the curve is also very close to zero at x=0,
        and at that point the first derivative is *also* zero.
        In many cases, it is reasonable to assume that
        the 0.0001 is an artifact of numerical roundoff,
        and the curve actually has a single zero of order 1 at x=0.
        The current tolerance is used to choose which case to report.
        In this example, a tolerance of 0.000001
        would mean that we consider 0.0001 a meaningful value (not just roundoff),
        so we would end up reporting two order-0 zeros at x=0.01 and x=-0.01.
        On the other hand, a tolerance of 0.01 would mean that
        we consider 0.0001 as just roundoff error,
        so we would end up reporting a single order-1 zero at x=0
        (the point at which the *first derivative* is zero).
        """
        inputs = _Tuple2_c_double_c_void_p(_float_tolerance(), self._ptr)
        output = _Result_List_c_void_p()
        _lib.opensolid_Curve_zeros(ctypes.byref(inputs), ctypes.byref(output))
        return (
            [
                Curve.Zero(ptr=c_void_p(item))
                for item in [
                    output.field2.field1[index] for index in range(output.field2.field0)
                ]
            ]
            if output.field0 == 0
            else _error(_text_to_str(output.field1))
        )

    def is_zero(self) -> bool:
        """Check if a curve is zero everywhere, within the current tolerance."""
        inputs = _Tuple2_c_double_c_void_p(_float_tolerance(), self._ptr)
        output = c_int64()
        _lib.opensolid_Curve_isZero(ctypes.byref(inputs), ctypes.byref(output))
        return bool(output.value)

    def __neg__(self) -> Curve:
        """Return ``-self``."""
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
        """Return ``self + rhs``."""
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
        """Return ``self - rhs``."""
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
    def __mul__(self, rhs: Area) -> AreaCurve:
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
        """Return ``self * rhs``."""
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
            case Area():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_Curve_mul_Curve_Area(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaCurve(ptr=output)
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
        """Return ``self / rhs``."""
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
        """Return ``lhs + self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Curve_add_Float_Curve(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def __rsub__(self, lhs: float) -> Curve:
        """Return ``lhs - self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Curve_sub_Float_Curve(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def __rmul__(self, lhs: float) -> Curve:
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Curve_mul_Float_Curve(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def __rtruediv__(self, lhs: float) -> Curve:
        """Return ``lhs / self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_Curve_div_Float_Curve(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    class Zero:
        """A point where a given curve is equal to zero."""

        def __init__(self, *, ptr: c_void_p) -> None:
            self._ptr = ptr

        def __del__(self) -> None:
            """Free the underlying Haskell value."""
            _lib.opensolid_release(self._ptr)

        def location(self) -> float:
            """Get the parameter value at which the curve is zero."""
            inputs = self._ptr
            output = c_double()
            _lib.opensolid_CurveZero_location(
                ctypes.byref(inputs), ctypes.byref(output)
            )
            return output.value

        def order(self) -> int:
            """Check whether the zero is a crossing zero, a tangent zero etc.

            * An order 0 zero means the curve crosses zero at the given location,
              with a non-zero first derivative.
            * An order 1 zero means the first derivative is also zero at the given
              location, but the second derivative is not (that is, the curve just
              'touches' zero at that point).
            * An order 2 zero means the first and second derivatives are zero at the
              given location, etc.
            """
            inputs = self._ptr
            output = c_int64()
            _lib.opensolid_CurveZero_order(ctypes.byref(inputs), ctypes.byref(output))
            return output.value

        def sign(self) -> int:
            """Check whether the curve 'curves up' or 'curves down' at the zero.

            A positive sign means that the curve is positive to the right of the zero
            (for a crossing zero, that means the curve will be negative to the left,
            but for an order 1 tangent zero, that means the curve will also be positive
            to the left!). Similarly, a negative sign means that the curve is negative
            to the right of the zero.
            """
            inputs = self._ptr
            output = c_int64()
            _lib.opensolid_CurveZero_sign(ctypes.byref(inputs), ctypes.byref(output))
            return output.value


class LengthCurve:
    """A parametric curve definining a length in terms of a parameter value."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    zero: LengthCurve = None  # type: ignore[assignment]
    """A curve equal to zero everywhere."""

    @staticmethod
    def constant(value: Length) -> LengthCurve:
        """Create a curve with the given constant value."""
        inputs = value._ptr
        output = c_void_p()
        _lib.opensolid_LengthCurve_constant_Length(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthCurve(ptr=output)

    def evaluate(self, parameter_value: float) -> Length:
        """Evaluate a curve at a given parameter value.

        The parameter value should be between 0 and 1.
        """
        inputs = _Tuple2_c_double_c_void_p(parameter_value, self._ptr)
        output = c_void_p()
        _lib.opensolid_LengthCurve_evaluate_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(ptr=output)

    def zeros(self) -> list[Curve.Zero]:
        """Find all points at which the given curve is zero.

        This includes not only points where the curve *crosses* zero,
        but also where it is *tangent* to zero.
        For example, y=x-3 crosses zero at x=3,
        while y=(x-3)^2 is tangent to zero at x=3.

        We define y=x-3 as having a zero of order 0 at x=3,
        since only the "derivative of order zero" (the curve itself)
        is zero at that point.
        Similarly, y=(x-3)^2 has a zero of order 1 at x=3,
        since the first derivative (but not the second derivative)
        is zero at that point.

        Currently, this function up to third-order zeros
        (e.g. y=x^4 has a third-order zero at x=0,
        since everything up to the third derivative is zero at x=0).

        The current tolerance is used to determine
        whether a given point should be considered a zero,
        and of what order.
        For example, the curve y=x^2-0.0001 is *exactly* zero at x=0.01 and x=-0.01.
        However, note that the curve is also very close to zero at x=0,
        and at that point the first derivative is *also* zero.
        In many cases, it is reasonable to assume that
        the 0.0001 is an artifact of numerical roundoff,
        and the curve actually has a single zero of order 1 at x=0.
        The current tolerance is used to choose which case to report.
        In this example, a tolerance of 0.000001
        would mean that we consider 0.0001 a meaningful value (not just roundoff),
        so we would end up reporting two order-0 zeros at x=0.01 and x=-0.01.
        On the other hand, a tolerance of 0.01 would mean that
        we consider 0.0001 as just roundoff error,
        so we would end up reporting a single order-1 zero at x=0
        (the point at which the *first derivative* is zero).
        """
        inputs = _Tuple2_c_void_p_c_void_p(_length_tolerance()._ptr, self._ptr)
        output = _Result_List_c_void_p()
        _lib.opensolid_LengthCurve_zeros(ctypes.byref(inputs), ctypes.byref(output))
        return (
            [
                Curve.Zero(ptr=c_void_p(item))
                for item in [
                    output.field2.field1[index] for index in range(output.field2.field0)
                ]
            ]
            if output.field0 == 0
            else _error(_text_to_str(output.field1))
        )

    def is_zero(self) -> bool:
        """Check if a curve is zero everywhere, within the current tolerance."""
        inputs = _Tuple2_c_void_p_c_void_p(_length_tolerance()._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_LengthCurve_isZero(ctypes.byref(inputs), ctypes.byref(output))
        return bool(output.value)

    def __neg__(self) -> LengthCurve:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_LengthCurve_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return LengthCurve(ptr=output)

    @overload
    def __add__(self, rhs: LengthCurve) -> LengthCurve:
        pass

    @overload
    def __add__(self, rhs: Length) -> LengthCurve:
        pass

    def __add__(self, rhs):
        """Return ``self + rhs``."""
        match rhs:
            case LengthCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthCurve_add_LengthCurve_LengthCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthCurve_add_LengthCurve_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: LengthCurve) -> LengthCurve:
        pass

    @overload
    def __sub__(self, rhs: Length) -> LengthCurve:
        pass

    def __sub__(self, rhs):
        """Return ``self - rhs``."""
        match rhs:
            case LengthCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthCurve_sub_LengthCurve_LengthCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthCurve_sub_LengthCurve_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __mul__(self, rhs: float) -> LengthCurve:
        pass

    @overload
    def __mul__(self, rhs: LengthCurve) -> AreaCurve:
        pass

    @overload
    def __mul__(self, rhs: Length) -> AreaCurve:
        pass

    @overload
    def __mul__(self, rhs: Curve) -> LengthCurve:
        pass

    def __mul__(self, rhs):
        """Return ``self * rhs``."""
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_LengthCurve_mul_LengthCurve_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case LengthCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthCurve_mul_LengthCurve_LengthCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaCurve(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_LengthCurve_mul_LengthCurve_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaCurve(ptr=output)
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
        """Return ``self / rhs``."""
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
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_LengthCurve_mul_Float_LengthCurve(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return LengthCurve(ptr=output)


class AreaCurve:
    """A parametric curve definining an area in terms of a parameter value."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    zero: AreaCurve = None  # type: ignore[assignment]
    """A curve equal to zero everywhere."""

    @staticmethod
    def constant(value: Area) -> AreaCurve:
        """Create a curve with the given constant value."""
        inputs = value._ptr
        output = c_void_p()
        _lib.opensolid_AreaCurve_constant_Area(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaCurve(ptr=output)

    def evaluate(self, parameter_value: float) -> Area:
        """Evaluate a curve at a given parameter value.

        The parameter value should be between 0 and 1.
        """
        inputs = _Tuple2_c_double_c_void_p(parameter_value, self._ptr)
        output = c_void_p()
        _lib.opensolid_AreaCurve_evaluate_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Area(ptr=output)

    def zeros(self) -> list[Curve.Zero]:
        """Find all points at which the given curve is zero.

        This includes not only points where the curve *crosses* zero,
        but also where it is *tangent* to zero.
        For example, y=x-3 crosses zero at x=3,
        while y=(x-3)^2 is tangent to zero at x=3.

        We define y=x-3 as having a zero of order 0 at x=3,
        since only the "derivative of order zero" (the curve itself)
        is zero at that point.
        Similarly, y=(x-3)^2 has a zero of order 1 at x=3,
        since the first derivative (but not the second derivative)
        is zero at that point.

        Currently, this function up to third-order zeros
        (e.g. y=x^4 has a third-order zero at x=0,
        since everything up to the third derivative is zero at x=0).

        The current tolerance is used to determine
        whether a given point should be considered a zero,
        and of what order.
        For example, the curve y=x^2-0.0001 is *exactly* zero at x=0.01 and x=-0.01.
        However, note that the curve is also very close to zero at x=0,
        and at that point the first derivative is *also* zero.
        In many cases, it is reasonable to assume that
        the 0.0001 is an artifact of numerical roundoff,
        and the curve actually has a single zero of order 1 at x=0.
        The current tolerance is used to choose which case to report.
        In this example, a tolerance of 0.000001
        would mean that we consider 0.0001 a meaningful value (not just roundoff),
        so we would end up reporting two order-0 zeros at x=0.01 and x=-0.01.
        On the other hand, a tolerance of 0.01 would mean that
        we consider 0.0001 as just roundoff error,
        so we would end up reporting a single order-1 zero at x=0
        (the point at which the *first derivative* is zero).
        """
        inputs = _Tuple2_c_void_p_c_void_p(_area_tolerance()._ptr, self._ptr)
        output = _Result_List_c_void_p()
        _lib.opensolid_AreaCurve_zeros(ctypes.byref(inputs), ctypes.byref(output))
        return (
            [
                Curve.Zero(ptr=c_void_p(item))
                for item in [
                    output.field2.field1[index] for index in range(output.field2.field0)
                ]
            ]
            if output.field0 == 0
            else _error(_text_to_str(output.field1))
        )

    def is_zero(self) -> bool:
        """Check if a curve is zero everywhere, within the current tolerance."""
        inputs = _Tuple2_c_void_p_c_void_p(_area_tolerance()._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_AreaCurve_isZero(ctypes.byref(inputs), ctypes.byref(output))
        return bool(output.value)

    def __neg__(self) -> AreaCurve:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_AreaCurve_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return AreaCurve(ptr=output)

    def __add__(self, rhs: AreaCurve) -> AreaCurve:
        """Return ``self + rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_AreaCurve_add_AreaCurve_AreaCurve(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaCurve(ptr=output)

    def __sub__(self, rhs: AreaCurve) -> AreaCurve:
        """Return ``self - rhs``."""
        inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
        output = c_void_p()
        _lib.opensolid_AreaCurve_sub_AreaCurve_AreaCurve(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaCurve(ptr=output)

    @overload
    def __mul__(self, rhs: float) -> AreaCurve:
        pass

    @overload
    def __mul__(self, rhs: Curve) -> AreaCurve:
        pass

    def __mul__(self, rhs):
        """Return ``self * rhs``."""
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_AreaCurve_mul_AreaCurve_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaCurve(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaCurve_mul_AreaCurve_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __truediv__(self, rhs: float) -> AreaCurve:
        pass

    @overload
    def __truediv__(self, rhs: AreaCurve) -> Curve:
        pass

    @overload
    def __truediv__(self, rhs: Length) -> LengthCurve:
        pass

    @overload
    def __truediv__(self, rhs: Area) -> Curve:
        pass

    @overload
    def __truediv__(self, rhs: Curve) -> AreaCurve:
        pass

    @overload
    def __truediv__(self, rhs: LengthCurve) -> LengthCurve:
        pass

    def __truediv__(self, rhs):
        """Return ``self / rhs``."""
        match rhs:
            case float() | int():
                inputs = _Tuple2_c_void_p_c_double(self._ptr, rhs)
                output = c_void_p()
                _lib.opensolid_AreaCurve_div_AreaCurve_Float(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaCurve(ptr=output)
            case AreaCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaCurve_div_AreaCurve_AreaCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case Length():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaCurve_div_AreaCurve_Length(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case Area():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaCurve_div_AreaCurve_Area(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return Curve(ptr=output)
            case Curve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaCurve_div_AreaCurve_Curve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AreaCurve(ptr=output)
            case LengthCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AreaCurve_div_AreaCurve_LengthCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthCurve(ptr=output)
            case _:
                return NotImplemented

    def __rmul__(self, lhs: float) -> AreaCurve:
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_AreaCurve_mul_Float_AreaCurve(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AreaCurve(ptr=output)


class AngleCurve:
    """A parametric curve definining an angle in terms of a parameter value."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    zero: AngleCurve = None  # type: ignore[assignment]
    """A curve equal to zero everywhere."""

    @staticmethod
    def constant(value: Angle) -> AngleCurve:
        """Create a curve with the given constant value."""
        inputs = value._ptr
        output = c_void_p()
        _lib.opensolid_AngleCurve_constant_Angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleCurve(ptr=output)

    def sin(self) -> Curve:
        """Compute the sine of a curve."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_AngleCurve_sin(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def cos(self) -> Curve:
        """Compute the cosine of a curve."""
        inputs = self._ptr
        output = c_void_p()
        _lib.opensolid_AngleCurve_cos(ctypes.byref(inputs), ctypes.byref(output))
        return Curve(ptr=output)

    def evaluate(self, parameter_value: float) -> Angle:
        """Evaluate a curve at a given parameter value.

        The parameter value should be between 0 and 1.
        """
        inputs = _Tuple2_c_double_c_void_p(parameter_value, self._ptr)
        output = c_void_p()
        _lib.opensolid_AngleCurve_evaluate_Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Angle(ptr=output)

    def zeros(self) -> list[Curve.Zero]:
        """Find all points at which the given curve is zero.

        This includes not only points where the curve *crosses* zero,
        but also where it is *tangent* to zero.
        For example, y=x-3 crosses zero at x=3,
        while y=(x-3)^2 is tangent to zero at x=3.

        We define y=x-3 as having a zero of order 0 at x=3,
        since only the "derivative of order zero" (the curve itself)
        is zero at that point.
        Similarly, y=(x-3)^2 has a zero of order 1 at x=3,
        since the first derivative (but not the second derivative)
        is zero at that point.

        Currently, this function up to third-order zeros
        (e.g. y=x^4 has a third-order zero at x=0,
        since everything up to the third derivative is zero at x=0).

        The current tolerance is used to determine
        whether a given point should be considered a zero,
        and of what order.
        For example, the curve y=x^2-0.0001 is *exactly* zero at x=0.01 and x=-0.01.
        However, note that the curve is also very close to zero at x=0,
        and at that point the first derivative is *also* zero.
        In many cases, it is reasonable to assume that
        the 0.0001 is an artifact of numerical roundoff,
        and the curve actually has a single zero of order 1 at x=0.
        The current tolerance is used to choose which case to report.
        In this example, a tolerance of 0.000001
        would mean that we consider 0.0001 a meaningful value (not just roundoff),
        so we would end up reporting two order-0 zeros at x=0.01 and x=-0.01.
        On the other hand, a tolerance of 0.01 would mean that
        we consider 0.0001 as just roundoff error,
        so we would end up reporting a single order-1 zero at x=0
        (the point at which the *first derivative* is zero).
        """
        inputs = _Tuple2_c_void_p_c_void_p(_angle_tolerance()._ptr, self._ptr)
        output = _Result_List_c_void_p()
        _lib.opensolid_AngleCurve_zeros(ctypes.byref(inputs), ctypes.byref(output))
        return (
            [
                Curve.Zero(ptr=c_void_p(item))
                for item in [
                    output.field2.field1[index] for index in range(output.field2.field0)
                ]
            ]
            if output.field0 == 0
            else _error(_text_to_str(output.field1))
        )

    def is_zero(self) -> bool:
        """Check if a curve is zero everywhere, within the current tolerance."""
        inputs = _Tuple2_c_void_p_c_void_p(_angle_tolerance()._ptr, self._ptr)
        output = c_int64()
        _lib.opensolid_AngleCurve_isZero(ctypes.byref(inputs), ctypes.byref(output))
        return bool(output.value)

    def __neg__(self) -> AngleCurve:
        """Return ``-self``."""
        output = c_void_p()
        _lib.opensolid_AngleCurve_neg(ctypes.byref(self._ptr), ctypes.byref(output))
        return AngleCurve(ptr=output)

    @overload
    def __add__(self, rhs: AngleCurve) -> AngleCurve:
        pass

    @overload
    def __add__(self, rhs: Angle) -> AngleCurve:
        pass

    def __add__(self, rhs):
        """Return ``self + rhs``."""
        match rhs:
            case AngleCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleCurve_add_AngleCurve_AngleCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleCurve_add_AngleCurve_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __sub__(self, rhs: AngleCurve) -> AngleCurve:
        pass

    @overload
    def __sub__(self, rhs: Angle) -> AngleCurve:
        pass

    def __sub__(self, rhs):
        """Return ``self - rhs``."""
        match rhs:
            case AngleCurve():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleCurve_sub_AngleCurve_AngleCurve(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case Angle():
                inputs = _Tuple2_c_void_p_c_void_p(self._ptr, rhs._ptr)
                output = c_void_p()
                _lib.opensolid_AngleCurve_sub_AngleCurve_Angle(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return AngleCurve(ptr=output)
            case _:
                return NotImplemented

    @overload
    def __mul__(self, rhs: float) -> AngleCurve:
        pass

    @overload
    def __mul__(self, rhs: Curve) -> AngleCurve:
        pass

    def __mul__(self, rhs):
        """Return ``self * rhs``."""
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
        """Return ``self / rhs``."""
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
        """Return ``lhs * self``."""
        inputs = _Tuple2_c_double_c_void_p(lhs, self._ptr)
        output = c_void_p()
        _lib.opensolid_AngleCurve_mul_Float_AngleCurve(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return AngleCurve(ptr=output)


class Drawing2d:
    """A set of functions for constructing 2D drawings."""

    def __init__(self, *, ptr: c_void_p) -> None:
        self._ptr = ptr

    def __del__(self) -> None:
        """Free the underlying Haskell value."""
        _lib.opensolid_release(self._ptr)

    black_stroke: Drawing2d.Attribute = None  # type: ignore[assignment]
    """Black stroke for curves and borders."""

    no_fill: Drawing2d.Attribute = None  # type: ignore[assignment]
    """Set shapes to have no fill."""

    @staticmethod
    def to_svg(view_box: Bounds2d, entities: list[Drawing2d.Entity]) -> str:
        """Render some entities to SVG.

        The given bounding box defines the overall size of the drawing,
        and in general should contain all the drawing entities
        (unless you *want* to crop some of them).
        """
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
        """Create a polygon with the given attributes and vertices."""
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
        """Create a circle with the given attributes, center point and radius."""
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
        """Set the stroke color for curves and borders."""
        inputs = color._ptr
        output = c_void_p()
        _lib.opensolid_Drawing2d_strokeColor_Color(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Drawing2d.Attribute(ptr=output)

    @staticmethod
    def fill_color(color: Color) -> Drawing2d.Attribute:
        """Set the fill color for shapes."""
        inputs = color._ptr
        output = c_void_p()
        _lib.opensolid_Drawing2d_fillColor_Color(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Drawing2d.Attribute(ptr=output)

    class Entity:
        """A drawing entity such as a shape or group."""

        def __init__(self, *, ptr: c_void_p) -> None:
            self._ptr = ptr

        def __del__(self) -> None:
            """Free the underlying Haskell value."""
            _lib.opensolid_release(self._ptr)

    class Attribute:
        """A drawing attribute such as fill color or stroke width."""

        def __init__(self, *, ptr: c_void_p) -> None:
            self._ptr = ptr

        def __del__(self) -> None:
            """Free the underlying Haskell value."""
            _lib.opensolid_release(self._ptr)


def _length_zero() -> Length:
    output = c_void_p()
    _lib.opensolid_Length_zero(c_void_p(), ctypes.byref(output))
    return Length(ptr=output)


Length.zero = _length_zero()


def _length_meter() -> Length:
    output = c_void_p()
    _lib.opensolid_Length_meter(c_void_p(), ctypes.byref(output))
    return Length(ptr=output)


Length.meter = _length_meter()


def _length_centimeter() -> Length:
    output = c_void_p()
    _lib.opensolid_Length_centimeter(c_void_p(), ctypes.byref(output))
    return Length(ptr=output)


Length.centimeter = _length_centimeter()


def _length_millimeter() -> Length:
    output = c_void_p()
    _lib.opensolid_Length_millimeter(c_void_p(), ctypes.byref(output))
    return Length(ptr=output)


Length.millimeter = _length_millimeter()


def _length_micrometer() -> Length:
    output = c_void_p()
    _lib.opensolid_Length_micrometer(c_void_p(), ctypes.byref(output))
    return Length(ptr=output)


Length.micrometer = _length_micrometer()


def _length_nanometer() -> Length:
    output = c_void_p()
    _lib.opensolid_Length_nanometer(c_void_p(), ctypes.byref(output))
    return Length(ptr=output)


Length.nanometer = _length_nanometer()


def _length_inch() -> Length:
    output = c_void_p()
    _lib.opensolid_Length_inch(c_void_p(), ctypes.byref(output))
    return Length(ptr=output)


Length.inch = _length_inch()


def _length_pixel() -> Length:
    output = c_void_p()
    _lib.opensolid_Length_pixel(c_void_p(), ctypes.byref(output))
    return Length(ptr=output)


Length.pixel = _length_pixel()


def _area_zero() -> Area:
    output = c_void_p()
    _lib.opensolid_Area_zero(c_void_p(), ctypes.byref(output))
    return Area(ptr=output)


Area.zero = _area_zero()


def _area_square_meter() -> Area:
    output = c_void_p()
    _lib.opensolid_Area_squareMeter(c_void_p(), ctypes.byref(output))
    return Area(ptr=output)


Area.square_meter = _area_square_meter()


def _area_square_inch() -> Area:
    output = c_void_p()
    _lib.opensolid_Area_squareInch(c_void_p(), ctypes.byref(output))
    return Area(ptr=output)


Area.square_inch = _area_square_inch()


def _angle_zero() -> Angle:
    output = c_void_p()
    _lib.opensolid_Angle_zero(c_void_p(), ctypes.byref(output))
    return Angle(ptr=output)


Angle.zero = _angle_zero()


def _angle_golden_angle() -> Angle:
    output = c_void_p()
    _lib.opensolid_Angle_goldenAngle(c_void_p(), ctypes.byref(output))
    return Angle(ptr=output)


Angle.golden_angle = _angle_golden_angle()


def _angle_radian() -> Angle:
    output = c_void_p()
    _lib.opensolid_Angle_radian(c_void_p(), ctypes.byref(output))
    return Angle(ptr=output)


Angle.radian = _angle_radian()


def _angle_degree() -> Angle:
    output = c_void_p()
    _lib.opensolid_Angle_degree(c_void_p(), ctypes.byref(output))
    return Angle(ptr=output)


Angle.degree = _angle_degree()


def _angle_full_turn() -> Angle:
    output = c_void_p()
    _lib.opensolid_Angle_fullTurn(c_void_p(), ctypes.byref(output))
    return Angle(ptr=output)


Angle.full_turn = _angle_full_turn()


def _angle_half_turn() -> Angle:
    output = c_void_p()
    _lib.opensolid_Angle_halfTurn(c_void_p(), ctypes.byref(output))
    return Angle(ptr=output)


Angle.half_turn = _angle_half_turn()


def _angle_quarter_turn() -> Angle:
    output = c_void_p()
    _lib.opensolid_Angle_quarterTurn(c_void_p(), ctypes.byref(output))
    return Angle(ptr=output)


Angle.quarter_turn = _angle_quarter_turn()


def _angle_pi() -> Angle:
    output = c_void_p()
    _lib.opensolid_Angle_pi(c_void_p(), ctypes.byref(output))
    return Angle(ptr=output)


Angle.pi = _angle_pi()


def _angle_two_pi() -> Angle:
    output = c_void_p()
    _lib.opensolid_Angle_twoPi(c_void_p(), ctypes.byref(output))
    return Angle(ptr=output)


Angle.two_pi = _angle_two_pi()


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


def _areavector2d_zero() -> AreaVector2d:
    output = c_void_p()
    _lib.opensolid_AreaVector2d_zero(c_void_p(), ctypes.byref(output))
    return AreaVector2d(ptr=output)


AreaVector2d.zero = _areavector2d_zero()


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


def _curve_zero() -> Curve:
    output = c_void_p()
    _lib.opensolid_Curve_zero(c_void_p(), ctypes.byref(output))
    return Curve(ptr=output)


Curve.zero = _curve_zero()


def _curve_t() -> Curve:
    output = c_void_p()
    _lib.opensolid_Curve_t(c_void_p(), ctypes.byref(output))
    return Curve(ptr=output)


Curve.t = _curve_t()


def _lengthcurve_zero() -> LengthCurve:
    output = c_void_p()
    _lib.opensolid_LengthCurve_zero(c_void_p(), ctypes.byref(output))
    return LengthCurve(ptr=output)


LengthCurve.zero = _lengthcurve_zero()


def _areacurve_zero() -> AreaCurve:
    output = c_void_p()
    _lib.opensolid_AreaCurve_zero(c_void_p(), ctypes.byref(output))
    return AreaCurve(ptr=output)


AreaCurve.zero = _areacurve_zero()


def _anglecurve_zero() -> AngleCurve:
    output = c_void_p()
    _lib.opensolid_AngleCurve_zero(c_void_p(), ctypes.byref(output))
    return AngleCurve(ptr=output)


AngleCurve.zero = _anglecurve_zero()


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


__all__ = [
    "Tolerance",
    "Length",
    "Area",
    "Angle",
    "Range",
    "LengthRange",
    "AreaRange",
    "AngleRange",
    "Color",
    "Vector2d",
    "Displacement2d",
    "AreaVector2d",
    "Direction2d",
    "Point2d",
    "UvPoint",
    "Bounds2d",
    "UvBounds",
    "Curve",
    "LengthCurve",
    "AreaCurve",
    "AngleCurve",
    "Drawing2d",
]
