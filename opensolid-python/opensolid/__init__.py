"""A collection of classes for 2D/3D geometric modelling."""

from __future__ import annotations

import ctypes
import os
from contextlib import contextmanager
from ctypes import (
    CDLL,
    Structure,
    Union,
    c_char_p,
    c_double,
    c_int64,
    c_size_t,
    c_void_p,
)
from pathlib import Path
from typing import Any, Generator, overload

# Load the native library, assuming it's located in the same directory as __init__.py
_lib_dir = Path(__file__).parent
match os.uname().sysname:
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

    @staticmethod
    @contextmanager
    def of(value: float | Length | Angle | None) -> Generator[None]:
        """Set the implicit tolerance to be used in the nested block.

        The tolerance will be restored to its previous value when the block exits.
        """
        saved = Tolerance.current
        Tolerance.current = value
        try:
            yield
        finally:
            Tolerance.current = saved


def _float_tolerance() -> float:
    if isinstance(Tolerance.current, float):
        return Tolerance.current
    if Tolerance.current is None:
        message = "No float tolerance set, please set one using Tolerance.of"
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
        message = "No length tolerance set, please set one using Tolerance.of"
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
        message = "No angle tolerance set, please set one using Tolerance.of"
        raise TypeError(message)
    message = (
        "Expected a tolerance of type Angle but current tolerance is of type "
        + type(Tolerance.current).__name__
    )
    raise TypeError(message)


class _Tuple2_c_double_c_void_p(Structure):  # noqa: N801
    _fields_ = [("field0", c_double), ("field1", c_void_p)]  # noqa: RUF012


class _Tuple2_c_double_c_double(Structure):  # noqa: N801
    _fields_ = [("field0", c_double), ("field1", c_double)]  # noqa: RUF012


class _Tuple2_c_void_p_c_void_p(Structure):  # noqa: N801
    _fields_ = [("field0", c_void_p), ("field1", c_void_p)]  # noqa: RUF012


class _Result_c_void_p(Structure):  # noqa: N801
    _fields_ = [("field0", c_int64), ("field1", _ErrorMessage), ("field2", c_void_p)]  # noqa: RUF012


class _Tuple3_c_void_p_c_void_p_c_void_p(Structure):  # noqa: N801
    _fields_ = [("field0", c_void_p), ("field1", c_void_p), ("field2", c_void_p)]  # noqa: RUF012


class _Maybe_c_void_p(Structure):  # noqa: N801
    _fields_ = [("field0", c_int64), ("field1", c_void_p)]  # noqa: RUF012


class FloatRange:
    @overload
    def __init__(self, *, __ptr__: c_void_p) -> None:
        pass

    @overload
    def __init__(self, value: float) -> None:
        pass

    @overload
    def __init__(self, low: float, high: float) -> None:
        pass

    def __init__(self, *args, **keywords):
        match (args, keywords):
            case ([], {"__ptr__": c_void_p() as __ptr__, **rest}) if not rest:
                self.__ptr__ = __ptr__
            case (
                ([float() | int() as value], {**rest})
                | ([], {"value": float() | int() as value, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = c_double(value)
                _lib.opensolid__FloatRange__constructor__Float(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case (
                ([float() | int() as low, float() | int() as high], {**rest})
                | (
                    [],
                    {
                        "low": float() | int() as low,
                        "high": float() | int() as high,
                        **rest,
                    },
                )
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(low, high)
                _lib.opensolid__FloatRange__constructor__Float_Float(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case _:
                message = "Unexpected constructor arguments"
                raise TypeError(message)

    @staticmethod
    def unit() -> FloatRange:
        output = c_void_p()
        _lib.opensolid__FloatRange__unit(c_void_p(), ctypes.byref(output))
        return FloatRange(__ptr__=output)

    @overload
    @staticmethod
    def aggregate(first: FloatRange, second: FloatRange) -> FloatRange:
        pass

    @overload
    @staticmethod
    def aggregate(
        first: FloatRange, second: FloatRange, third: FloatRange
    ) -> FloatRange:
        pass

    @staticmethod
    def aggregate(*args, **keywords):
        match (args, keywords):
            case (
                ([FloatRange() as first, FloatRange() as second], {**rest})
                | (
                    [],
                    {
                        "first": FloatRange() as first,
                        "second": FloatRange() as second,
                        **rest,
                    },
                )
            ) if not rest:
                inputs = _Tuple2_c_void_p_c_void_p(first.__ptr__, second.__ptr__)
                output = c_void_p()
                _lib.opensolid__FloatRange__aggregate__FloatRange_FloatRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return FloatRange(__ptr__=output)
            case (
                (
                    [
                        FloatRange() as first,
                        FloatRange() as second,
                        FloatRange() as third,
                    ],
                    {**rest},
                )
                | (
                    [],
                    {
                        "first": FloatRange() as first,
                        "second": FloatRange() as second,
                        "third": FloatRange() as third,
                        **rest,
                    },
                )
            ) if not rest:
                inputs = _Tuple3_c_void_p_c_void_p_c_void_p(
                    first.__ptr__, second.__ptr__, third.__ptr__
                )
                output = c_void_p()
                _lib.opensolid__FloatRange__aggregate__FloatRange_FloatRange_FloatRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return FloatRange(__ptr__=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    def endpoints(self) -> tuple[float, float]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid__FloatRange__endpoints(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (output.field0, output.field1)

    def intersection(self, other: FloatRange) -> FloatRange | None:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = _Maybe_c_void_p()
        _lib.opensolid__FloatRange__intersection__FloatRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            FloatRange(__ptr__=c_void_p(output.field1)) if output.field0 == 0 else None
        )


class LengthRange:
    @overload
    def __init__(self, *, __ptr__: c_void_p) -> None:
        pass

    @overload
    def __init__(self, value: Length) -> None:
        pass

    @overload
    def __init__(self, low: Length, high: Length) -> None:
        pass

    def __init__(self, *args, **keywords):
        match (args, keywords):
            case ([], {"__ptr__": c_void_p() as __ptr__, **rest}) if not rest:
                self.__ptr__ = __ptr__
            case (
                ([Length() as value], {**rest})
                | ([], {"value": Length() as value, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = c_double(value.value)
                _lib.opensolid__LengthRange__constructor__Length(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case (
                ([Length() as low, Length() as high], {**rest})
                | ([], {"low": Length() as low, "high": Length() as high, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(low.value, high.value)
                _lib.opensolid__LengthRange__constructor__Length_Length(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case _:
                message = "Unexpected constructor arguments"
                raise TypeError(message)

    @overload
    @staticmethod
    def aggregate(first: LengthRange, second: LengthRange) -> LengthRange:
        pass

    @overload
    @staticmethod
    def aggregate(
        first: LengthRange, second: LengthRange, third: LengthRange
    ) -> LengthRange:
        pass

    @staticmethod
    def aggregate(*args, **keywords):
        match (args, keywords):
            case (
                ([LengthRange() as first, LengthRange() as second], {**rest})
                | (
                    [],
                    {
                        "first": LengthRange() as first,
                        "second": LengthRange() as second,
                        **rest,
                    },
                )
            ) if not rest:
                inputs = _Tuple2_c_void_p_c_void_p(first.__ptr__, second.__ptr__)
                output = c_void_p()
                _lib.opensolid__LengthRange__aggregate__LengthRange_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(__ptr__=output)
            case (
                (
                    [
                        LengthRange() as first,
                        LengthRange() as second,
                        LengthRange() as third,
                    ],
                    {**rest},
                )
                | (
                    [],
                    {
                        "first": LengthRange() as first,
                        "second": LengthRange() as second,
                        "third": LengthRange() as third,
                        **rest,
                    },
                )
            ) if not rest:
                inputs = _Tuple3_c_void_p_c_void_p_c_void_p(
                    first.__ptr__, second.__ptr__, third.__ptr__
                )
                output = c_void_p()
                _lib.opensolid__LengthRange__aggregate__LengthRange_LengthRange_LengthRange(
                    ctypes.byref(inputs), ctypes.byref(output)
                )
                return LengthRange(__ptr__=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    def endpoints(self) -> tuple[Length, Length]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid__LengthRange__endpoints(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (Length(output.field0), Length(output.field1))

    def intersection(self, other: LengthRange) -> LengthRange | None:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = _Maybe_c_void_p()
        _lib.opensolid__LengthRange__intersection__LengthRange(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return (
            LengthRange(__ptr__=c_void_p(output.field1)) if output.field0 == 0 else None
        )


class Vector2f:
    @overload
    def __init__(self, *, __ptr__: c_void_p) -> None:
        pass

    @overload
    def __init__(self, direction: Direction2d) -> None:
        pass

    @overload
    def __init__(self, x: float, y: float) -> None:
        pass

    @overload
    def __init__(self, components: tuple[float, float]) -> None:
        pass

    def __init__(self, *args, **keywords):
        match (args, keywords):
            case ([], {"__ptr__": c_void_p() as __ptr__, **rest}) if not rest:
                self.__ptr__ = __ptr__
            case (
                ([Direction2d() as direction], {**rest})
                | ([], {"direction": Direction2d() as direction, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = direction.__ptr__
                _lib.opensolid__Vector2f__constructor__Direction2d(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case (
                ([float() | int() as x, float() | int() as y], {**rest})
                | ([], {"x": float() | int() as x, "y": float() | int() as y, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(x, y)
                _lib.opensolid__Vector2f__constructor__Float_Float(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case (
                ([(float() | int(), float() | int()) as components], {**rest})
                | (
                    [],
                    {
                        "components": (float() | int(), float() | int()) as components,
                        **rest,
                    },
                )
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(components[0], components[1])
                _lib.opensolid__Vector2f__constructor__Tuple2_Float_Float(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case _:
                message = "Unexpected constructor arguments"
                raise TypeError(message)

    @staticmethod
    def zero() -> Vector2f:
        output = c_void_p()
        _lib.opensolid__Vector2f__zero(c_void_p(), ctypes.byref(output))
        return Vector2f(__ptr__=output)

    @staticmethod
    def x(x: float) -> Vector2f:
        inputs = c_double(x)
        output = c_void_p()
        _lib.opensolid__Vector2f__x_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Vector2f(__ptr__=output)

    @staticmethod
    def y(y: float) -> Vector2f:
        inputs = c_double(y)
        output = c_void_p()
        _lib.opensolid__Vector2f__y_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Vector2f(__ptr__=output)

    def components(self) -> tuple[float, float]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid__Vector2f__components(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1)

    def direction(self) -> Direction2d:
        tolerance = _float_tolerance()
        inputs = _Tuple2_c_double_c_void_p(tolerance, self.__ptr__)
        output = _Result_c_void_p()
        _lib.opensolid__Vector2f__direction(ctypes.byref(inputs), ctypes.byref(output))
        return (
            Direction2d(__ptr__=c_void_p(output.field2))
            if output.field0 == 0
            else _error(output)
        )


class Vector2d:
    @overload
    def __init__(self, *, __ptr__: c_void_p) -> None:
        pass

    @overload
    def __init__(self, x: Length, y: Length) -> None:
        pass

    @overload
    def __init__(self, components: tuple[Length, Length]) -> None:
        pass

    def __init__(self, *args, **keywords):
        match (args, keywords):
            case ([], {"__ptr__": c_void_p() as __ptr__, **rest}) if not rest:
                self.__ptr__ = __ptr__
            case (
                ([Length() as x, Length() as y], {**rest})
                | ([], {"x": Length() as x, "y": Length() as y, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(x.value, y.value)
                _lib.opensolid__Vector2d__constructor__Length_Length(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case (
                ([(Length(), Length()) as components], {**rest})
                | ([], {"components": (Length(), Length()) as components, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(
                    components[0].value, components[1].value
                )
                _lib.opensolid__Vector2d__constructor__Tuple2_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case _:
                message = "Unexpected constructor arguments"
                raise TypeError(message)

    @staticmethod
    def zero() -> Vector2d:
        output = c_void_p()
        _lib.opensolid__Vector2d__zero(c_void_p(), ctypes.byref(output))
        return Vector2d(__ptr__=output)

    @staticmethod
    def x(x: Length) -> Vector2d:
        inputs = c_double(x.value)
        output = c_void_p()
        _lib.opensolid__Vector2d__x_Length(ctypes.byref(inputs), ctypes.byref(output))
        return Vector2d(__ptr__=output)

    @staticmethod
    def y(y: Length) -> Vector2d:
        inputs = c_double(y.value)
        output = c_void_p()
        _lib.opensolid__Vector2d__y_Length(ctypes.byref(inputs), ctypes.byref(output))
        return Vector2d(__ptr__=output)

    def components(self) -> tuple[Length, Length]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid__Vector2d__components(ctypes.byref(inputs), ctypes.byref(output))
        return (Length(output.field0), Length(output.field1))

    def direction(self) -> Direction2d:
        tolerance = _length_tolerance()
        inputs = _Tuple2_c_double_c_void_p(tolerance.value, self.__ptr__)
        output = _Result_c_void_p()
        _lib.opensolid__Vector2d__direction(ctypes.byref(inputs), ctypes.byref(output))
        return (
            Direction2d(__ptr__=c_void_p(output.field2))
            if output.field0 == 0
            else _error(output)
        )


class Direction2d:
    @overload
    def __init__(self, *, __ptr__: c_void_p) -> None:
        pass

    @overload
    def __init__(self, angle: Angle) -> None:
        pass

    def __init__(self, *args, **keywords):
        match (args, keywords):
            case ([], {"__ptr__": c_void_p() as __ptr__, **rest}) if not rest:
                self.__ptr__ = __ptr__
            case (
                ([Angle() as angle], {**rest})
                | ([], {"angle": Angle() as angle, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = c_double(angle.value)
                _lib.opensolid__Direction2d__constructor__Angle(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case _:
                message = "Unexpected constructor arguments"
                raise TypeError(message)

    @staticmethod
    def x() -> Direction2d:
        output = c_void_p()
        _lib.opensolid__Direction2d__x(c_void_p(), ctypes.byref(output))
        return Direction2d(__ptr__=output)

    @staticmethod
    def y() -> Direction2d:
        output = c_void_p()
        _lib.opensolid__Direction2d__y(c_void_p(), ctypes.byref(output))
        return Direction2d(__ptr__=output)

    @staticmethod
    def positive_x() -> Direction2d:
        output = c_void_p()
        _lib.opensolid__Direction2d__positive_x(c_void_p(), ctypes.byref(output))
        return Direction2d(__ptr__=output)

    @staticmethod
    def positive_y() -> Direction2d:
        output = c_void_p()
        _lib.opensolid__Direction2d__positive_y(c_void_p(), ctypes.byref(output))
        return Direction2d(__ptr__=output)

    @staticmethod
    def negative_x() -> Direction2d:
        output = c_void_p()
        _lib.opensolid__Direction2d__negative_x(c_void_p(), ctypes.byref(output))
        return Direction2d(__ptr__=output)

    @staticmethod
    def negative_y() -> Direction2d:
        output = c_void_p()
        _lib.opensolid__Direction2d__negative_y(c_void_p(), ctypes.byref(output))
        return Direction2d(__ptr__=output)

    def to_angle(self) -> Angle:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid__Direction2d__to_angle(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Angle(output.value)


class Point2f:
    @overload
    def __init__(self, *, __ptr__: c_void_p) -> None:
        pass

    @overload
    def __init__(self, x: float, y: float) -> None:
        pass

    @overload
    def __init__(self, coordinates: tuple[float, float]) -> None:
        pass

    def __init__(self, *args, **keywords):
        match (args, keywords):
            case ([], {"__ptr__": c_void_p() as __ptr__, **rest}) if not rest:
                self.__ptr__ = __ptr__
            case (
                ([float() | int() as x, float() | int() as y], {**rest})
                | ([], {"x": float() | int() as x, "y": float() | int() as y, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(x, y)
                _lib.opensolid__Point2f__constructor__Float_Float(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case (
                ([(float() | int(), float() | int()) as coordinates], {**rest})
                | (
                    [],
                    {
                        "coordinates": (
                            float()
                            | int(),
                            float()
                            | int(),
                        ) as coordinates,
                        **rest,
                    },
                )
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(coordinates[0], coordinates[1])
                _lib.opensolid__Point2f__constructor__Tuple2_Float_Float(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case _:
                message = "Unexpected constructor arguments"
                raise TypeError(message)

    @staticmethod
    def origin() -> Point2f:
        output = c_void_p()
        _lib.opensolid__Point2f__origin(c_void_p(), ctypes.byref(output))
        return Point2f(__ptr__=output)

    @staticmethod
    def x(x: float) -> Point2f:
        inputs = c_double(x)
        output = c_void_p()
        _lib.opensolid__Point2f__x_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Point2f(__ptr__=output)

    @staticmethod
    def y(y: float) -> Point2f:
        inputs = c_double(y)
        output = c_void_p()
        _lib.opensolid__Point2f__y_Float(ctypes.byref(inputs), ctypes.byref(output))
        return Point2f(__ptr__=output)

    def coordinates(self) -> tuple[float, float]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid__Point2f__coordinates(ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1)

    def distance_to(self, other: Point2f) -> float:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_double()
        _lib.opensolid__Point2f__distance_to__Point2f(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def midpoint(self, other: Point2f) -> Point2f:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_void_p()
        _lib.opensolid__Point2f__midpoint__Point2f(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2f(__ptr__=output)


class Point2d:
    @overload
    def __init__(self, *, __ptr__: c_void_p) -> None:
        pass

    @overload
    def __init__(self, x: Length, y: Length) -> None:
        pass

    @overload
    def __init__(self, coordinates: tuple[Length, Length]) -> None:
        pass

    def __init__(self, *args, **keywords):
        match (args, keywords):
            case ([], {"__ptr__": c_void_p() as __ptr__, **rest}) if not rest:
                self.__ptr__ = __ptr__
            case (
                ([Length() as x, Length() as y], {**rest})
                | ([], {"x": Length() as x, "y": Length() as y, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(x.value, y.value)
                _lib.opensolid__Point2d__constructor__Length_Length(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case (
                ([(Length(), Length()) as coordinates], {**rest})
                | ([], {"coordinates": (Length(), Length()) as coordinates, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(
                    coordinates[0].value, coordinates[1].value
                )
                _lib.opensolid__Point2d__constructor__Tuple2_Length_Length(
                    ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case _:
                message = "Unexpected constructor arguments"
                raise TypeError(message)

    @staticmethod
    def origin() -> Point2d:
        output = c_void_p()
        _lib.opensolid__Point2d__origin(c_void_p(), ctypes.byref(output))
        return Point2d(__ptr__=output)

    @staticmethod
    def x(x: Length) -> Point2d:
        inputs = c_double(x.value)
        output = c_void_p()
        _lib.opensolid__Point2d__x_Length(ctypes.byref(inputs), ctypes.byref(output))
        return Point2d(__ptr__=output)

    @staticmethod
    def y(y: Length) -> Point2d:
        inputs = c_double(y.value)
        output = c_void_p()
        _lib.opensolid__Point2d__y_Length(ctypes.byref(inputs), ctypes.byref(output))
        return Point2d(__ptr__=output)

    def coordinates(self) -> tuple[Length, Length]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid__Point2d__coordinates(ctypes.byref(inputs), ctypes.byref(output))
        return (Length(output.field0), Length(output.field1))

    def distance_to(self, other: Point2d) -> Length:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_double()
        _lib.opensolid__Point2d__distance_to__Point2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Length(output.value)

    def midpoint(self, other: Point2d) -> Point2d:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_void_p()
        _lib.opensolid__Point2d__midpoint__Point2d(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return Point2d(__ptr__=output)


class Curve1f:
    def __init__(self, *, __ptr__: c_void_p) -> None:
        self.__ptr__ = __ptr__

    @staticmethod
    def t() -> Curve1f:
        output = c_void_p()
        _lib.opensolid__Curve1f__t(c_void_p(), ctypes.byref(output))
        return Curve1f(__ptr__=output)

    def evaluate(self, parameter_value: float) -> float:
        inputs = _Tuple2_c_double_c_void_p(parameter_value, self.__ptr__)
        output = c_double()
        _lib.opensolid__Curve1f__evaluate__Float(
            ctypes.byref(inputs), ctypes.byref(output)
        )
        return output.value

    def squared(self) -> Curve1f:
        inputs = self.__ptr__
        output = c_void_p()
        _lib.opensolid__Curve1f__squared(ctypes.byref(inputs), ctypes.byref(output))
        return Curve1f(__ptr__=output)
