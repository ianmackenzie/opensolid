"""A collection of classes for 2D/3D geometric modelling."""

from __future__ import annotations

import ctypes
import platform
from contextlib import contextmanager
from ctypes import (
    CDLL,
    Structure,
    Union,
    c_char_p,
    c_double,
    c_int,
    c_int64,
    c_size_t,
    c_void_p,
)
from typing import Any, Generator, overload

# Load the native library
_load_path = "libopensolid-ffi.so"
if platform.system() == "Darwin":
    _load_path = "libopensolid-ffi.dylib"
_lib: CDLL = ctypes.cdll.LoadLibrary(_load_path)

# Define the signatures of the C API functions
_lib.opensolid_invoke.argtypes = [c_int, c_int, c_void_p, c_void_p]
_lib.opensolid_malloc.argtypes = [c_size_t]
_lib.opensolid_malloc.restype = c_void_p
_lib.opensolid_free.argtypes = [c_void_p]
_lib.opensolid_release.argtypes = [c_void_p]


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


class _Tuple2_c_double_c_double(Structure):
    _fields_ = [("field0", c_double), ("field1", c_double)]  # noqa: RUF012


class _Tuple2_c_void_p_c_void_p(Structure):
    _fields_ = [("field0", c_void_p), ("field1", c_void_p)]  # noqa: RUF012


class _Tuple2_c_double_c_void_p(Structure):
    _fields_ = [("field0", c_double), ("field1", c_void_p)]  # noqa: RUF012


class _Result_c_void_p(Structure):
    _fields_ = [("field0", c_int64), ("field1", _ErrorMessage), ("field2", c_void_p)]  # noqa: RUF012


class _Tuple3_c_void_p_c_void_p_c_void_p(Structure):
    _fields_ = [("field0", c_void_p), ("field1", c_void_p), ("field2", c_void_p)]  # noqa: RUF012


class _Maybe_c_void_p(Structure):
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
                _lib.opensolid_invoke(
                    0, 0, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
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
                _lib.opensolid_invoke(
                    0, 1, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case _:
                message = "Unexpected constructor arguments"
                raise TypeError(message)

    @staticmethod
    def unit() -> FloatRange:
        output = c_void_p()
        _lib.opensolid_invoke(0, 2, c_void_p(), ctypes.byref(output))
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
                _lib.opensolid_invoke(0, 3, ctypes.byref(inputs), ctypes.byref(output))
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
                _lib.opensolid_invoke(0, 4, ctypes.byref(inputs), ctypes.byref(output))
                return FloatRange(__ptr__=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    def endpoints(self) -> tuple[float, float]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_invoke(0, 5, ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1)

    def intersection(self, other: FloatRange) -> FloatRange | None:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = _Maybe_c_void_p()
        _lib.opensolid_invoke(0, 6, ctypes.byref(inputs), ctypes.byref(output))
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
                _lib.opensolid_invoke(
                    1, 0, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case (
                ([Length() as low, Length() as high], {**rest})
                | ([], {"low": Length() as low, "high": Length() as high, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(low.value, high.value)
                _lib.opensolid_invoke(
                    1, 1, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
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
                _lib.opensolid_invoke(1, 2, ctypes.byref(inputs), ctypes.byref(output))
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
                _lib.opensolid_invoke(1, 3, ctypes.byref(inputs), ctypes.byref(output))
                return LengthRange(__ptr__=output)
            case _:
                message = "Unexpected function arguments"
                raise TypeError(message)

    def endpoints(self) -> tuple[Length, Length]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_invoke(1, 4, ctypes.byref(inputs), ctypes.byref(output))
        return (Length(output.field0), Length(output.field1))

    def intersection(self, other: LengthRange) -> LengthRange | None:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = _Maybe_c_void_p()
        _lib.opensolid_invoke(1, 5, ctypes.byref(inputs), ctypes.byref(output))
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
                _lib.opensolid_invoke(
                    2, 0, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case (
                ([float() | int() as x, float() | int() as y], {**rest})
                | ([], {"x": float() | int() as x, "y": float() | int() as y, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(x, y)
                _lib.opensolid_invoke(
                    2, 1, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
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
                _lib.opensolid_invoke(
                    2, 2, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case _:
                message = "Unexpected constructor arguments"
                raise TypeError(message)

    @staticmethod
    def zero() -> Vector2f:
        output = c_void_p()
        _lib.opensolid_invoke(2, 3, c_void_p(), ctypes.byref(output))
        return Vector2f(__ptr__=output)

    @staticmethod
    def x(x: float) -> Vector2f:
        inputs = c_double(x)
        output = c_void_p()
        _lib.opensolid_invoke(2, 4, ctypes.byref(inputs), ctypes.byref(output))
        return Vector2f(__ptr__=output)

    @staticmethod
    def y(y: float) -> Vector2f:
        inputs = c_double(y)
        output = c_void_p()
        _lib.opensolid_invoke(2, 5, ctypes.byref(inputs), ctypes.byref(output))
        return Vector2f(__ptr__=output)

    def components(self) -> tuple[float, float]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_invoke(2, 6, ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1)

    def direction(self) -> Direction2d:
        tolerance = _float_tolerance()
        inputs = _Tuple2_c_double_c_void_p(tolerance, self.__ptr__)
        output = _Result_c_void_p()
        _lib.opensolid_invoke(2, 7, ctypes.byref(inputs), ctypes.byref(output))
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
                _lib.opensolid_invoke(
                    3, 0, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case (
                ([(Length(), Length()) as components], {**rest})
                | ([], {"components": (Length(), Length()) as components, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(
                    components[0].value, components[1].value
                )
                _lib.opensolid_invoke(
                    3, 1, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case _:
                message = "Unexpected constructor arguments"
                raise TypeError(message)

    @staticmethod
    def zero() -> Vector2d:
        output = c_void_p()
        _lib.opensolid_invoke(3, 2, c_void_p(), ctypes.byref(output))
        return Vector2d(__ptr__=output)

    @staticmethod
    def x(x: Length) -> Vector2d:
        inputs = c_double(x.value)
        output = c_void_p()
        _lib.opensolid_invoke(3, 3, ctypes.byref(inputs), ctypes.byref(output))
        return Vector2d(__ptr__=output)

    @staticmethod
    def y(y: Length) -> Vector2d:
        inputs = c_double(y.value)
        output = c_void_p()
        _lib.opensolid_invoke(3, 4, ctypes.byref(inputs), ctypes.byref(output))
        return Vector2d(__ptr__=output)

    def components(self) -> tuple[Length, Length]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_invoke(3, 5, ctypes.byref(inputs), ctypes.byref(output))
        return (Length(output.field0), Length(output.field1))

    def direction(self) -> Direction2d:
        tolerance = _length_tolerance()
        inputs = _Tuple2_c_double_c_void_p(tolerance.value, self.__ptr__)
        output = _Result_c_void_p()
        _lib.opensolid_invoke(3, 6, ctypes.byref(inputs), ctypes.byref(output))
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
                _lib.opensolid_invoke(
                    4, 0, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case _:
                message = "Unexpected constructor arguments"
                raise TypeError(message)

    @staticmethod
    def x() -> Direction2d:
        output = c_void_p()
        _lib.opensolid_invoke(4, 1, c_void_p(), ctypes.byref(output))
        return Direction2d(__ptr__=output)

    @staticmethod
    def y() -> Direction2d:
        output = c_void_p()
        _lib.opensolid_invoke(4, 2, c_void_p(), ctypes.byref(output))
        return Direction2d(__ptr__=output)

    @staticmethod
    def positive_x() -> Direction2d:
        output = c_void_p()
        _lib.opensolid_invoke(4, 3, c_void_p(), ctypes.byref(output))
        return Direction2d(__ptr__=output)

    @staticmethod
    def positive_y() -> Direction2d:
        output = c_void_p()
        _lib.opensolid_invoke(4, 4, c_void_p(), ctypes.byref(output))
        return Direction2d(__ptr__=output)

    @staticmethod
    def negative_x() -> Direction2d:
        output = c_void_p()
        _lib.opensolid_invoke(4, 5, c_void_p(), ctypes.byref(output))
        return Direction2d(__ptr__=output)

    @staticmethod
    def negative_y() -> Direction2d:
        output = c_void_p()
        _lib.opensolid_invoke(4, 6, c_void_p(), ctypes.byref(output))
        return Direction2d(__ptr__=output)

    def to_angle(self) -> Angle:
        inputs = self.__ptr__
        output = c_double()
        _lib.opensolid_invoke(4, 7, ctypes.byref(inputs), ctypes.byref(output))
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
                _lib.opensolid_invoke(
                    5, 0, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
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
                _lib.opensolid_invoke(
                    5, 1, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case _:
                message = "Unexpected constructor arguments"
                raise TypeError(message)

    @staticmethod
    def origin() -> Point2f:
        output = c_void_p()
        _lib.opensolid_invoke(5, 2, c_void_p(), ctypes.byref(output))
        return Point2f(__ptr__=output)

    @staticmethod
    def x(x: float) -> Point2f:
        inputs = c_double(x)
        output = c_void_p()
        _lib.opensolid_invoke(5, 3, ctypes.byref(inputs), ctypes.byref(output))
        return Point2f(__ptr__=output)

    @staticmethod
    def y(y: float) -> Point2f:
        inputs = c_double(y)
        output = c_void_p()
        _lib.opensolid_invoke(5, 4, ctypes.byref(inputs), ctypes.byref(output))
        return Point2f(__ptr__=output)

    def coordinates(self) -> tuple[float, float]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_invoke(5, 5, ctypes.byref(inputs), ctypes.byref(output))
        return (output.field0, output.field1)

    def distance_to(self, other: Point2f) -> float:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_double()
        _lib.opensolid_invoke(5, 6, ctypes.byref(inputs), ctypes.byref(output))
        return output.value

    def midpoint(self, other: Point2f) -> Point2f:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_invoke(5, 7, ctypes.byref(inputs), ctypes.byref(output))
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
                _lib.opensolid_invoke(
                    6, 0, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case (
                ([(Length(), Length()) as coordinates], {**rest})
                | ([], {"coordinates": (Length(), Length()) as coordinates, **rest})
            ) if not rest:
                self.__ptr__ = c_void_p()
                inputs = _Tuple2_c_double_c_double(
                    coordinates[0].value, coordinates[1].value
                )
                _lib.opensolid_invoke(
                    6, 1, ctypes.byref(inputs), ctypes.byref(self.__ptr__)
                )
            case _:
                message = "Unexpected constructor arguments"
                raise TypeError(message)

    @staticmethod
    def origin() -> Point2d:
        output = c_void_p()
        _lib.opensolid_invoke(6, 2, c_void_p(), ctypes.byref(output))
        return Point2d(__ptr__=output)

    @staticmethod
    def x(x: Length) -> Point2d:
        inputs = c_double(x.value)
        output = c_void_p()
        _lib.opensolid_invoke(6, 3, ctypes.byref(inputs), ctypes.byref(output))
        return Point2d(__ptr__=output)

    @staticmethod
    def y(y: Length) -> Point2d:
        inputs = c_double(y.value)
        output = c_void_p()
        _lib.opensolid_invoke(6, 4, ctypes.byref(inputs), ctypes.byref(output))
        return Point2d(__ptr__=output)

    def coordinates(self) -> tuple[Length, Length]:
        inputs = self.__ptr__
        output = _Tuple2_c_double_c_double()
        _lib.opensolid_invoke(6, 5, ctypes.byref(inputs), ctypes.byref(output))
        return (Length(output.field0), Length(output.field1))

    def distance_to(self, other: Point2d) -> Length:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_double()
        _lib.opensolid_invoke(6, 6, ctypes.byref(inputs), ctypes.byref(output))
        return Length(output.value)

    def midpoint(self, other: Point2d) -> Point2d:
        inputs = _Tuple2_c_void_p_c_void_p(other.__ptr__, self.__ptr__)
        output = c_void_p()
        _lib.opensolid_invoke(6, 7, ctypes.byref(inputs), ctypes.byref(output))
        return Point2d(__ptr__=output)
