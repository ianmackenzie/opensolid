from __future__ import annotations
from contextlib import contextmanager
from typing import Optional
import platform
from ctypes import *
global lib
system = platform.system()
if system == 'Darwin':
    lib = cdll.LoadLibrary('libopensolid-ffi.dylib')
elif system == 'Linux':
    lib = cdll.LoadLibrary('libopensolid-ffi.so')
else:
    raise Exception('System ' + system + ' is not supported')
lib.opensolid_free.argtypes = [c_void_p]
global_tolerance = None
@contextmanager
def Tolerance(new_tolerance:float ):
    global global_tolerance
    (saved_tolerance, global_tolerance) = (global_tolerance, new_tolerance)
    try:
        yield
    finally:
        global_tolerance = saved_tolerance
class Axis2d:
    def __init__(self, ptr:c_void_p ) -> None:
        self.ptr = ptr
    lib.opensolid_axis2d_origin_point.argtypes = [c_void_p]
    lib.opensolid_axis2d_origin_point.restype = c_void_p
    def origin_point(self) -> Point2d:
        return Point2d(lib.opensolid_axis2d_origin_point(self.ptr))
    lib.opensolid_axis2d_direction.argtypes = [c_void_p]
    lib.opensolid_axis2d_direction.restype = c_void_p
    def direction(self) -> Direction2d:
        return Direction2d(lib.opensolid_axis2d_direction(self.ptr))
    lib.opensolid_axis2d_x.argtypes = []
    lib.opensolid_axis2d_x.restype = c_void_p
    @staticmethod
    def x() -> Axis2d:
        return Axis2d(lib.opensolid_axis2d_x())
    lib.opensolid_axis2d_y.argtypes = []
    lib.opensolid_axis2d_y.restype = c_void_p
    @staticmethod
    def y() -> Axis2d:
        return Axis2d(lib.opensolid_axis2d_y())
    lib.opensolid_axis2d_through.argtypes = [c_void_p, c_void_p]
    lib.opensolid_axis2d_through.restype = c_void_p
    @staticmethod
    def through(point:Point2d , direction:Direction2d ) -> Axis2d:
        return Axis2d(lib.opensolid_axis2d_through(point.ptr, direction.ptr))
    def __del__(self) -> None:
        lib.opensolid_free(self.ptr)
    def __repr__(self) -> str:
        return "Axis2d(" + str(self.origin_point()) + ", " + str(self.direction()) + ")"
class Bounds2d:
    def __init__(self, ptr:c_void_p ) -> None:
        self.ptr = ptr
    lib.opensolid_bounds2d_x_coordinate.argtypes = [c_void_p]
    lib.opensolid_bounds2d_x_coordinate.restype = c_void_p
    def x_coordinate(self) -> Range:
        return Range(lib.opensolid_bounds2d_x_coordinate(self.ptr))
    lib.opensolid_bounds2d_y_coordinate.argtypes = [c_void_p]
    lib.opensolid_bounds2d_y_coordinate.restype = c_void_p
    def y_coordinate(self) -> Range:
        return Range(lib.opensolid_bounds2d_y_coordinate(self.ptr))
    lib.opensolid_bounds2d_constant.argtypes = [c_void_p]
    lib.opensolid_bounds2d_constant.restype = c_void_p
    @staticmethod
    def constant(point:Point2d ) -> Bounds2d:
        return Bounds2d(lib.opensolid_bounds2d_constant(point.ptr))
    lib.opensolid_bounds2d_hull2.argtypes = [c_void_p, c_void_p]
    lib.opensolid_bounds2d_hull2.restype = c_void_p
    @staticmethod
    def hull2(p1:Point2d , p2:Point2d ) -> Bounds2d:
        return Bounds2d(lib.opensolid_bounds2d_hull2(p1.ptr, p2.ptr))
    lib.opensolid_bounds2d_hull3.argtypes = [c_void_p, c_void_p, c_void_p]
    lib.opensolid_bounds2d_hull3.restype = c_void_p
    @staticmethod
    def hull3(p1:Point2d , p2:Point2d , p3:Point2d ) -> Bounds2d:
        return Bounds2d(lib.opensolid_bounds2d_hull3(p1.ptr, p2.ptr, p3.ptr))
    lib.opensolid_bounds2d_hull4.argtypes = [c_void_p, c_void_p, c_void_p, c_void_p]
    lib.opensolid_bounds2d_hull4.restype = c_void_p
    @staticmethod
    def hull4(p1:Point2d , p2:Point2d , p3:Point2d , p4:Point2d ) -> Bounds2d:
        return Bounds2d(lib.opensolid_bounds2d_hull4(p1.ptr, p2.ptr, p3.ptr, p4.ptr))
    lib.opensolid_bounds2d_aggregate2.argtypes = [c_void_p, c_void_p]
    lib.opensolid_bounds2d_aggregate2.restype = c_void_p
    @staticmethod
    def aggregate2(bounds1:Bounds2d , bounds2:Bounds2d ) -> Bounds2d:
        return Bounds2d(lib.opensolid_bounds2d_aggregate2(bounds1.ptr, bounds2.ptr))
    lib.opensolid_bounds2d_intersects.argtypes = [c_void_p, c_void_p]
    lib.opensolid_bounds2d_intersects.restype = c_bool
    def intersects(self, bounds1:Bounds2d ) -> bool:
        return lib.opensolid_bounds2d_intersects(bounds1.ptr, self.ptr)
    lib.opensolid_bounds2d_intersection.argtypes = [c_void_p, c_void_p]
    lib.opensolid_bounds2d_intersection.restype = c_void_p
    def intersection(self, bounds1:Bounds2d ) -> Optional[Bounds2d]:
        ret_val = lib.opensolid_bounds2d_intersection(bounds1.ptr, self.ptr)
        return Bounds2d(ret_val) if ret_val else None
    lib.opensolid_bounds2d_interpolate.argtypes = [c_void_p, c_double, c_double]
    lib.opensolid_bounds2d_interpolate.restype = c_void_p
    @staticmethod
    def interpolate(bounds:Bounds2d , u:float , v:float ) -> Point2d:
        return Point2d(lib.opensolid_bounds2d_interpolate(bounds.ptr, u, v))
    def __del__(self) -> None:
        lib.opensolid_free(self.ptr)
    def __repr__(self) -> str:
        return "Bounds2d(" + str(self.x_coordinate()) + ", " + str(self.y_coordinate()) + ")"
class Direction2d:
    def __init__(self, ptr:c_void_p ) -> None:
        self.ptr = ptr
    lib.opensolid_direction2d_x_component.argtypes = [c_void_p]
    lib.opensolid_direction2d_x_component.restype = c_double
    def x_component(self) -> float:
        return lib.opensolid_direction2d_x_component(self.ptr)
    lib.opensolid_direction2d_y_component.argtypes = [c_void_p]
    lib.opensolid_direction2d_y_component.restype = c_double
    def y_component(self) -> float:
        return lib.opensolid_direction2d_y_component(self.ptr)
    lib.opensolid_direction2d_unsafe.argtypes = [c_void_p]
    lib.opensolid_direction2d_unsafe.restype = c_void_p
    @staticmethod
    def unsafe(v:Vector2d ) -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_unsafe(v.ptr))
    lib.opensolid_direction2d_unwrap.argtypes = [c_void_p]
    lib.opensolid_direction2d_unwrap.restype = c_void_p
    def unwrap(self) -> Vector2d:
        return Vector2d(lib.opensolid_direction2d_unwrap(self.ptr))
    lib.opensolid_direction2d_x.argtypes = []
    lib.opensolid_direction2d_x.restype = c_void_p
    @staticmethod
    def x() -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_x())
    lib.opensolid_direction2d_positive_x.argtypes = []
    lib.opensolid_direction2d_positive_x.restype = c_void_p
    @staticmethod
    def positive_x() -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_positive_x())
    lib.opensolid_direction2d_negative_x.argtypes = []
    lib.opensolid_direction2d_negative_x.restype = c_void_p
    @staticmethod
    def negative_x() -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_negative_x())
    lib.opensolid_direction2d_y.argtypes = []
    lib.opensolid_direction2d_y.restype = c_void_p
    @staticmethod
    def y() -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_y())
    lib.opensolid_direction2d_positive_y.argtypes = []
    lib.opensolid_direction2d_positive_y.restype = c_void_p
    @staticmethod
    def positive_y() -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_positive_y())
    lib.opensolid_direction2d_negative_y.argtypes = []
    lib.opensolid_direction2d_negative_y.restype = c_void_p
    @staticmethod
    def negative_y() -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_negative_y())
    lib.opensolid_direction2d_u.argtypes = []
    lib.opensolid_direction2d_u.restype = c_void_p
    @staticmethod
    def u() -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_u())
    lib.opensolid_direction2d_v.argtypes = []
    lib.opensolid_direction2d_v.restype = c_void_p
    @staticmethod
    def v() -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_v())
    lib.opensolid_direction2d_from_angle.argtypes = [c_double]
    lib.opensolid_direction2d_from_angle.restype = c_void_p
    @staticmethod
    def from_angle(angle:float ) -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_from_angle(angle))
    lib.opensolid_direction2d_to_angle.argtypes = [c_void_p]
    lib.opensolid_direction2d_to_angle.restype = c_double
    @staticmethod
    def to_angle(dir:Direction2d ) -> float:
        return lib.opensolid_direction2d_to_angle(dir.ptr)
    lib.opensolid_direction2d_degrees.argtypes = [c_double]
    lib.opensolid_direction2d_degrees.restype = c_void_p
    @staticmethod
    def degrees(value:float ) -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_degrees(value))
    lib.opensolid_direction2d_radians.argtypes = [c_double]
    lib.opensolid_direction2d_radians.restype = c_void_p
    @staticmethod
    def radians(value:float ) -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_radians(value))
    lib.opensolid_direction2d_angle_from.argtypes = [c_void_p, c_void_p]
    lib.opensolid_direction2d_angle_from.restype = c_double
    def angle_from(self, d1:Direction2d ) -> float:
        return lib.opensolid_direction2d_angle_from(d1.ptr, self.ptr)
    lib.opensolid_direction2d_perpendicular_to.argtypes = [c_void_p]
    lib.opensolid_direction2d_perpendicular_to.restype = c_void_p
    def perpendicular_to(self) -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_perpendicular_to(self.ptr))
    lib.opensolid_direction2d_rotate_left.argtypes = [c_void_p]
    lib.opensolid_direction2d_rotate_left.restype = c_void_p
    def rotate_left(self) -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_rotate_left(self.ptr))
    lib.opensolid_direction2d_rotate_right.argtypes = [c_void_p]
    lib.opensolid_direction2d_rotate_right.restype = c_void_p
    def rotate_right(self) -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_rotate_right(self.ptr))
    lib.opensolid_direction2d_place_in.argtypes = [c_void_p, c_void_p]
    lib.opensolid_direction2d_place_in.restype = c_void_p
    def place_in(self, frame:Frame2d ) -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_place_in(frame.ptr, self.ptr))
    lib.opensolid_direction2d_relative_to.argtypes = [c_void_p, c_void_p]
    lib.opensolid_direction2d_relative_to.restype = c_void_p
    def relative_to(self, frame:Frame2d ) -> Direction2d:
        return Direction2d(lib.opensolid_direction2d_relative_to(frame.ptr, self.ptr))
    def __del__(self) -> None:
        lib.opensolid_free(self.ptr)
    def __repr__(self) -> str:
        return "Direction2d(" + str(self.x_component()) + ", " + str(self.y_component()) + ")"
class Frame2d:
    def __init__(self, ptr:c_void_p ) -> None:
        self.ptr = ptr
    lib.opensolid_frame2d_at_origin.argtypes = []
    lib.opensolid_frame2d_at_origin.restype = c_void_p
    @staticmethod
    def at_origin() -> Frame2d:
        return Frame2d(lib.opensolid_frame2d_at_origin())
    lib.opensolid_frame2d_at_point.argtypes = [c_void_p]
    lib.opensolid_frame2d_at_point.restype = c_void_p
    @staticmethod
    def at_point(point:Point2d ) -> Frame2d:
        return Frame2d(lib.opensolid_frame2d_at_point(point.ptr))
    lib.opensolid_frame2d_origin_point.argtypes = []
    lib.opensolid_frame2d_origin_point.restype = c_void_p
    def origin_point(self) -> Point2d:
        return Point2d(lib.opensolid_frame2d_origin_point(self.ptr))
    lib.opensolid_frame2d_x_direction.argtypes = []
    lib.opensolid_frame2d_x_direction.restype = c_void_p
    def x_direction(self) -> Direction2d:
        return Direction2d(lib.opensolid_frame2d_x_direction(self.ptr))
    lib.opensolid_frame2d_y_direction.argtypes = []
    lib.opensolid_frame2d_y_direction.restype = c_void_p
    def y_direction(self) -> Direction2d:
        return Direction2d(lib.opensolid_frame2d_y_direction(self.ptr))
    lib.opensolid_frame2d_x_axis.argtypes = [c_void_p]
    lib.opensolid_frame2d_x_axis.restype = c_void_p
    def x_axis(self) -> Axis2d:
        return Axis2d(lib.opensolid_frame2d_x_axis(self.ptr))
    lib.opensolid_frame2d_y_axis.argtypes = [c_void_p]
    lib.opensolid_frame2d_y_axis.restype = c_void_p
    def y_axis(self) -> Axis2d:
        return Axis2d(lib.opensolid_frame2d_y_axis(self.ptr))
    lib.opensolid_frame2d_with_x_direction.argtypes = [c_void_p, c_void_p]
    lib.opensolid_frame2d_with_x_direction.restype = c_void_p
    @staticmethod
    def with_x_direction(direction:Direction2d , point:Point2d ) -> Frame2d:
        return Frame2d(lib.opensolid_frame2d_with_x_direction(direction.ptr, point.ptr))
    lib.opensolid_frame2d_with_y_direction.argtypes = [c_void_p, c_void_p]
    lib.opensolid_frame2d_with_y_direction.restype = c_void_p
    @staticmethod
    def with_y_direction(direction:Direction2d , point:Point2d ) -> Frame2d:
        return Frame2d(lib.opensolid_frame2d_with_y_direction(direction.ptr, point.ptr))
    lib.opensolid_frame2d_from_x_axis.argtypes = [c_void_p]
    lib.opensolid_frame2d_from_x_axis.restype = c_void_p
    @staticmethod
    def from_x_axis(axis:Axis2d ) -> Frame2d:
        return Frame2d(lib.opensolid_frame2d_from_x_axis(axis.ptr))
    lib.opensolid_frame2d_from_y_axis.argtypes = [c_void_p]
    lib.opensolid_frame2d_from_y_axis.restype = c_void_p
    @staticmethod
    def from_y_axis(axis:Axis2d ) -> Frame2d:
        return Frame2d(lib.opensolid_frame2d_from_y_axis(axis.ptr))
    def __del__(self) -> None:
        lib.opensolid_free(self.ptr)
    def __repr__(self) -> str:
        return "Frame2d(" + str(self.origin_point()) + ", " + str(self.x_direction()) + ", " + str(self.y_direction()) + ")"
class Point2d:
    def __init__(self, ptr:c_void_p ) -> None:
        self.ptr = ptr
    lib.opensolid_point2d_x_coordinate.argtypes = [c_void_p]
    lib.opensolid_point2d_x_coordinate.restype = c_double
    def x_coordinate(self) -> float:
        return lib.opensolid_point2d_x_coordinate(self.ptr)
    lib.opensolid_point2d_y_coordinate.argtypes = [c_void_p]
    lib.opensolid_point2d_y_coordinate.restype = c_double
    def y_coordinate(self) -> float:
        return lib.opensolid_point2d_y_coordinate(self.ptr)
    lib.opensolid_point2d_angle_from.argtypes = [c_void_p, c_void_p]
    lib.opensolid_point2d_angle_from.restype = c_double
    @staticmethod
    def angle_from(p1:Point2d , p2:Point2d ) -> float:
        return lib.opensolid_point2d_angle_from(p1.ptr, p2.ptr)
    lib.opensolid_point2d_distance_from.argtypes = [c_void_p, c_void_p]
    lib.opensolid_point2d_distance_from.restype = c_double
    @staticmethod
    def distance_from(p1:Point2d , p2:Point2d ) -> float:
        return lib.opensolid_point2d_distance_from(p1.ptr, p2.ptr)
    lib.opensolid_point2d_interpolate_from.argtypes = [c_void_p, c_void_p, c_double]
    lib.opensolid_point2d_interpolate_from.restype = c_void_p
    @staticmethod
    def interpolate_from(p1:Point2d , p2:Point2d , t:float ) -> Point2d:
        return Point2d(lib.opensolid_point2d_interpolate_from(p1.ptr, p2.ptr, t))
    lib.opensolid_point2d_meters.argtypes = [c_double, c_double]
    lib.opensolid_point2d_meters.restype = c_void_p
    @staticmethod
    def meters(px:float , py:float ) -> Point2d:
        return Point2d(lib.opensolid_point2d_meters(px, py))
    lib.opensolid_point2d_midpoint.argtypes = [c_void_p, c_void_p]
    lib.opensolid_point2d_midpoint.restype = c_void_p
    @staticmethod
    def midpoint(p1:Point2d , p2:Point2d ) -> Point2d:
        return Point2d(lib.opensolid_point2d_midpoint(p1.ptr, p2.ptr))
    lib.opensolid_point2d_origin.argtypes = []
    lib.opensolid_point2d_origin.restype = c_void_p
    @staticmethod
    def origin() -> Point2d:
        return Point2d(lib.opensolid_point2d_origin())
    lib.opensolid_point2d_place_in.argtypes = [c_void_p, c_void_p]
    lib.opensolid_point2d_place_in.restype = c_void_p
    def place_in(self, frame:Frame2d ) -> Point2d:
        return Point2d(lib.opensolid_point2d_place_in(frame.ptr, self.ptr))
    lib.opensolid_point2d_relative_to.argtypes = [c_void_p, c_void_p]
    lib.opensolid_point2d_relative_to.restype = c_void_p
    def relative_to(self, frame:Frame2d ) -> Point2d:
        return Point2d(lib.opensolid_point2d_relative_to(frame.ptr, self.ptr))
    lib.opensolid_point2d_signed_distance_along.argtypes = [c_void_p, c_void_p]
    lib.opensolid_point2d_signed_distance_along.restype = c_double
    def signed_distance_along(self, axis:Axis2d ) -> float:
        return lib.opensolid_point2d_signed_distance_along(axis.ptr, self.ptr)
    lib.opensolid_point2d_signed_distance_from.argtypes = [c_void_p, c_void_p]
    lib.opensolid_point2d_signed_distance_from.restype = c_double
    def signed_distance_from(self, axis:Axis2d ) -> float:
        return lib.opensolid_point2d_signed_distance_from(axis.ptr, self.ptr)
    lib.opensolid_point2d_uv.argtypes = [c_double, c_double]
    lib.opensolid_point2d_uv.restype = c_void_p
    @staticmethod
    def uv(u:float , v:float ) -> Point2d:
        return Point2d(lib.opensolid_point2d_uv(u, v))
    lib.opensolid_point2d_x.argtypes = [c_double]
    lib.opensolid_point2d_x.restype = c_void_p
    @staticmethod
    def x(px:float ) -> Point2d:
        return Point2d(lib.opensolid_point2d_x(px))
    lib.opensolid_point2d_xy.argtypes = [c_double, c_double]
    lib.opensolid_point2d_xy.restype = c_void_p
    @staticmethod
    def xy(x:float , y:float ) -> Point2d:
        return Point2d(lib.opensolid_point2d_xy(x, y))
    lib.opensolid_point2d_y.argtypes = [c_double]
    lib.opensolid_point2d_y.restype = c_void_p
    @staticmethod
    def y(py:float ) -> Point2d:
        return Point2d(lib.opensolid_point2d_y(py))
    def __del__(self) -> None:
        lib.opensolid_free(self.ptr)
    def __repr__(self) -> str:
        return "Point2d(" + str(self.x_coordinate()) + ", " + str(self.y_coordinate()) + ")"
class Range:
    def __init__(self, ptr:c_void_p ) -> None:
        self.ptr = ptr
    lib.opensolid_range_unsafe.argtypes = [c_double, c_double]
    lib.opensolid_range_unsafe.restype = c_void_p
    @staticmethod
    def unsafe(min:float , max:float ) -> Range:
        return Range(lib.opensolid_range_unsafe(min, max))
    lib.opensolid_range_constant.argtypes = [c_double]
    lib.opensolid_range_constant.restype = c_void_p
    @staticmethod
    def constant(value:float ) -> Range:
        return Range(lib.opensolid_range_constant(value))
    lib.opensolid_range_hull3.argtypes = [c_double, c_double, c_double]
    lib.opensolid_range_hull3.restype = c_void_p
    @staticmethod
    def hull3(a:float , b:float , c:float ) -> Range:
        return Range(lib.opensolid_range_hull3(a, b, c))
    lib.opensolid_range_min_value.argtypes = [c_void_p]
    lib.opensolid_range_min_value.restype = c_double
    def min_value(self) -> float:
        return lib.opensolid_range_min_value(self.ptr)
    lib.opensolid_range_max_value.argtypes = [c_void_p]
    lib.opensolid_range_max_value.restype = c_double
    def max_value(self) -> float:
        return lib.opensolid_range_max_value(self.ptr)
    lib.opensolid_range_midpoint.argtypes = [c_void_p]
    lib.opensolid_range_midpoint.restype = c_double
    def midpoint(self) -> float:
        return lib.opensolid_range_midpoint(self.ptr)
    lib.opensolid_range_width.argtypes = [c_void_p]
    lib.opensolid_range_width.restype = c_double
    def width(self) -> float:
        return lib.opensolid_range_width(self.ptr)
    lib.opensolid_range_squared.argtypes = [c_void_p]
    lib.opensolid_range_squared.restype = c_void_p
    def squared(self) -> Range:
        return Range(lib.opensolid_range_squared(self.ptr))
    lib.opensolid_range_includes.argtypes = [c_double, c_void_p]
    lib.opensolid_range_includes.restype = c_bool
    def includes(self, value:float ) -> bool:
        return lib.opensolid_range_includes(value, self.ptr)
    lib.opensolid_range_approximately_includes.argtypes = [c_double, c_double, c_void_p]
    lib.opensolid_range_approximately_includes.restype = c_bool
    def approximately_includes(self, value:float , tolerance:Optional[float] =None) -> bool:
        if tolerance is None and global_tolerance is None:
            raise Exception('Tolerance is not set')
        return lib.opensolid_range_approximately_includes(tolerance or global_tolerance, value, self.ptr)
    lib.opensolid_range_contains.argtypes = [c_void_p, c_void_p]
    lib.opensolid_range_contains.restype = c_bool
    def contains(self, range2:Range ) -> bool:
        return lib.opensolid_range_contains(range2.ptr, self.ptr)
    lib.opensolid_range_is_contained_in.argtypes = [c_void_p, c_void_p]
    lib.opensolid_range_is_contained_in.restype = c_bool
    def is_contained_in(self, range1:Range ) -> bool:
        return lib.opensolid_range_is_contained_in(range1.ptr, self.ptr)
    lib.opensolid_range_tolerant.argtypes = [c_double, c_void_p]
    lib.opensolid_range_tolerant.restype = c_void_p
    def tolerant(self, tolerance:Optional[float] =None) -> Range:
        if tolerance is None and global_tolerance is None:
            raise Exception('Tolerance is not set')
        return Range(lib.opensolid_range_tolerant(tolerance or global_tolerance, self.ptr))
    lib.opensolid_range_is_atomic.argtypes = [c_void_p]
    lib.opensolid_range_is_atomic.restype = c_bool
    def is_atomic(self) -> bool:
        return lib.opensolid_range_is_atomic(self.ptr)
    lib.opensolid_range_abs.argtypes = [c_void_p]
    lib.opensolid_range_abs.restype = c_void_p
    def abs(self) -> Range:
        return Range(lib.opensolid_range_abs(self.ptr))
    lib.opensolid_range_sqrt.argtypes = [c_void_p]
    lib.opensolid_range_sqrt.restype = c_void_p
    def sqrt(self) -> Range:
        return Range(lib.opensolid_range_sqrt(self.ptr))
    lib.opensolid_range_hypot2.argtypes = [c_void_p, c_void_p]
    lib.opensolid_range_hypot2.restype = c_void_p
    @staticmethod
    def hypot2(range1:Range , range2:Range ) -> Range:
        return Range(lib.opensolid_range_hypot2(range1.ptr, range2.ptr))
    lib.opensolid_range_hypot3.argtypes = [c_void_p, c_void_p, c_void_p]
    lib.opensolid_range_hypot3.restype = c_void_p
    @staticmethod
    def hypot3(range1:Range , range2:Range , range3:Range ) -> Range:
        return Range(lib.opensolid_range_hypot3(range1.ptr, range2.ptr, range3.ptr))
    lib.opensolid_range_aggregate2.argtypes = [c_void_p, c_void_p]
    lib.opensolid_range_aggregate2.restype = c_void_p
    @staticmethod
    def aggregate2(range1:Range , range2:Range ) -> Range:
        return Range(lib.opensolid_range_aggregate2(range1.ptr, range2.ptr))
    lib.opensolid_range_aggregate3.argtypes = [c_void_p, c_void_p, c_void_p]
    lib.opensolid_range_aggregate3.restype = c_void_p
    @staticmethod
    def aggregate3(range1:Range , range2:Range , range3:Range ) -> Range:
        return Range(lib.opensolid_range_aggregate3(range1.ptr, range2.ptr, range3.ptr))
    lib.opensolid_range_min.argtypes = [c_void_p, c_void_p]
    lib.opensolid_range_min.restype = c_void_p
    @staticmethod
    def min(range1:Range , range2:Range ) -> Range:
        return Range(lib.opensolid_range_min(range1.ptr, range2.ptr))
    lib.opensolid_range_max.argtypes = [c_void_p, c_void_p]
    lib.opensolid_range_max.restype = c_void_p
    @staticmethod
    def max(range1:Range , range2:Range ) -> Range:
        return Range(lib.opensolid_range_max(range1.ptr, range2.ptr))
    lib.opensolid_range_smaller.argtypes = [c_void_p, c_void_p]
    lib.opensolid_range_smaller.restype = c_void_p
    @staticmethod
    def smaller(first:Range , second:Range ) -> Range:
        return Range(lib.opensolid_range_smaller(first.ptr, second.ptr))
    lib.opensolid_range_larger.argtypes = [c_void_p, c_void_p]
    lib.opensolid_range_larger.restype = c_void_p
    @staticmethod
    def larger(first:Range , second:Range ) -> Range:
        return Range(lib.opensolid_range_larger(first.ptr, second.ptr))
    lib.opensolid_range_sin.argtypes = [c_void_p]
    lib.opensolid_range_sin.restype = c_void_p
    def sin(self) -> Range:
        return Range(lib.opensolid_range_sin(self.ptr))
    lib.opensolid_range_cos.argtypes = [c_void_p]
    lib.opensolid_range_cos.restype = c_void_p
    def cos(self) -> Range:
        return Range(lib.opensolid_range_cos(self.ptr))
    lib.opensolid_range_interpolate.argtypes = [c_void_p, c_double]
    lib.opensolid_range_interpolate.restype = c_double
    @staticmethod
    def interpolate(range:Range , t:float ) -> float:
        return lib.opensolid_range_interpolate(range.ptr, t)
    lib.opensolid_range_interpolation_parameter.argtypes = [c_void_p, c_double]
    lib.opensolid_range_interpolation_parameter.restype = c_double
    @staticmethod
    def interpolation_parameter(range:Range , value:float ) -> float:
        return lib.opensolid_range_interpolation_parameter(range.ptr, value)
    lib.opensolid_range_resolution.argtypes = [c_void_p]
    lib.opensolid_range_resolution.restype = c_double
    def resolution(self) -> float:
        return lib.opensolid_range_resolution(self.ptr)
    lib.opensolid_range_intersects.argtypes = [c_void_p, c_void_p]
    lib.opensolid_range_intersects.restype = c_bool
    def intersects(self, range1:Range ) -> bool:
        return lib.opensolid_range_intersects(range1.ptr, self.ptr)
    lib.opensolid_range_intersection.argtypes = [c_void_p, c_void_p]
    lib.opensolid_range_intersection.restype = c_void_p
    def intersection(self, range1:Range ) -> Optional[Range]:
        ret_val = lib.opensolid_range_intersection(range1.ptr, self.ptr)
        return Range(ret_val) if ret_val else None
    def __del__(self) -> None:
        lib.opensolid_free(self.ptr)
    def __repr__(self) -> str:
        return "Range(" + str(self.min_value()) + ", " + str(self.max_value()) + ")"
class Vector2d:
    def __init__(self, ptr:c_void_p ) -> None:
        self.ptr = ptr
    lib.opensolid_vector2d_zero.argtypes = []
    lib.opensolid_vector2d_zero.restype = c_void_p
    @staticmethod
    def zero() -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_zero())
    lib.opensolid_vector2d_x.argtypes = [c_double]
    lib.opensolid_vector2d_x.restype = c_void_p
    @staticmethod
    def x(vx:float ) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_x(vx))
    lib.opensolid_vector2d_y.argtypes = [c_double]
    lib.opensolid_vector2d_y.restype = c_void_p
    @staticmethod
    def y(vy:float ) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_y(vy))
    lib.opensolid_vector2d_xy.argtypes = [c_double, c_double]
    lib.opensolid_vector2d_xy.restype = c_void_p
    @staticmethod
    def xy(vx:float , vy:float ) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_xy(vx, vy))
    lib.opensolid_vector2d_meters.argtypes = [c_double, c_double]
    lib.opensolid_vector2d_meters.restype = c_void_p
    @staticmethod
    def meters(vx:float , vy:float ) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_meters(vx, vy))
    lib.opensolid_vector2d_square_meters.argtypes = [c_double, c_double]
    lib.opensolid_vector2d_square_meters.restype = c_void_p
    @staticmethod
    def square_meters(vx:float , vy:float ) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_square_meters(vx, vy))
    lib.opensolid_vector2d_polar.argtypes = [c_double, c_double]
    lib.opensolid_vector2d_polar.restype = c_void_p
    @staticmethod
    def polar(r:float , theta:float ) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_polar(r, theta))
    lib.opensolid_vector2d_x_component.argtypes = [c_void_p]
    lib.opensolid_vector2d_x_component.restype = c_double
    def x_component(self) -> float:
        return lib.opensolid_vector2d_x_component(self.ptr)
    lib.opensolid_vector2d_y_component.argtypes = [c_void_p]
    lib.opensolid_vector2d_y_component.restype = c_double
    def y_component(self) -> float:
        return lib.opensolid_vector2d_y_component(self.ptr)
    lib.opensolid_vector2d_midpoint.argtypes = [c_void_p, c_void_p]
    lib.opensolid_vector2d_midpoint.restype = c_void_p
    @staticmethod
    def midpoint(v1:Vector2d , v2:Vector2d ) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_midpoint(v1.ptr, v2.ptr))
    lib.opensolid_vector2d_interpolate_from.argtypes = [c_void_p, c_void_p, c_double]
    lib.opensolid_vector2d_interpolate_from.restype = c_void_p
    @staticmethod
    def interpolate_from(v1:Vector2d , v2:Vector2d , t:float ) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_interpolate_from(v1.ptr, v2.ptr, t))
    lib.opensolid_vector2d_magnitude.argtypes = [c_void_p]
    lib.opensolid_vector2d_magnitude.restype = c_double
    def magnitude(self) -> float:
        return lib.opensolid_vector2d_magnitude(self.ptr)
    lib.opensolid_vector2d_squared_magnitude.argtypes = [c_void_p]
    lib.opensolid_vector2d_squared_magnitude.restype = c_double
    def squared_magnitude(self) -> float:
        return lib.opensolid_vector2d_squared_magnitude(self.ptr)
    lib.opensolid_vector2d_angle.argtypes = [c_void_p]
    lib.opensolid_vector2d_angle.restype = c_double
    def angle(self) -> float:
        return lib.opensolid_vector2d_angle(self.ptr)
    lib.opensolid_vector2d_normalize.argtypes = [c_void_p]
    lib.opensolid_vector2d_normalize.restype = c_void_p
    def normalize(self) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_normalize(self.ptr))
    lib.opensolid_vector2d_rotate_right.argtypes = [c_void_p]
    lib.opensolid_vector2d_rotate_right.restype = c_void_p
    def rotate_right(self) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_rotate_right(self.ptr))
    lib.opensolid_vector2d_rotate_left.argtypes = [c_void_p]
    lib.opensolid_vector2d_rotate_left.restype = c_void_p
    def rotate_left(self) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_rotate_left(self.ptr))
    lib.opensolid_vector2d_place_in.argtypes = [c_void_p, c_void_p]
    lib.opensolid_vector2d_place_in.restype = c_void_p
    def place_in(self, frame:Frame2d ) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_place_in(frame.ptr, self.ptr))
    lib.opensolid_vector2d_relative_to.argtypes = [c_void_p, c_void_p]
    lib.opensolid_vector2d_relative_to.restype = c_void_p
    def relative_to(self, frame:Frame2d ) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_relative_to(frame.ptr, self.ptr))
    def __del__(self) -> None:
        lib.opensolid_free(self.ptr)
    def __repr__(self) -> str:
        return "Vector2d(" + str(self.x_component()) + ", " + str(self.y_component()) + ")"
