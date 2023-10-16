from __future__ import annotations
import platform
from ctypes import *
global lib
system = platform.system()
if system == "Darwin":
    lib = cdll.LoadLibrary("libopensolid-ffi.dylib")
elif system == "Linux":
    lib = cdll.LoadLibrary("libopensolid-ffi.so")
else:
    raise Exception("System " + system + " is not supported")
lib.opensolid_free.argtypes = [c_void_p]
class Axis2d:
    def __init__(self, ptr:c_void_p ):
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
    def __del__(self):
        lib.opensolid_free(self.ptr)
    def __repr__(self) -> str:
        return "Axis2d(" + str(self.origin_point()) + ", " + str(self.direction()) + ")"
class Direction2d:
    def __init__(self, ptr:c_void_p ):
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
    def __del__(self):
        lib.opensolid_free(self.ptr)
    def __repr__(self) -> str:
        return "Direction2d(" + str(self.x_component()) + ", " + str(self.y_component()) + ")"
class Frame2d:
    def __init__(self, ptr:c_void_p ):
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
    def __del__(self):
        lib.opensolid_free(self.ptr)
    def __repr__(self) -> str:
        return "Frame2d(" + str(self.origin_point()) + ", " + str(self.x_direction()) + ", " + str(self.y_direction()) + ")"
class Point2d:
    def __init__(self, ptr:c_void_p ):
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
    def __del__(self):
        lib.opensolid_free(self.ptr)
    def __repr__(self) -> str:
        return "Point2d(" + str(self.x_coordinate()) + ", " + str(self.y_coordinate()) + ")"
class Vector2d:
    def __init__(self, ptr:c_void_p ):
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
    lib.opensolid_vector2d_from.argtypes = [c_void_p, c_void_p]
    lib.opensolid_vector2d_from.restype = c_void_p
    @staticmethod
    def from_(p1:Point2d , p2:Point2d ) -> Vector2d:
        return Vector2d(lib.opensolid_vector2d_from(p1.ptr, p2.ptr))
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
    def __del__(self):
        lib.opensolid_free(self.ptr)
    def __repr__(self) -> str:
        return "Vector2d(" + str(self.x_component()) + ", " + str(self.y_component()) + ")"
