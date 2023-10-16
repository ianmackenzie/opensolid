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
