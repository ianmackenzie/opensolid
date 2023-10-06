from ctypes import *
import platform

global lib

match platform.system():
    case "Darwin":
        lib = cdll.LoadLibrary("libopensolid-ffi.dylib")
    case "Linux":
        lib = cdll.LoadLibrary("libopensolid-ffi.so")
    case os:
        raise Exception(f"System {os} is not supported")

class Point2d:
    def __init__(self, ptr: c_void_p):
        self.ptr = ptr

    lib.opensolid_point2d_xy.argtypes = [c_double, c_double]
    lib.opensolid_point2d_xy.restype = c_void_p
    @staticmethod
    def xy(x: float, y: float) -> 'Point2d':
        return Point2d(lib.opensolid_point2d_xy(x, y))

    lib.opensolid_point2d_x_coordinate.argtypes = [c_void_p]
    lib.opensolid_point2d_x_coordinate.restype = c_double
    @property
    def x_coordinate(self) -> float:
        return lib.opensolid_point2d_x_coordinate(self.ptr)

    lib.opensolid_point2d_y_coordinate.argtypes = [c_void_p]
    lib.opensolid_point2d_y_coordinate.restype = c_double
    @property
    def y_coordinate(self) -> float:
        return lib.opensolid_point2d_y_coordinate(self.ptr)

    lib.opensolid_free.argtypes = [c_void_p]
    def __del__(self):
        lib.opensolid_free(self.ptr)

    def __repr__(self):
        return f"Point2d({self.x_coordinate}, {self.y_coordinate})"
