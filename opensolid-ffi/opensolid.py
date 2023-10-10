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
class Point2d:
    def __init__(self, ptr:c_void_p ):
        self.ptr = ptr
    lib.opensolid_point2d_xy.argtypes = [c_double, c_double]
    lib.opensolid_point2d_xy.restype = c_void_p
    @staticmethod
    def xy(x:float , y:float ) -> "Point2d":
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
        return "Point2d(" + str(self.x_coordinate) + ", " + str(self.y_coordinate) + ")"
