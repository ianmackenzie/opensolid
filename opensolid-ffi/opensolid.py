from ctypes import *

lib = cdll.LoadLibrary("libopensolid-ffi.so")

lib.xy.argtypes = (c_double, c_double)
lib.xy.restype = c_void_p

lib.xCoordinate.argtypes = (c_void_p,)
lib.xCoordinate.restype = c_double

lib.yCoordinate.argtypes = (c_void_p,)
lib.yCoordinate.restype = c_double

lib.freePoint.argtypes = (c_void_p,)

class Point2d:
    def __init__(self, ptr: c_void_p):
        self.ptr = ptr

    @staticmethod
    def xy(x: float, y: float) -> 'Point2d':
        return Point2d(lib.xy(x, y))

    @property
    def x(self) -> float:
        return lib.xCoordinate(self.ptr)

    @property
    def y(self) -> float:
        return lib.yCoordinate(self.ptr)

    def __del__(self):
        lib.freePoint(self.ptr)

    def __repr__(self):
        return f"Point2d({self.x}, {self.y})"
