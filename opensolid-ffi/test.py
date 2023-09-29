from ctypes import *

lib = None


def load():
    global lib
    lib = cdll.LoadLibrary("libopensolid-ffi.dylib")
    lib.xy.argtypes = (c_double, c_double)
    lib.xy.restype = Point2d
    lib.xCoordinate.argtypes = (Point2d,)
    lib.xCoordinate.restype = c_double
    lib.yCoordinate.argtypes = (Point2d,)
    lib.yCoordinate.restype = c_double
    lib.freePoint.argtypes = (Point2d,)
    lib.example_init()


def unload():
    global lib
    lib.example_exit()

    def raise_exception(*args, **kwargs):
        raise Exception("The opensolid library has been unloaded")
    def no_op(*args, **kwargs):
        pass
    # Replace all methods on the lib object
    class NoLib: pass
    for key, value in lib.__dict__.items():
        if key == "freePoint":
            setattr(NoLib, key, no_op)
        elif callable(value):
            setattr(NoLib, key, raise_exception)
    lib = NoLib()


class Point2d(c_void_p):
    @staticmethod
    def xy(x: float, y: float) -> 'Point2d':
        return lib.xy(x, y)

    @property
    def x(self) -> float:
        return lib.xCoordinate(self)

    @property
    def y(self) -> float:
        return lib.yCoordinate(self)

    def __del__(self):
        lib.freePoint(self)

    def __repr__(self):
        return f"Point2d({self.x}, {self.y})"


load()

point = Point2d.xy(5, 6)
print(point)

unload()
