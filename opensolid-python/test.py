from opensolid import Point2d, Frame2d, Axis2d, Direction2d
from math import pi

point = Point2d.xy(0, 1)
frame = Frame2d.from_x_axis(
  Axis2d.through(Point2d.origin(), Direction2d.from_angle(pi / 4))
)
print(frame)
print(point)
point = point.relative_to(frame)
print(point)
print(point.place_in(frame))
