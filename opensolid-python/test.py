from opensolid import Point2d, Frame2d, Axis2d, Direction2d, Vector2d, Range, Tolerance, IsZero
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

rng = Range.unsafe(0, 5)
print(rng.intersection(Range.unsafe(6, 7)))
print(rng.intersection(Range.unsafe(3, 7)))

# Testing tolerance
with Tolerance(1):
  print(rng.tolerant())
  with Tolerance(2):
    print(rng.tolerant())
  print(rng.tolerant())
try:
  print(rng.tolerant())
except Exception as err:
  print(err)  # Tolerance is not set

# Testing result
print(Vector2d.x(5).direction(tolerance=0.1))
try:
  print(Vector2d.x(0).direction(tolerance=0.1))
except IsZero:
  print("Zero vector doesn't have direction")
