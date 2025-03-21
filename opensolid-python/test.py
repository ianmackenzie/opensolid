from opensolid import Point2d, Frame2d, Axis2d, Direction2d, Vector2d, Range, Tolerance
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

# Testing returning a tuple
print(rng.bisect())

# Testing tolerance
with Tolerance(1):
  print(Vector2d.x(1.5).direction())
  with Tolerance(2):
    try:
      print(Vector2d.x(1.5).direction())
    except Vector2d.IsZero:
      print("Zero vector doesn't have direction")
  print(Vector2d.x(1.5).direction())
try:
  print(Vector2d.x(1.5).direction())
except Exception as err:
  print(err)  # Tolerance is not set

# Testing result of a tuple
print(Vector2d.x(5).magnitude_and_direction(tolerance=0.1))
try:
  print(Vector2d.x(0).magnitude_and_direction(tolerance=0.1))
except Vector2d.IsZero:
  print("Zero vector doesn't have direction")

# Testing result
print(Vector2d.x(5).direction(tolerance=0.1))
try:
  print(Vector2d.x(0).direction(tolerance=0.1))
except Vector2d.IsZero:
  print("Zero vector doesn't have direction")
