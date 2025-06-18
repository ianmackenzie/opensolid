from opensolid import (
    Angle,
    Curve,
    Point2d,
    LengthBounds,
    Tolerance,
    Vector2d,
    Color,
    Bounds2d,
    Drawing2d,
    Length,
)

p1 = Point2d.meters(1, 2)
p2 = Point2d.meters(3, 4)
print(p1.distance_to(p2))
print(Curve.t.evaluate(0.5))
print(Curve.t.squared().evaluate(0.5))

r1 = LengthBounds(Length.meters(1), Length.meters(5))
r2 = LengthBounds(Length.meters(3), Length.meters(7))
r3 = LengthBounds(Length.meters(1), Length.meters(5))
r4 = LengthBounds(Length.meters(4), Length.meters(10))
r = r3.intersection(r4)
if r is None:
    print("No intersection!")
else:
    print("Intersection:", r)

v = Vector2d(1, 2)
with Tolerance(1e-9):
    print(v.direction().components)

theta = Angle.two_pi * Curve.t
c = theta.cos()
with Tolerance(1e-9):
    roots = [zero.location for zero in c.zeros()]
    print(roots)

color1 = Color.from_hex("#555555")
print(color1.rgb_float_components)
color2 = Color.rgb_int(225, 225, 225)
print(color2.to_hex())

bounds = Bounds2d.from_corners(Point2d.origin, Point2d.centimeters(30, 20))
print(bounds.x_coordinate)
vertices = [
    Point2d.centimeters(5, 5),
    Point2d.centimeters(25, 5),
    Point2d.centimeters(25, 15),
]
triangle = Drawing2d.polygon(vertices)
print(triangle.to_svg(bounds))
