from opensolid import (
    Angle,
    Bounds2D,
    Color,
    Curve,
    Length,
    LengthInterval,
    Point2D,
    Svg,
    Tolerance,
    Triangle2D,
    Vector2D,
)

p1 = Point2D.meters(1, 2)
p2 = Point2D.meters(3, 4)
print(p1.distance_to(p2))
print(Curve.t.evaluate(0.5))
print(Curve.t.squared().evaluate(0.5))

r1 = LengthInterval(Length.meters(1), Length.meters(5))
r2 = LengthInterval(Length.meters(3), Length.meters(7))
r3 = LengthInterval(Length.meters(1), Length.meters(5))
r4 = LengthInterval(Length.meters(4), Length.meters(10))
r = r3.intersection(r4)
if r is None:
    print("No intersection!")
else:
    print("Intersection:", r)

v = Vector2D(1, 2)
with Tolerance(1e-9):
    print(v.direction().components)

theta = Angle.two_pi * Curve.t
c = theta.cos()
with Tolerance(1e-9):
    roots = [zero.location for zero in c.zeros()]
    print(roots)

color1 = Color.hex("#555555")
print(color1.to_rgb1())
color2 = Color.rgb255(225, 225, 225)
print(color2.to_hex())

bounds = Bounds2D.from_corners(Point2D.origin, Point2D.centimeters(30, 20))
print(bounds.x_coordinate)
triangle = Triangle2D(
    Point2D.centimeters(5, 5),
    Point2D.centimeters(25, 5),
    Point2D.centimeters(25, 15),
)
triangle = Svg.triangle(triangle)
print(triangle.to_text(bounds))
