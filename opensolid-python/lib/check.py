from opensolid import Angle, Curve, Point2d, LengthRange, Tolerance, Vector2d, Color, Bounds2d, Drawing2d

p1 = Point2d.meters(1, 2)
p2 = Point2d.meters(3, 4)
print(p1.distance_to(p2))
print(Curve.t.evaluate(0.5))
print(Curve.t.squared().evaluate(0.5))

r1 = LengthRange.meters(1, 5)
r2 = LengthRange.meters(3, 7)
r3 = LengthRange.meters(1, 5)
r4 = LengthRange.meters(4, 10)
r = r3.intersection(r4)
if r is None:
    print("No intersection!")
else:
    print("Intersection:", r)

v = Vector2d.xy(1,2)
with Tolerance(1e-9):
    print(v.direction().components())

c = (Curve.t * Angle.turns(1)).cos()
with Tolerance(1e-9):
    roots = [zero.location() for zero in c.zeros()]
    print(roots)

color1 = Color.from_hex("#555555")
print(color1.components())
color2 = Color.rgb_255(225, 225, 225)
print(color2.to_hex())

bounds = Bounds2d.from_corners(Point2d.origin, Point2d.centimeters(30, 20))
print(bounds.x_coordinate())
vertices = [Point2d.centimeters(5, 5), Point2d.centimeters(25, 5), Point2d.centimeters(25, 15)]
triangle = Drawing2d.polygon([], vertices)
print(Drawing2d.to_svg(bounds, [triangle]))
