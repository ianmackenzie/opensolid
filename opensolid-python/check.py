from opensolid import Angle, Curve1d, Point2d, Range, Tolerance, Vector2d

p1 = Point2d.meters(1, 2)
p2 = Point2d.meters(3, 4)
print(p1.distance_to(p2).in_centimeters())
print(Curve1d.t.evaluate(0.5))
print(Curve1d.t.squared().evaluate(0.5))

r1 = Range.meters(1, 5)
r2 = Range.meters(3, 7)
r3 = Range.meters(1, 5)
r4 = Range.meters(4, 10)
r = r3.intersection(r4)
if r is None:
    print("No intersection!")
else:
    (a, b) = r.endpoints()
    print(a.in_meters())
    print(b.in_meters())

v = Vector2d.xy(1,2)
with Tolerance(1e-9):
    print(v.direction().components())

c = Curve1d.cos(Curve1d.t * Angle.turns(1))
with Tolerance(1e-9):
    print([root.value() for root in c.zeros()])
