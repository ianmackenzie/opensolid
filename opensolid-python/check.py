from opensolid import Curve1d, Point2d, Range, Length, Tolerance, Vector2d

p1 = Point2d.xy(1, 2)
p2 = Point2d.xy(3, 4)
print(p1.distance_to(p2))
c1 = Curve1d.t()
print(c1.evaluate(0.5))
c2 = c1.squared()
print(c2.evaluate(0.5))

r1 = Range.from_endpoints(1, 5)
r2 = Range.from_endpoints(3, 7)
r3 = Range.from_endpoints(Length.meters(1), Length.meters(5))
r4 = Range.from_endpoints(Length.meters(4), Length.meters(10))
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

c = Curve1d.sin(Curve1d.t())
with Tolerance(1e-9):
    print([root.value() for root in c.zeros()])
