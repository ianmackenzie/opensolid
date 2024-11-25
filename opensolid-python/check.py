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
r3 = Range.from_endpoints(Length(1), Length(5))
r4 = Range.from_endpoints(Length(4), Length(10))
r = r3.intersection(r4)
if r is None:
    print("No intersection!")
else:
    (a, b) = r.endpoints()
    print(a.value)
    print(b.value)

v = Vector2d.xy(1,2)
with Tolerance(1e-12):
    print(v.direction())
