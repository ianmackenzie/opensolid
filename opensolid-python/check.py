from opensolid import Curve1f, Point2f

p1 = Point2f(1, 2)
p2 = Point2f(3, 4)
print(p1.distance_to(p2))
c1 = Curve1f.t()
print(c1.evaluate(0.5))
c2 = c1.squared()
print(c2.evaluate(0.5))
