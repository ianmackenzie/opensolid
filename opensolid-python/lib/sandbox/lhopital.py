from opensolid import Curve, Tolerance

with Tolerance(1e-9):
    t = Curve.t
    x = 1 - Curve.cos(t)
    y = x.squared()
    x1 = x.derivative
    x2 = x1.derivative
    x3 = x2.derivative
    x4 = x3.derivative
    x5 = x4.derivative
    y1 = y.derivative
    y2 = y1.derivative
    y3 = y2.derivative
    y4 = y3.derivative
    y5 = y4.derivative
    n = y2 * x1 - y1 * x2
    d = x1.cubed()
    n1 = n.derivative
    n2 = n1.derivative
    n3 = n2.derivative
    d1 = d.derivative
    d2 = d1.derivative
    d3 = d2.derivative

    t0 = 0.3
    print(n3.evaluate(t0))
    print((y5 * x1 + 2 * y4 * x2 - 2 * y2 * x4 - y1 * x5).evaluate(t0))
    print(d3.evaluate(t0))
    print((6 * x2.cubed() + 18 * x1 * x2 * x3 + 3 * x1.squared() * x4).evaluate(t0))
