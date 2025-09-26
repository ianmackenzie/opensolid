from opensolid import Curve
import math


def curve_power(curve: Curve, n: int) -> Curve:
    if n == 0:
        return Curve.constant(1.0)
    elif n == 1:
        return curve
    else:
        return curve * curve_power(curve, n - 1)


def nth_derivative(curve: Curve, n: int) -> Curve:
    if n == 0:
        return curve
    else:
        return nth_derivative(curve.derivative, n - 1)


def coeff(n: int) -> float:
    t = Curve.t
    f = curve_power(t, n) * curve_power(1 - t, n)
    d = nth_derivative(f, n)
    k = 1 / d.evaluate(0)
    return 1.0 / (k * f.evaluate(0.5))


if __name__ == "__main__":
    for n in range(1, 9):
        print(f"{n}: {coeff(n)} ({math.factorial(n) * 2 ** (2 * n)})")
