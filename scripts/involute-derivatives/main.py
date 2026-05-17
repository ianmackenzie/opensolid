from sympy import Expr, cos, diff, sin, symbols
from tabulate import tabulate

x0, y0, rx, ry, t = symbols("x0 y0 rx ry t")


def print_table(label, expression):
    d = expression
    rows = []
    for i in range(11):
        with_t = d.coeff(t, 1)
        without_t = (d - t * with_t).simplify()
        assert isinstance(with_t, Expr)
        assert isinstance(without_t, Expr)
        rx_term = without_t.coeff(rx)
        ry_term = without_t.coeff(ry)
        rx_t_term = with_t.coeff(rx)
        ry_t_term = with_t.coeff(ry)
        rows.append([i, rx_term, ry_term, rx_t_term, ry_t_term])
        d = diff(d, t)

    print(label + ":")
    print(tabulate(rows, headers=["i", "rx", "ry", "rx * t", "ry * t"]))


x = x0 + rx * cos(t) - ry * sin(t) + t * ry * cos(t) + t * rx * sin(t)
y = y0 + ry * cos(t) + rx * sin(t) - t * rx * cos(t) + t * ry * sin(t)
print_table("x", x)
print_table("y", y)
