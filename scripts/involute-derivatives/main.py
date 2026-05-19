from sympy import Expr, cos, diff, sin, symbols
from tabulate import tabulate

vx, vy, t = symbols("vx vy t")


def print_table(label, expression):
    d = expression
    rows = []
    for i in range(11):
        with_t = d.coeff(t, 1)
        without_t = (d - t * with_t).simplify()
        assert isinstance(with_t, Expr)
        assert isinstance(without_t, Expr)
        vx_term = without_t.coeff(vx)
        vy_term = without_t.coeff(vy)
        vx_t_term = with_t.coeff(vx)
        vy_t_term = with_t.coeff(vy)
        rows.append([i, vx_term, vy_term, vx_t_term, vy_t_term])
        d = diff(d, t)

    print(label + ":")
    print(tabulate(rows, headers=["i", "vx", "vy", "vx * t", "vy * t"]))


v = vx * cos(t) + vy * sin(t) + t * vx * sin(t) - t * vy * cos(t)
print_table("v", v)
