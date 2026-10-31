= Surface derivatives

Consider a parametric surface

$ arrow(f)(u, v) = vec(x(u, v), y(u, v), z(u, v)) $

and assume that the projected partial derivatives $(x_u, x_v)$ and $(y_u, y_v)$ are everywhere linearly independent,
meaning that the surface can be considered as function $z(x, y)$.

We wish to compute $z_x$ and $z_y$ in terms of $x_u$, $x_v$, $y_u$, $y_v$, $z_u$ and $z_v$.
Start with expressions for $dif x$, $dif y$ and $dif z$:

$ dif z & = z_u dif u + z_v dif v $ <dz>
$ dif x & = x_u dif u + x_v dif v $ <dx>
$ dif y & = y_u dif u + y_v dif v $ <dy>

Solve for $dif u$ and $dif v$ in terms of $dif x$ and $dif y$:

$
  vec(dif x, dif y) & = mat(x_u, x_v; y_u, y_v) vec(dif u, dif v) \
  vec(dif u, dif v) & = 1 / (x_u y_v - x_v y_u) mat(y_v, -x_v; -y_u, x_u) vec(dif x, dif y)
$

Note that $x_u y_v - x_v y_u$ is the 2D cross product $D_"uv"$ of $(x_u, x_v)$ and $(y_u, y_v)$,
and so will be non-zero based on the assumption that those two vectors are linearly independent. So

$ dif u = (y_v dif x - x_v dif y) / D_"uv" $ <du>

$ dif v = (x_u dif y - y_u dif x) / D_"uv" $ <dv>

To compute $z_x$, substitute @du and @dv into @dz and @dx and divide, setting $dif y = 0$:

$
  z_x & = (dif z) / (dif x) \
      & = (z_u dif u + z_v dif v) / (x_u dif u + x_v dif v) \
      & = (z_u ((y_v dif x) / D_"uv") + z_v ((-y_u dif x) / D_"uv")) /
        (x_u ((y_v dif x) / D_"uv") + x_v ((-y_u dif x) / D_"uv")) \
      & = (z_u y_v - z_v y_u) / (x_u y_v - x_v y_u)
$

Similarly, to compute $z_y$, substitute @du and @dv into @dz and @dy and divide, setting $dif x = 0$:

$
  z_y & = (dif z) / (dif y) \
      & = (z_u dif u + z_v dif v) / (y_u dif u + y_v dif v) \
      & = (z_u ((-x_v dif y) / D_"uv") + z_v ((x_u dif y) / D_"uv")) /
        (y_u ((-x_v dif y) / D_"uv") + y_v ((x_u dif y) / D_"uv")) \
      & = (z_v x_u - z_u x_v) / (x_u y_v - x_v y_u)
$

To find the second partial derivatives $z_"xx"$, $z_"xy"$ and $z_"yy"$,
use @du and @dv to find the partial derivatives $u_x$, $u_y$, $v_x$ and $v_y$:

$ u_x & = y_v / D_"uv" $
$ u_y & = -x_v / D_"uv" $
$ v_x & = -y_u / D_"uv" $
$ v_y & = x_u / D_"uv" $

Then,

$
  z_"xx" & = (partial z_x) / (partial x) \
         & = (partial z_x) / (partial u) u_x + (partial z_x) / (partial v) v_x \
         & = partial / (partial u) ((z_u y_v - z_v y_u) / (x_u y_v - x_v y_u)) (y_v / (x_u y_v - x_v y_u))
           + partial / (partial v) ((z_u y_v - z_v y_u) / (x_u y_v - x_v y_u)) (-y_u / (x_u y_v - x_v y_u)) \
         & = (y_v partial / (partial u) (z_u y_v - z_v y_u) - y_u partial / (partial v) (z_u y_v - z_v y_u))
           / D_"uv"^2 \
         & quad - ((z_u y_v - z_v y_u)(y_v (partial D_"uv") / (partial u) - y_u (partial D_"uv") / (partial v)))
           / D_"uv"^3
$ <zxx-quotient>

using the quotient rule. Expanding the derivatives in the first numerator:

$
  & y_v partial / (partial u) (z_u y_v - z_v y_u) - y_u partial / (partial v) (z_u y_v - z_v y_u) \
  & = y_v (z_"uu" y_v + z_u y_"uv" - z_"uv" y_u - z_v y_"uu")
    - y_u (z_"uv" y_v + z_u y_"vv" - z_"vv" y_u - z_v y_"uv") \
  & = y_v^2 z_"uu" - 2 y_u y_v z_"uv" + y_u^2 z_"vv"
    + (y_v y_"uv" - y_u y_"vv") z_u - (y_v y_"uu" - y_u y_"uv") z_v
$

Since $D_"uv" = x_u y_v - x_v y_u$ has the same form as $z_u y_v - z_v y_u$ but with $x$ in place of $z$,
the second numerator similarly expands to

$
  & y_v (partial D_"uv") / (partial u) - y_u (partial D_"uv") / (partial v) \
  & = y_v^2 x_"uu" - 2 y_u y_v x_"uv" + y_u^2 x_"vv"
    + (y_v y_"uv" - y_u y_"vv") x_u - (y_v y_"uu" - y_u y_"uv") x_v
$

Substituting these into @zxx-quotient, multiplying through by $D_"uv"^3$
and grouping the terms involving $y_v y_"uv" - y_u y_"vv"$ and $y_v y_"uu" - y_u y_"uv"$:

$
  D_"uv"^3 z_"xx" & = D_"uv" (y_v^2 z_"uu" - 2 y_u y_v z_"uv" + y_u^2 z_"vv") \
                  & quad - (z_u y_v - z_v y_u)(y_v^2 x_"uu" - 2 y_u y_v x_"uv" + y_u^2 x_"vv") \
                  & quad + (D_"uv" z_u - (z_u y_v - z_v y_u) x_u)(y_v y_"uv" - y_u y_"vv") \
                  & quad - (D_"uv" z_v - (z_u y_v - z_v y_u) x_v)(y_v y_"uu" - y_u y_"uv")
$

Expanding $D_"uv"$, the first-derivative factors in the last two terms simplify to

$
  D_"uv" z_u - (z_u y_v - z_v y_u) x_u & = (x_u y_v - x_v y_u) z_u - (z_u y_v - z_v y_u) x_u
                                         = y_u (x_u z_v - x_v z_u) \
  D_"uv" z_v - (z_u y_v - z_v y_u) x_v & = (x_u y_v - x_v y_u) z_v - (z_u y_v - z_v y_u) x_v
                                         = y_v (x_u z_v - x_v z_u)
$

so those two terms combine into

$
  & (x_u z_v - x_v z_u)(y_u (y_v y_"uv" - y_u y_"vv") - y_v (y_v y_"uu" - y_u y_"uv")) \
  & = -(x_u z_v - x_v z_u)(y_v^2 y_"uu" - 2 y_u y_v y_"uv" + y_u^2 y_"vv")
$

giving

$
  D_"uv"^3 z_"xx" & = D_"uv" (y_v^2 z_"uu" - 2 y_u y_v z_"uv" + y_u^2 z_"vv") \
                  & quad - (z_u y_v - z_v y_u)(y_v^2 x_"uu" - 2 y_u y_v x_"uv" + y_u^2 x_"vv") \
                  & quad - (x_u z_v - x_v z_u)(y_v^2 y_"uu" - 2 y_u y_v y_"uv" + y_u^2 y_"vv")
$

Finally, since $z_u y_v - z_v y_u = D_"uv" z_x$ and $x_u z_v - x_v z_u = D_"uv" z_y$,
this can be written more compactly as

$
  D_"uv"^2 z_"xx" & = (y_v^2 z_"uu" - 2 y_u y_v z_"uv" + y_u^2 z_"vv") \
                  & quad - z_x (y_v^2 x_"uu" - 2 y_u y_v x_"uv" + y_u^2 x_"vv") \
                  & quad - z_y (y_v^2 y_"uu" - 2 y_u y_v y_"uv" + y_u^2 y_"vv")
$

However, when looking at the _relative_ curvature between two surfaces,
we only care about the case where $z_x$ and $z_y$ are zero:
if either one is not zero, then we have a crossing intersection, not a tangent one,
and we don't need to consider curvature. In that case, $z_"xx"$ simplifies to

$
  z_"xx" & = (y_v^2 z_"uu" - 2 y_u y_v z_"uv" + y_u^2 z_"vv") / D_"uv"^2 \
         & = (y_v^2 z_"uu" - 2 y_u y_v z_"uv" + y_u^2 z_"vv") / (x_u y_v - x_v y_u)^2
$ <z_xx>

Similarly,

$
  z_"yy" & = (x_v^2 z_"uu" - 2 x_u x_v z_"uv" + x_u^2 z_"vv") / D_"uv"^2 \
         & =(x_v^2 z_"uu" - 2 x_u x_v z_"uv" + x_u^2 z_"vv") / (x_u y_v - x_v y_u)^2
$ <z_yy>
