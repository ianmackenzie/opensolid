= Curve derivatives

Given the derivative of a 2D curve $((dif x)/(dif t), (dif y)/(dif t))$,
find $(dif y)/(dif x)$:

$ dif x = (dif x)/(dif t) dif t $
$ dif y = (dif y)/(dif t) dif t $
$ (dif y)/(dif x) = ((dif y)/(dif t))/((dif x)/(dif t)) $

Differentiate to find $(dif^2 y)/(dif x^2)$:

$
  (dif^2 y)/(dif x^2) & = dif/(dif x) ((dif y)/(dif x)) \
                      & = dif/(dif t) ((dif y)/(dif x)) (dif t)/(dif x) \
                      & = dif/(dif t) (((dif y)/(dif t))/((dif x)/(dif t))) 1/((dif x)/(dif t)) \
                      & = ((dif x)/(dif t) (dif^2 y)/(dif t^2) - (dif y)/(dif t) (dif^2 x)/(dif t^2))
                        / ((dif x)/(dif t))^2
                        1/((dif x)/(dif t)) \
                      & = ((dif x)/(dif t) (dif^2 y)/(dif t^2) - (dif y)/(dif t) (dif^2 x)/(dif t^2))
                        / ((dif x)/(dif t))^3
$

Differentiate again to find $(dif^3 y)/(dif x^3)$.
First, differentiate the numerator of $(dif^2 y)/(dif x^2)$ with respect to $t$
(the $(dif^2 x)/(dif t^2) (dif^2 y)/(dif t^2)$ terms cancel):

$
  & dif/(dif t) ((dif x)/(dif t) (dif^2 y)/(dif t^2) - (dif y)/(dif t) (dif^2 x)/(dif t^2)) \
  & = (dif^2 x)/(dif t^2) (dif^2 y)/(dif t^2) + (dif x)/(dif t) (dif^3 y)/(dif t^3)
    - (dif^2 y)/(dif t^2) (dif^2 x)/(dif t^2) - (dif y)/(dif t) (dif^3 x)/(dif t^3) \
  & = (dif x)/(dif t) (dif^3 y)/(dif t^3) - (dif y)/(dif t) (dif^3 x)/(dif t^3)
$

Then apply the chain rule and quotient rule as before:

$
  (dif^3 y)/(dif x^3) & = dif/(dif x) ((dif^2 y)/(dif x^2)) \
                      & = dif/(dif t) ((dif^2 y)/(dif x^2)) (dif t)/(dif x) \
                      & = dif/(dif t) (((dif x)/(dif t) (dif^2 y)/(dif t^2) - (dif y)/(dif t) (dif^2 x)/(dif t^2))
                          / ((dif x)/(dif t))^3)
                        1/((dif x)/(dif t)) \
                      & = (((dif x)/(dif t))^3 ((dif x)/(dif t) (dif^3 y)/(dif t^3) - (dif y)/(dif t) (dif^3 x)/(dif t^3))
                        - 3 ((dif x)/(dif t))^2 (dif^2 x)/(dif t^2)
                        ((dif x)/(dif t) (dif^2 y)/(dif t^2) - (dif y)/(dif t) (dif^2 x)/(dif t^2)))
                        / ((dif x)/(dif t))^6
                        1/((dif x)/(dif t)) \
                      & = ((dif x)/(dif t) ((dif x)/(dif t) (dif^3 y)/(dif t^3) - (dif y)/(dif t) (dif^3 x)/(dif t^3))
                        - 3 (dif^2 x)/(dif t^2)
                        ((dif x)/(dif t) (dif^2 y)/(dif t^2) - (dif y)/(dif t) (dif^2 x)/(dif t^2)))
                        / ((dif x)/(dif t))^5 \
                      & = (((dif x)/(dif t))^2 (dif^3 y)/(dif t^3)
                        - (dif x)/(dif t) (dif y)/(dif t) (dif^3 x)/(dif t^3)
                        - 3 (dif x)/(dif t) (dif^2 x)/(dif t^2) (dif^2 y)/(dif t^2)
                        + 3 (dif y)/(dif t) ((dif^2 x)/(dif t^2))^2)
                        / ((dif x)/(dif t))^5
$
