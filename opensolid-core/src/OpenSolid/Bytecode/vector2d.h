#pragma once

#include "bounds.h"

template <class S> struct Vector2d {
  S x;
  S y;

  inline Vector2d() :
      x{},
      y{} {
  }

  inline Vector2d(S x, S y) :
      x(x),
      y(y) {
  }

  static Vector2d<Bounds>
  invalid();

  static Vector2d<Bounds>
  hull2(Vector2d<double> v1, Vector2d<double> v2);

  static Vector2d<Bounds>
  hull3(Vector2d<double> v1, Vector2d<double> v2, Vector2d<double> v3);

  static Vector2d<Bounds>
  hull4(Vector2d<double> v1, Vector2d<double> v2, Vector2d<double> v3, Vector2d<double> v4);

  static Vector2d<Bounds>
  hull5(
    Vector2d<double> v1,
    Vector2d<double> v2,
    Vector2d<double> v3,
    Vector2d<double> v4,
    Vector2d<double> v5
  );

  static Vector2d<Bounds>
  hull6(
    Vector2d<double> v1,
    Vector2d<double> v2,
    Vector2d<double> v3,
    Vector2d<double> v4,
    Vector2d<double> v5,
    Vector2d<double> v6
  );

  template <class S2>
  inline auto
  dot(Vector2d<S2> other) const {
    return x * other.x + y * other.y;
  }

  template <class S2>
  inline auto
  cross(Vector2d<S2> other) const {
    return x * other.y - y * other.x;
  }

  S
  squaredMagnitude() const;

  S
  magnitude() const;
};

template <>
inline Vector2d<Bounds>
Vector2d<Bounds>::invalid() {
  return Vector2d(Bounds::invalid(), Bounds::invalid());
}

template <>
inline Vector2d<Bounds>
Vector2d<Bounds>::hull2(Vector2d<double> v1, Vector2d<double> v2) {
  return Vector2d(Bounds::hull2(v1.x, v2.x), Bounds::hull2(v1.y, v2.y));
}

template <>
inline Vector2d<Bounds>
Vector2d<Bounds>::hull3(Vector2d<double> v1, Vector2d<double> v2, Vector2d<double> v3) {
  return Vector2d(Bounds::hull3(v1.x, v2.x, v3.x), Bounds::hull3(v1.y, v2.y, v3.y));
}

template <>
inline Vector2d<Bounds>
Vector2d<Bounds>::hull4(
  Vector2d<double> v1,
  Vector2d<double> v2,
  Vector2d<double> v3,
  Vector2d<double> v4
) {
  return Vector2d(Bounds::hull4(v1.x, v2.x, v3.x, v4.x), Bounds::hull4(v1.y, v2.y, v3.y, v4.y));
}

template <>
inline Vector2d<Bounds>
Vector2d<Bounds>::hull5(
  Vector2d<double> v1,
  Vector2d<double> v2,
  Vector2d<double> v3,
  Vector2d<double> v4,
  Vector2d<double> v5
) {
  return Vector2d(
    Bounds::hull5(v1.x, v2.x, v3.x, v4.x, v5.x),
    Bounds::hull5(v1.y, v2.y, v3.y, v4.y, v5.y)
  );
}

template <>
inline Vector2d<Bounds>
Vector2d<Bounds>::hull6(
  Vector2d<double> v1,
  Vector2d<double> v2,
  Vector2d<double> v3,
  Vector2d<double> v4,
  Vector2d<double> v5,
  Vector2d<double> v6
) {
  return Vector2d(
    Bounds::hull6(v1.x, v2.x, v3.x, v4.x, v5.x, v6.x),
    Bounds::hull6(v1.y, v2.y, v3.y, v4.y, v5.y, v6.y)
  );
}

template <>
inline double
Vector2d<double>::squaredMagnitude() const {
  return x * x + y * y;
}

template <>
inline Bounds
Vector2d<Bounds>::squaredMagnitude() const {
  return x.squared() + y.squared();
}

template <>
inline double
Vector2d<double>::magnitude() const {
  return std::sqrt(squaredMagnitude());
}

template <>
inline Bounds
Vector2d<Bounds>::magnitude() const {
  return squaredMagnitude().sqrt();
}

static_assert(
  sizeof(Vector2d<double>) == 16,
  "Expected Vector2d<double> to be exactly the size of two double values"
);
static_assert(
  sizeof(Vector2d<Bounds>) == 32,
  "Expected Vector2d<Bounds> to be exactly the size of four double values"
);

template <class S>
inline auto
operator-(const Vector2d<S>& v) {
  return Vector2d(-v.x, -v.y);
}

template <class S1, class S2>
inline auto
operator+(Vector2d<S1> lhs, Vector2d<S2> rhs) {
  return Vector2d(lhs.x + rhs.x, lhs.y + rhs.y);
}

template <class S1, class S2>
inline auto
operator-(Vector2d<S1> lhs, Vector2d<S2> rhs) {
  return Vector2d(lhs.x - rhs.x, lhs.y - rhs.y);
}

template <class S1, class S2>
inline auto
operator*(S1 lhs, Vector2d<S2> rhs) {
  return Vector2d(lhs * rhs.x, lhs * rhs.y);
}

template <class S1, class S2>
inline auto
operator*(Vector2d<S1> lhs, S2 rhs) {
  return Vector2d(lhs.x * rhs, lhs.y * rhs);
}

template <class S1, class S2>
inline auto
operator/(Vector2d<S1> lhs, S2 rhs) {
  return Vector2d(lhs.x / rhs, lhs.y / rhs);
}
