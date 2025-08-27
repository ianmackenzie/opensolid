#pragma once

#include "bounds.h"

template <class S> struct Vector3d {
  S x;
  S y;
  S z;

  inline Vector3d() :
      x{},
      y{},
      z{} {
  }

  inline Vector3d(S x, S y, S z) :
      x(x),
      y(y),
      z(z) {
  }

  static Vector3d<Bounds>
  invalid();

  static Vector3d<Bounds>
  hull2(Vector3d<double> v1, Vector3d<double> v2);

  static Vector3d<Bounds>
  hull3(Vector3d<double> v1, Vector3d<double> v2, Vector3d<double> v3);

  static Vector3d<Bounds>
  hull4(Vector3d<double> v1, Vector3d<double> v2, Vector3d<double> v3, Vector3d<double> v4);

  static Vector3d<Bounds>
  hull5(
    Vector3d<double> v1,
    Vector3d<double> v2,
    Vector3d<double> v3,
    Vector3d<double> v4,
    Vector3d<double> v5
  );

  static Vector3d<Bounds>
  hull6(
    Vector3d<double> v1,
    Vector3d<double> v2,
    Vector3d<double> v3,
    Vector3d<double> v4,
    Vector3d<double> v5,
    Vector3d<double> v6
  );

  template <class S2>
  inline auto
  dot(Vector3d<S2> other) const {
    return x * other.x + y * other.y + z * other.z;
  }

  template <class S2>
  inline auto
  cross(Vector3d<S2> other) const {
    return Vector3d(
      y * other.z - z * other.y,
      z * other.x - x * other.z,
      x * other.y - y * other.x
    );
  }

  S
  squaredMagnitude() const;

  S
  magnitude() const;
};

template <>
inline Vector3d<Bounds>
Vector3d<Bounds>::invalid() {
  return Vector3d(Bounds::invalid(), Bounds::invalid(), Bounds::invalid());
}

template <>
inline Vector3d<Bounds>
Vector3d<Bounds>::hull2(Vector3d<double> v1, Vector3d<double> v2) {
  return Vector3d(Bounds::hull2(v1.x, v2.x), Bounds::hull2(v1.y, v2.y), Bounds::hull2(v1.z, v2.z));
}

template <>
inline Vector3d<Bounds>
Vector3d<Bounds>::hull3(Vector3d<double> v1, Vector3d<double> v2, Vector3d<double> v3) {
  return Vector3d(
    Bounds::hull3(v1.x, v2.x, v3.x),
    Bounds::hull3(v1.y, v2.y, v3.y),
    Bounds::hull3(v1.z, v2.z, v3.z)
  );
}

template <>
inline Vector3d<Bounds>
Vector3d<Bounds>::hull4(
  Vector3d<double> v1,
  Vector3d<double> v2,
  Vector3d<double> v3,
  Vector3d<double> v4
) {
  return Vector3d(
    Bounds::hull4(v1.x, v2.x, v3.x, v4.x),
    Bounds::hull4(v1.y, v2.y, v3.y, v4.y),
    Bounds::hull4(v1.z, v2.z, v3.z, v4.z)
  );
}

template <>
inline Vector3d<Bounds>
Vector3d<Bounds>::hull5(
  Vector3d<double> v1,
  Vector3d<double> v2,
  Vector3d<double> v3,
  Vector3d<double> v4,
  Vector3d<double> v5
) {
  return Vector3d(
    Bounds::hull5(v1.x, v2.x, v3.x, v4.x, v5.x),
    Bounds::hull5(v1.y, v2.y, v3.y, v4.y, v5.y),
    Bounds::hull5(v1.z, v2.z, v3.z, v4.z, v5.z)
  );
}

template <>
inline Vector3d<Bounds>
Vector3d<Bounds>::hull6(
  Vector3d<double> v1,
  Vector3d<double> v2,
  Vector3d<double> v3,
  Vector3d<double> v4,
  Vector3d<double> v5,
  Vector3d<double> v6
) {
  return Vector3d(
    Bounds::hull6(v1.x, v2.x, v3.x, v4.x, v5.x, v6.x),
    Bounds::hull6(v1.y, v2.y, v3.y, v4.y, v5.y, v6.y),
    Bounds::hull6(v1.z, v2.z, v3.z, v4.z, v5.z, v6.z)
  );
}

template <>
inline double
Vector3d<double>::squaredMagnitude() const {
  return x * x + y * y + z * z;
}

template <>
inline Bounds
Vector3d<Bounds>::squaredMagnitude() const {
  return x.squared() + y.squared() + z.squared();
}

template <>
inline double
Vector3d<double>::magnitude() const {
  return std::sqrt(squaredMagnitude());
}

template <>
inline Bounds
Vector3d<Bounds>::magnitude() const {
  return squaredMagnitude().sqrt();
}

static_assert(
  sizeof(Vector3d<double>) == 24,
  "Expected Vector3d<double> to be exactly the size of three double values"
);
static_assert(
  sizeof(Vector3d<Bounds>) == 48,
  "Expected Vector3d<Bounds> to be exactly the size of six double values"
);

template <class S>
inline auto
operator-(Vector3d<S> v) {
  return Vector3d(-v.x, -v.y, -v.z);
}

template <class S1, class S2>
inline auto
operator+(Vector3d<S1> lhs, Vector3d<S2> rhs) {
  return Vector3d(lhs.x + rhs.x, lhs.y + rhs.y, lhs.z + rhs.z);
}

template <class S1, class S2>
inline auto
operator-(Vector3d<S1> lhs, Vector3d<S2> rhs) {
  return Vector3d(lhs.x - rhs.x, lhs.y - rhs.y, lhs.z - rhs.z);
}

template <class S1, class S2>
inline auto
operator*(S1 lhs, Vector3d<S2> rhs) {
  return Vector3d(lhs * rhs.x, lhs * rhs.y, lhs * rhs.z);
}

template <class S1, class S2>
inline auto
operator*(Vector3d<S1> lhs, S2 rhs) {
  return Vector3d(lhs.x * rhs, lhs.y * rhs, lhs.z * rhs);
}

template <class S1, class S2>
inline auto
operator/(Vector3d<S1> lhs, S2 rhs) {
  return Vector3d(lhs.x / rhs, lhs.y / rhs, lhs.z / rhs);
}
