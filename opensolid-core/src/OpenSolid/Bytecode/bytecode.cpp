#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstring>

#include "bounds.h"
#include "bytecode.h"
#include "vector2d.h"
#include "vector3d.h"

// Used when evaluating Desingularized#d opcodes,
// to determine whether a given parameter value is at a given endpoint
// or whether a given parameter range includes a given endpoint;
// should be kept in sync with the constants used in Desingularization.hs
#define T0 0.0625 // 1/16
#define T1 0.9375 // 15/16

inline double
squared(double x) {
  return x * x;
}

inline Bounds
squared(Bounds x) {
  return x.squared();
}

inline double
cubed(double x) {
  return x * x * x;
}

inline Bounds
cubed(Bounds x) {
  return x.cubed();
}

inline double
fourthPower(double x) {
  return x * x * x * x;
}

inline Bounds
fourthPower(Bounds x) {
  return x.fourthPower();
}

inline double
squareRoot(double x) {
  return std::sqrt(std::max(x, 0.0));
}

inline Bounds
squareRoot(Bounds x) {
  return x.sqrt();
}

inline double
sine(double x) {
  return std::sin(x);
}

inline Bounds
sine(Bounds x) {
  return x.sin();
}

inline double
cosine(double x) {
  return std::cos(x);
}

inline Bounds
cosine(Bounds x) {
  return x.cos();
}

inline double
interpolateFrom(double a, double b, double t) {
  return a + t * (b - a);
}

inline Bounds
interpolateFrom(double a, double b, Bounds t) {
  if (a <= b) {
    double lower = interpolateFrom(a, b, t.lower);
    double upper = interpolateFrom(a, b, t.upper);
    return Bounds(lower, upper);
  } else {
    double lower = interpolateFrom(a, b, t.upper);
    double upper = interpolateFrom(a, b, t.lower);
    return Bounds(lower, upper);
  }
}

inline Bounds
interpolateFrom(Bounds a, Bounds b, double t) {
  if (t >= 0.0 && t <= 1.0) [[likely]] {
    // Expected case: t is in the range [0,1]
    double lower = interpolateFrom(a.lower, b.lower, t);
    double upper = interpolateFrom(a.upper, b.upper, t);
    return Bounds(lower, upper);
  } else {
    // Fallback case
    double ll = interpolateFrom(a.lower, b.lower, t);
    double lu = interpolateFrom(a.lower, b.upper, t);
    double ul = interpolateFrom(a.upper, b.lower, t);
    double uu = interpolateFrom(a.upper, b.upper, t);
    return Bounds::hull4(ll, lu, ul, uu);
  }
}

inline double
interpolateLower(double a, double b, Bounds t) {
  return interpolateFrom(a, b, a <= b ? t.lower : t.upper);
}

inline double
interpolateUpper(double a, double b, Bounds t) {
  return interpolateFrom(a, b, a <= b ? t.upper : t.lower);
}

inline Bounds
interpolateFrom(Bounds a, Bounds b, Bounds t) {
  if (t.lower >= 0.0 && t.upper <= 1.0) {
    // Expected case: t is in the range [0,1]
    double lower = interpolateFrom(a.lower, b.lower, a.lower <= b.lower ? t.lower : t.upper);
    double upper = interpolateFrom(a.upper, b.upper, a.upper <= b.upper ? t.upper : t.lower);
    return Bounds(lower, upper);
  } else {
    Bounds bounds1 = interpolateFrom(a, b, t.lower);
    Bounds bounds2 = interpolateFrom(a, b, t.upper);
    return Bounds::aggregate2(bounds1, bounds2);
  }
}

inline Vector2d<double>
interpolateFrom(Vector2d<double> a, Vector2d<double> b, double t) {
  double x = interpolateFrom(a.x, b.x, t);
  double y = interpolateFrom(a.y, b.y, t);
  return Vector2d(x, y);
}

inline Vector3d<double>
interpolateFrom(const Vector3d<double>& a, const Vector3d<double>& b, double t) {
  double x = interpolateFrom(a.x, b.x, t);
  double y = interpolateFrom(a.y, b.y, t);
  double z = interpolateFrom(a.z, b.z, t);
  return Vector3d(x, y, z);
}

inline Vector2d<Bounds>
interpolateFrom(Vector2d<double> a, Vector2d<double> b, Bounds t) {
  Bounds x = interpolateFrom(a.x, b.x, t);
  Bounds y = interpolateFrom(a.y, b.y, t);
  return Vector2d(x, y);
}

inline Vector3d<Bounds>
interpolateFrom(const Vector3d<double>& a, const Vector3d<double>& b, Bounds t) {
  Bounds x = interpolateFrom(a.x, b.x, t);
  Bounds y = interpolateFrom(a.y, b.y, t);
  Bounds z = interpolateFrom(a.z, b.z, t);
  return Vector3d(x, y, z);
}

inline Vector2d<Bounds>
interpolateFrom(const Vector2d<Bounds>& a, const Vector2d<Bounds>& b, Bounds t) {
  Bounds x = interpolateFrom(a.x, b.x, t);
  Bounds y = interpolateFrom(a.y, b.y, t);
  return Vector2d(x, y);
}

inline Vector3d<Bounds>
interpolateFrom(const Vector3d<Bounds>& a, const Vector3d<Bounds>& b, Bounds t) {
  Bounds x = interpolateFrom(a.x, b.x, t);
  Bounds y = interpolateFrom(a.y, b.y, t);
  Bounds z = interpolateFrom(a.z, b.z, t);
  return Vector3d(x, y, z);
}

int
prod(int a, int b) {
  int result = a;
  while (a < b) {
    ++a;
    result *= a;
  }
  return result;
}

int
choose(int n, int k) {
  static const int lookup[] = {
    /*00*/ 1,
    /*01*/ 1, 1,
    /*02*/ 1, 2, 1,
    /*03*/ 1, 3, 3,  1,
    /*04*/ 1, 4, 6,  4,  1,
    /*05*/ 1, 5, 10, 10, 5,  1,
    /*06*/ 1, 6, 15, 20, 15, 6,  1,
    /*07*/ 1, 7, 21, 35, 35, 21, 7,  1,
    /*08*/ 1, 8, 28, 56, 70, 56, 28, 8, 1,
  };
  if (n < 0 || k < 0 || k > n) {
    return 0;
  } else if (n <= 8) {
    return lookup[(n * (n + 1)) / 2 + k];
  } else {
    int d = std::min(k, n - k);
    return d == 0 ? 1 : prod(n - d + 1, n) / prod(1, d);
  }
}

template <class Vector>
inline Vector
quadraticBlossom(const Vector* p, double t1, double t2) {
  Vector q0 = interpolateFrom(p[0], p[1], t1);
  Vector q1 = interpolateFrom(p[1], p[2], t1);
  return interpolateFrom(q0, q1, t2);
}

inline double
quadraticBezier(const double* p, double t) {
  return quadraticBlossom(p, t, t);
}

inline Vector2d<double>
quadraticBezier(const Vector2d<double>* p, double t) {
  return quadraticBlossom(p, t, t);
}

inline Vector3d<double>
quadraticBezier(const Vector3d<double>* p, double t) {
  return quadraticBlossom(p, t, t);
}

template <class Vector>
inline Vector
cubicBlossom(const Vector* p, double t1, double t2, double t3) {
  Vector q[3];
  q[0] = interpolateFrom(p[0], p[1], t1);
  q[1] = interpolateFrom(p[1], p[2], t1);
  q[2] = interpolateFrom(p[2], p[3], t1);
  return quadraticBlossom(q, t2, t3);
}

inline double
cubicBezier(const double* p, double t) {
  return cubicBlossom(p, t, t, t);
}

inline Vector2d<double>
cubicBezier(const Vector2d<double>* p, double t) {
  return cubicBlossom(p, t, t, t);
}

inline Vector3d<double>
cubicBezier(const Vector3d<double>* p, double t) {
  return cubicBlossom(p, t, t, t);
}

template <class Vector>
inline Vector
quarticBlossom(const Vector* p, double t1, double t2, double t3, double t4) {
  Vector q[4];
  q[0] = interpolateFrom(p[0], p[1], t1);
  q[1] = interpolateFrom(p[1], p[2], t1);
  q[2] = interpolateFrom(p[2], p[3], t1);
  q[3] = interpolateFrom(p[3], p[4], t1);
  return cubicBlossom(q, t2, t3, t4);
}

inline double
quarticBezier(const double* p, double t) {
  return quarticBlossom(p, t, t, t, t);
}

inline Vector2d<double>
quarticBezier(const Vector2d<double>* p, double t) {
  return quarticBlossom(p, t, t, t, t);
}

inline Vector3d<double>
quarticBezier(const Vector3d<double>* p, double t) {
  return quarticBlossom(p, t, t, t, t);
}

template <class Vector>
inline Vector
quinticBlossom(const Vector* p, double t1, double t2, double t3, double t4, double t5) {
  Vector q[5];
  q[0] = interpolateFrom(p[0], p[1], t1);
  q[1] = interpolateFrom(p[1], p[2], t1);
  q[2] = interpolateFrom(p[2], p[3], t1);
  q[3] = interpolateFrom(p[3], p[4], t1);
  q[4] = interpolateFrom(p[4], p[5], t1);
  return quarticBlossom(q, t2, t3, t4, t5);
}

inline double
quinticBezier(const double* p, double t) {
  return quinticBlossom(p, t, t, t, t, t);
}

inline Vector2d<double>
quinticBezier(const Vector2d<double>* p, double t) {
  return quinticBlossom(p, t, t, t, t, t);
}

inline Vector3d<double>
quinticBezier(const Vector3d<double>* p, double t) {
  return quinticBlossom(p, t, t, t, t, t);
}

template <class Vector>
Vector
bezierBlossom(
  int numControlPoints,
  const Vector* controlPoints,
  double tLower,
  double tUpper,
  int nLow
) {
  if (numControlPoints == 1) {
    return *controlPoints;
  }
  Vector* q = (Vector*)alloca(sizeof(Vector) * (numControlPoints - 1));
  for (int m = numControlPoints - 1; m > 0; --m) { // m is number of points to collapse to
    double t = m <= nLow ? tLower : tUpper;
    for (int i = 0; i < m; ++i) { // i is index of the point to collapse to
      q[i] = interpolateFrom(controlPoints[i], controlPoints[i + 1], t);
    }
    controlPoints = q; // After the first loop iteration, work in place within the outputs
  }
  return *q;
}

inline double
bezier(int numControlPoints, const double* controlPoints, double t) {
  return bezierBlossom(numControlPoints, controlPoints, t, t, 0);
}

inline Vector2d<double>
bezier(int numControlPoints, const Vector2d<double>* controlPoints, double t) {
  return bezierBlossom(numControlPoints, controlPoints, t, t, 0);
}

inline Vector3d<double>
bezier(int numControlPoints, const Vector3d<double>* controlPoints, double t) {
  return bezierBlossom(numControlPoints, controlPoints, t, t, 0);
}

Bounds
quadraticBezier(const double* p, Bounds t) {
  double a = quadraticBlossom(p, t.lower, t.lower);
  double b = quadraticBlossom(p, t.lower, t.upper);
  double c = quadraticBlossom(p, t.upper, t.upper);
  return Bounds::hull3(a, b, c);
}

Vector2d<Bounds>
quadraticBezier(const Vector2d<double>* p, Bounds t) {
  Vector2d<double> a = quadraticBlossom(p, t.lower, t.lower);
  Vector2d<double> b = quadraticBlossom(p, t.lower, t.upper);
  Vector2d<double> c = quadraticBlossom(p, t.upper, t.upper);
  return Vector2d<Bounds>::hull3(a, b, c);
}

Vector3d<Bounds>
quadraticBezier(const Vector3d<double>* p, Bounds t) {
  Vector3d<double> a = quadraticBlossom(p, t.lower, t.lower);
  Vector3d<double> b = quadraticBlossom(p, t.lower, t.upper);
  Vector3d<double> c = quadraticBlossom(p, t.upper, t.upper);
  return Vector3d<Bounds>::hull3(a, b, c);
}

Bounds
cubicBezier(const double* p, Bounds t) {
  double a = cubicBlossom(p, t.lower, t.lower, t.lower);
  double b = cubicBlossom(p, t.lower, t.lower, t.upper);
  double c = cubicBlossom(p, t.lower, t.upper, t.upper);
  double d = cubicBlossom(p, t.upper, t.upper, t.upper);
  return Bounds::hull4(a, b, c, d);
}

Vector2d<Bounds>
cubicBezier(const Vector2d<double>* p, Bounds t) {
  Vector2d<double> a = cubicBlossom(p, t.lower, t.lower, t.lower);
  Vector2d<double> b = cubicBlossom(p, t.lower, t.lower, t.upper);
  Vector2d<double> c = cubicBlossom(p, t.lower, t.upper, t.upper);
  Vector2d<double> d = cubicBlossom(p, t.upper, t.upper, t.upper);
  return Vector2d<Bounds>::hull4(a, b, c, d);
}

Vector3d<Bounds>
cubicBezier(const Vector3d<double>* p, Bounds t) {
  Vector3d<double> a = cubicBlossom(p, t.lower, t.lower, t.lower);
  Vector3d<double> b = cubicBlossom(p, t.lower, t.lower, t.upper);
  Vector3d<double> c = cubicBlossom(p, t.lower, t.upper, t.upper);
  Vector3d<double> d = cubicBlossom(p, t.upper, t.upper, t.upper);
  return Vector3d<Bounds>::hull4(a, b, c, d);
}

Bounds
quarticBezier(const double* p, Bounds t) {
  double a = quarticBlossom(p, t.lower, t.lower, t.lower, t.lower);
  double b = quarticBlossom(p, t.lower, t.lower, t.lower, t.upper);
  double c = quarticBlossom(p, t.lower, t.lower, t.upper, t.upper);
  double d = quarticBlossom(p, t.lower, t.upper, t.upper, t.upper);
  double e = quarticBlossom(p, t.upper, t.upper, t.upper, t.upper);
  return Bounds::hull5(a, b, c, d, e);
}

Vector2d<Bounds>
quarticBezier(const Vector2d<double>* p, Bounds t) {
  Vector2d<double> a = quarticBlossom(p, t.lower, t.lower, t.lower, t.lower);
  Vector2d<double> b = quarticBlossom(p, t.lower, t.lower, t.lower, t.upper);
  Vector2d<double> c = quarticBlossom(p, t.lower, t.lower, t.upper, t.upper);
  Vector2d<double> d = quarticBlossom(p, t.lower, t.upper, t.upper, t.upper);
  Vector2d<double> e = quarticBlossom(p, t.upper, t.upper, t.upper, t.upper);
  return Vector2d<Bounds>::hull5(a, b, c, d, e);
}

inline Vector3d<Bounds>
quarticBezier(const Vector3d<double>* p, Bounds t) {
  Vector3d<double> a = quarticBlossom(p, t.lower, t.lower, t.lower, t.lower);
  Vector3d<double> b = quarticBlossom(p, t.lower, t.lower, t.lower, t.upper);
  Vector3d<double> c = quarticBlossom(p, t.lower, t.lower, t.upper, t.upper);
  Vector3d<double> d = quarticBlossom(p, t.lower, t.upper, t.upper, t.upper);
  Vector3d<double> e = quarticBlossom(p, t.upper, t.upper, t.upper, t.upper);
  return Vector3d<Bounds>::hull5(a, b, c, d, e);
}

Bounds
quinticBezier(const double* p, Bounds t) {
  double a = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.lower);
  double b = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.upper);
  double c = quinticBlossom(p, t.lower, t.lower, t.lower, t.upper, t.upper);
  double d = quinticBlossom(p, t.lower, t.lower, t.upper, t.upper, t.upper);
  double e = quinticBlossom(p, t.lower, t.upper, t.upper, t.upper, t.upper);
  double f = quinticBlossom(p, t.upper, t.upper, t.upper, t.upper, t.upper);
  return Bounds::hull6(a, b, c, d, e, f);
}

Vector2d<Bounds>
quinticBezier(const Vector2d<double>* p, Bounds t) {
  Vector2d<double> a = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.lower);
  Vector2d<double> b = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.upper);
  Vector2d<double> c = quinticBlossom(p, t.lower, t.lower, t.lower, t.upper, t.upper);
  Vector2d<double> d = quinticBlossom(p, t.lower, t.lower, t.upper, t.upper, t.upper);
  Vector2d<double> e = quinticBlossom(p, t.lower, t.upper, t.upper, t.upper, t.upper);
  Vector2d<double> f = quinticBlossom(p, t.upper, t.upper, t.upper, t.upper, t.upper);
  return Vector2d<Bounds>::hull6(a, b, c, d, e, f);
}

Vector3d<Bounds>
quinticBezier(const Vector3d<double>* p, Bounds t) {
  Vector3d<double> a = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.lower);
  Vector3d<double> b = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.upper);
  Vector3d<double> c = quinticBlossom(p, t.lower, t.lower, t.lower, t.upper, t.upper);
  Vector3d<double> d = quinticBlossom(p, t.lower, t.lower, t.upper, t.upper, t.upper);
  Vector3d<double> e = quinticBlossom(p, t.lower, t.upper, t.upper, t.upper, t.upper);
  Vector3d<double> f = quinticBlossom(p, t.upper, t.upper, t.upper, t.upper, t.upper);
  return Vector3d<Bounds>::hull6(a, b, c, d, e, f);
}

Bounds
bezier(int numControlPoints, const double* controlPoints, Bounds t) {
  if (numControlPoints <= 0) {
    return Bounds::invalid();
  }
  double hullValue = bezierBlossom(numControlPoints, controlPoints, t.lower, t.upper, 0);
  double minValue = hullValue;
  double maxValue = hullValue;
  for (int i = 1; i < numControlPoints; ++i) {
    hullValue = bezierBlossom(numControlPoints, controlPoints, t.lower, t.upper, i);
    minValue = std::min(minValue, hullValue);
    maxValue = std::max(maxValue, hullValue);
  }
  return Bounds(minValue, maxValue);
}

Vector2d<Bounds>
bezier(int numControlPoints, const Vector2d<double>* controlPoints, Bounds t) {
  if (numControlPoints <= 0) {
    return Vector2d<Bounds>::invalid();
  }
  Vector2d hullPoint = bezierBlossom(numControlPoints, controlPoints, t.lower, t.upper, 0);
  double minX = hullPoint.x;
  double maxX = hullPoint.x;
  double minY = hullPoint.y;
  double maxY = hullPoint.y;
  for (int i = 1; i < numControlPoints; ++i) {
    hullPoint = bezierBlossom(numControlPoints, controlPoints, t.lower, t.upper, i);
    minX = std::min(minX, hullPoint.x);
    maxX = std::max(maxX, hullPoint.x);
    minY = std::min(minY, hullPoint.y);
    maxY = std::max(maxY, hullPoint.y);
  }
  return Vector2d(Bounds(minX, maxX), Bounds(minY, maxY));
}

Vector3d<Bounds>
bezier(int numControlPoints, const Vector3d<double>* controlPoints, Bounds t) {
  if (numControlPoints <= 0) {
    return Vector3d<Bounds>::invalid();
  }
  Vector3d hullPoint = bezierBlossom(numControlPoints, controlPoints, t.lower, t.upper, 0);
  double minX = hullPoint.x;
  double maxX = hullPoint.x;
  double minY = hullPoint.y;
  double maxY = hullPoint.y;
  double minZ = hullPoint.z;
  double maxZ = hullPoint.z;
  for (int i = 1; i < numControlPoints; ++i) {
    hullPoint = bezierBlossom(numControlPoints, controlPoints, t.lower, t.upper, i);
    minX = std::min(minX, hullPoint.x);
    maxX = std::max(maxX, hullPoint.x);
    minY = std::min(minY, hullPoint.y);
    maxY = std::max(maxY, hullPoint.y);
    minZ = std::min(minZ, hullPoint.z);
    maxZ = std::max(maxZ, hullPoint.z);
  }
  return Vector3d(Bounds(minX, maxX), Bounds(minY, maxY), Bounds(minZ, maxZ));
}

template <class Scalar>
Vector2d<Scalar>
involute(int n, Vector2d<double> r, double theta1, double theta2, Scalar t) {
  Scalar theta = interpolateFrom(theta1, theta2, t);
  Scalar sinTheta = sine(theta);
  Scalar cosTheta = cosine(theta);
  double scale = n == 0 ? 1.0 : std::pow(theta2 - theta1, n);
  Scalar s0;
  Scalar s1;
  Scalar s2;
  Scalar s3;
  switch (n % 4) {
    case 0:
      s0 = sinTheta;
      s1 = cosTheta;
      s2 = -sinTheta;
      s3 = -cosTheta;
      break;
    case 1:
      s0 = cosTheta;
      s1 = -sinTheta;
      s2 = -cosTheta;
      s3 = sinTheta;
      break;
    case 2:
      s0 = -sinTheta;
      s1 = -cosTheta;
      s2 = sinTheta;
      s3 = cosTheta;
      break;
    default: // 3
      s0 = -cosTheta;
      s1 = sinTheta;
      s2 = cosTheta;
      s3 = -sinTheta;
      break;
  }
  Scalar x = (n - 1) * (s3 * r.x + s0 * r.y) + theta * (s0 * r.x + s1 * r.y);
  Scalar y = (n - 1) * (s2 * r.x + s3 * r.y) + theta * (s3 * r.x + s0 * r.y);
  return scale * Vector2d<Scalar>(x, y);
}

inline bool
isStart(double t) {
  return t <= T0;
}

inline bool
isStart(Bounds t) {
  return t.upper <= T0;
}

inline bool
isEnd(double t) {
  return t >= T1;
}

inline bool
isEnd(Bounds t) {
  return t.lower >= T1;
}

template <class Scalar>
Scalar
b00(Scalar t) {
  return 3.0 * fourthPower(t) - 4.0 * cubed(t) + 1.0;
}

template <class Scalar>
Scalar
b00d1(Scalar t) {
  return 12.0 * (cubed(t) - squared(t));
}

template <class Scalar>
Scalar
b00d2(Scalar t) {
  return 36.0 * squared(t) - 24.0 * t;
}

template <class Scalar>
Scalar
b00d3(Scalar t) {
  return 72.0 * t - 24.0;
}

template <class Scalar>
Scalar
b01(Scalar t) {
  return 2.0 * fourthPower(t) - 3.0 * cubed(t) + t;
}

template <class Scalar>
Scalar
b01d1(Scalar t) {
  return 8.0 * cubed(t) - 9.0 * squared(t) + 1.0;
}

template <class Scalar>
Scalar
b01d2(Scalar t) {
  return 24.0 * squared(t) - 18.0 * t;
}

template <class Scalar>
Scalar
b01d3(Scalar t) {
  return 48.0 * t - 18.0;
}

template <class Scalar>
Scalar
b02(Scalar t) {
  return 0.5 * fourthPower(t) - cubed(t) + 0.5 * squared(t);
}

template <class Scalar>
Scalar
b02d1(Scalar t) {
  return 2.0 * cubed(t) - 3.0 * squared(t) + t;
}

template <class Scalar>
Scalar
b02d2(Scalar t) {
  return 6.0 * squared(t) - 6.0 * t + 1.0;
}

template <class Scalar>
Scalar
b02d3(Scalar t) {
  return 12.0 * t - 6.0;
}

template <class Scalar>
Scalar
b10(Scalar t) {
  return -3.0 * fourthPower(t) + 4.0 * cubed(t);
}

template <class Scalar>
Scalar
b10d1(Scalar t) {
  return 12.0 * (squared(t) - cubed(t));
}

template <class Scalar>
Scalar
b10d2(Scalar t) {
  return -36.0 * squared(t) + 24.0 * t;
}

template <class Scalar>
Scalar
b10d3(Scalar t) {
  return -72.0 * t + 24.0;
}

template <class Scalar>
Scalar
b11(Scalar t) {
  return fourthPower(t) - cubed(t);
}

template <class Scalar>
Scalar
b11d1(Scalar t) {
  return 4.0 * cubed(t) - 3.0 * squared(t);
}

template <class Scalar>
Scalar
b11d2(Scalar t) {
  return 12.0 * squared(t) - 6.0 * t;
}

template <class Scalar>
Scalar
b11d3(Scalar t) {
  return 24.0 * t - 6.0;
}

template <class T>
void
evaluate(
  const uint16_t* wordsPointer,
  const double* constantsPointer,
  T* variablesPointer,
  T* returnValuesPointer
) {
  auto getInt = [&]() -> int {
    int value = *wordsPointer;
    ++wordsPointer;
    return value;
  };
  auto getConstantScalarPointer = [&]() -> const double* {
    return constantsPointer + getInt();
  };
  auto getConstantScalar = [&]() -> double {
    return *getConstantScalarPointer();
  };
  auto getConstantVector2dPointer = [&]() -> const Vector2d<double>* {
    return (const Vector2d<double>*)getConstantScalarPointer();
  };
  auto getConstantVector2d = [&]() -> Vector2d<double> {
    return *getConstantVector2dPointer();
  };
  auto getConstantVector3dPointer = [&]() -> const Vector3d<double>* {
    return (const Vector3d<double>*)getConstantScalarPointer();
  };
  auto getConstantVector3d = [&]() -> Vector3d<double> {
    return *getConstantVector3dPointer();
  };
  auto getScalarPointer = [&]() -> T* {
    return variablesPointer + getInt();
  };
  auto getScalar = [&]() -> T {
    return *getScalarPointer();
  };
  auto getVector2dPointer = [&]() -> Vector2d<T>* {
    return (Vector2d<T>*)getScalarPointer();
  };
  auto getVector2d = [&]() -> Vector2d<T> {
    return *getVector2dPointer();
  };
  auto getVector3dPointer = [&]() -> Vector3d<T>* {
    return (Vector3d<T>*)getScalarPointer();
  };
  auto getVector3d = [&]() -> Vector3d<T> {
    return *getVector3dPointer();
  };
  while (true) {
    int opcode = getInt();
    assert(opcode < OPCODE_END && "Unrecognized opcode");
    switch (Opcode(opcode)) {
      case Return: {
        int dimension = getInt();
        T* valuesPointer = getScalarPointer();
        std::memcpy(returnValuesPointer, valuesPointer, sizeof(T) * dimension);
        return;
      }
      case XComponent: {
        const T* vec = getScalarPointer();
        T* output = getScalarPointer();
        *output = vec[0];
        break;
      }
      case YComponent: {
        const T* vec = getScalarPointer();
        T* output = getScalarPointer();
        *output = vec[1];
        break;
      }
      case ZComponent: {
        const T* vec = getScalarPointer();
        T* output = getScalarPointer();
        *output = vec[2];
        break;
      }
      case Negate1d: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = -input;
        break;
      }
      case Add1d: {
        T lhs = getScalar();
        T rhs = getScalar();
        T* output = getScalarPointer();
        *output = lhs + rhs;
        break;
      }
      case AddVariableConstant1d: {
        T lhs = getScalar();
        double rhs = getConstantScalar();
        T* output = getScalarPointer();
        *output = lhs + rhs;
        break;
      }
      case Subtract1d: {
        T lhs = getScalar();
        T rhs = getScalar();
        T* output = getScalarPointer();
        *output = lhs - rhs;
        break;
      }
      case SubtractConstantVariable1d: {
        double lhs = getConstantScalar();
        T rhs = getScalar();
        T* output = getScalarPointer();
        *output = lhs - rhs;
        break;
      }
      case Multiply1d: {
        T lhs = getScalar();
        T rhs = getScalar();
        T* output = getScalarPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyVariableConstant1d: {
        T lhs = getScalar();
        double rhs = getConstantScalar();
        T* output = getScalarPointer();
        *output = lhs * rhs;
        break;
      }
      case Divide1d: {
        T lhs = getScalar();
        T rhs = getScalar();
        T* output = getScalarPointer();
        *output = lhs / rhs;
        break;
      }
      case DivideConstantVariable1d: {
        double lhs = getConstantScalar();
        T rhs = getScalar();
        T* output = getScalarPointer();
        *output = lhs / rhs;
        break;
      }
      case Square1d: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = input * input;
        break;
      }
      case Sqrt1d: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = squareRoot(input);
        break;
      }
      case Sin1d: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = sine(input);
        break;
      }
      case Cos1d: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = cosine(input);
        break;
      }
      case Linear1d: {
        const double* endpoints = getConstantScalarPointer();
        T parameter = getScalar();
        T* output = getScalarPointer();
        *output = interpolateFrom(endpoints[0], endpoints[1], parameter);
        break;
      }
      case Quadratic1d: {
        const double* controlPoints = getConstantScalarPointer();
        const T parameter = getScalar();
        T* output = getScalarPointer();
        *output = quadraticBezier(controlPoints, parameter);
        break;
      }
      case Cubic1d: {
        const double* controlPoints = getConstantScalarPointer();
        T parameter = getScalar();
        T* output = getScalarPointer();
        *output = cubicBezier(controlPoints, parameter);
        break;
      }
      case Quartic1d: {
        const double* controlPoints = getConstantScalarPointer();
        T parameter = getScalar();
        T* output = getScalarPointer();
        *output = quarticBezier(controlPoints, parameter);
        break;
      }
      case Quintic1d: {
        const double* controlPoints = getConstantScalarPointer();
        T parameter = getScalar();
        T* output = getScalarPointer();
        *output = quinticBezier(controlPoints, parameter);
        break;
      }
      case Bezier1d: {
        int numControlPoints = getInt();
        const double* controlPoints = getConstantScalarPointer();
        T parameter = getScalar();
        T* output = getScalarPointer();
        *output = bezier(numControlPoints, controlPoints, parameter);
        break;
      }
      case XY2d: {
        T x = getScalar();
        T y = getScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = Vector2d(x, y);
        break;
      }
      case XC2d: {
        T x = getScalar();
        double y = getConstantScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = Vector2d<T>(x, y);
        break;
      }
      case CY2d: {
        double x = getConstantScalar();
        T y = getScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = Vector2d<T>(x, y);
        break;
      }
      case Negate2d: {
        Vector2d<T> input = getVector2d();
        Vector2d<T>* output = getVector2dPointer();
        *output = -input;
        break;
      }
      case Add2d: {
        Vector2d<T> lhs = getVector2d();
        Vector2d<T> rhs = getVector2d();
        Vector2d<T>* output = getVector2dPointer();
        *output = lhs + rhs;
        break;
      }
      case AddVariableConstant2d: {
        Vector2d<T> lhs = getVector2d();
        Vector2d<double> rhs = getConstantVector2d();
        Vector2d<T>* output = getVector2dPointer();
        *output = lhs + rhs;
        break;
      }
      case Subtract2d: {
        Vector2d<T> lhs = getVector2d();
        Vector2d<T> rhs = getVector2d();
        Vector2d<T>* output = getVector2dPointer();
        *output = lhs - rhs;
        break;
      }
      case SubtractConstantVariable2d: {
        Vector2d<double> lhs = getConstantVector2d();
        Vector2d<T> rhs = getVector2d();
        Vector2d<T>* output = getVector2dPointer();
        *output = lhs - rhs;
        break;
      }
      case Multiply2d: {
        Vector2d<T> lhs = getVector2d();
        T rhs = getScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyVariableConstant2d: {
        Vector2d<T> lhs = getVector2d();
        double rhs = getConstantScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyConstantVariable2d: {
        Vector2d<double> lhs = getConstantVector2d();
        T rhs = getScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = lhs * rhs;
        break;
      }
      case Divide2d: {
        Vector2d<T> lhs = getVector2d();
        T rhs = getScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = lhs / rhs;
        break;
      }
      case DivideConstantVariable2d: {
        Vector2d<double> lhs = getConstantVector2d();
        T rhs = getScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = lhs / rhs;
        break;
      }
      case SquaredMagnitude2d: {
        Vector2d<T> arg = getVector2d();
        T* output = getScalarPointer();
        *output = arg.squaredMagnitude();
        break;
      }
      case Magnitude2d: {
        Vector2d<T> arg = getVector2d();
        T* output = getScalarPointer();
        *output = arg.magnitude();
        break;
      }
      case Dot2d: {
        Vector2d<T> lhs = getVector2d();
        Vector2d<T> rhs = getVector2d();
        T* output = getScalarPointer();
        *output = lhs.dot(rhs);
        break;
      }
      case DotVariableConstant2d: {
        Vector2d<T> lhs = getVector2d();
        Vector2d<double> rhs = getConstantVector2d();
        T* output = getScalarPointer();
        *output = lhs.dot(rhs);
        break;
      }
      case Cross2d: {
        Vector2d<T> lhs = getVector2d();
        Vector2d<T> rhs = getVector2d();
        T* output = getScalarPointer();
        *output = lhs.cross(rhs);
        break;
      }
      case CrossVariableConstant2d: {
        Vector2d<T> lhs = getVector2d();
        Vector2d<double> rhs = getConstantVector2d();
        T* output = getScalarPointer();
        *output = lhs.cross(rhs);
        break;
      }
      case Linear2d: {
        const Vector2d<double>* endpoints = getConstantVector2dPointer();
        T parameter = getScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = interpolateFrom(endpoints[0], endpoints[1], parameter);
        break;
      }
      case Quadratic2d: {
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        T parameter = getScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = quadraticBezier(controlPoints, parameter);
        break;
      }
      case Cubic2d: {
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        T parameter = getScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = cubicBezier(controlPoints, parameter);
        break;
      }
      case Quartic2d: {
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        T parameter = getScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = quarticBezier(controlPoints, parameter);
        break;
      }
      case Quintic2d: {
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        T parameter = getScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = quinticBezier(controlPoints, parameter);
        break;
      }
      case Bezier2d: {
        int numControlPoints = getInt();
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        T parameter = getScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = bezier(numControlPoints, controlPoints, parameter);
        break;
      }
      case TransformVector2d: {
        const double* matrix = getConstantScalarPointer();
        Vector2d<T> input = getVector2d();
        Vector2d<T>* output = getVector2dPointer();
        *output = Vector2d(
          matrix[0] * input.x + matrix[2] * input.y,
          matrix[1] * input.x + matrix[3] * input.y
        );
        break;
      }
      case TransformPoint2d: {
        const double* matrix = getConstantScalarPointer();
        Vector2d<T> input = getVector2d();
        Vector2d<T>* output = getVector2dPointer();
        *output = Vector2d(
          matrix[0] * input.x + matrix[2] * input.y + matrix[4],
          matrix[1] * input.x + matrix[3] * input.y + matrix[5]
        );
        break;
      }
      case ProjectVector3d: {
        const double* matrix = getConstantScalarPointer();
        Vector3d<T> input = getVector3d();
        Vector2d<T>* output = getVector2dPointer();
        *output = Vector2d(
          matrix[0] * input.x + matrix[1] * input.y + matrix[2] * input.z,
          matrix[3] * input.x + matrix[4] * input.y + matrix[5] * input.z
        );
        break;
      }
      case ProjectPoint3d: {
        const double* matrix = getConstantScalarPointer();
        Vector3d<T> input = getVector3d();
        Vector2d<T>* output = getVector2dPointer();
        T dx = input.x - matrix[6];
        T dy = input.y - matrix[7];
        T dz = input.z - matrix[8];
        *output = Vector2d(
          matrix[0] * dx + matrix[1] * dy + matrix[2] * dz,
          matrix[3] * dx + matrix[4] * dy + matrix[5] * dz
        );
        break;
      }
      case XYZ3d: {
        T x = getScalar();
        T y = getScalar();
        T z = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = Vector3d(x, y, z);
        break;
      }
      case XYC3d: {
        T x = getScalar();
        T y = getScalar();
        double z = getConstantScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = Vector3d<T>(x, y, z);
        break;
      }
      case XCZ3d: {
        T x = getScalar();
        double y = getConstantScalar();
        T z = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = Vector3d<T>(x, y, z);
        break;
      }
      case CYZ3d: {
        double x = getConstantScalar();
        T y = getScalar();
        T z = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = Vector3d<T>(x, y, z);
        break;
      }
      case XCC3d: {
        T x = getScalar();
        double y = getConstantScalar();
        double z = getConstantScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = Vector3d<T>(x, y, z);
        break;
      }
      case CYC3d: {
        double x = getConstantScalar();
        T y = getScalar();
        double z = getConstantScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = Vector3d<T>(x, y, z);
        break;
      }
      case CCZ3d: {
        double x = getConstantScalar();
        double y = getConstantScalar();
        T z = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = Vector3d<T>(x, y, z);
        break;
      }
      case Negate3d: {
        Vector3d<T> input = getVector3d();
        Vector3d<T>* output = getVector3dPointer();
        *output = -input;
        break;
      }
      case Add3d: {
        Vector3d<T> lhs = getVector3d();
        Vector3d<T> rhs = getVector3d();
        Vector3d<T>* output = getVector3dPointer();
        *output = lhs + rhs;
        break;
      }
      case AddVariableConstant3d: {
        Vector3d<T> lhs = getVector3d();
        Vector3d<double> rhs = getConstantVector3d();
        Vector3d<T>* output = getVector3dPointer();
        *output = lhs + rhs;
        break;
      }
      case Subtract3d: {
        Vector3d<T> lhs = getVector3d();
        Vector3d<T> rhs = getVector3d();
        Vector3d<T>* output = getVector3dPointer();
        *output = lhs - rhs;
        break;
      }
      case SubtractConstantVariable3d: {
        Vector3d<double> lhs = getConstantVector3d();
        Vector3d<T> rhs = getVector3d();
        Vector3d<T>* output = getVector3dPointer();
        *output = lhs - rhs;
        break;
      }
      case Multiply3d: {
        Vector3d<T> lhs = getVector3d();
        T rhs = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyVariableConstant3d: {
        Vector3d<T> lhs = getVector3d();
        double rhs = getConstantScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyConstantVariable3d: {
        Vector3d<double> lhs = getConstantVector3d();
        T rhs = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = lhs * rhs;
        break;
      }
      case Divide3d: {
        Vector3d<T> lhs = getVector3d();
        T rhs = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = lhs / rhs;
        break;
      }
      case DivideConstantVariable3d: {
        Vector3d<double> lhs = getConstantVector3d();
        T rhs = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = lhs / rhs;
        break;
      }
      case SquaredMagnitude3d: {
        Vector3d<T> arg = getVector3d();
        T* output = getScalarPointer();
        *output = arg.squaredMagnitude();
        break;
      }
      case Magnitude3d: {
        Vector3d<T> arg = getVector3d();
        T* output = getScalarPointer();
        *output = arg.magnitude();
        break;
      }
      case Dot3d: {
        Vector3d<T> lhs = getVector3d();
        Vector3d<T> rhs = getVector3d();
        T* output = getScalarPointer();
        *output = lhs.dot(rhs);
        break;
      }
      case DotVariableConstant3d: {
        Vector3d<T> lhs = getVector3d();
        Vector3d<double> rhs = getConstantVector3d();
        T* output = getScalarPointer();
        *output = lhs.dot(rhs);
        break;
      }
      case Cross3d: {
        Vector3d<T> lhs = getVector3d();
        Vector3d<T> rhs = getVector3d();
        Vector3d<T>* output = getVector3dPointer();
        *output = lhs.cross(rhs);
        break;
      }
      case CrossVariableConstant3d: {
        Vector3d<T> lhs = getVector3d();
        Vector3d<double> rhs = getConstantVector3d();
        Vector3d<T>* output = getVector3dPointer();
        *output = lhs.cross(rhs);
        break;
      }
      case Linear3d: {
        const Vector3d<double>* endpoints = getConstantVector3dPointer();
        T parameter = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = interpolateFrom(endpoints[0], endpoints[1], parameter);
        break;
      }
      case Quadratic3d: {
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        T parameter = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = quadraticBezier(controlPoints, parameter);
        break;
      }
      case Cubic3d: {
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        T parameter = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = cubicBezier(controlPoints, parameter);
        break;
      }
      case Quartic3d: {
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        T parameter = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = quarticBezier(controlPoints, parameter);
        break;
      }
      case Quintic3d: {
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        T parameter = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = quinticBezier(controlPoints, parameter);
        break;
      }
      case Bezier3d: {
        int numControlPoints = getInt();
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        T parameter = getScalar();
        Vector3d<T>* output = getVector3dPointer();
        *output = bezier(numControlPoints, controlPoints, parameter);
        break;
      }
      case TransformVector3d: {
        const double* matrix = getConstantScalarPointer();
        Vector3d<T> input = getVector3d();
        Vector3d<T>* output = getVector3dPointer();
        *output = Vector3d(
          matrix[0] * input.x + matrix[3] * input.y + matrix[6] * input.z,
          matrix[1] * input.x + matrix[4] * input.y + matrix[7] * input.z,
          matrix[2] * input.x + matrix[5] * input.y + matrix[8] * input.z
        );
        break;
      }
      case TransformPoint3d: {
        const double* matrix = getConstantScalarPointer();
        Vector3d<T> input = getVector3d();
        Vector3d<T>* output = getVector3dPointer();
        *output = Vector3d(
          matrix[0] * input.x + matrix[3] * input.y + matrix[6] * input.z + matrix[9],
          matrix[1] * input.x + matrix[4] * input.y + matrix[7] * input.z + matrix[10],
          matrix[2] * input.x + matrix[5] * input.y + matrix[8] * input.z + matrix[11]
        );
        break;
      }
      case PlaceVector2d: {
        const double* matrix = getConstantScalarPointer();
        Vector2d<T> input = getVector2d();
        Vector3d<T>* output = getVector3dPointer();
        *output = Vector3d(
          matrix[0] * input.x + matrix[3] * input.y,
          matrix[1] * input.x + matrix[4] * input.y,
          matrix[2] * input.x + matrix[5] * input.y
        );
        break;
      }
      case PlacePoint2d: {
        const double* matrix = getConstantScalarPointer();
        Vector2d<T> input = getVector2d();
        Vector3d<T>* output = getVector3dPointer();
        *output = Vector3d(
          matrix[0] * input.x + matrix[3] * input.y + matrix[6],
          matrix[1] * input.x + matrix[4] * input.y + matrix[7],
          matrix[2] * input.x + matrix[5] * input.y + matrix[8]
        );
        break;
      }
      case Desingularized1d: {
        T t = getScalar();
        T start = getScalar();
        T middle = getScalar();
        T end = getScalar();
        T* output = getScalarPointer();
        if (isStart(t)) {
          *output = start;
        } else if (isEnd(t)) {
          *output = end;
        } else {
          *output = middle;
        }
        break;
      }
      case Desingularized2d: {
        T t = getScalar();
        Vector2d<T> start = getVector2d();
        Vector2d<T> middle = getVector2d();
        Vector2d<T> end = getVector2d();
        Vector2d<T>* output = getVector2dPointer();
        if (isStart(t)) {
          *output = start;
        } else if (isEnd(t)) {
          *output = end;
        } else {
          *output = middle;
        }
        break;
      }
      case Desingularized3d: {
        T t = getScalar();
        Vector3d<T> start = getVector3d();
        Vector3d<T> middle = getVector3d();
        Vector3d<T> end = getVector3d();
        Vector3d<T>* output = getVector3dPointer();
        if (isStart(t)) {
          *output = start;
        } else if (isEnd(t)) {
          *output = end;
        } else {
          *output = middle;
        }
        break;
      }
      case Cube1d: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = cubed(input);
        break;
      }
      case Blend00: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b00(input);
        break;
      }
      case Blend00d1: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b00d1(input);
        break;
      }
      case Blend00d2: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b00d2(input);
        break;
      }
      case Blend00d3: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b00d3(input);
        break;
      }
      case Blend01: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b01(input);
        break;
      }
      case Blend01d1: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b01d1(input);
        break;
      }
      case Blend01d2: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b01d2(input);
        break;
      }
      case Blend01d3: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b01d3(input);
        break;
      }
      case Blend02: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b02(input);
        break;
      }
      case Blend02d1: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b02d1(input);
        break;
      }
      case Blend02d2: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b02d2(input);
        break;
      }
      case Blend02d3: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b02d3(input);
        break;
      }
      case Blend10: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b10(input);
        break;
      }
      case Blend10d1: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b10d1(input);
        break;
      }
      case Blend10d2: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b10d2(input);
        break;
      }
      case Blend10d3: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b10d3(input);
        break;
      }
      case Blend11: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b11(input);
        break;
      }
      case Blend11d1: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b11d1(input);
        break;
      }
      case Blend11d2: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b11d2(input);
        break;
      }
      case Blend11d3: {
        T input = getScalar();
        T* output = getScalarPointer();
        *output = b11d3(input);
        break;
      }
      case Involute2d: {
        int order = getInt();
        Vector2d<double> r = getConstantVector2d();
        double theta1 = getConstantScalar();
        double theta2 = getConstantScalar();
        T parameter = getScalar();
        Vector2d<T>* output = getVector2dPointer();
        *output = involute(order, r, theta1, theta2, parameter);
        break;
      }
      case OPCODE_END: {
        assert(false && "Should never hit dummy OPCODE_END value");
        break;
      }
    }
  }
}

template <class F, class D>
double
solveMonotonic(double tol, F f, D d, double xMin, double xMax, bool pos1, double x1, double x2) {
  double xMid = 0.5 * (x1 + x2);
  if (xMid == x1 || xMid == x2) return xMid; // Found a root by bisection
  double yMid = f(xMid);
  if (yMid == 0.0) return xMid; // Got lucky, root was exactly at the subdomain midpoint
  double x = newtonRaphson(tol, f, d, xMin, xMax, xMid, yMid, 0); // Attempt Newton-Raphson
  if (std::isnan(x)) { // Newton-Raphson did not converge
    if ((yMid > 0) == pos1) {
      // yMid has the same sign as y1, so replace x1 with xMid
      return solveMonotonic(tol, f, d, xMin, xMax, pos1, xMid, x2);
    } else {
      // yMid has the same sign as y2, so replace x2 with xMid
      return solveMonotonic(tol, f, d, xMin, xMax, pos1, x1, xMid);
    }
  } else { // Newton-Raphson converged, so return the found root
    return x;
  }
}

template <class F, class D>
double
newtonRaphson(double tol, F f, D d, double xMin, double xMax, double x1, double y1, int n) {
  if (n > 10) return NAN; // Too many iterations
  double dy1 = d(x1);
  if (dy1 == 0.0) return NAN; // Can't converge if derivative is zero
  double x2 = x1 - y1 / dy1; // Perform Newton step
  if (x2 < xMin || x2 > xMax) { // Check if we stepped outside the given bounds
    // If so, clamp to the given bounds then try one more step
    double xBorder = std::clamp(x2, xMin, xMax);
    double yBorder = f(xBorder);
    double dyBorder = d(xBorder);
    if (dyBorder == 0.0) return NAN;
    x2 = xBorder - yBorder / dyBorder;
    // If even after clamping we stepped outside the bounds again,
    // then return NaN to indicate divergence
    if (x2 < xMin || x2 > xMax) return NAN;
    // Otherwise, x2 is now a valid point inside the bounds
    // so we can continue with the logic below
  }
  double y2 = f(x2);
  double y1Abs = std::abs(y1);
  double y2Abs = std::abs(y2);
  if (y2Abs >= y1Abs) { // Check if we've stopped converging
    // We've stopped converging,
    // so either return the root if we've converged to one
    // or return NaN if we're diverging
    return y1Abs <= tol ? x1 : NAN;
  } else {
    // We're still converging, so keep going
    return newtonRaphson(tol, f, d, xMin, xMax, x2, y2, n + 1);
  }
}

struct Function {
  const double* constantsPointer;
  const uint16_t* wordsPointer;
  int numVariableComponents;

  inline Function(const char* functionPointer) {
    int numConstantComponents = *(const std::uint16_t*)functionPointer;
    this->numVariableComponents = *(const std::uint16_t*)(functionPointer + 2);
    this->constantsPointer = (const double*)(functionPointer + 8);
    this->wordsPointer = (const std::uint16_t*)(this->constantsPointer + numConstantComponents);
  }
};

extern "C" {
  void
  opensolid_curve_value(const char* functionPointer, double t, double* returnValuesPointer) {
    Function function(functionPointer);
    double* variablesPointer = (double*)alloca(sizeof(double) * function.numVariableComponents);
    variablesPointer[0] = t;
    evaluate(
      function.wordsPointer,
      function.constantsPointer,
      variablesPointer,
      returnValuesPointer
    );
  }

  void
  opensolid_curve_bounds(
    const char* functionPointer,
    double tLower,
    double tUpper,
    double* returnValuesPointer
  ) {
    Function function(functionPointer);
    Bounds* variablesPointer = (Bounds*)alloca(sizeof(Bounds) * function.numVariableComponents);
    variablesPointer[0] = Bounds(tLower, tUpper);
    evaluate(
      function.wordsPointer,
      function.constantsPointer,
      variablesPointer,
      (Bounds*)returnValuesPointer
    );
  }

  void
  opensolid_surface_value(
    const char* functionPointer,
    double u,
    double v,
    double* returnValuesPointer
  ) {
    Function function(functionPointer);
    double* variablesPointer = (double*)alloca(sizeof(double) * function.numVariableComponents);
    variablesPointer[0] = u;
    variablesPointer[1] = v;
    evaluate(
      function.wordsPointer,
      function.constantsPointer,
      variablesPointer,
      returnValuesPointer
    );
  }

  void
  opensolid_surface_bounds(
    const char* functionPointer,
    double uLower,
    double uUpper,
    double vLower,
    double vUpper,
    double* returnValuesPointer
  ) {
    Function function(functionPointer);
    Bounds* variablesPointer = (Bounds*)alloca(sizeof(Bounds) * function.numVariableComponents);
    variablesPointer[0] = Bounds(uLower, uUpper);
    variablesPointer[1] = Bounds(vLower, vUpper);
    evaluate(
      function.wordsPointer,
      function.constantsPointer,
      variablesPointer,
      (Bounds*)returnValuesPointer
    );
  }

  double
  opensolid_solve_monotonic_surface_u(
    double tol,
    const char* functionPointer,
    const char* derivativePointer,
    double u1,
    double u2,
    double v
  ) {
    Function function(functionPointer);
    Function derivative(derivativePointer);
    double* functionVariablesPointer =
      (double*)alloca(sizeof(double) * function.numVariableComponents);
    double* derivativeVariablesPointer =
      (double*)alloca(sizeof(double) * derivative.numVariableComponents);
    auto f = [function, functionVariablesPointer, v](double u) {
      functionVariablesPointer[0] = u;
      functionVariablesPointer[1] = v;
      double result;
      evaluate(function.wordsPointer, function.constantsPointer, functionVariablesPointer, &result);
      return result;
    };
    auto d = [derivative, derivativeVariablesPointer, v](double u) {
      derivativeVariablesPointer[0] = u;
      derivativeVariablesPointer[1] = v;
      double result;
      evaluate(
        derivative.wordsPointer,
        derivative.constantsPointer,
        derivativeVariablesPointer,
        &result
      );
      return result;
    };
    double f1 = f(u1);
    double f2 = f(u2);
    if (f1 >= 0.0 && f2 >= 0.0) {
      return f1 <= f2 ? u1 : u2;
    } else if (f1 <= 0.0 && f2 <= 0.0) {
      return f1 >= f2 ? u1 : u2;
    } else {
      return solveMonotonic(tol, f, d, u1, u2, f1 > 0, u1, u2);
    }
  }

  double
  opensolid_solve_monotonic_surface_v(
    double tol,
    const char* functionPointer,
    const char* derivativePointer,
    double u,
    double v1,
    double v2
  ) {
    Function function(functionPointer);
    Function derivative(derivativePointer);
    double* functionVariablesPointer =
      (double*)alloca(sizeof(double) * function.numVariableComponents);
    double* derivativeVariablesPointer =
      (double*)alloca(sizeof(double) * derivative.numVariableComponents);
    auto f = [function, functionVariablesPointer, u](double v) {
      functionVariablesPointer[0] = u;
      functionVariablesPointer[1] = v;
      double result;
      evaluate(function.wordsPointer, function.constantsPointer, functionVariablesPointer, &result);
      return result;
    };
    auto d = [derivative, derivativeVariablesPointer, u](double v) {
      derivativeVariablesPointer[0] = u;
      derivativeVariablesPointer[1] = v;
      double result;
      evaluate(
        derivative.wordsPointer,
        derivative.constantsPointer,
        derivativeVariablesPointer,
        &result
      );
      return result;
    };
    double f1 = f(v1);
    double f2 = f(v2);
    if (f1 >= 0.0 && f2 >= 0.0) {
      return f1 <= f2 ? v1 : v2;
    } else if (f1 <= 0.0 && f2 <= 0.0) {
      return f1 >= f2 ? v1 : v2;
    } else {
      return solveMonotonic(tol, f, d, v1, v2, f1 > 0, v1, v2);
    }
  }
}
