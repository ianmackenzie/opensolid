#pragma once

#include <cmath>

#define PI 3.14159265358979323846

inline bool
hasSinusoidalExtreme(double lower, double upper, double location) {
  int lowerIndex = std::floor((lower - location) / (2.0 * PI));
  int upperIndex = std::floor((upper - location) / (2.0 * PI));
  return lowerIndex != upperIndex;
}

inline double
sqrtClamped(double value) {
  return std::sqrt(std::max(value, 0.0));
}

struct Bounds {
  double lower;
  double upper;

  inline Bounds() :
      lower(0.0),
      upper(0.0) {
  }

  inline Bounds(double value) {
    if (std::isnan(value)) {
      this->lower = -INFINITY;
      this->upper = INFINITY;
    } else {
      this->lower = value;
      this->upper = value;
    }
  }

  inline Bounds(double a, double b) {
    if (std::isnan(a) || std::isnan(b)) {
      this->lower = -INFINITY;
      this->upper = INFINITY;
    } else if (a <= b) {
      this->lower = a;
      this->upper = b;
    } else {
      this->lower = b;
      this->upper = a;
    }
  }

  inline static Bounds
  infinite() {
    return Bounds(-INFINITY, INFINITY);
  }

  inline static Bounds
  invalid() {
    return Bounds(NAN, NAN);
  }

  inline static Bounds
  hull2(double a, double b) {
    double min = std::min(a, b);
    double max = std::max(a, b);
    return Bounds(min, max);
  }

  inline static Bounds
  hull3(double a, double b, double c) {
    double min = std::min(std::min(a, b), c);
    double max = std::max(std::max(a, b), c);
    return Bounds(min, max);
  }

  inline static Bounds
  hull4(double a, double b, double c, double d) {
    double min = std::min(std::min(std::min(a, b), c), d);
    double max = std::max(std::max(std::max(a, b), c), d);
    return Bounds(min, max);
  }

  inline static Bounds
  hull5(double a, double b, double c, double d, double e) {
    double min = std::min(std::min(std::min(std::min(a, b), c), d), e);
    double max = std::max(std::max(std::max(std::max(a, b), c), d), e);
    return Bounds(min, max);
  }

  inline static Bounds
  hull6(double a, double b, double c, double d, double e, double f) {
    double min = std::min(std::min(std::min(std::min(std::min(a, b), c), d), e), f);
    double max = std::max(std::max(std::max(std::max(std::max(a, b), c), d), e), f);
    return Bounds(min, max);
  }

  inline double
  midpoint() const {
    return 0.5 * (lower + upper);
  }

  inline Bounds
  squared() const {
    double ll = lower * lower;
    double uu = upper * upper;
    if (lower >= 0) {
      return Bounds(ll, uu);
    } else if (upper <= 0) {
      return Bounds(uu, ll);
    } else {
      return Bounds(0.0, std::max(ll, uu));
    }
  }

  inline Bounds
  sqrt() const {
    return Bounds(sqrtClamped(lower), sqrtClamped(upper));
  }

  inline Bounds
  cubed() const {
    return Bounds(lower * lower * lower, upper * upper * upper);
  }

  inline Bounds
  sin() {
    double sinLower = std::sin(lower);
    double sinUpper = std::sin(upper);
    return Bounds(
      hasSinusoidalExtreme(lower, upper, -PI / 2) ? -1.0 : std::min(sinLower, sinUpper),
      hasSinusoidalExtreme(lower, upper, PI / 2) ? 1.0 : std::max(sinLower, sinUpper)
    );
  }

  inline Bounds
  cos() {
    double cosLower = std::cos(lower);
    double cosUpper = std::cos(upper);
    return Bounds(
      hasSinusoidalExtreme(lower, upper, PI) ? -1.0 : std::min(cosLower, cosUpper),
      hasSinusoidalExtreme(lower, upper, 0.0) ? 1.0 : std::max(cosLower, cosUpper)
    );
  }
};

static_assert(sizeof(Bounds) == 16, "Expected Bounds to be exactly the size of two double values");

inline Bounds
operator-(Bounds arg) {
  return Bounds(-arg.upper, -arg.lower);
}

inline Bounds
operator+(Bounds lhs, double rhs) {
  return Bounds(lhs.lower + rhs, lhs.upper + rhs);
}

inline Bounds
operator+(Bounds lhs, Bounds rhs) {
  return Bounds(lhs.lower + rhs.lower, lhs.upper + rhs.upper);
}

inline Bounds
operator-(Bounds lhs, Bounds rhs) {
  return Bounds(lhs.lower - rhs.upper, lhs.upper - rhs.lower);
}

inline Bounds
operator-(Bounds lhs, double rhs) {
  return Bounds(lhs.lower - rhs, lhs.upper - rhs);
}

inline Bounds
operator-(double lhs, Bounds rhs) {
  return Bounds(lhs - rhs.upper, lhs - rhs.lower);
}

inline Bounds
operator*(Bounds lhs, Bounds rhs) {
  double ll = lhs.lower * rhs.lower;
  double lu = lhs.lower * rhs.upper;
  double ul = lhs.upper * rhs.lower;
  double uu = lhs.upper * rhs.upper;
  double lower = std::min(std::min(std::min(ll, lu), ul), uu);
  double upper = std::max(std::max(std::max(ll, lu), ul), uu);
  return Bounds(lower, upper);
}

inline Bounds
operator*(Bounds lhs, double rhs) {
  return Bounds::hull2(lhs.lower * rhs, lhs.upper * rhs);
}

inline Bounds
operator*(double lhs, Bounds rhs) {
  return Bounds::hull2(lhs * rhs.lower, lhs * rhs.upper);
}

inline Bounds
operator/(Bounds lhs, Bounds rhs) {
  if (rhs.lower > 0.0 || rhs.upper < 0.0) {
    double ll = lhs.lower / rhs.lower;
    double lu = lhs.lower / rhs.upper;
    double ul = lhs.upper / rhs.lower;
    double uu = lhs.upper / rhs.upper;
    double lower = std::min(std::min(std::min(ll, lu), ul), uu);
    double upper = std::max(std::max(std::max(ll, lu), ul), uu);
    return Bounds(lower, upper);
  } else {
    return Bounds::infinite();
  }
}

inline Bounds
operator/(double lhs, Bounds rhs) {
  if (rhs.lower > 0.0 || rhs.upper < 0.0) {
    return Bounds::hull2(lhs / rhs.lower, lhs / rhs.upper);
  } else {
    return Bounds::infinite();
  }
}
