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

struct Range {
  double lower;
  double upper;

  inline Range() :
      lower(0.0),
      upper(0.0) {
  }

  inline Range(double value) {
    if (std::isnan(value)) {
      this->lower = -INFINITY;
      this->upper = INFINITY;
    } else {
      this->lower = value;
      this->upper = value;
    }
  }

  inline Range(double a, double b) {
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

  inline static Range
  unbounded() {
    return Range(-INFINITY, INFINITY);
  }

  inline static Range
  hull2(double a, double b) {
    double min = std::min(a, b);
    double max = std::max(a, b);
    return Range(min, max);
  }

  inline static Range
  hull3(double a, double b, double c) {
    double min = std::min(std::min(a, b), c);
    double max = std::max(std::max(a, b), c);
    return Range(min, max);
  }

  inline static Range
  hull4(double a, double b, double c, double d) {
    double min = std::min(std::min(std::min(a, b), c), d);
    double max = std::max(std::max(std::max(a, b), c), d);
    return Range(min, max);
  }

  inline static Range
  hull5(double a, double b, double c, double d, double e) {
    double min = std::min(std::min(std::min(std::min(a, b), c), d), e);
    double max = std::max(std::max(std::max(std::max(a, b), c), d), e);
    return Range(min, max);
  }

  inline static Range
  hull6(double a, double b, double c, double d, double e, double f) {
    double min = std::min(std::min(std::min(std::min(std::min(a, b), c), d), e), f);
    double max = std::max(std::max(std::max(std::max(std::max(a, b), c), d), e), f);
    return Range(min, max);
  }

  inline double
  midpoint() const {
    return 0.5 * (lower + upper);
  }

  inline Range
  squared() const {
    double ll = lower * lower;
    double uu = upper * upper;
    if (lower >= 0) {
      return Range(ll, uu);
    } else if (upper <= 0) {
      return Range(uu, ll);
    } else {
      return Range(0.0, std::max(ll, uu));
    }
  }

  inline Range
  sqrt() const {
    return Range(sqrtClamped(lower), sqrtClamped(upper));
  }

  inline Range
  sin() {
    double sinLower = std::sin(lower);
    double sinUpper = std::sin(upper);
    return Range(
      hasSinusoidalExtreme(lower, upper, -PI / 2) ? -1.0 : std::min(sinLower, sinUpper),
      hasSinusoidalExtreme(lower, upper, PI / 2) ? 1.0 : std::max(sinLower, sinUpper)
    );
  }

  inline Range
  cos() {
    double cosLower = std::cos(lower);
    double cosUpper = std::cos(upper);
    return Range(
      hasSinusoidalExtreme(lower, upper, PI) ? -1.0 : std::min(cosLower, cosUpper),
      hasSinusoidalExtreme(lower, upper, 0.0) ? 1.0 : std::max(cosLower, cosUpper)
    );
  }
};

static_assert(sizeof(Range) == 16, "Expected Range to be exactly the size of two double values");

inline Range
operator-(Range arg) {
  return Range(-arg.upper, -arg.lower);
}

inline Range
operator+(Range lhs, double rhs) {
  return Range(lhs.lower + rhs, lhs.upper + rhs);
}

inline Range
operator+(Range lhs, Range rhs) {
  return Range(lhs.lower + rhs.lower, lhs.upper + rhs.upper);
}

inline Range
operator-(Range lhs, Range rhs) {
  return Range(lhs.lower - rhs.upper, lhs.upper - rhs.lower);
}

inline Range
operator-(Range lhs, double rhs) {
  return Range(lhs.lower - rhs, lhs.upper - rhs);
}

inline Range
operator-(double lhs, Range rhs) {
  return Range(lhs - rhs.upper, lhs - rhs.lower);
}

inline Range
operator*(Range lhs, Range rhs) {
  double ll = lhs.lower * rhs.lower;
  double lu = lhs.lower * rhs.upper;
  double ul = lhs.upper * rhs.lower;
  double uu = lhs.upper * rhs.upper;
  double lower = std::min(std::min(std::min(ll, lu), ul), uu);
  double upper = std::max(std::max(std::max(ll, lu), ul), uu);
  return Range(lower, upper);
}

inline Range
operator*(Range lhs, double rhs) {
  return Range::hull2(lhs.lower * rhs, lhs.upper * rhs);
}

inline Range
operator*(double lhs, Range rhs) {
  return Range::hull2(lhs * rhs.lower, lhs * rhs.upper);
}

inline Range
operator/(Range lhs, Range rhs) {
  if (rhs.lower > 0.0 || rhs.upper < 0.0) {
    double ll = lhs.lower / rhs.lower;
    double lu = lhs.lower / rhs.upper;
    double ul = lhs.upper / rhs.lower;
    double uu = lhs.upper / rhs.upper;
    double lower = std::min(std::min(std::min(ll, lu), ul), uu);
    double upper = std::max(std::max(std::max(ll, lu), ul), uu);
    return Range(lower, upper);
  } else {
    return Range::unbounded();
  }
}

inline Range
operator/(double lhs, Range rhs) {
  if (rhs.lower > 0.0 || rhs.upper < 0.0) {
    return Range::hull2(lhs / rhs.lower, lhs / rhs.upper);
  } else {
    return Range::unbounded();
  }
}
