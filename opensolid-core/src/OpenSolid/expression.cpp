#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstring>

#include "expression.h"
#include "range.h"

inline double
lerp(double a, double b, double t) {
  return a + t * (b - a);
}

inline double
quadraticBlossom(const double* p, double t1, double t2) {
  double q0 = lerp(p[0], p[1], t1);
  double q1 = lerp(p[1], p[2], t1);
  return lerp(q0, q1, t2);
}

inline double
quadraticValue(const double* p, double t) {
  return quadraticBlossom(p, t, t);
}

inline double
cubicBlossom(const double* p, double t1, double t2, double t3) {
  double q[3];
  q[0] = lerp(p[0], p[1], t1);
  q[1] = lerp(p[1], p[2], t1);
  q[2] = lerp(p[2], p[3], t1);
  return quadraticBlossom(q, t2, t3);
}

inline double
cubicValue(const double* p, double t) {
  return cubicBlossom(p, t, t, t);
}

inline double
quarticBlossom(const double* p, double t1, double t2, double t3, double t4) {
  double q[4];
  q[0] = lerp(p[0], p[1], t1);
  q[1] = lerp(p[1], p[2], t1);
  q[2] = lerp(p[2], p[3], t1);
  q[3] = lerp(p[3], p[4], t1);
  return cubicBlossom(q, t2, t3, t4);
}

inline double
quarticValue(const double* p, double t) {
  return quarticBlossom(p, t, t, t, t);
}

inline double
quinticBlossom(const double* p, double t1, double t2, double t3, double t4, double t5) {
  double q[5];
  q[0] = lerp(p[0], p[1], t1);
  q[1] = lerp(p[1], p[2], t1);
  q[2] = lerp(p[2], p[3], t1);
  q[3] = lerp(p[3], p[4], t1);
  q[4] = lerp(p[4], p[5], t1);
  return quarticBlossom(q, t2, t3, t4, t5);
}

inline double
quinticValue(const double* p, double t) {
  return quinticBlossom(p, t, t, t, t, t);
}

inline double
bezierBlossom(int n, const double* p, double tLower, double tUpper, int nLow) {
  double* q = (double*)alloca(sizeof(double) * (n - 1));
  for (int m = n - 1; m > 0; --m) { // m is number of points to collapse to
    double t = m <= nLow ? tLower : tUpper;
    for (int i = 0; i < m; ++i) { // i is index of the point to collapse to
      q[i] = lerp(p[i], p[i + 1], t);
    }
    p = q; // After the first loop iteration, work in place within the outputs
  }
  return *q;
}

inline double
bezierValue(int n, const double* p, double t) {
  return bezierBlossom(n, p, t, t, 0);
}

void
computeValue(
  const uint16_t* wordsPointer,
  const double* constantsPointer,
  double* variablesPointer,
  double* returnValuesPointer
) {
  auto getInt = [&]() -> int {
    int value = *wordsPointer;
    ++wordsPointer;
    return value;
  };
  auto getConstantPointer = [&]() -> const double* {
    return constantsPointer + getInt();
  };
  auto getVariablePointer = [&]() -> double* {
    return variablesPointer + getInt();
  };
  while (true) {
    int opcode = getInt();
    assert(opcode < OPCODE_END && "Unrecognized opcode");
    switch (Opcode(opcode)) {
      case Return: {
        int dimension = getInt();
        double* valuesPointer = getVariablePointer();
        std::memcpy(returnValuesPointer, valuesPointer, sizeof(double) * dimension);
        return;
      }
      case Negate1d: {
        double input = *getVariablePointer();
        double* output = getVariablePointer();
        *output = -input;
        break;
      }
      case Add1d: {
        double lhs = *getVariablePointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        *output = lhs + rhs;
        break;
      }
      case AddVariableConstant1d: {
        double lhs = *getVariablePointer();
        double rhs = *getConstantPointer();
        double* output = getVariablePointer();
        *output = lhs + rhs;
        break;
      }
      case Subtract1d: {
        double lhs = *getVariablePointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        *output = lhs - rhs;
        break;
      }
      case SubtractConstantVariable1d: {
        double lhs = *getConstantPointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        *output = lhs - rhs;
        break;
      }
      case Square1d: {
        double input = *getVariablePointer();
        double* output = getVariablePointer();
        *output = input * input;
        break;
      }
      case Multiply1d: {
        double lhs = *getVariablePointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyVariableConstant1d: {
        double lhs = *getVariablePointer();
        double rhs = *getConstantPointer();
        double* output = getVariablePointer();
        *output = lhs * rhs;
        break;
      }
      case Divide1d: {
        double lhs = *getVariablePointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        *output = lhs / rhs;
        break;
      }
      case DivideConstantVariable1d: {
        double lhs = *getConstantPointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        *output = lhs / rhs;
        break;
      }
      case Sqrt1d: {
        double input = *getVariablePointer();
        double* output = getVariablePointer();
        *output = std::sqrt(input);
        break;
      }
      case Sin1d: {
        double input = *getVariablePointer();
        double* output = getVariablePointer();
        *output = std::sin(input);
        break;
      }
      case Cos1d: {
        double input = *getVariablePointer();
        double* output = getVariablePointer();
        *output = std::cos(input);
        break;
      }
      case Linear1d: {
        const double* endpoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        *output = lerp(endpoints[0], endpoints[1], parameter);
        break;
      }
      case Quadratic1d: {
        const double* controlPoints = getConstantPointer();
        const double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        *output = quadraticValue(controlPoints, parameter);
        break;
      }
      case Cubic1d: {
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        *output = cubicValue(controlPoints, parameter);
        break;
      }
      case Quartic1d: {
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        *output = quarticValue(controlPoints, parameter);
        break;
      }
      case Quintic1d: {
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        *output = quinticValue(controlPoints, parameter);
        break;
      }
      case Bezier1d: {
        int n = getInt();
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        *output = bezierValue(n, controlPoints, parameter);
        break;
      }
      case OPCODE_END: {
        assert(false && "Should never hit dummy OPCODE_END value");
        break;
      }
    }
  }
}

inline Range
linearBounds(const double* p, Range t) {
  double a = lerp(p[0], p[1], t.lower);
  double b = lerp(p[0], p[1], t.upper);
  return Range::hull2(a, b);
}

inline Range
quadraticBounds(const double* p, Range t) {
  double a = quadraticBlossom(p, t.lower, t.lower);
  double b = quadraticBlossom(p, t.lower, t.upper);
  double c = quadraticBlossom(p, t.upper, t.upper);
  return Range::hull3(a, b, c);
}

inline Range
cubicBounds(const double* p, Range t) {
  double a = cubicBlossom(p, t.lower, t.lower, t.lower);
  double b = cubicBlossom(p, t.lower, t.lower, t.upper);
  double c = cubicBlossom(p, t.lower, t.upper, t.upper);
  double d = cubicBlossom(p, t.upper, t.upper, t.upper);
  return Range::hull4(a, b, c, d);
}

inline Range
quarticBounds(const double* p, Range t) {
  double a = quarticBlossom(p, t.lower, t.lower, t.lower, t.lower);
  double b = quarticBlossom(p, t.lower, t.lower, t.lower, t.upper);
  double c = quarticBlossom(p, t.lower, t.lower, t.upper, t.upper);
  double d = quarticBlossom(p, t.lower, t.upper, t.upper, t.upper);
  double e = quarticBlossom(p, t.upper, t.upper, t.upper, t.upper);
  return Range::hull5(a, b, c, d, e);
}

inline Range
quinticBounds(const double* p, Range t) {
  double a = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.lower);
  double b = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.upper);
  double c = quinticBlossom(p, t.lower, t.lower, t.lower, t.upper, t.upper);
  double d = quinticBlossom(p, t.lower, t.lower, t.upper, t.upper, t.upper);
  double e = quinticBlossom(p, t.lower, t.upper, t.upper, t.upper, t.upper);
  double f = quinticBlossom(p, t.upper, t.upper, t.upper, t.upper, t.upper);
  return Range::hull6(a, b, c, d, e, f);
}

inline Range
bezierBounds(int n, const double* p, Range t) {
  double* hullPoints = (double*)alloca(sizeof(double) * n);
  for (int i = 0; i < n; ++i) {
    hullPoints[i] = bezierBlossom(n, p, t.lower, t.upper, i);
  }
  std::pair<double*, double*> bounds = std::minmax_element(hullPoints, hullPoints + n);
  return Range(*bounds.first, *bounds.second);
}

void
computeBounds(
  const uint16_t* wordsPointer,
  const double* constantsPointer,
  Range* variablesPointer,
  Range* returnValuesPointer
) {
  auto getInt = [&]() -> int {
    int value = *wordsPointer;
    ++wordsPointer;
    return value;
  };
  auto getConstantPointer = [&]() -> const double* {
    return constantsPointer + getInt();
  };
  auto getVariablePointer = [&]() -> Range* {
    return variablesPointer + getInt();
  };
  while (true) {
    int opcode = getInt();
    assert(opcode < OPCODE_END && "Unrecognized opcode");
    switch (Opcode(opcode)) {
      case Return: {
        int dimension = getInt();
        Range* valuesPointer = getVariablePointer();
        std::memcpy(returnValuesPointer, valuesPointer, sizeof(Range) * dimension);
        return;
      }
      case Negate1d: {
        Range input = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = -input;
        break;
      }
      case Add1d: {
        Range lhs = *getVariablePointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = lhs + rhs;
        break;
      }
      case AddVariableConstant1d: {
        Range lhs = *getVariablePointer();
        double rhs = *getConstantPointer();
        Range* output = getVariablePointer();
        *output = lhs + rhs;
        break;
      }
      case Subtract1d: {
        Range lhs = *getVariablePointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = lhs - rhs;
        break;
      }
      case SubtractConstantVariable1d: {
        double lhs = *getConstantPointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = lhs - rhs;
        break;
      }
      case Square1d: {
        Range input = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = input.squared();
        break;
      }
      case Multiply1d: {
        Range lhs = *getVariablePointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyVariableConstant1d: {
        Range lhs = *getVariablePointer();
        double rhs = *getConstantPointer();
        Range* output = getVariablePointer();
        *output = lhs * rhs;
        break;
      }
      case Divide1d: {
        Range lhs = *getVariablePointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = lhs / rhs;
        break;
      }
      case DivideConstantVariable1d: {
        double lhs = *getConstantPointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = lhs / rhs;
        break;
      }
      case Sqrt1d: {
        Range input = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = input.sqrt();
        break;
      }
      case Sin1d: {
        Range input = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = input.sin();
        break;
      }
      case Cos1d: {
        Range input = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = input.cos();
        break;
      }
      case Linear1d: {
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = linearBounds(controlPoints, parameter);
        break;
      }
      case Quadratic1d: {
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = quadraticBounds(controlPoints, parameter);
        break;
      }
      case Cubic1d: {
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = cubicBounds(controlPoints, parameter);
        break;
      }
      case Quartic1d: {
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = quarticBounds(controlPoints, parameter);
        break;
      }
      case Quintic1d: {
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = quinticBounds(controlPoints, parameter);
        break;
      }
      case Bezier1d: {
        int n = getInt();
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = bezierBounds(n, controlPoints, parameter);
        break;
      }
      case OPCODE_END: {
        assert(false && "Should never hit dummy OPCODE_END value");
        break;
      }
    }
  }
}

extern "C" {
  void
  opensolid_curve1d_value(
    const uint16_t* wordsPointer,
    double t,
    const double* constantsPointer,
    int numVariables,
    double* returnValuesPointer
  ) {
    double* variablesPointer = (double*)alloca(sizeof(double) * numVariables);
    variablesPointer[0] = t;
    computeValue(wordsPointer, constantsPointer, variablesPointer, returnValuesPointer);
  }

  void
  opensolid_curve1d_bounds(
    const uint16_t* wordsPointer,
    double tLower,
    double tUpper,
    const double* constantsPointer,
    int numVariables,
    double* returnValuesPointer
  ) {
    Range* variablesPointer = (Range*)alloca(sizeof(Range) * numVariables);
    variablesPointer[0] = Range(tLower, tUpper);
    computeBounds(wordsPointer, constantsPointer, variablesPointer, (Range*)returnValuesPointer);
  }

  void
  opensolid_surface1d_value(
    const uint16_t* wordsPointer,
    double u,
    double v,
    const double* constantsPointer,
    int numVariables,
    double* returnValuesPointer
  ) {
    double* variablesPointer = (double*)alloca(sizeof(double) * numVariables);
    variablesPointer[0] = u;
    variablesPointer[1] = v;
    computeValue(wordsPointer, constantsPointer, variablesPointer, returnValuesPointer);
  }

  void
  opensolid_surface1d_bounds(
    const uint16_t* wordsPointer,
    double uLower,
    double uUpper,
    double vLower,
    double vUpper,
    const double* constantsPointer,
    int numVariables,
    double* returnValuesPointer
  ) {
    Range* variablesPointer = (Range*)alloca(sizeof(Range) * numVariables);
    variablesPointer[0] = Range(uLower, uUpper);
    variablesPointer[1] = Range(vLower, vUpper);
    computeBounds(wordsPointer, constantsPointer, variablesPointer, (Range*)returnValuesPointer);
  }
}
