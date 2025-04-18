#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstring>

#include "bytecode.h"
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
  if (n == 1) {
    return *p;
  }
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
      case XComponent: {
        const double* vec = getVariablePointer();
        double* output = getVariablePointer();
        *output = vec[0];
        break;
      }
      case YComponent: {
        const double* vec = getVariablePointer();
        double* output = getVariablePointer();
        *output = vec[1];
        break;
      }
      case ZComponent: {
        const double* vec = getVariablePointer();
        double* output = getVariablePointer();
        *output = vec[2];
        break;
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
      case Square1d: {
        double input = *getVariablePointer();
        double* output = getVariablePointer();
        *output = input * input;
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
      case XY2d: {
        double x = *getVariablePointer();
        double y = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        break;
      }
      case XC2d: {
        double x = *getVariablePointer();
        double y = *getConstantPointer();
        double* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        break;
      }
      case CY2d: {
        double x = *getConstantPointer();
        double y = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        break;
      }
      case Negate2d: {
        const double* input = getVariablePointer();
        double* output = getVariablePointer();
        output[0] = -input[0];
        output[1] = -input[1];
        break;
      }
      case Add2d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] + rhs[0];
        output[1] = lhs[1] + rhs[1];
        break;
      }
      case AddVariableConstant2d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] + rhs[0];
        output[1] = lhs[1] + rhs[1];
        break;
      }
      case Subtract2d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] - rhs[0];
        output[1] = lhs[1] - rhs[1];
        break;
      }
      case SubtractConstantVariable2d: {
        const double* lhs = getConstantPointer();
        const double* rhs = getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] - rhs[0];
        output[1] = lhs[1] - rhs[1];
        break;
      }
      case Multiply2d: {
        const double* lhs = getVariablePointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        break;
      }
      case MultiplyVariableConstant2d: {
        const double* lhs = getVariablePointer();
        double rhs = *getConstantPointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        break;
      }
      case MultiplyConstantVariable2d: {
        const double* lhs = getConstantPointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        break;
      }
      case Divide2d: {
        const double* lhs = getVariablePointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] / rhs;
        output[1] = lhs[1] / rhs;
        break;
      }
      case DivideConstantVariable2d: {
        const double* lhs = getConstantPointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] / rhs;
        output[1] = lhs[1] / rhs;
        break;
      }
      case SquaredMagnitude2d: {
        const double* arg = getVariablePointer();
        double* output = getVariablePointer();
        double x = arg[0];
        double y = arg[1];
        *output = x * x + y * y;
        break;
      }
      case Magnitude2d: {
        const double* arg = getVariablePointer();
        double* output = getVariablePointer();
        double x = arg[0];
        double y = arg[1];
        *output = std::sqrt(x * x + y * y);
        break;
      }
      case Dot2d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getVariablePointer();
        double* output = getVariablePointer();
        *output = lhs[0] * rhs[0] + lhs[1] * rhs[1];
        break;
      }
      case DotVariableConstant2d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        double* output = getVariablePointer();
        *output = lhs[0] * rhs[0] + lhs[1] * rhs[1];
        break;
      }
      case Cross2d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getVariablePointer();
        double* output = getVariablePointer();
        *output = lhs[0] * rhs[1] - lhs[1] * rhs[0];
        break;
      }
      case CrossVariableConstant2d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        double* output = getVariablePointer();
        *output = lhs[0] * rhs[1] - lhs[1] * rhs[0];
        break;
      }
      case Linear2d: {
        const double* endpoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = endpoints;
        const double* y = endpoints + 2;
        output[0] = lerp(x[0], x[1], parameter);
        output[1] = lerp(y[0], y[1], parameter);
        break;
      }
      case Quadratic2d: {
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 3;
        output[0] = quadraticValue(x, parameter);
        output[1] = quadraticValue(y, parameter);
        break;
      }
      case Cubic2d: {
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 4;
        output[0] = cubicValue(x, parameter);
        output[1] = cubicValue(y, parameter);
        break;
      }
      case Quartic2d: {
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 5;
        output[0] = quarticValue(x, parameter);
        output[1] = quarticValue(y, parameter);
        break;
      }
      case Quintic2d: {
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 6;
        output[0] = quinticValue(x, parameter);
        output[1] = quinticValue(y, parameter);
        break;
      }
      case Bezier2d: {
        int n = getInt();
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + n;
        output[0] = bezierValue(n, x, parameter);
        output[1] = bezierValue(n, y, parameter);
        break;
      }
      case TransformVector2d: {
        const double* matrix = getConstantPointer();
        const double* input = getVariablePointer();
        double* output = getVariablePointer();
        double vx = input[0];
        double vy = input[1];
        output[0] = matrix[0] * vx + matrix[2] * vy;
        output[1] = matrix[1] * vx + matrix[3] * vy;
        break;
      }
      case TransformPoint2d: {
        const double* matrix = getConstantPointer();
        const double* input = getVariablePointer();
        double* output = getVariablePointer();
        double px = input[0];
        double py = input[1];
        output[0] = matrix[0] * px + matrix[2] * py + matrix[4];
        output[1] = matrix[1] * px + matrix[3] * py + matrix[5];
        break;
      }
      case ProjectVector3d: {
        const double* matrix = getConstantPointer();
        const double* input = getVariablePointer();
        double* output = getVariablePointer();
        double vx = input[0];
        double vy = input[1];
        double vz = input[2];
        output[0] = matrix[0] * vx + matrix[1] * vy + matrix[2] * vz;
        output[1] = matrix[3] * vx + matrix[4] * vy + matrix[5] * vz;
        break;
      }
      case ProjectPoint3d: {
        const double* matrix = getConstantPointer();
        const double* input = getVariablePointer();
        double* output = getVariablePointer();
        double dx = input[0] - matrix[6];
        double dy = input[1] - matrix[7];
        double dz = input[2] - matrix[8];
        output[0] = matrix[0] * dx + matrix[1] * dy + matrix[2] * dz;
        output[1] = matrix[3] * dx + matrix[4] * dy + matrix[5] * dz;
        break;
      }
      case XYZ3d: {
        double x = *getVariablePointer();
        double y = *getVariablePointer();
        double z = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        output[2] = z;
        break;
      }
      case XYC3d: {
        double x = *getVariablePointer();
        double y = *getVariablePointer();
        double z = *getConstantPointer();
        double* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        output[2] = z;
        break;
      }
      case XCZ3d: {
        double x = *getVariablePointer();
        double y = *getConstantPointer();
        double z = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        output[2] = z;
        break;
      }
      case CYZ3d: {
        double x = *getConstantPointer();
        double y = *getVariablePointer();
        double z = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        output[2] = z;
        break;
      }
      case XCC3d: {
        double x = *getVariablePointer();
        double y = *getConstantPointer();
        double z = *getConstantPointer();
        double* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        output[2] = z;
        break;
      }
      case CYC3d: {
        double x = *getConstantPointer();
        double y = *getVariablePointer();
        double z = *getConstantPointer();
        double* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        output[2] = z;
        break;
      }
      case CCZ3d: {
        double x = *getConstantPointer();
        double y = *getConstantPointer();
        double z = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        output[2] = z;
        break;
      }
      case Negate3d: {
        const double* input = getVariablePointer();
        double* output = getVariablePointer();
        output[0] = -input[0];
        output[1] = -input[1];
        output[2] = -input[2];
        break;
      }
      case Add3d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] + rhs[0];
        output[1] = lhs[1] + rhs[1];
        output[2] = lhs[2] + rhs[2];
        break;
      }
      case AddVariableConstant3d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] + rhs[0];
        output[1] = lhs[1] + rhs[1];
        output[2] = lhs[2] + rhs[2];
        break;
      }
      case Subtract3d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] - rhs[0];
        output[1] = lhs[1] - rhs[1];
        output[2] = lhs[2] - rhs[2];
        break;
      }
      case SubtractConstantVariable3d: {
        const double* lhs = getConstantPointer();
        const double* rhs = getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] - rhs[0];
        output[1] = lhs[1] - rhs[1];
        output[2] = lhs[2] - rhs[2];
        break;
      }
      case Multiply3d: {
        const double* lhs = getVariablePointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        output[2] = lhs[2] * rhs;
        break;
      }
      case MultiplyVariableConstant3d: {
        const double* lhs = getVariablePointer();
        double rhs = *getConstantPointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        output[2] = lhs[2] * rhs;
        break;
      }
      case MultiplyConstantVariable3d: {
        const double* lhs = getConstantPointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        output[2] = lhs[2] * rhs;
        break;
      }
      case Divide3d: {
        const double* lhs = getVariablePointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] / rhs;
        output[1] = lhs[1] / rhs;
        output[2] = lhs[2] / rhs;
        break;
      }
      case DivideConstantVariable3d: {
        const double* lhs = getConstantPointer();
        double rhs = *getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[0] / rhs;
        output[1] = lhs[1] / rhs;
        output[2] = lhs[2] / rhs;
        break;
      }
      case SquaredMagnitude3d: {
        const double* arg = getVariablePointer();
        double* output = getVariablePointer();
        double x = arg[0];
        double y = arg[1];
        double z = arg[2];
        *output = x * x + y * y + z * z;
        break;
      }
      case Magnitude3d: {
        const double* arg = getVariablePointer();
        double* output = getVariablePointer();
        double x = arg[0];
        double y = arg[1];
        double z = arg[2];
        *output = std::sqrt(x * x + y * y + z * z);
        break;
      }
      case Dot3d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getVariablePointer();
        double* output = getVariablePointer();
        *output = lhs[0] * rhs[0] + lhs[1] * rhs[1] + lhs[2] * rhs[2];
        break;
      }
      case DotVariableConstant3d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        double* output = getVariablePointer();
        *output = lhs[0] * rhs[0] + lhs[1] * rhs[1] + lhs[2] * rhs[2];
        break;
      }
      case Cross3d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getVariablePointer();
        double* output = getVariablePointer();
        output[0] = lhs[1] * rhs[2] - lhs[2] * rhs[1];
        output[1] = lhs[2] * rhs[0] - lhs[0] * rhs[2];
        output[2] = lhs[0] * rhs[1] - lhs[1] * rhs[0];
        break;
      }
      case CrossVariableConstant3d: {
        const double* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        double* output = getVariablePointer();
        output[0] = lhs[1] * rhs[2] - lhs[2] * rhs[1];
        output[1] = lhs[2] * rhs[0] - lhs[0] * rhs[2];
        output[2] = lhs[0] * rhs[1] - lhs[1] * rhs[0];
        break;
      }
      case Linear3d: {
        const double* endpoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = endpoints;
        const double* y = endpoints + 2;
        const double* z = endpoints + 4;
        output[0] = lerp(x[0], x[1], parameter);
        output[1] = lerp(y[0], y[1], parameter);
        output[2] = lerp(z[0], z[1], parameter);
        break;
      }
      case Quadratic3d: {
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 3;
        const double* z = controlPoints + 6;
        output[0] = quadraticValue(x, parameter);
        output[1] = quadraticValue(y, parameter);
        output[2] = quadraticValue(z, parameter);
        break;
      }
      case Cubic3d: {
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 4;
        const double* z = controlPoints + 8;
        output[0] = cubicValue(x, parameter);
        output[1] = cubicValue(y, parameter);
        output[2] = cubicValue(z, parameter);
        break;
      }
      case Quartic3d: {
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 5;
        const double* z = controlPoints + 10;
        output[0] = quarticValue(x, parameter);
        output[1] = quarticValue(y, parameter);
        output[2] = quarticValue(z, parameter);
        break;
      }
      case Quintic3d: {
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 6;
        const double* z = controlPoints + 12;
        output[0] = quinticValue(x, parameter);
        output[1] = quinticValue(y, parameter);
        output[2] = quinticValue(z, parameter);
        break;
      }
      case Bezier3d: {
        int n = getInt();
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + n;
        const double* z = controlPoints + n * 2;
        output[0] = bezierValue(n, x, parameter);
        output[1] = bezierValue(n, y, parameter);
        output[2] = bezierValue(n, z, parameter);
        break;
      }
      case TransformVector3d: {
        const double* matrix = getConstantPointer();
        const double* input = getVariablePointer();
        double* output = getVariablePointer();
        double vx = input[0];
        double vy = input[1];
        double vz = input[2];
        output[0] = matrix[0] * vx + matrix[3] * vy + matrix[6] * vz;
        output[1] = matrix[1] * vx + matrix[4] * vy + matrix[7] * vz;
        output[2] = matrix[2] * vx + matrix[5] * vy + matrix[8] * vz;
        break;
      }
      case TransformPoint3d: {
        const double* matrix = getConstantPointer();
        const double* input = getVariablePointer();
        double* output = getVariablePointer();
        double px = input[0];
        double py = input[1];
        double pz = input[2];
        output[0] = matrix[0] * px + matrix[3] * py + matrix[6] * pz + matrix[9];
        output[1] = matrix[1] * px + matrix[4] * py + matrix[7] * pz + matrix[10];
        output[2] = matrix[2] * px + matrix[5] * py + matrix[8] * pz + matrix[11];
        break;
      }
      case PlaceVector2d: {
        const double* matrix = getConstantPointer();
        const double* input = getVariablePointer();
        double* output = getVariablePointer();
        double vx = input[0];
        double vy = input[1];
        output[0] = matrix[0] * vx + matrix[3] * vy;
        output[1] = matrix[1] * vx + matrix[4] * vy;
        output[2] = matrix[2] * vx + matrix[5] * vy;
        break;
      }
      case PlacePoint2d: {
        const double* matrix = getConstantPointer();
        const double* input = getVariablePointer();
        double* output = getVariablePointer();
        double px = input[0];
        double py = input[1];
        output[0] = matrix[0] * px + matrix[3] * py + matrix[6];
        output[1] = matrix[1] * px + matrix[4] * py + matrix[7];
        output[2] = matrix[2] * px + matrix[5] * py + matrix[8];
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
      case XComponent: {
        const Range* vec = getVariablePointer();
        Range* output = getVariablePointer();
        *output = vec[0];
        break;
      }
      case YComponent: {
        const Range* vec = getVariablePointer();
        Range* output = getVariablePointer();
        *output = vec[1];
        break;
      }
      case ZComponent: {
        const Range* vec = getVariablePointer();
        Range* output = getVariablePointer();
        *output = vec[2];
        break;
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
      case Square1d: {
        Range input = *getVariablePointer();
        Range* output = getVariablePointer();
        *output = input.squared();
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
      case XY2d: {
        Range x = *getVariablePointer();
        Range y = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        break;
      }
      case XC2d: {
        Range x = *getVariablePointer();
        double y = *getConstantPointer();
        Range* output = getVariablePointer();
        output[0] = x;
        output[1] = Range::constant(y);
        break;
      }
      case CY2d: {
        double x = *getConstantPointer();
        Range y = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = Range::constant(x);
        output[1] = y;
        break;
      }
      case Negate2d: {
        const Range* input = getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = -input[0];
        output[1] = -input[1];
        break;
      }
      case Add2d: {
        const Range* lhs = getVariablePointer();
        const Range* rhs = getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] + rhs[0];
        output[1] = lhs[1] + rhs[1];
        break;
      }
      case AddVariableConstant2d: {
        const Range* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] + rhs[0];
        output[1] = lhs[1] + rhs[1];
        break;
      }
      case Subtract2d: {
        const Range* lhs = getVariablePointer();
        const Range* rhs = getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] - rhs[0];
        output[1] = lhs[1] - rhs[1];
        break;
      }
      case SubtractConstantVariable2d: {
        const double* lhs = getConstantPointer();
        const Range* rhs = getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] - rhs[0];
        output[1] = lhs[1] - rhs[1];
        break;
      }
      case Multiply2d: {
        const Range* lhs = getVariablePointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        break;
      }
      case MultiplyVariableConstant2d: {
        const Range* lhs = getVariablePointer();
        double rhs = *getConstantPointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        break;
      }
      case MultiplyConstantVariable2d: {
        const double* lhs = getConstantPointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        break;
      }
      case Divide2d: {
        const Range* lhs = getVariablePointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] / rhs;
        output[1] = lhs[1] / rhs;
        break;
      }
      case DivideConstantVariable2d: {
        const double* lhs = getConstantPointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] / rhs;
        output[1] = lhs[1] / rhs;
        break;
      }
      case SquaredMagnitude2d: {
        const Range* arg = getVariablePointer();
        Range* output = getVariablePointer();
        *output = arg[0].squared() + arg[1].squared();
        break;
      }
      case Magnitude2d: {
        const Range* arg = getVariablePointer();
        Range* output = getVariablePointer();
        // TODO add specialized Range.hypot2 for tighter bounds
        *output = (arg[0].squared() + arg[1].squared()).sqrt();
        break;
      }
      case Dot2d: {
        const Range* lhs = getVariablePointer();
        const Range* rhs = getVariablePointer();
        Range* output = getVariablePointer();
        *output = lhs[0] * rhs[0] + lhs[1] * rhs[1];
        break;
      }
      case DotVariableConstant2d: {
        const Range* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        Range* output = getVariablePointer();
        *output = lhs[0] * rhs[0] + lhs[1] * rhs[1];
        break;
      }
      case Cross2d: {
        const Range* lhs = getVariablePointer();
        const Range* rhs = getVariablePointer();
        Range* output = getVariablePointer();
        *output = lhs[0] * rhs[1] - lhs[1] * rhs[0];
        break;
      }
      case CrossVariableConstant2d: {
        const Range* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        Range* output = getVariablePointer();
        *output = lhs[0] * rhs[1] - lhs[1] * rhs[0];
        break;
      }
      case Linear2d: {
        const double* endpoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        const double* x = endpoints;
        const double* y = endpoints + 2;
        output[0] = linearBounds(x, parameter);
        output[1] = linearBounds(y, parameter);
        break;
      }
      case Quadratic2d: {
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 3;
        output[0] = quadraticBounds(x, parameter);
        output[1] = quadraticBounds(y, parameter);
        break;
      }
      case Cubic2d: {
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 4;
        output[0] = cubicBounds(x, parameter);
        output[1] = cubicBounds(y, parameter);
        break;
      }
      case Quartic2d: {
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 5;
        output[0] = quarticBounds(x, parameter);
        output[1] = quarticBounds(y, parameter);
        break;
      }
      case Quintic2d: {
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 6;
        output[0] = quinticBounds(x, parameter);
        output[1] = quinticBounds(y, parameter);
        break;
      }
      case Bezier2d: {
        int n = getInt();
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + n;
        output[0] = bezierBounds(n, x, parameter);
        output[1] = bezierBounds(n, y, parameter);
        break;
      }
      case TransformVector2d: {
        const double* matrix = getConstantPointer();
        const Range* input = getVariablePointer();
        Range* output = getVariablePointer();
        Range vx = input[0];
        Range vy = input[1];
        output[0] = matrix[0] * vx + matrix[2] * vy;
        output[1] = matrix[1] * vx + matrix[3] * vy;
        break;
      }
      case TransformPoint2d: {
        const double* matrix = getConstantPointer();
        const Range* point = getVariablePointer();
        Range* output = getVariablePointer();
        Range px = point[0];
        Range py = point[1];
        output[0] = matrix[0] * px + matrix[2] * py + matrix[4];
        output[1] = matrix[1] * px + matrix[3] * py + matrix[5];
        break;
      }
      case ProjectVector3d: {
        const double* matrix = getConstantPointer();
        const Range* input = getVariablePointer();
        Range* output = getVariablePointer();
        Range vx = input[0];
        Range vy = input[1];
        Range vz = input[2];
        output[0] = matrix[0] * vx + matrix[1] * vy + matrix[2] * vz;
        output[1] = matrix[3] * vx + matrix[4] * vy + matrix[5] * vz;
        break;
      }
      case ProjectPoint3d: {
        const double* matrix = getConstantPointer();
        const Range* input = getVariablePointer();
        Range* output = getVariablePointer();
        Range dx = input[0] - matrix[6];
        Range dy = input[1] - matrix[7];
        Range dz = input[2] - matrix[8];
        output[0] = matrix[0] * dx + matrix[1] * dy + matrix[2] * dz;
        output[1] = matrix[3] * dx + matrix[4] * dy + matrix[5] * dz;
        break;
      }
      case XYZ3d: {
        Range x = *getVariablePointer();
        Range y = *getVariablePointer();
        Range z = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        output[2] = z;
        break;
      }
      case XYC3d: {
        Range x = *getVariablePointer();
        Range y = *getVariablePointer();
        double z = *getConstantPointer();
        Range* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        output[2] = Range::constant(z);
        break;
      }
      case XCZ3d: {
        Range x = *getVariablePointer();
        double y = *getConstantPointer();
        Range z = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = x;
        output[1] = Range::constant(y);
        output[2] = z;
        break;
      }
      case CYZ3d: {
        double x = *getConstantPointer();
        Range y = *getVariablePointer();
        Range z = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = Range::constant(x);
        output[1] = y;
        output[2] = z;
        break;
      }
      case XCC3d: {
        Range x = *getVariablePointer();
        double y = *getConstantPointer();
        double z = *getConstantPointer();
        Range* output = getVariablePointer();
        output[0] = x;
        output[1] = Range::constant(y);
        output[2] = Range::constant(z);
        break;
      }
      case CYC3d: {
        double x = *getConstantPointer();
        Range y = *getVariablePointer();
        double z = *getConstantPointer();
        Range* output = getVariablePointer();
        output[0] = Range::constant(x);
        output[1] = y;
        output[2] = Range::constant(z);
        break;
      }
      case CCZ3d: {
        double x = *getConstantPointer();
        double y = *getConstantPointer();
        Range z = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = Range::constant(x);
        output[1] = Range::constant(y);
        output[2] = z;
        break;
      }
      case Negate3d: {
        const Range* input = getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = -input[0];
        output[1] = -input[1];
        output[2] = -input[2];
        break;
      }
      case Add3d: {
        const Range* lhs = getVariablePointer();
        const Range* rhs = getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] + rhs[0];
        output[1] = lhs[1] + rhs[1];
        output[2] = lhs[2] + rhs[2];
        break;
      }
      case AddVariableConstant3d: {
        const Range* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] + rhs[0];
        output[1] = lhs[1] + rhs[1];
        output[2] = lhs[2] + rhs[2];
        break;
      }
      case Subtract3d: {
        const Range* lhs = getVariablePointer();
        const Range* rhs = getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] - rhs[0];
        output[1] = lhs[1] - rhs[1];
        output[2] = lhs[2] - rhs[2];
        break;
      }
      case SubtractConstantVariable3d: {
        const double* lhs = getConstantPointer();
        const Range* rhs = getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] - rhs[0];
        output[1] = lhs[1] - rhs[1];
        output[2] = lhs[2] - rhs[2];
        break;
      }
      case Multiply3d: {
        const Range* lhs = getVariablePointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        output[2] = lhs[2] * rhs;
        break;
      }
      case MultiplyVariableConstant3d: {
        const Range* lhs = getVariablePointer();
        double rhs = *getConstantPointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        output[2] = lhs[2] * rhs;
        break;
      }
      case MultiplyConstantVariable3d: {
        const double* lhs = getConstantPointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        output[2] = lhs[2] * rhs;
        break;
      }
      case Divide3d: {
        const Range* lhs = getVariablePointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] / rhs;
        output[1] = lhs[1] / rhs;
        output[2] = lhs[2] / rhs;
        break;
      }
      case DivideConstantVariable3d: {
        const double* lhs = getConstantPointer();
        Range rhs = *getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[0] / rhs;
        output[1] = lhs[1] / rhs;
        output[2] = lhs[2] / rhs;
        break;
      }
      case SquaredMagnitude3d: {
        const Range* arg = getVariablePointer();
        Range* output = getVariablePointer();
        *output = arg[0].squared() + arg[1].squared() + arg[2].squared();
        break;
      }
      case Magnitude3d: {
        const Range* arg = getVariablePointer();
        Range* output = getVariablePointer();
        *output = (arg[0].squared() + arg[1].squared() + arg[2].squared()).sqrt();
        break;
      }
      case Dot3d: {
        const Range* lhs = getVariablePointer();
        const Range* rhs = getVariablePointer();
        Range* output = getVariablePointer();
        *output = lhs[0] * rhs[0] + lhs[1] * rhs[1] + lhs[2] * rhs[2];
        break;
      }
      case DotVariableConstant3d: {
        const Range* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        Range* output = getVariablePointer();
        *output = lhs[0] * rhs[0] + lhs[1] * rhs[1] + lhs[2] * rhs[2];
        break;
      }
      case Cross3d: {
        const Range* lhs = getVariablePointer();
        const Range* rhs = getVariablePointer();
        Range* output = getVariablePointer();
        output[0] = lhs[1] * rhs[2] - lhs[2] * rhs[1];
        output[1] = lhs[2] * rhs[0] - lhs[0] * rhs[2];
        output[2] = lhs[0] * rhs[1] - lhs[1] * rhs[0];
        break;
      }
      case CrossVariableConstant3d: {
        const Range* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        Range* output = getVariablePointer();
        output[0] = lhs[1] * rhs[2] - lhs[2] * rhs[1];
        output[1] = lhs[2] * rhs[0] - lhs[0] * rhs[2];
        output[2] = lhs[0] * rhs[1] - lhs[1] * rhs[0];
        break;
      }
      case Linear3d: {
        const double* endpoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        const double* x = endpoints;
        const double* y = endpoints + 2;
        const double* z = endpoints + 4;
        output[0] = linearBounds(x, parameter);
        output[1] = linearBounds(y, parameter);
        output[2] = linearBounds(z, parameter);
        break;
      }
      case Quadratic3d: {
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 3;
        const double* z = controlPoints + 6;
        output[0] = quadraticBounds(x, parameter);
        output[1] = quadraticBounds(y, parameter);
        output[2] = quadraticBounds(z, parameter);
        break;
      }
      case Cubic3d: {
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 4;
        const double* z = controlPoints + 8;
        output[0] = cubicBounds(x, parameter);
        output[1] = cubicBounds(y, parameter);
        output[2] = cubicBounds(z, parameter);
        break;
      }
      case Quartic3d: {
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 5;
        const double* z = controlPoints + 10;
        output[0] = quarticBounds(x, parameter);
        output[1] = quarticBounds(y, parameter);
        output[2] = quarticBounds(z, parameter);
        break;
      }
      case Quintic3d: {
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 6;
        const double* z = controlPoints + 12;
        output[0] = quinticBounds(x, parameter);
        output[1] = quinticBounds(y, parameter);
        output[2] = quinticBounds(z, parameter);
        break;
      }
      case Bezier3d: {
        int n = getInt();
        const double* controlPoints = getConstantPointer();
        Range parameter = *getVariablePointer();
        Range* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + n;
        const double* z = controlPoints + n * 2;
        output[0] = bezierBounds(n, x, parameter);
        output[1] = bezierBounds(n, y, parameter);
        output[2] = bezierBounds(n, z, parameter);
        break;
      }
      case TransformVector3d: {
        const double* matrix = getConstantPointer();
        const Range* input = getVariablePointer();
        Range* output = getVariablePointer();
        Range vx = input[0];
        Range vy = input[1];
        Range vz = input[2];
        output[0] = matrix[0] * vx + matrix[3] * vy + matrix[6] * vz;
        output[1] = matrix[1] * vx + matrix[4] * vy + matrix[7] * vz;
        output[2] = matrix[2] * vx + matrix[5] * vy + matrix[8] * vz;
        break;
      }
      case TransformPoint3d: {
        const double* matrix = getConstantPointer();
        const Range* input = getVariablePointer();
        Range* output = getVariablePointer();
        Range px = input[0];
        Range py = input[1];
        Range pz = input[2];
        output[0] = matrix[0] * px + matrix[3] * py + matrix[6] * pz + matrix[9];
        output[1] = matrix[1] * px + matrix[4] * py + matrix[7] * pz + matrix[10];
        output[2] = matrix[2] * px + matrix[5] * py + matrix[8] * pz + matrix[11];
        break;
      }
      case PlaceVector2d: {
        const double* matrix = getConstantPointer();
        const Range* input = getVariablePointer();
        Range* output = getVariablePointer();
        Range vx = input[0];
        Range vy = input[1];
        output[0] = matrix[0] * vx + matrix[3] * vy;
        output[1] = matrix[1] * vx + matrix[4] * vy;
        output[2] = matrix[2] * vx + matrix[5] * vy;
        break;
      }
      case PlacePoint2d: {
        const double* matrix = getConstantPointer();
        const Range* input = getVariablePointer();
        Range* output = getVariablePointer();
        Range px = input[0];
        Range py = input[1];
        output[0] = matrix[0] * px + matrix[3] * py + matrix[6];
        output[1] = matrix[1] * px + matrix[4] * py + matrix[7];
        output[2] = matrix[2] * px + matrix[5] * py + matrix[8];
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
  opensolid_curve_value(
    const uint16_t* wordsPointer,
    double t,
    const double* constantsPointer,
    int numVariableComponents,
    double* returnValuesPointer
  ) {
    double* variablesPointer = (double*)alloca(sizeof(double) * numVariableComponents);
    variablesPointer[0] = t;
    computeValue(wordsPointer, constantsPointer, variablesPointer, returnValuesPointer);
  }

  void
  opensolid_curve_bounds(
    const uint16_t* wordsPointer,
    double tLower,
    double tUpper,
    const double* constantsPointer,
    int numVariableComponents,
    double* returnValuesPointer
  ) {
    Range* variablesPointer = (Range*)alloca(sizeof(Range) * numVariableComponents);
    variablesPointer[0] = Range(tLower, tUpper);
    computeBounds(wordsPointer, constantsPointer, variablesPointer, (Range*)returnValuesPointer);
  }

  void
  opensolid_surface_value(
    const uint16_t* wordsPointer,
    double u,
    double v,
    const double* constantsPointer,
    int numVariableComponents,
    double* returnValuesPointer
  ) {
    double* variablesPointer = (double*)alloca(sizeof(double) * numVariableComponents);
    variablesPointer[0] = u;
    variablesPointer[1] = v;
    computeValue(wordsPointer, constantsPointer, variablesPointer, returnValuesPointer);
  }

  void
  opensolid_surface_bounds(
    const uint16_t* wordsPointer,
    double uLower,
    double uUpper,
    double vLower,
    double vUpper,
    const double* constantsPointer,
    int numVariableComponents,
    double* returnValuesPointer
  ) {
    Range* variablesPointer = (Range*)alloca(sizeof(Range) * numVariableComponents);
    variablesPointer[0] = Range(uLower, uUpper);
    variablesPointer[1] = Range(vLower, vUpper);
    computeBounds(wordsPointer, constantsPointer, variablesPointer, (Range*)returnValuesPointer);
  }
}
