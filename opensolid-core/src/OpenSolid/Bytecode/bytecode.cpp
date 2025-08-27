#include <algorithm>
#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstring>

#include "bounds.h"
#include "bytecode.h"

// Used when evaluating Desingularized#d opcodes,
// to determine whether a given parameter value is at a given endpoint
// or whether a given parameter range includes a given endpoint;
// should be kept in sync with the constants used in Desingularization.hs
#define T0 0.001
#define T1 0.999

inline double
lerp(double a, double b, double t) {
  return a + t * (b - a);
}

inline Bounds
lerp(Bounds a, Bounds b, Bounds t) {
  if (t.midpoint() <= 0.5) {
    return a + t * (b - a);
  } else {
    return b + (1.0 - t) * (a - b);
  }
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

template <class T>
inline T
bezierBlossom(int numControlPoints, const T* controlPoints, T tLower, T tUpper, int nLow) {
  if (numControlPoints == 1) {
    return *controlPoints;
  }
  T* q = (T*)alloca(sizeof(T) * (numControlPoints - 1));
  for (int m = numControlPoints - 1; m > 0; --m) { // m is number of points to collapse to
    T t = m <= nLow ? tLower : tUpper;
    for (int i = 0; i < m; ++i) { // i is index of the point to collapse to
      q[i] = lerp(controlPoints[i], controlPoints[i + 1], t);
    }
    controlPoints = q; // After the first loop iteration, work in place within the outputs
  }
  return *q;
}

template <class T>
inline T
bezierValue(int numControlPoints, const T* controlPoints, T t) {
  return bezierBlossom(numControlPoints, controlPoints, t, t, 0);
}

template <class T>
T
hermiteControlPointOffset(int controlPointIndex, const T* scaledDerivatives) {
  T result{};
  for (int derivativeOrder = 1; derivativeOrder <= controlPointIndex; ++derivativeOrder) {
    int coefficient = choose(controlPointIndex - 1, derivativeOrder - 1);
    result = result + coefficient * scaledDerivatives[derivativeOrder - 1];
  }
  return result;
}

template <class T>
void
hermiteToBezier(
  T startValue,
  int numStartDerivatives,
  T* startDerivatives,
  T endValue,
  int numEndDerivatives,
  T* endDerivatives,
  T* controlPoints
) {
  int curveDegree = numStartDerivatives + numEndDerivatives + 1;
  double scale;
  scale = 1.0;
  for (int i = 0; i < numStartDerivatives; ++i) {
    scale /= (curveDegree - i);
    startDerivatives[i] = startDerivatives[i] * scale;
  }
  scale = 1.0;
  for (int i = 0; i < numEndDerivatives; ++i) {
    scale /= -(curveDegree - i);
    endDerivatives[i] = endDerivatives[i] * scale;
  }
  controlPoints[0] = startValue;
  controlPoints[curveDegree] = endValue;
  for (int forwardIndex = 1; forwardIndex <= numStartDerivatives; ++forwardIndex) {
    T offset = hermiteControlPointOffset(forwardIndex, startDerivatives);
    controlPoints[forwardIndex] = controlPoints[forwardIndex - 1] + offset;
  }
  for (int reverseIndex = 1; reverseIndex <= numEndDerivatives; ++reverseIndex) {
    T offset = hermiteControlPointOffset(reverseIndex, endDerivatives);
    int forwardIndex = curveDegree - reverseIndex;
    controlPoints[forwardIndex] = controlPoints[forwardIndex + 1] + offset;
  }
}

template <class T>
T
blend(
  T startValue,
  int numStartDerivatives,
  T* startDerivatives,
  T endValue,
  int numEndDerivatives,
  T* endDerivatives,
  T t
) {
  int numControlPoints = numStartDerivatives + numEndDerivatives + 2;
  T* controlPoints = (T*)alloca(sizeof(T) * numControlPoints);
  hermiteToBezier(
    startValue,
    numStartDerivatives,
    startDerivatives,
    endValue,
    numEndDerivatives,
    endDerivatives,
    controlPoints
  );
  return bezierValue(numControlPoints, controlPoints, t);
}

double
opensolid_blend_values_1d(
  double startValue,
  int numStartDerivatives,
  double* startDerivatives,
  double endValue,
  int numEndDerivatives,
  double* endDerivatives,
  double t
) {
  return blend(
    startValue,
    numStartDerivatives,
    startDerivatives,
    endValue,
    numEndDerivatives,
    endDerivatives,
    t
  );
}

void
opensolid_blend_bounds_1d(
  double startValueLower,
  double startValueUpper,
  int numStartDerivatives,
  double* startDerivatives,
  double endValueLower,
  double endValueUpper,
  int numEndDerivatives,
  double* endDerivatives,
  double tLower,
  double tUpper,
  double* returnValuesPointer
) {
  Bounds result = blend(
    Bounds(startValueLower, startValueUpper),
    numStartDerivatives,
    (Bounds*)startDerivatives,
    Bounds(endValueLower, endValueUpper),
    numEndDerivatives,
    (Bounds*)endDerivatives,
    Bounds(tLower, tUpper)
  );
  returnValuesPointer[0] = result.lower;
  returnValuesPointer[1] = result.upper;
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
  auto getValue = [&]() -> double {
    int taggedIndex = getInt();
    bool isConstant = taggedIndex >= 32768;
    const double* pointer = isConstant ? constantsPointer : variablesPointer;
    int index = isConstant ? taggedIndex - 32768 : taggedIndex;
    return pointer[index];
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
        *output = std::sqrt(std::max(input, 0.0));
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
        int numControlPoints = getInt();
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        *output = bezierValue(numControlPoints, controlPoints, parameter);
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
        int numControlPoints = getInt();
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + numControlPoints;
        output[0] = bezierValue(numControlPoints, x, parameter);
        output[1] = bezierValue(numControlPoints, y, parameter);
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
        int numControlPoints = getInt();
        const double* controlPoints = getConstantPointer();
        double parameter = *getVariablePointer();
        double* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + numControlPoints;
        const double* z = controlPoints + numControlPoints * 2;
        output[0] = bezierValue(numControlPoints, x, parameter);
        output[1] = bezierValue(numControlPoints, y, parameter);
        output[2] = bezierValue(numControlPoints, z, parameter);
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
      case Desingularized1d: {
        double t = *getVariablePointer();
        double start = *getVariablePointer();
        double middle = *getVariablePointer();
        double end = *getVariablePointer();
        double* output = getVariablePointer();
        if (t <= T0) {
          *output = start;
        } else if (t >= T1) {
          *output = end;
        } else {
          *output = middle;
        }
        break;
      }
      case Desingularized2d: {
        double t = *getVariablePointer();
        double* start = getVariablePointer();
        double* middle = getVariablePointer();
        double* end = getVariablePointer();
        double* output = getVariablePointer();
        if (t <= T0) {
          output[0] = start[0];
          output[1] = start[1];
        } else if (t >= T1) {
          output[0] = end[0];
          output[1] = end[1];
        } else {
          output[0] = middle[0];
          output[1] = middle[1];
        }
        break;
      }
      case Desingularized3d: {
        double t = *getVariablePointer();
        double* start = getVariablePointer();
        double* middle = getVariablePointer();
        double* end = getVariablePointer();
        double* output = getVariablePointer();
        if (t <= T0) {
          output[0] = start[0];
          output[1] = start[1];
          output[2] = start[2];
        } else if (t >= T1) {
          output[0] = end[0];
          output[1] = end[1];
          output[2] = end[2];
        } else {
          output[0] = middle[0];
          output[1] = middle[1];
          output[2] = middle[2];
        }
        break;
      }
      case Blend1d: {
        double startValue = getValue();
        int numStartDerivatives = getInt();
        double* startDerivatives = (double*)alloca(sizeof(double) * numStartDerivatives);
        for (int i = 0; i < numStartDerivatives; ++i) {
          startDerivatives[i] = getValue();
        }
        double endValue = getValue();
        int numEndDerivatives = getInt();
        double* endDerivatives = (double*)alloca(sizeof(double) * numEndDerivatives);
        for (int i = 0; i < numEndDerivatives; ++i) {
          endDerivatives[i] = getValue();
        }
        double t = *getVariablePointer();
        double* output = getVariablePointer();
        *output = blend(
          startValue,
          numStartDerivatives,
          startDerivatives,
          endValue,
          numEndDerivatives,
          endDerivatives,
          t
        );
        break;
      }
      case OPCODE_END: {
        assert(false && "Should never hit dummy OPCODE_END value");
        break;
      }
    }
  }
}

inline Bounds
linearBounds(const double* p, Bounds t) {
  double a = lerp(p[0], p[1], t.lower);
  double b = lerp(p[0], p[1], t.upper);
  return Bounds::hull2(a, b);
}

inline Bounds
quadraticBounds(const double* p, Bounds t) {
  double a = quadraticBlossom(p, t.lower, t.lower);
  double b = quadraticBlossom(p, t.lower, t.upper);
  double c = quadraticBlossom(p, t.upper, t.upper);
  return Bounds::hull3(a, b, c);
}

inline Bounds
cubicBounds(const double* p, Bounds t) {
  double a = cubicBlossom(p, t.lower, t.lower, t.lower);
  double b = cubicBlossom(p, t.lower, t.lower, t.upper);
  double c = cubicBlossom(p, t.lower, t.upper, t.upper);
  double d = cubicBlossom(p, t.upper, t.upper, t.upper);
  return Bounds::hull4(a, b, c, d);
}

inline Bounds
quarticBounds(const double* p, Bounds t) {
  double a = quarticBlossom(p, t.lower, t.lower, t.lower, t.lower);
  double b = quarticBlossom(p, t.lower, t.lower, t.lower, t.upper);
  double c = quarticBlossom(p, t.lower, t.lower, t.upper, t.upper);
  double d = quarticBlossom(p, t.lower, t.upper, t.upper, t.upper);
  double e = quarticBlossom(p, t.upper, t.upper, t.upper, t.upper);
  return Bounds::hull5(a, b, c, d, e);
}

inline Bounds
quinticBounds(const double* p, Bounds t) {
  double a = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.lower);
  double b = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.upper);
  double c = quinticBlossom(p, t.lower, t.lower, t.lower, t.upper, t.upper);
  double d = quinticBlossom(p, t.lower, t.lower, t.upper, t.upper, t.upper);
  double e = quinticBlossom(p, t.lower, t.upper, t.upper, t.upper, t.upper);
  double f = quinticBlossom(p, t.upper, t.upper, t.upper, t.upper, t.upper);
  return Bounds::hull6(a, b, c, d, e, f);
}

inline Bounds
bezierBounds(int numControlPoints, const double* controlPoints, Bounds t) {
  double* hullPoints = (double*)alloca(sizeof(double) * numControlPoints);
  for (int i = 0; i < numControlPoints; ++i) {
    hullPoints[i] = bezierBlossom(numControlPoints, controlPoints, t.lower, t.upper, i);
  }
  std::pair<double*, double*> bounds =
    std::minmax_element(hullPoints, hullPoints + numControlPoints);
  return Bounds(*bounds.first, *bounds.second);
}

void
computeBounds(
  const uint16_t* wordsPointer,
  const double* constantsPointer,
  Bounds* variablesPointer,
  Bounds* returnValuesPointer
) {
  auto getInt = [&]() -> int {
    int value = *wordsPointer;
    ++wordsPointer;
    return value;
  };
  auto getConstantPointer = [&]() -> const double* {
    return constantsPointer + getInt();
  };
  auto getVariablePointer = [&]() -> Bounds* {
    return variablesPointer + getInt();
  };
  auto getValue = [&]() -> Bounds {
    int taggedIndex = getInt();
    bool isConstant = taggedIndex >= 32768;
    if (isConstant) {
      return Bounds(constantsPointer[taggedIndex - 32768]);
    } else {
      return variablesPointer[taggedIndex];
    }
  };
  while (true) {
    int opcode = getInt();
    assert(opcode < OPCODE_END && "Unrecognized opcode");
    switch (Opcode(opcode)) {
      case Return: {
        int dimension = getInt();
        Bounds* valuesPointer = getVariablePointer();
        std::memcpy(returnValuesPointer, valuesPointer, sizeof(Bounds) * dimension);
        return;
      }
      case XComponent: {
        const Bounds* vec = getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = vec[0];
        break;
      }
      case YComponent: {
        const Bounds* vec = getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = vec[1];
        break;
      }
      case ZComponent: {
        const Bounds* vec = getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = vec[2];
        break;
      }
      case Negate1d: {
        Bounds input = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = -input;
        break;
      }
      case Add1d: {
        Bounds lhs = *getVariablePointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = lhs + rhs;
        break;
      }
      case AddVariableConstant1d: {
        Bounds lhs = *getVariablePointer();
        double rhs = *getConstantPointer();
        Bounds* output = getVariablePointer();
        *output = lhs + rhs;
        break;
      }
      case Subtract1d: {
        Bounds lhs = *getVariablePointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = lhs - rhs;
        break;
      }
      case SubtractConstantVariable1d: {
        double lhs = *getConstantPointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = lhs - rhs;
        break;
      }
      case Multiply1d: {
        Bounds lhs = *getVariablePointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyVariableConstant1d: {
        Bounds lhs = *getVariablePointer();
        double rhs = *getConstantPointer();
        Bounds* output = getVariablePointer();
        *output = lhs * rhs;
        break;
      }
      case Divide1d: {
        Bounds lhs = *getVariablePointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = lhs / rhs;
        break;
      }
      case DivideConstantVariable1d: {
        double lhs = *getConstantPointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = lhs / rhs;
        break;
      }
      case Square1d: {
        Bounds input = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = input.squared();
        break;
      }
      case Sqrt1d: {
        Bounds input = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = input.sqrt();
        break;
      }
      case Sin1d: {
        Bounds input = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = input.sin();
        break;
      }
      case Cos1d: {
        Bounds input = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = input.cos();
        break;
      }
      case Linear1d: {
        const double* controlPoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = linearBounds(controlPoints, parameter);
        break;
      }
      case Quadratic1d: {
        const double* controlPoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = quadraticBounds(controlPoints, parameter);
        break;
      }
      case Cubic1d: {
        const double* controlPoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = cubicBounds(controlPoints, parameter);
        break;
      }
      case Quartic1d: {
        const double* controlPoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = quarticBounds(controlPoints, parameter);
        break;
      }
      case Quintic1d: {
        const double* controlPoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = quinticBounds(controlPoints, parameter);
        break;
      }
      case Bezier1d: {
        int numControlPoints = getInt();
        const double* controlPoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = bezierBounds(numControlPoints, controlPoints, parameter);
        break;
      }
      case XY2d: {
        Bounds x = *getVariablePointer();
        Bounds y = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        break;
      }
      case XC2d: {
        Bounds x = *getVariablePointer();
        double y = *getConstantPointer();
        Bounds* output = getVariablePointer();
        output[0] = x;
        output[1] = Bounds(y);
        break;
      }
      case CY2d: {
        double x = *getConstantPointer();
        Bounds y = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = Bounds(x);
        output[1] = y;
        break;
      }
      case Negate2d: {
        const Bounds* input = getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = -input[0];
        output[1] = -input[1];
        break;
      }
      case Add2d: {
        const Bounds* lhs = getVariablePointer();
        const Bounds* rhs = getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] + rhs[0];
        output[1] = lhs[1] + rhs[1];
        break;
      }
      case AddVariableConstant2d: {
        const Bounds* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] + rhs[0];
        output[1] = lhs[1] + rhs[1];
        break;
      }
      case Subtract2d: {
        const Bounds* lhs = getVariablePointer();
        const Bounds* rhs = getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] - rhs[0];
        output[1] = lhs[1] - rhs[1];
        break;
      }
      case SubtractConstantVariable2d: {
        const double* lhs = getConstantPointer();
        const Bounds* rhs = getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] - rhs[0];
        output[1] = lhs[1] - rhs[1];
        break;
      }
      case Multiply2d: {
        const Bounds* lhs = getVariablePointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        break;
      }
      case MultiplyVariableConstant2d: {
        const Bounds* lhs = getVariablePointer();
        double rhs = *getConstantPointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        break;
      }
      case MultiplyConstantVariable2d: {
        const double* lhs = getConstantPointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        break;
      }
      case Divide2d: {
        const Bounds* lhs = getVariablePointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] / rhs;
        output[1] = lhs[1] / rhs;
        break;
      }
      case DivideConstantVariable2d: {
        const double* lhs = getConstantPointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] / rhs;
        output[1] = lhs[1] / rhs;
        break;
      }
      case SquaredMagnitude2d: {
        const Bounds* arg = getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = arg[0].squared() + arg[1].squared();
        break;
      }
      case Magnitude2d: {
        const Bounds* arg = getVariablePointer();
        Bounds* output = getVariablePointer();
        // TODO add specialized Range.hypot2 for tighter bounds
        *output = (arg[0].squared() + arg[1].squared()).sqrt();
        break;
      }
      case Dot2d: {
        const Bounds* lhs = getVariablePointer();
        const Bounds* rhs = getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = lhs[0] * rhs[0] + lhs[1] * rhs[1];
        break;
      }
      case DotVariableConstant2d: {
        const Bounds* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        Bounds* output = getVariablePointer();
        *output = lhs[0] * rhs[0] + lhs[1] * rhs[1];
        break;
      }
      case Cross2d: {
        const Bounds* lhs = getVariablePointer();
        const Bounds* rhs = getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = lhs[0] * rhs[1] - lhs[1] * rhs[0];
        break;
      }
      case CrossVariableConstant2d: {
        const Bounds* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        Bounds* output = getVariablePointer();
        *output = lhs[0] * rhs[1] - lhs[1] * rhs[0];
        break;
      }
      case Linear2d: {
        const double* endpoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        const double* x = endpoints;
        const double* y = endpoints + 2;
        output[0] = linearBounds(x, parameter);
        output[1] = linearBounds(y, parameter);
        break;
      }
      case Quadratic2d: {
        const double* controlPoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 3;
        output[0] = quadraticBounds(x, parameter);
        output[1] = quadraticBounds(y, parameter);
        break;
      }
      case Cubic2d: {
        const double* controlPoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 4;
        output[0] = cubicBounds(x, parameter);
        output[1] = cubicBounds(y, parameter);
        break;
      }
      case Quartic2d: {
        const double* controlPoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 5;
        output[0] = quarticBounds(x, parameter);
        output[1] = quarticBounds(y, parameter);
        break;
      }
      case Quintic2d: {
        const double* controlPoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 6;
        output[0] = quinticBounds(x, parameter);
        output[1] = quinticBounds(y, parameter);
        break;
      }
      case Bezier2d: {
        int numControlPoints = getInt();
        const double* controlPoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + numControlPoints;
        output[0] = bezierBounds(numControlPoints, x, parameter);
        output[1] = bezierBounds(numControlPoints, y, parameter);
        break;
      }
      case TransformVector2d: {
        const double* matrix = getConstantPointer();
        const Bounds* input = getVariablePointer();
        Bounds* output = getVariablePointer();
        Bounds vx = input[0];
        Bounds vy = input[1];
        output[0] = matrix[0] * vx + matrix[2] * vy;
        output[1] = matrix[1] * vx + matrix[3] * vy;
        break;
      }
      case TransformPoint2d: {
        const double* matrix = getConstantPointer();
        const Bounds* point = getVariablePointer();
        Bounds* output = getVariablePointer();
        Bounds px = point[0];
        Bounds py = point[1];
        output[0] = matrix[0] * px + matrix[2] * py + matrix[4];
        output[1] = matrix[1] * px + matrix[3] * py + matrix[5];
        break;
      }
      case ProjectVector3d: {
        const double* matrix = getConstantPointer();
        const Bounds* input = getVariablePointer();
        Bounds* output = getVariablePointer();
        Bounds vx = input[0];
        Bounds vy = input[1];
        Bounds vz = input[2];
        output[0] = matrix[0] * vx + matrix[1] * vy + matrix[2] * vz;
        output[1] = matrix[3] * vx + matrix[4] * vy + matrix[5] * vz;
        break;
      }
      case ProjectPoint3d: {
        const double* matrix = getConstantPointer();
        const Bounds* input = getVariablePointer();
        Bounds* output = getVariablePointer();
        Bounds dx = input[0] - matrix[6];
        Bounds dy = input[1] - matrix[7];
        Bounds dz = input[2] - matrix[8];
        output[0] = matrix[0] * dx + matrix[1] * dy + matrix[2] * dz;
        output[1] = matrix[3] * dx + matrix[4] * dy + matrix[5] * dz;
        break;
      }
      case XYZ3d: {
        Bounds x = *getVariablePointer();
        Bounds y = *getVariablePointer();
        Bounds z = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        output[2] = z;
        break;
      }
      case XYC3d: {
        Bounds x = *getVariablePointer();
        Bounds y = *getVariablePointer();
        double z = *getConstantPointer();
        Bounds* output = getVariablePointer();
        output[0] = x;
        output[1] = y;
        output[2] = Bounds(z);
        break;
      }
      case XCZ3d: {
        Bounds x = *getVariablePointer();
        double y = *getConstantPointer();
        Bounds z = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = x;
        output[1] = Bounds(y);
        output[2] = z;
        break;
      }
      case CYZ3d: {
        double x = *getConstantPointer();
        Bounds y = *getVariablePointer();
        Bounds z = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = Bounds(x);
        output[1] = y;
        output[2] = z;
        break;
      }
      case XCC3d: {
        Bounds x = *getVariablePointer();
        double y = *getConstantPointer();
        double z = *getConstantPointer();
        Bounds* output = getVariablePointer();
        output[0] = x;
        output[1] = Bounds(y);
        output[2] = Bounds(z);
        break;
      }
      case CYC3d: {
        double x = *getConstantPointer();
        Bounds y = *getVariablePointer();
        double z = *getConstantPointer();
        Bounds* output = getVariablePointer();
        output[0] = Bounds(x);
        output[1] = y;
        output[2] = Bounds(z);
        break;
      }
      case CCZ3d: {
        double x = *getConstantPointer();
        double y = *getConstantPointer();
        Bounds z = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = Bounds(x);
        output[1] = Bounds(y);
        output[2] = z;
        break;
      }
      case Negate3d: {
        const Bounds* input = getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = -input[0];
        output[1] = -input[1];
        output[2] = -input[2];
        break;
      }
      case Add3d: {
        const Bounds* lhs = getVariablePointer();
        const Bounds* rhs = getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] + rhs[0];
        output[1] = lhs[1] + rhs[1];
        output[2] = lhs[2] + rhs[2];
        break;
      }
      case AddVariableConstant3d: {
        const Bounds* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] + rhs[0];
        output[1] = lhs[1] + rhs[1];
        output[2] = lhs[2] + rhs[2];
        break;
      }
      case Subtract3d: {
        const Bounds* lhs = getVariablePointer();
        const Bounds* rhs = getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] - rhs[0];
        output[1] = lhs[1] - rhs[1];
        output[2] = lhs[2] - rhs[2];
        break;
      }
      case SubtractConstantVariable3d: {
        const double* lhs = getConstantPointer();
        const Bounds* rhs = getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] - rhs[0];
        output[1] = lhs[1] - rhs[1];
        output[2] = lhs[2] - rhs[2];
        break;
      }
      case Multiply3d: {
        const Bounds* lhs = getVariablePointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        output[2] = lhs[2] * rhs;
        break;
      }
      case MultiplyVariableConstant3d: {
        const Bounds* lhs = getVariablePointer();
        double rhs = *getConstantPointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        output[2] = lhs[2] * rhs;
        break;
      }
      case MultiplyConstantVariable3d: {
        const double* lhs = getConstantPointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] * rhs;
        output[1] = lhs[1] * rhs;
        output[2] = lhs[2] * rhs;
        break;
      }
      case Divide3d: {
        const Bounds* lhs = getVariablePointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] / rhs;
        output[1] = lhs[1] / rhs;
        output[2] = lhs[2] / rhs;
        break;
      }
      case DivideConstantVariable3d: {
        const double* lhs = getConstantPointer();
        Bounds rhs = *getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[0] / rhs;
        output[1] = lhs[1] / rhs;
        output[2] = lhs[2] / rhs;
        break;
      }
      case SquaredMagnitude3d: {
        const Bounds* arg = getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = arg[0].squared() + arg[1].squared() + arg[2].squared();
        break;
      }
      case Magnitude3d: {
        const Bounds* arg = getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = (arg[0].squared() + arg[1].squared() + arg[2].squared()).sqrt();
        break;
      }
      case Dot3d: {
        const Bounds* lhs = getVariablePointer();
        const Bounds* rhs = getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = lhs[0] * rhs[0] + lhs[1] * rhs[1] + lhs[2] * rhs[2];
        break;
      }
      case DotVariableConstant3d: {
        const Bounds* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        Bounds* output = getVariablePointer();
        *output = lhs[0] * rhs[0] + lhs[1] * rhs[1] + lhs[2] * rhs[2];
        break;
      }
      case Cross3d: {
        const Bounds* lhs = getVariablePointer();
        const Bounds* rhs = getVariablePointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[1] * rhs[2] - lhs[2] * rhs[1];
        output[1] = lhs[2] * rhs[0] - lhs[0] * rhs[2];
        output[2] = lhs[0] * rhs[1] - lhs[1] * rhs[0];
        break;
      }
      case CrossVariableConstant3d: {
        const Bounds* lhs = getVariablePointer();
        const double* rhs = getConstantPointer();
        Bounds* output = getVariablePointer();
        output[0] = lhs[1] * rhs[2] - lhs[2] * rhs[1];
        output[1] = lhs[2] * rhs[0] - lhs[0] * rhs[2];
        output[2] = lhs[0] * rhs[1] - lhs[1] * rhs[0];
        break;
      }
      case Linear3d: {
        const double* endpoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
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
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
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
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
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
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
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
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + 6;
        const double* z = controlPoints + 12;
        output[0] = quinticBounds(x, parameter);
        output[1] = quinticBounds(y, parameter);
        output[2] = quinticBounds(z, parameter);
        break;
      }
      case Bezier3d: {
        int numControlPoints = getInt();
        const double* controlPoints = getConstantPointer();
        Bounds parameter = *getVariablePointer();
        Bounds* output = getVariablePointer();
        const double* x = controlPoints;
        const double* y = controlPoints + numControlPoints;
        const double* z = controlPoints + numControlPoints * 2;
        output[0] = bezierBounds(numControlPoints, x, parameter);
        output[1] = bezierBounds(numControlPoints, y, parameter);
        output[2] = bezierBounds(numControlPoints, z, parameter);
        break;
      }
      case TransformVector3d: {
        const double* matrix = getConstantPointer();
        const Bounds* input = getVariablePointer();
        Bounds* output = getVariablePointer();
        Bounds vx = input[0];
        Bounds vy = input[1];
        Bounds vz = input[2];
        output[0] = matrix[0] * vx + matrix[3] * vy + matrix[6] * vz;
        output[1] = matrix[1] * vx + matrix[4] * vy + matrix[7] * vz;
        output[2] = matrix[2] * vx + matrix[5] * vy + matrix[8] * vz;
        break;
      }
      case TransformPoint3d: {
        const double* matrix = getConstantPointer();
        const Bounds* input = getVariablePointer();
        Bounds* output = getVariablePointer();
        Bounds px = input[0];
        Bounds py = input[1];
        Bounds pz = input[2];
        output[0] = matrix[0] * px + matrix[3] * py + matrix[6] * pz + matrix[9];
        output[1] = matrix[1] * px + matrix[4] * py + matrix[7] * pz + matrix[10];
        output[2] = matrix[2] * px + matrix[5] * py + matrix[8] * pz + matrix[11];
        break;
      }
      case PlaceVector2d: {
        const double* matrix = getConstantPointer();
        const Bounds* input = getVariablePointer();
        Bounds* output = getVariablePointer();
        Bounds vx = input[0];
        Bounds vy = input[1];
        output[0] = matrix[0] * vx + matrix[3] * vy;
        output[1] = matrix[1] * vx + matrix[4] * vy;
        output[2] = matrix[2] * vx + matrix[5] * vy;
        break;
      }
      case PlacePoint2d: {
        const double* matrix = getConstantPointer();
        const Bounds* input = getVariablePointer();
        Bounds* output = getVariablePointer();
        Bounds px = input[0];
        Bounds py = input[1];
        output[0] = matrix[0] * px + matrix[3] * py + matrix[6];
        output[1] = matrix[1] * px + matrix[4] * py + matrix[7];
        output[2] = matrix[2] * px + matrix[5] * py + matrix[8];
        break;
      }
      case Desingularized1d: {
        Bounds t = *getVariablePointer();
        Bounds start = *getVariablePointer();
        Bounds middle = *getVariablePointer();
        Bounds end = *getVariablePointer();
        Bounds* output = getVariablePointer();
        if (t.upper <= T0) {
          *output = start;
        } else if (t.lower >= T1) {
          *output = end;
        } else {
          *output = middle;
        }
        break;
      }
      case Desingularized2d: {
        Bounds t = *getVariablePointer();
        Bounds* start = getVariablePointer();
        Bounds* middle = getVariablePointer();
        Bounds* end = getVariablePointer();
        Bounds* output = getVariablePointer();
        if (t.upper <= T0) {
          output[0] = start[0];
          output[1] = start[1];
        } else if (t.lower >= T1) {
          output[0] = end[0];
          output[1] = end[1];
        } else {
          output[0] = middle[0];
          output[1] = middle[1];
        }
        break;
      }
      case Desingularized3d: {
        Bounds t = *getVariablePointer();
        Bounds* start = getVariablePointer();
        Bounds* middle = getVariablePointer();
        Bounds* end = getVariablePointer();
        Bounds* output = getVariablePointer();
        if (t.upper <= T0) {
          output[0] = start[0];
          output[1] = start[1];
          output[2] = start[2];
        } else if (t.lower >= T1) {
          output[0] = end[0];
          output[1] = end[1];
          output[2] = end[2];
        } else {
          output[0] = middle[0];
          output[1] = middle[1];
          output[2] = middle[2];
        }
        break;
      }
      case Blend1d: {
        Bounds startValue = getValue();
        int numStartDerivatives = getInt();
        Bounds* startDerivatives = (Bounds*)alloca(sizeof(Bounds) * numStartDerivatives);
        for (int i = 0; i < numStartDerivatives; ++i) {
          startDerivatives[i] = getValue();
        }
        Bounds endValue = getValue();
        int numEndDerivatives = getInt();
        Bounds* endDerivatives = (Bounds*)alloca(sizeof(Bounds) * numEndDerivatives);
        for (int i = 0; i < numEndDerivatives; ++i) {
          endDerivatives[i] = getValue();
        }
        Bounds t = *getVariablePointer();
        Bounds* output = getVariablePointer();
        *output = blend(
          startValue,
          numStartDerivatives,
          startDerivatives,
          endValue,
          numEndDerivatives,
          endDerivatives,
          t
        );
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
    computeValue(
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
    computeBounds(
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
    computeValue(
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
    computeBounds(
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
      computeValue(
        function.wordsPointer,
        function.constantsPointer,
        functionVariablesPointer,
        &result
      );
      return result;
    };
    auto d = [derivative, derivativeVariablesPointer, v](double u) {
      derivativeVariablesPointer[0] = u;
      derivativeVariablesPointer[1] = v;
      double result;
      computeValue(
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
      computeValue(
        function.wordsPointer,
        function.constantsPointer,
        functionVariablesPointer,
        &result
      );
      return result;
    };
    auto d = [derivative, derivativeVariablesPointer, u](double v) {
      derivativeVariablesPointer[0] = u;
      derivativeVariablesPointer[1] = v;
      double result;
      computeValue(
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
