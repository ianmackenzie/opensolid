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
#define T0 0.001
#define T1 0.999

template <class V>
inline V
lerp(V a, V b, double t) {
  return a + t * (b - a);
}

template <class V>
inline auto
lerp(V a, V b, Bounds t) {
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

template <class V, class S>
inline V
quadraticBlossom(const V* p, S t1, S t2) {
  V q0 = lerp(p[0], p[1], t1);
  V q1 = lerp(p[1], p[2], t1);
  return lerp(q0, q1, t2);
}

template <class V, class S>
inline V
quadraticValue(const V* p, S t) {
  return quadraticBlossom(p, t, t);
}

template <class V, class S>
inline V
cubicBlossom(const V* p, S t1, S t2, S t3) {
  V q[3];
  q[0] = lerp(p[0], p[1], t1);
  q[1] = lerp(p[1], p[2], t1);
  q[2] = lerp(p[2], p[3], t1);
  return quadraticBlossom(q, t2, t3);
}

template <class V, class S>
inline V
cubicValue(const V* p, S t) {
  return cubicBlossom(p, t, t, t);
}

template <class V, class S>
inline V
quarticBlossom(const V* p, S t1, S t2, S t3, S t4) {
  V q[4];
  q[0] = lerp(p[0], p[1], t1);
  q[1] = lerp(p[1], p[2], t1);
  q[2] = lerp(p[2], p[3], t1);
  q[3] = lerp(p[3], p[4], t1);
  return cubicBlossom(q, t2, t3, t4);
}

template <class V, class S>
inline V
quarticValue(const V* p, S t) {
  return quarticBlossom(p, t, t, t, t);
}

template <class V, class S>
inline V
quinticBlossom(const V* p, S t1, S t2, S t3, S t4, S t5) {
  V q[5];
  q[0] = lerp(p[0], p[1], t1);
  q[1] = lerp(p[1], p[2], t1);
  q[2] = lerp(p[2], p[3], t1);
  q[3] = lerp(p[3], p[4], t1);
  q[4] = lerp(p[4], p[5], t1);
  return quarticBlossom(q, t2, t3, t4, t5);
}

template <class V, class S>
inline V
quinticValue(const V* p, S t) {
  return quinticBlossom(p, t, t, t, t, t);
}

template <class V, class S>
inline V
bezierBlossom(int numControlPoints, const V* controlPoints, S tLower, S tUpper, int nLow) {
  if (numControlPoints == 1) {
    return *controlPoints;
  }
  V* q = (V*)alloca(sizeof(V) * (numControlPoints - 1));
  for (int m = numControlPoints - 1; m > 0; --m) { // m is number of points to collapse to
    S t = m <= nLow ? tLower : tUpper;
    for (int i = 0; i < m; ++i) { // i is index of the point to collapse to
      q[i] = lerp(controlPoints[i], controlPoints[i + 1], t);
    }
    controlPoints = q; // After the first loop iteration, work in place within the outputs
  }
  return *q;
}

template <class V, class S>
inline V
bezierValue(int numControlPoints, const V* controlPoints, S t) {
  return bezierBlossom(numControlPoints, controlPoints, t, t, 0);
}

template <class V>
V
hermiteControlPointOffset(int controlPointIndex, const V* scaledDerivatives) {
  V result{};
  for (int derivativeOrder = 1; derivativeOrder <= controlPointIndex; ++derivativeOrder) {
    int coefficient = choose(controlPointIndex - 1, derivativeOrder - 1);
    result = result + coefficient * scaledDerivatives[derivativeOrder - 1];
  }
  return result;
}

template <class V>
void
hermiteToBezier(
  V startValue,
  int numStartDerivatives,
  V* startDerivatives,
  V endValue,
  int numEndDerivatives,
  V* endDerivatives,
  V* controlPoints
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
    V offset = hermiteControlPointOffset(forwardIndex, startDerivatives);
    controlPoints[forwardIndex] = controlPoints[forwardIndex - 1] + offset;
  }
  for (int reverseIndex = 1; reverseIndex <= numEndDerivatives; ++reverseIndex) {
    V offset = hermiteControlPointOffset(reverseIndex, endDerivatives);
    int forwardIndex = curveDegree - reverseIndex;
    controlPoints[forwardIndex] = controlPoints[forwardIndex + 1] + offset;
  }
}

template <class V, class S>
V
blend(
  V startValue,
  int numStartDerivatives,
  V* startDerivatives,
  V endValue,
  int numEndDerivatives,
  V* endDerivatives,
  S t
) {
  int numControlPoints = numStartDerivatives + numEndDerivatives + 2;
  V* controlPoints = (V*)alloca(sizeof(V) * numControlPoints);
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
  auto getScalarPointer = [&]() -> double* {
    return variablesPointer + getInt();
  };
  auto getScalar = [&]() -> double {
    return *getScalarPointer();
  };
  auto getVector2dPointer = [&]() -> Vector2d<double>* {
    return (Vector2d<double>*)getScalarPointer();
  };
  auto getVector2d = [&]() -> Vector2d<double> {
    return *getVector2dPointer();
  };
  auto getVector3dPointer = [&]() -> Vector3d<double>* {
    return (Vector3d<double>*)getScalarPointer();
  };
  auto getVector3d = [&]() -> Vector3d<double> {
    return *getVector3dPointer();
  };
  auto getScalarValuePointer = [&]() -> const double* {
    int taggedIndex = getInt();
    bool isConstant = taggedIndex >= 32768;
    const double* basePointer = isConstant ? constantsPointer : variablesPointer;
    int index = isConstant ? taggedIndex - 32768 : taggedIndex;
    return basePointer + index;
  };
  auto getScalarValue = [&]() -> double {
    return *getScalarValuePointer();
  };
  auto getVector2dValue = [&]() -> Vector2d<double> {
    return *(Vector2d<double>*)getScalarValuePointer();
  };
  auto getVector3dValue = [&]() -> Vector3d<double> {
    return *(Vector3d<double>*)getScalarValuePointer();
  };
  while (true) {
    int opcode = getInt();
    assert(opcode < OPCODE_END && "Unrecognized opcode");
    switch (Opcode(opcode)) {
      case Return: {
        int dimension = getInt();
        double* valuesPointer = getScalarPointer();
        std::memcpy(returnValuesPointer, valuesPointer, sizeof(double) * dimension);
        return;
      }
      case XComponent: {
        const double* vec = getScalarPointer();
        double* output = getScalarPointer();
        *output = vec[0];
        break;
      }
      case YComponent: {
        const double* vec = getScalarPointer();
        double* output = getScalarPointer();
        *output = vec[1];
        break;
      }
      case ZComponent: {
        const double* vec = getScalarPointer();
        double* output = getScalarPointer();
        *output = vec[2];
        break;
      }
      case Negate1d: {
        double input = getScalar();
        double* output = getScalarPointer();
        *output = -input;
        break;
      }
      case Add1d: {
        double lhs = getScalar();
        double rhs = getScalar();
        double* output = getScalarPointer();
        *output = lhs + rhs;
        break;
      }
      case AddVariableConstant1d: {
        double lhs = getScalar();
        double rhs = getConstantScalar();
        double* output = getScalarPointer();
        *output = lhs + rhs;
        break;
      }
      case Subtract1d: {
        double lhs = getScalar();
        double rhs = getScalar();
        double* output = getScalarPointer();
        *output = lhs - rhs;
        break;
      }
      case SubtractConstantVariable1d: {
        double lhs = getConstantScalar();
        double rhs = getScalar();
        double* output = getScalarPointer();
        *output = lhs - rhs;
        break;
      }
      case Multiply1d: {
        double lhs = getScalar();
        double rhs = getScalar();
        double* output = getScalarPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyVariableConstant1d: {
        double lhs = getScalar();
        double rhs = getConstantScalar();
        double* output = getScalarPointer();
        *output = lhs * rhs;
        break;
      }
      case Divide1d: {
        double lhs = getScalar();
        double rhs = getScalar();
        double* output = getScalarPointer();
        *output = lhs / rhs;
        break;
      }
      case DivideConstantVariable1d: {
        double lhs = getConstantScalar();
        double rhs = getScalar();
        double* output = getScalarPointer();
        *output = lhs / rhs;
        break;
      }
      case Square1d: {
        double input = getScalar();
        double* output = getScalarPointer();
        *output = input * input;
        break;
      }
      case Sqrt1d: {
        double input = getScalar();
        double* output = getScalarPointer();
        *output = std::sqrt(std::max(input, 0.0));
        break;
      }
      case Sin1d: {
        double input = getScalar();
        double* output = getScalarPointer();
        *output = std::sin(input);
        break;
      }
      case Cos1d: {
        double input = getScalar();
        double* output = getScalarPointer();
        *output = std::cos(input);
        break;
      }
      case Linear1d: {
        const double* endpoints = getConstantScalarPointer();
        double parameter = getScalar();
        double* output = getScalarPointer();
        *output = lerp(endpoints[0], endpoints[1], parameter);
        break;
      }
      case Quadratic1d: {
        const double* controlPoints = getConstantScalarPointer();
        const double parameter = getScalar();
        double* output = getScalarPointer();
        *output = quadraticValue(controlPoints, parameter);
        break;
      }
      case Cubic1d: {
        const double* controlPoints = getConstantScalarPointer();
        double parameter = getScalar();
        double* output = getScalarPointer();
        *output = cubicValue(controlPoints, parameter);
        break;
      }
      case Quartic1d: {
        const double* controlPoints = getConstantScalarPointer();
        double parameter = getScalar();
        double* output = getScalarPointer();
        *output = quarticValue(controlPoints, parameter);
        break;
      }
      case Quintic1d: {
        const double* controlPoints = getConstantScalarPointer();
        double parameter = getScalar();
        double* output = getScalarPointer();
        *output = quinticValue(controlPoints, parameter);
        break;
      }
      case Bezier1d: {
        int numControlPoints = getInt();
        const double* controlPoints = getConstantScalarPointer();
        double parameter = getScalar();
        double* output = getScalarPointer();
        *output = bezierValue(numControlPoints, controlPoints, parameter);
        break;
      }
      case XY2d: {
        double x = getScalar();
        double y = getScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = Vector2d(x, y);
        break;
      }
      case XC2d: {
        double x = getScalar();
        double y = getConstantScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = Vector2d(x, y);
        break;
      }
      case CY2d: {
        double x = getConstantScalar();
        double y = getScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = Vector2d(x, y);
        break;
      }
      case Negate2d: {
        Vector2d<double> input = getVector2d();
        Vector2d<double>* output = getVector2dPointer();
        *output = -input;
        break;
      }
      case Add2d: {
        Vector2d<double> lhs = getVector2d();
        Vector2d<double> rhs = getVector2d();
        Vector2d<double>* output = getVector2dPointer();
        *output = lhs + rhs;
        break;
      }
      case AddVariableConstant2d: {
        Vector2d<double> lhs = getVector2d();
        Vector2d<double> rhs = getConstantVector2d();
        Vector2d<double>* output = getVector2dPointer();
        *output = lhs + rhs;
        break;
      }
      case Subtract2d: {
        Vector2d<double> lhs = getVector2d();
        Vector2d<double> rhs = getVector2d();
        Vector2d<double>* output = getVector2dPointer();
        *output = lhs - rhs;
        break;
      }
      case SubtractConstantVariable2d: {
        Vector2d<double> lhs = getConstantVector2d();
        Vector2d<double> rhs = getVector2d();
        Vector2d<double>* output = getVector2dPointer();
        *output = lhs - rhs;
        break;
      }
      case Multiply2d: {
        Vector2d<double> lhs = getVector2d();
        double rhs = getScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyVariableConstant2d: {
        Vector2d<double> lhs = getVector2d();
        double rhs = getConstantScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyConstantVariable2d: {
        Vector2d<double> lhs = getConstantVector2d();
        double rhs = getScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = lhs * rhs;
        break;
      }
      case Divide2d: {
        Vector2d<double> lhs = getVector2d();
        double rhs = getScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = lhs / rhs;
        break;
      }
      case DivideConstantVariable2d: {
        Vector2d<double> lhs = getConstantVector2d();
        double rhs = getScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = lhs / rhs;
        break;
      }
      case SquaredMagnitude2d: {
        Vector2d<double> arg = getVector2d();
        double* output = getScalarPointer();
        *output = arg.squaredMagnitude();
        break;
      }
      case Magnitude2d: {
        Vector2d<double> arg = getVector2d();
        double* output = getScalarPointer();
        *output = arg.magnitude();
        break;
      }
      case Dot2d: {
        Vector2d<double> lhs = getVector2d();
        Vector2d<double> rhs = getVector2d();
        double* output = getScalarPointer();
        *output = lhs.dot(rhs);
        break;
      }
      case DotVariableConstant2d: {
        Vector2d<double> lhs = getVector2d();
        Vector2d<double> rhs = getConstantVector2d();
        double* output = getScalarPointer();
        *output = lhs.dot(rhs);
        break;
      }
      case Cross2d: {
        Vector2d<double> lhs = getVector2d();
        Vector2d<double> rhs = getVector2d();
        double* output = getScalarPointer();
        *output = lhs.cross(rhs);
        break;
      }
      case CrossVariableConstant2d: {
        Vector2d<double> lhs = getVector2d();
        Vector2d<double> rhs = getConstantVector2d();
        double* output = getScalarPointer();
        *output = lhs.cross(rhs);
        break;
      }
      case Linear2d: {
        const Vector2d<double>* endpoints = getConstantVector2dPointer();
        double parameter = getScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = lerp(endpoints[0], endpoints[1], parameter);
        break;
      }
      case Quadratic2d: {
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        double parameter = getScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = quadraticValue(controlPoints, parameter);
        break;
      }
      case Cubic2d: {
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        double parameter = getScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = cubicValue(controlPoints, parameter);
        break;
      }
      case Quartic2d: {
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        double parameter = getScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = quarticValue(controlPoints, parameter);
        break;
      }
      case Quintic2d: {
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        double parameter = getScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = quinticValue(controlPoints, parameter);
        break;
      }
      case Bezier2d: {
        int numControlPoints = getInt();
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        double parameter = getScalar();
        Vector2d<double>* output = getVector2dPointer();
        *output = bezierValue(numControlPoints, controlPoints, parameter);
        break;
      }
      case TransformVector2d: {
        const double* matrix = getConstantScalarPointer();
        Vector2d<double> input = getVector2d();
        Vector2d<double>* output = getVector2dPointer();
        *output = Vector2d(
          matrix[0] * input.x + matrix[2] * input.y,
          matrix[1] * input.x + matrix[3] * input.y
        );
        break;
      }
      case TransformPoint2d: {
        const double* matrix = getConstantScalarPointer();
        Vector2d<double> input = getVector2d();
        Vector2d<double>* output = getVector2dPointer();
        *output = Vector2d(
          matrix[0] * input.x + matrix[2] * input.y + matrix[4],
          matrix[1] * input.x + matrix[3] * input.y + matrix[5]
        );
        break;
      }
      case ProjectVector3d: {
        const double* matrix = getConstantScalarPointer();
        Vector3d<double> input = getVector3d();
        Vector2d<double>* output = getVector2dPointer();
        *output = Vector2d(
          matrix[0] * input.x + matrix[1] * input.y + matrix[2] * input.z,
          matrix[3] * input.x + matrix[4] * input.y + matrix[5] * input.z
        );
        break;
      }
      case ProjectPoint3d: {
        const double* matrix = getConstantScalarPointer();
        Vector3d<double> input = getVector3d();
        Vector2d<double>* output = getVector2dPointer();
        double dx = input.x - matrix[6];
        double dy = input.y - matrix[7];
        double dz = input.z - matrix[8];
        *output = Vector2d(
          matrix[0] * dx + matrix[1] * dy + matrix[2] * dz,
          matrix[3] * dx + matrix[4] * dy + matrix[5] * dz
        );
        break;
      }
      case XYZ3d: {
        double x = getScalar();
        double y = getScalar();
        double z = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = Vector3d(x, y, z);
        break;
      }
      case XYC3d: {
        double x = getScalar();
        double y = getScalar();
        double z = getConstantScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = Vector3d(x, y, z);
        break;
      }
      case XCZ3d: {
        double x = getScalar();
        double y = getConstantScalar();
        double z = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = Vector3d(x, y, z);
        break;
      }
      case CYZ3d: {
        double x = getConstantScalar();
        double y = getScalar();
        double z = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = Vector3d(x, y, z);
        break;
      }
      case XCC3d: {
        double x = getScalar();
        double y = getConstantScalar();
        double z = getConstantScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = Vector3d(x, y, z);
        break;
      }
      case CYC3d: {
        double x = getConstantScalar();
        double y = getScalar();
        double z = getConstantScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = Vector3d(x, y, z);
        break;
      }
      case CCZ3d: {
        double x = getConstantScalar();
        double y = getConstantScalar();
        double z = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = Vector3d(x, y, z);
        break;
      }
      case Negate3d: {
        Vector3d<double> input = getVector3d();
        Vector3d<double>* output = getVector3dPointer();
        *output = -input;
        break;
      }
      case Add3d: {
        Vector3d<double> lhs = getVector3d();
        Vector3d<double> rhs = getVector3d();
        Vector3d<double>* output = getVector3dPointer();
        *output = lhs + rhs;
        break;
      }
      case AddVariableConstant3d: {
        Vector3d<double> lhs = getVector3d();
        Vector3d<double> rhs = getConstantVector3d();
        Vector3d<double>* output = getVector3dPointer();
        *output = lhs + rhs;
        break;
      }
      case Subtract3d: {
        Vector3d<double> lhs = getVector3d();
        Vector3d<double> rhs = getVector3d();
        Vector3d<double>* output = getVector3dPointer();
        *output = lhs - rhs;
        break;
      }
      case SubtractConstantVariable3d: {
        Vector3d<double> lhs = getConstantVector3d();
        Vector3d<double> rhs = getVector3d();
        Vector3d<double>* output = getVector3dPointer();
        *output = lhs - rhs;
        break;
      }
      case Multiply3d: {
        Vector3d<double> lhs = getVector3d();
        double rhs = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyVariableConstant3d: {
        Vector3d<double> lhs = getVector3d();
        double rhs = getConstantScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyConstantVariable3d: {
        Vector3d<double> lhs = getConstantVector3d();
        double rhs = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = lhs * rhs;
        break;
      }
      case Divide3d: {
        Vector3d<double> lhs = getVector3d();
        double rhs = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = lhs / rhs;
        break;
      }
      case DivideConstantVariable3d: {
        Vector3d<double> lhs = getConstantVector3d();
        double rhs = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = lhs / rhs;
        break;
      }
      case SquaredMagnitude3d: {
        Vector3d<double> arg = getVector3d();
        double* output = getScalarPointer();
        *output = arg.squaredMagnitude();
        break;
      }
      case Magnitude3d: {
        Vector3d<double> arg = getVector3d();
        double* output = getScalarPointer();
        *output = arg.magnitude();
        break;
      }
      case Dot3d: {
        Vector3d<double> lhs = getVector3d();
        Vector3d<double> rhs = getVector3d();
        double* output = getScalarPointer();
        *output = lhs.dot(rhs);
        break;
      }
      case DotVariableConstant3d: {
        Vector3d<double> lhs = getVector3d();
        Vector3d<double> rhs = getConstantVector3d();
        double* output = getScalarPointer();
        *output = lhs.dot(rhs);
        break;
      }
      case Cross3d: {
        Vector3d<double> lhs = getVector3d();
        Vector3d<double> rhs = getVector3d();
        Vector3d<double>* output = getVector3dPointer();
        *output = lhs.cross(rhs);
        break;
      }
      case CrossVariableConstant3d: {
        Vector3d<double> lhs = getVector3d();
        Vector3d<double> rhs = getConstantVector3d();
        Vector3d<double>* output = getVector3dPointer();
        *output = lhs.cross(rhs);
        break;
      }
      case Linear3d: {
        const Vector3d<double>* endpoints = getConstantVector3dPointer();
        double parameter = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = lerp(endpoints[0], endpoints[1], parameter);
        break;
      }
      case Quadratic3d: {
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        double parameter = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = quadraticValue(controlPoints, parameter);
        break;
      }
      case Cubic3d: {
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        double parameter = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = cubicValue(controlPoints, parameter);
        break;
      }
      case Quartic3d: {
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        double parameter = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = quarticValue(controlPoints, parameter);
        break;
      }
      case Quintic3d: {
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        double parameter = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = quinticValue(controlPoints, parameter);
        break;
      }
      case Bezier3d: {
        int numControlPoints = getInt();
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        double parameter = getScalar();
        Vector3d<double>* output = getVector3dPointer();
        *output = bezierValue(numControlPoints, controlPoints, parameter);
        break;
      }
      case TransformVector3d: {
        const double* matrix = getConstantScalarPointer();
        Vector3d<double> input = getVector3d();
        Vector3d<double>* output = getVector3dPointer();
        *output = Vector3d(
          matrix[0] * input.x + matrix[3] * input.y + matrix[6] * input.z,
          matrix[1] * input.x + matrix[4] * input.y + matrix[7] * input.z,
          matrix[2] * input.x + matrix[5] * input.y + matrix[8] * input.z
        );
        break;
      }
      case TransformPoint3d: {
        const double* matrix = getConstantScalarPointer();
        Vector3d<double> input = getVector3d();
        Vector3d<double>* output = getVector3dPointer();
        *output = Vector3d(
          matrix[0] * input.x + matrix[3] * input.y + matrix[6] * input.z + matrix[9],
          matrix[1] * input.x + matrix[4] * input.y + matrix[7] * input.z + matrix[10],
          matrix[2] * input.x + matrix[5] * input.y + matrix[8] * input.z + matrix[11]
        );
        break;
      }
      case PlaceVector2d: {
        const double* matrix = getConstantScalarPointer();
        Vector2d<double> input = getVector2d();
        Vector3d<double>* output = getVector3dPointer();
        *output = Vector3d(
          matrix[0] * input.x + matrix[3] * input.y,
          matrix[1] * input.x + matrix[4] * input.y,
          matrix[2] * input.x + matrix[5] * input.y
        );
        break;
      }
      case PlacePoint2d: {
        const double* matrix = getConstantScalarPointer();
        Vector2d<double> input = getVector2d();
        Vector3d<double>* output = getVector3dPointer();
        *output = Vector3d(
          matrix[0] * input.x + matrix[3] * input.y + matrix[6],
          matrix[1] * input.x + matrix[4] * input.y + matrix[7],
          matrix[2] * input.x + matrix[5] * input.y + matrix[8]
        );
        break;
      }
      case Desingularized1d: {
        double t = getScalar();
        double start = getScalar();
        double middle = getScalar();
        double end = getScalar();
        double* output = getScalarPointer();
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
        double t = getScalar();
        Vector2d<double> start = getVector2d();
        Vector2d<double> middle = getVector2d();
        Vector2d<double> end = getVector2d();
        Vector2d<double>* output = getVector2dPointer();
        if (t <= T0) {
          *output = start;
        } else if (t >= T1) {
          *output = end;
        } else {
          *output = middle;
        }
        break;
      }
      case Desingularized3d: {
        double t = getScalar();
        Vector3d<double> start = getVector3d();
        Vector3d<double> middle = getVector3d();
        Vector3d<double> end = getVector3d();
        Vector3d<double>* output = getVector3dPointer();
        if (t <= T0) {
          *output = start;
        } else if (t >= T1) {
          *output = end;
        } else {
          *output = middle;
        }
        break;
      }
      case Blend1d: {
        double startValue = getScalarValue();
        int numStartDerivatives = getInt();
        double* startDerivatives = (double*)alloca(sizeof(double) * numStartDerivatives);
        for (int i = 0; i < numStartDerivatives; ++i) {
          startDerivatives[i] = getScalarValue();
        }
        double endValue = getScalarValue();
        int numEndDerivatives = getInt();
        double* endDerivatives = (double*)alloca(sizeof(double) * numEndDerivatives);
        for (int i = 0; i < numEndDerivatives; ++i) {
          endDerivatives[i] = getScalarValue();
        }
        double t = getScalar();
        double* output = getScalarPointer();
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
      case Blend2d: {
        Vector2d<double> startValue = getVector2dValue();
        int numStartDerivatives = getInt();
        Vector2d<double>* startDerivatives =
          (Vector2d<double>*)alloca(sizeof(Vector2d<double>) * numStartDerivatives);
        for (int i = 0; i < numStartDerivatives; ++i) {
          startDerivatives[i] = getVector2dValue();
        }
        Vector2d<double> endValue = getVector2dValue();
        int numEndDerivatives = getInt();
        Vector2d<double>* endDerivatives =
          (Vector2d<double>*)alloca(sizeof(Vector2d<double>) * numEndDerivatives);
        for (int i = 0; i < numEndDerivatives; ++i) {
          endDerivatives[i] = getVector2dValue();
        }
        double t = getScalar();
        Vector2d<double>* output = getVector2dPointer();
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
      case Blend3d: {
        Vector3d<double> startValue = getVector3dValue();
        int numStartDerivatives = getInt();
        Vector3d<double>* startDerivatives =
          (Vector3d<double>*)alloca(sizeof(Vector3d<double>) * numStartDerivatives);
        for (int i = 0; i < numStartDerivatives; ++i) {
          startDerivatives[i] = getVector3dValue();
        }
        Vector3d<double> endValue = getVector3dValue();
        int numEndDerivatives = getInt();
        Vector3d<double>* endDerivatives =
          (Vector3d<double>*)alloca(sizeof(Vector3d<double>) * numEndDerivatives);
        for (int i = 0; i < numEndDerivatives; ++i) {
          endDerivatives[i] = getVector3dValue();
        }
        double t = getScalar();
        Vector3d<double>* output = getVector3dPointer();
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
      case Cube1d: {
        double input = getScalar();
        double* output = getScalarPointer();
        *output = input * input * input;
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

inline Vector2d<Bounds>
linearBounds(const Vector2d<double>* p, Bounds t) {
  Vector2d<double> a = lerp(p[0], p[1], t.lower);
  Vector2d<double> b = lerp(p[0], p[1], t.upper);
  return Vector2d<Bounds>::hull2(a, b);
}

inline Vector3d<Bounds>
linearBounds(const Vector3d<double>* p, Bounds t) {
  Vector3d<double> a = lerp(p[0], p[1], t.lower);
  Vector3d<double> b = lerp(p[0], p[1], t.upper);
  return Vector3d<Bounds>::hull2(a, b);
}

inline Bounds
quadraticBounds(const double* p, Bounds t) {
  double a = quadraticBlossom(p, t.lower, t.lower);
  double b = quadraticBlossom(p, t.lower, t.upper);
  double c = quadraticBlossom(p, t.upper, t.upper);
  return Bounds::hull3(a, b, c);
}

inline Vector2d<Bounds>
quadraticBounds(const Vector2d<double>* p, Bounds t) {
  Vector2d<double> a = quadraticBlossom(p, t.lower, t.lower);
  Vector2d<double> b = quadraticBlossom(p, t.lower, t.upper);
  Vector2d<double> c = quadraticBlossom(p, t.upper, t.upper);
  return Vector2d<Bounds>::hull3(a, b, c);
}

inline Vector3d<Bounds>
quadraticBounds(const Vector3d<double>* p, Bounds t) {
  Vector3d<double> a = quadraticBlossom(p, t.lower, t.lower);
  Vector3d<double> b = quadraticBlossom(p, t.lower, t.upper);
  Vector3d<double> c = quadraticBlossom(p, t.upper, t.upper);
  return Vector3d<Bounds>::hull3(a, b, c);
}

inline Bounds
cubicBounds(const double* p, Bounds t) {
  double a = cubicBlossom(p, t.lower, t.lower, t.lower);
  double b = cubicBlossom(p, t.lower, t.lower, t.upper);
  double c = cubicBlossom(p, t.lower, t.upper, t.upper);
  double d = cubicBlossom(p, t.upper, t.upper, t.upper);
  return Bounds::hull4(a, b, c, d);
}

inline Vector2d<Bounds>
cubicBounds(const Vector2d<double>* p, Bounds t) {
  Vector2d<double> a = cubicBlossom(p, t.lower, t.lower, t.lower);
  Vector2d<double> b = cubicBlossom(p, t.lower, t.lower, t.upper);
  Vector2d<double> c = cubicBlossom(p, t.lower, t.upper, t.upper);
  Vector2d<double> d = cubicBlossom(p, t.upper, t.upper, t.upper);
  return Vector2d<Bounds>::hull4(a, b, c, d);
}

inline Vector3d<Bounds>
cubicBounds(const Vector3d<double>* p, Bounds t) {
  Vector3d<double> a = cubicBlossom(p, t.lower, t.lower, t.lower);
  Vector3d<double> b = cubicBlossom(p, t.lower, t.lower, t.upper);
  Vector3d<double> c = cubicBlossom(p, t.lower, t.upper, t.upper);
  Vector3d<double> d = cubicBlossom(p, t.upper, t.upper, t.upper);
  return Vector3d<Bounds>::hull4(a, b, c, d);
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

inline Vector2d<Bounds>
quarticBounds(const Vector2d<double>* p, Bounds t) {
  Vector2d<double> a = quarticBlossom(p, t.lower, t.lower, t.lower, t.lower);
  Vector2d<double> b = quarticBlossom(p, t.lower, t.lower, t.lower, t.upper);
  Vector2d<double> c = quarticBlossom(p, t.lower, t.lower, t.upper, t.upper);
  Vector2d<double> d = quarticBlossom(p, t.lower, t.upper, t.upper, t.upper);
  Vector2d<double> e = quarticBlossom(p, t.upper, t.upper, t.upper, t.upper);
  return Vector2d<Bounds>::hull5(a, b, c, d, e);
}

inline Vector3d<Bounds>
quarticBounds(const Vector3d<double>* p, Bounds t) {
  Vector3d<double> a = quarticBlossom(p, t.lower, t.lower, t.lower, t.lower);
  Vector3d<double> b = quarticBlossom(p, t.lower, t.lower, t.lower, t.upper);
  Vector3d<double> c = quarticBlossom(p, t.lower, t.lower, t.upper, t.upper);
  Vector3d<double> d = quarticBlossom(p, t.lower, t.upper, t.upper, t.upper);
  Vector3d<double> e = quarticBlossom(p, t.upper, t.upper, t.upper, t.upper);
  return Vector3d<Bounds>::hull5(a, b, c, d, e);
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

inline Vector2d<Bounds>
quinticBounds(const Vector2d<double>* p, Bounds t) {
  Vector2d<double> a = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.lower);
  Vector2d<double> b = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.upper);
  Vector2d<double> c = quinticBlossom(p, t.lower, t.lower, t.lower, t.upper, t.upper);
  Vector2d<double> d = quinticBlossom(p, t.lower, t.lower, t.upper, t.upper, t.upper);
  Vector2d<double> e = quinticBlossom(p, t.lower, t.upper, t.upper, t.upper, t.upper);
  Vector2d<double> f = quinticBlossom(p, t.upper, t.upper, t.upper, t.upper, t.upper);
  return Vector2d<Bounds>::hull6(a, b, c, d, e, f);
}

inline Vector3d<Bounds>
quinticBounds(const Vector3d<double>* p, Bounds t) {
  Vector3d<double> a = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.lower);
  Vector3d<double> b = quinticBlossom(p, t.lower, t.lower, t.lower, t.lower, t.upper);
  Vector3d<double> c = quinticBlossom(p, t.lower, t.lower, t.lower, t.upper, t.upper);
  Vector3d<double> d = quinticBlossom(p, t.lower, t.lower, t.upper, t.upper, t.upper);
  Vector3d<double> e = quinticBlossom(p, t.lower, t.upper, t.upper, t.upper, t.upper);
  Vector3d<double> f = quinticBlossom(p, t.upper, t.upper, t.upper, t.upper, t.upper);
  return Vector3d<Bounds>::hull6(a, b, c, d, e, f);
}

inline Bounds
bezierBounds(int numControlPoints, const double* controlPoints, Bounds t) {
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

inline Vector2d<Bounds>
bezierBounds(int numControlPoints, const Vector2d<double>* controlPoints, Bounds t) {
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

inline Vector3d<Bounds>
bezierBounds(int numControlPoints, const Vector3d<double>* controlPoints, Bounds t) {
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
  auto getScalarPointer = [&]() -> Bounds* {
    return variablesPointer + getInt();
  };
  auto getScalar = [&]() -> Bounds {
    return *getScalarPointer();
  };
  auto getVector2dPointer = [&]() -> Vector2d<Bounds>* {
    return (Vector2d<Bounds>*)getScalarPointer();
  };
  auto getVector2d = [&]() -> Vector2d<Bounds> {
    return *getVector2dPointer();
  };
  auto getVector3dPointer = [&]() -> Vector3d<Bounds>* {
    return (Vector3d<Bounds>*)getScalarPointer();
  };
  auto getVector3d = [&]() -> Vector3d<Bounds> {
    return *getVector3dPointer();
  };
  auto getScalarValue = [&]() -> Bounds {
    int taggedIndex = getInt();
    if (taggedIndex >= 32768) {
      return Bounds(constantsPointer[taggedIndex - 32768]);
    } else {
      return variablesPointer[taggedIndex];
    }
  };
  auto getVector2dValue = [&]() -> Vector2d<Bounds> {
    int taggedIndex = getInt();
    if (taggedIndex >= 32768) {
      const double* components = constantsPointer + (taggedIndex - 32768);
      return Vector2d<Bounds>(components[0], components[1]);
    } else {
      return *(Vector2d<Bounds>*)(variablesPointer + taggedIndex);
    }
  };
  auto getVector3dValue = [&]() -> Vector3d<Bounds> {
    int taggedIndex = getInt();
    if (taggedIndex >= 32768) {
      const double* components = constantsPointer + (taggedIndex - 32768);
      return Vector3d<Bounds>(components[0], components[1], components[2]);
    } else {
      return *(Vector3d<Bounds>*)(variablesPointer + taggedIndex);
    }
  };

  while (true) {
    int opcode = getInt();
    assert(opcode < OPCODE_END && "Unrecognized opcode");
    switch (Opcode(opcode)) {
      case Return: {
        int dimension = getInt();
        Bounds* valuesPointer = getScalarPointer();
        std::memcpy(returnValuesPointer, valuesPointer, sizeof(Bounds) * dimension);
        return;
      }
      case XComponent: {
        const Bounds* vec = getScalarPointer();
        Bounds* output = getScalarPointer();
        *output = vec[0];
        break;
      }
      case YComponent: {
        const Bounds* vec = getScalarPointer();
        Bounds* output = getScalarPointer();
        *output = vec[1];
        break;
      }
      case ZComponent: {
        const Bounds* vec = getScalarPointer();
        Bounds* output = getScalarPointer();
        *output = vec[2];
        break;
      }
      case Negate1d: {
        Bounds input = getScalar();
        Bounds* output = getScalarPointer();
        *output = -input;
        break;
      }
      case Add1d: {
        Bounds lhs = getScalar();
        Bounds rhs = getScalar();
        Bounds* output = getScalarPointer();
        *output = lhs + rhs;
        break;
      }
      case AddVariableConstant1d: {
        Bounds lhs = getScalar();
        double rhs = getConstantScalar();
        Bounds* output = getScalarPointer();
        *output = lhs + rhs;
        break;
      }
      case Subtract1d: {
        Bounds lhs = getScalar();
        Bounds rhs = getScalar();
        Bounds* output = getScalarPointer();
        *output = lhs - rhs;
        break;
      }
      case SubtractConstantVariable1d: {
        double lhs = getConstantScalar();
        Bounds rhs = getScalar();
        Bounds* output = getScalarPointer();
        *output = lhs - rhs;
        break;
      }
      case Multiply1d: {
        Bounds lhs = getScalar();
        Bounds rhs = getScalar();
        Bounds* output = getScalarPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyVariableConstant1d: {
        Bounds lhs = getScalar();
        double rhs = getConstantScalar();
        Bounds* output = getScalarPointer();
        *output = lhs * rhs;
        break;
      }
      case Divide1d: {
        Bounds lhs = getScalar();
        Bounds rhs = getScalar();
        Bounds* output = getScalarPointer();
        *output = lhs / rhs;
        break;
      }
      case DivideConstantVariable1d: {
        double lhs = getConstantScalar();
        Bounds rhs = getScalar();
        Bounds* output = getScalarPointer();
        *output = lhs / rhs;
        break;
      }
      case Square1d: {
        Bounds input = getScalar();
        Bounds* output = getScalarPointer();
        *output = input.squared();
        break;
      }
      case Sqrt1d: {
        Bounds input = getScalar();
        Bounds* output = getScalarPointer();
        *output = input.sqrt();
        break;
      }
      case Sin1d: {
        Bounds input = getScalar();
        Bounds* output = getScalarPointer();
        *output = input.sin();
        break;
      }
      case Cos1d: {
        Bounds input = getScalar();
        Bounds* output = getScalarPointer();
        *output = input.cos();
        break;
      }
      case Linear1d: {
        const double* endpoints = getConstantScalarPointer();
        Bounds parameter = getScalar();
        Bounds* output = getScalarPointer();
        *output = linearBounds(endpoints, parameter);
        break;
      }
      case Quadratic1d: {
        const double* controlPoints = getConstantScalarPointer();
        const Bounds parameter = getScalar();
        Bounds* output = getScalarPointer();
        *output = quadraticBounds(controlPoints, parameter);
        break;
      }
      case Cubic1d: {
        const double* controlPoints = getConstantScalarPointer();
        const Bounds parameter = getScalar();
        Bounds* output = getScalarPointer();
        *output = cubicBounds(controlPoints, parameter);
        break;
      }
      case Quartic1d: {
        const double* controlPoints = getConstantScalarPointer();
        const Bounds parameter = getScalar();
        Bounds* output = getScalarPointer();
        *output = quarticBounds(controlPoints, parameter);
        break;
      }
      case Quintic1d: {
        const double* controlPoints = getConstantScalarPointer();
        const Bounds parameter = getScalar();
        Bounds* output = getScalarPointer();
        *output = quinticBounds(controlPoints, parameter);
        break;
      }
      case Bezier1d: {
        int numControlPoints = getInt();
        const double* controlPoints = getConstantScalarPointer();
        const Bounds parameter = getScalar();
        Bounds* output = getScalarPointer();
        *output = bezierBounds(numControlPoints, controlPoints, parameter);
        break;
      }
      case XY2d: {
        Bounds x = getScalar();
        Bounds y = getScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = Vector2d(x, y);
        break;
      }
      case XC2d: {
        Bounds x = getScalar();
        double y = getConstantScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = Vector2d(x, Bounds(y));
        break;
      }
      case CY2d: {
        double x = getConstantScalar();
        Bounds y = getScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = Vector2d(Bounds(x), y);
        break;
      }
      case Negate2d: {
        Vector2d<Bounds> input = getVector2d();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = -input;
        break;
      }
      case Add2d: {
        Vector2d<Bounds> lhs = getVector2d();
        Vector2d<Bounds> rhs = getVector2d();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = lhs + rhs;
        break;
      }
      case AddVariableConstant2d: {
        Vector2d<Bounds> lhs = getVector2d();
        Vector2d<double> rhs = getConstantVector2d();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = lhs + rhs;
        break;
      }
      case Subtract2d: {
        Vector2d<Bounds> lhs = getVector2d();
        Vector2d<Bounds> rhs = getVector2d();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = lhs - rhs;
        break;
      }
      case SubtractConstantVariable2d: {
        Vector2d<double> lhs = getConstantVector2d();
        Vector2d<Bounds> rhs = getVector2d();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = lhs - rhs;
        break;
      }
      case Multiply2d: {
        Vector2d<Bounds> lhs = getVector2d();
        Bounds rhs = getScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyVariableConstant2d: {
        Vector2d<Bounds> lhs = getVector2d();
        double rhs = getConstantScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyConstantVariable2d: {
        Vector2d<double> lhs = getConstantVector2d();
        Bounds rhs = getScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = lhs * rhs;
        break;
      }
      case Divide2d: {
        Vector2d<Bounds> lhs = getVector2d();
        Bounds rhs = getScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = lhs / rhs;
        break;
      }
      case DivideConstantVariable2d: {
        Vector2d<double> lhs = getConstantVector2d();
        Bounds rhs = getScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = lhs / rhs;
        break;
      }
      case SquaredMagnitude2d: {
        Vector2d<Bounds> arg = getVector2d();
        Bounds* output = getScalarPointer();
        *output = arg.squaredMagnitude();
        break;
      }
      case Magnitude2d: {
        Vector2d<Bounds> arg = getVector2d();
        Bounds* output = getScalarPointer();
        *output = arg.magnitude();
        break;
      }
      case Dot2d: {
        Vector2d<Bounds> lhs = getVector2d();
        Vector2d<Bounds> rhs = getVector2d();
        Bounds* output = getScalarPointer();
        *output = lhs.dot(rhs);
        break;
      }
      case DotVariableConstant2d: {
        Vector2d<Bounds> lhs = getVector2d();
        Vector2d<double> rhs = getConstantVector2d();
        Bounds* output = getScalarPointer();
        *output = lhs.dot(rhs);
        break;
      }
      case Cross2d: {
        Vector2d<Bounds> lhs = getVector2d();
        Vector2d<Bounds> rhs = getVector2d();
        Bounds* output = getScalarPointer();
        *output = lhs.cross(rhs);
        break;
      }
      case CrossVariableConstant2d: {
        Vector2d<Bounds> lhs = getVector2d();
        Vector2d<double> rhs = getConstantVector2d();
        Bounds* output = getScalarPointer();
        *output = lhs.cross(rhs);
        break;
      }
      case Linear2d: {
        const Vector2d<double>* endpoints = getConstantVector2dPointer();
        Bounds parameter = getScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = linearBounds(endpoints, parameter);
        break;
      }
      case Quadratic2d: {
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        Bounds parameter = getScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = quadraticBounds(controlPoints, parameter);
        break;
      }
      case Cubic2d: {
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        Bounds parameter = getScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = cubicBounds(controlPoints, parameter);
        break;
      }
      case Quartic2d: {
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        Bounds parameter = getScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = quarticBounds(controlPoints, parameter);
        break;
      }
      case Quintic2d: {
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        Bounds parameter = getScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = quinticBounds(controlPoints, parameter);
        break;
      }
      case Bezier2d: {
        int numControlPoints = getInt();
        const Vector2d<double>* controlPoints = getConstantVector2dPointer();
        Bounds parameter = getScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = bezierBounds(numControlPoints, controlPoints, parameter);
        break;
      }
      case TransformVector2d: {
        const double* matrix = getConstantScalarPointer();
        Vector2d<Bounds> input = getVector2d();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = Vector2d(
          matrix[0] * input.x + matrix[2] * input.y,
          matrix[1] * input.x + matrix[3] * input.y
        );
        break;
      }
      case TransformPoint2d: {
        const double* matrix = getConstantScalarPointer();
        Vector2d<Bounds> input = getVector2d();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = Vector2d(
          matrix[0] * input.x + matrix[2] * input.y + matrix[4],
          matrix[1] * input.x + matrix[3] * input.y + matrix[5]
        );
        break;
      }
      case ProjectVector3d: {
        const double* matrix = getConstantScalarPointer();
        Vector3d<Bounds> input = getVector3d();
        Vector2d<Bounds>* output = getVector2dPointer();
        *output = Vector2d(
          matrix[0] * input.x + matrix[1] * input.y + matrix[2] * input.z,
          matrix[3] * input.x + matrix[4] * input.y + matrix[5] * input.z
        );
        break;
      }
      case ProjectPoint3d: {
        const double* matrix = getConstantScalarPointer();
        Vector3d<Bounds> input = getVector3d();
        Vector2d<Bounds>* output = getVector2dPointer();
        Bounds dx = input.x - matrix[6];
        Bounds dy = input.y - matrix[7];
        Bounds dz = input.z - matrix[8];
        *output = Vector2d(
          matrix[0] * dx + matrix[1] * dy + matrix[2] * dz,
          matrix[3] * dx + matrix[4] * dy + matrix[5] * dz
        );
        break;
      }
      case XYZ3d: {
        Bounds x = getScalar();
        Bounds y = getScalar();
        Bounds z = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = Vector3d(x, y, z);
        break;
      }
      case XYC3d: {
        Bounds x = getScalar();
        Bounds y = getScalar();
        double z = getConstantScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = Vector3d(x, y, Bounds(z));
        break;
      }
      case XCZ3d: {
        Bounds x = getScalar();
        double y = getConstantScalar();
        Bounds z = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = Vector3d(x, Bounds(y), z);
        break;
      }
      case CYZ3d: {
        double x = getConstantScalar();
        Bounds y = getScalar();
        Bounds z = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = Vector3d(Bounds(x), y, z);
        break;
      }
      case XCC3d: {
        Bounds x = getScalar();
        double y = getConstantScalar();
        double z = getConstantScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = Vector3d(x, Bounds(y), Bounds(z));
        break;
      }
      case CYC3d: {
        double x = getConstantScalar();
        Bounds y = getScalar();
        double z = getConstantScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = Vector3d(Bounds(x), y, Bounds(z));
        break;
      }
      case CCZ3d: {
        double x = getConstantScalar();
        double y = getConstantScalar();
        Bounds z = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = Vector3d(Bounds(x), Bounds(y), z);
        break;
      }
      case Negate3d: {
        Vector3d<Bounds> input = getVector3d();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = -input;
        break;
      }
      case Add3d: {
        Vector3d<Bounds> lhs = getVector3d();
        Vector3d<Bounds> rhs = getVector3d();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = lhs + rhs;
        break;
      }
      case AddVariableConstant3d: {
        Vector3d<Bounds> lhs = getVector3d();
        Vector3d<double> rhs = getConstantVector3d();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = lhs + rhs;
        break;
      }
      case Subtract3d: {
        Vector3d<Bounds> lhs = getVector3d();
        Vector3d<Bounds> rhs = getVector3d();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = lhs - rhs;
        break;
      }
      case SubtractConstantVariable3d: {
        Vector3d<double> lhs = getConstantVector3d();
        Vector3d<Bounds> rhs = getVector3d();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = lhs - rhs;
        break;
      }
      case Multiply3d: {
        Vector3d<Bounds> lhs = getVector3d();
        Bounds rhs = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyVariableConstant3d: {
        Vector3d<Bounds> lhs = getVector3d();
        double rhs = getConstantScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = lhs * rhs;
        break;
      }
      case MultiplyConstantVariable3d: {
        Vector3d<double> lhs = getConstantVector3d();
        Bounds rhs = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = lhs * rhs;
        break;
      }
      case Divide3d: {
        Vector3d<Bounds> lhs = getVector3d();
        Bounds rhs = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = lhs / rhs;
        break;
      }
      case DivideConstantVariable3d: {
        Vector3d<double> lhs = getConstantVector3d();
        Bounds rhs = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = lhs / rhs;
        break;
      }
      case SquaredMagnitude3d: {
        Vector3d<Bounds> arg = getVector3d();
        Bounds* output = getScalarPointer();
        *output = arg.squaredMagnitude();
        break;
      }
      case Magnitude3d: {
        Vector3d<Bounds> arg = getVector3d();
        Bounds* output = getScalarPointer();
        *output = arg.magnitude();
        break;
      }
      case Dot3d: {
        Vector3d<Bounds> lhs = getVector3d();
        Vector3d<Bounds> rhs = getVector3d();
        Bounds* output = getScalarPointer();
        *output = lhs.dot(rhs);
        break;
      }
      case DotVariableConstant3d: {
        Vector3d<Bounds> lhs = getVector3d();
        Vector3d<double> rhs = getConstantVector3d();
        Bounds* output = getScalarPointer();
        *output = lhs.dot(rhs);
        break;
      }
      case Cross3d: {
        Vector3d<Bounds> lhs = getVector3d();
        Vector3d<Bounds> rhs = getVector3d();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = lhs.cross(rhs);
        break;
      }
      case CrossVariableConstant3d: {
        Vector3d<Bounds> lhs = getVector3d();
        Vector3d<double> rhs = getConstantVector3d();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = lhs.cross(rhs);
        break;
      }
      case Linear3d: {
        const Vector3d<double>* endpoints = getConstantVector3dPointer();
        Bounds parameter = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = linearBounds(endpoints, parameter);
        break;
      }
      case Quadratic3d: {
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        Bounds parameter = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = quadraticBounds(controlPoints, parameter);
        break;
      }
      case Cubic3d: {
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        Bounds parameter = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = cubicBounds(controlPoints, parameter);
        break;
      }
      case Quartic3d: {
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        Bounds parameter = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = quarticBounds(controlPoints, parameter);
        break;
      }
      case Quintic3d: {
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        Bounds parameter = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = quinticBounds(controlPoints, parameter);
        break;
      }
      case Bezier3d: {
        int numControlPoints = getInt();
        const Vector3d<double>* controlPoints = getConstantVector3dPointer();
        Bounds parameter = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = bezierBounds(numControlPoints, controlPoints, parameter);
        break;
      }
      case TransformVector3d: {
        const double* matrix = getConstantScalarPointer();
        Vector3d<Bounds> input = getVector3d();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = Vector3d(
          matrix[0] * input.x + matrix[3] * input.y + matrix[6] * input.z,
          matrix[1] * input.x + matrix[4] * input.y + matrix[7] * input.z,
          matrix[2] * input.x + matrix[5] * input.y + matrix[8] * input.z
        );
        break;
      }
      case TransformPoint3d: {
        const double* matrix = getConstantScalarPointer();
        Vector3d<Bounds> input = getVector3d();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = Vector3d(
          matrix[0] * input.x + matrix[3] * input.y + matrix[6] * input.z + matrix[9],
          matrix[1] * input.x + matrix[4] * input.y + matrix[7] * input.z + matrix[10],
          matrix[2] * input.x + matrix[5] * input.y + matrix[8] * input.z + matrix[11]
        );
        break;
      }
      case PlaceVector2d: {
        const double* matrix = getConstantScalarPointer();
        Vector2d<Bounds> input = getVector2d();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = Vector3d(
          matrix[0] * input.x + matrix[3] * input.y,
          matrix[1] * input.x + matrix[4] * input.y,
          matrix[2] * input.x + matrix[5] * input.y
        );
        break;
      }
      case PlacePoint2d: {
        const double* matrix = getConstantScalarPointer();
        Vector2d<Bounds> input = getVector2d();
        Vector3d<Bounds>* output = getVector3dPointer();
        *output = Vector3d(
          matrix[0] * input.x + matrix[3] * input.y + matrix[6],
          matrix[1] * input.x + matrix[4] * input.y + matrix[7],
          matrix[2] * input.x + matrix[5] * input.y + matrix[8]
        );
        break;
      }
      case Desingularized1d: {
        Bounds t = getScalar();
        Bounds start = getScalar();
        Bounds middle = getScalar();
        Bounds end = getScalar();
        Bounds* output = getScalarPointer();
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
        Bounds t = getScalar();
        Vector2d<Bounds> start = getVector2d();
        Vector2d<Bounds> middle = getVector2d();
        Vector2d<Bounds> end = getVector2d();
        Vector2d<Bounds>* output = getVector2dPointer();
        if (t.upper <= T0) {
          *output = start;
        } else if (t.lower >= T1) {
          *output = end;
        } else {
          *output = middle;
        }
        break;
      }
      case Desingularized3d: {
        Bounds t = getScalar();
        Vector3d<Bounds> start = getVector3d();
        Vector3d<Bounds> middle = getVector3d();
        Vector3d<Bounds> end = getVector3d();
        Vector3d<Bounds>* output = getVector3dPointer();
        if (t.upper <= T0) {
          *output = start;
        } else if (t.lower >= T1) {
          *output = end;
        } else {
          *output = middle;
        }
        break;
      }
      case Blend1d: {
        Bounds startValue = getScalarValue();
        int numStartDerivatives = getInt();
        Bounds* startDerivatives = (Bounds*)alloca(sizeof(Bounds) * numStartDerivatives);
        for (int i = 0; i < numStartDerivatives; ++i) {
          startDerivatives[i] = getScalarValue();
        }
        Bounds endValue = getScalarValue();
        int numEndDerivatives = getInt();
        Bounds* endDerivatives = (Bounds*)alloca(sizeof(Bounds) * numEndDerivatives);
        for (int i = 0; i < numEndDerivatives; ++i) {
          endDerivatives[i] = getScalarValue();
        }
        Bounds t = getScalar();
        Bounds* output = getScalarPointer();
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

      case Blend2d: {
        Vector2d<Bounds> startValue = getVector2dValue();
        int numStartDerivatives = getInt();
        Vector2d<Bounds>* startDerivatives =
          (Vector2d<Bounds>*)alloca(sizeof(Vector2d<Bounds>) * numStartDerivatives);
        for (int i = 0; i < numStartDerivatives; ++i) {
          startDerivatives[i] = getVector2dValue();
        }
        Vector2d<Bounds> endValue = getVector2dValue();
        int numEndDerivatives = getInt();
        Vector2d<Bounds>* endDerivatives =
          (Vector2d<Bounds>*)alloca(sizeof(Vector2d<Bounds>) * numEndDerivatives);
        for (int i = 0; i < numEndDerivatives; ++i) {
          endDerivatives[i] = getVector2dValue();
        }
        Bounds t = getScalar();
        Vector2d<Bounds>* output = getVector2dPointer();
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
      case Blend3d: {
        Vector3d<Bounds> startValue = getVector3dValue();
        int numStartDerivatives = getInt();
        Vector3d<Bounds>* startDerivatives =
          (Vector3d<Bounds>*)alloca(sizeof(Vector3d<Bounds>) * numStartDerivatives);
        for (int i = 0; i < numStartDerivatives; ++i) {
          startDerivatives[i] = getVector3dValue();
        }
        Vector3d<Bounds> endValue = getVector3dValue();
        int numEndDerivatives = getInt();
        Vector3d<Bounds>* endDerivatives =
          (Vector3d<Bounds>*)alloca(sizeof(Vector3d<Bounds>) * numEndDerivatives);
        for (int i = 0; i < numEndDerivatives; ++i) {
          endDerivatives[i] = getVector3dValue();
        }
        Bounds t = getScalar();
        Vector3d<Bounds>* output = getVector3dPointer();
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
      case Cube1d: {
        Bounds input = getScalar();
        Bounds* output = getScalarPointer();
        *output = input.cubed();
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
