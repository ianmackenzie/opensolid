#include <cassert>
#include <cmath>
#include <cstdint>
#include <cstring>

#include "expression.h"

inline double
lerp(double start, double end, double t) {
  return start + t * (end - start);
}

inline void
line(const double* in, double* out, double t) {
  *out = lerp(in[0], in[1], t);
}

inline void
quadraticBlossom(const double* in, double* out, double t1, double t2) {
  line(in + 0, out + 0, t1);
  line(in + 1, out + 1, t1);
  line(in, out, t2);
}

inline void
quadraticSpline(const double* in, double* out, double t) {
  quadraticBlossom(in, out, t, t);
}

inline void
cubicBlossom(const double* in, double* out, double t1, double t2, double t3) {
  line(in + 0, out + 0, t1);
  line(in + 1, out + 1, t1);
  line(in + 2, out + 2, t1);
  quadraticBlossom(in, out, t2, t3);
}

inline void
cubicSpline(const double* in, double* out, double t) {
  cubicBlossom(in, out, t, t, t);
}

inline void
quarticBlossom(const double* in, double* out, double t1, double t2, double t3, double t4) {
  line(in + 0, out + 0, t1);
  line(in + 1, out + 1, t1);
  line(in + 2, out + 2, t1);
  line(in + 3, out + 3, t1);
  cubicBlossom(in, out, t2, t3, t4);
}

inline void
quarticSpline(const double* in, double* out, double t) {
  quarticBlossom(in, out, t, t, t, t);
}

inline void
quinticBlossom(
  const double* in,
  double* out,
  double t1,
  double t2,
  double t3,
  double t4,
  double t5
) {
  line(in + 0, out + 0, t1);
  line(in + 1, out + 1, t1);
  line(in + 2, out + 2, t1);
  line(in + 3, out + 3, t1);
  line(in + 4, out + 4, t1);
  quarticBlossom(in, out, t2, t3, t4, t5);
}

inline void
quinticSpline(const double* in, double* out, double t) {
  quinticBlossom(in, out, t, t, t, t, t);
}

inline void
bezierBlossom(int n, const double* in, double* out, double tLow, double tHigh, int nLow) {
  const double* src = in; // Start by reading from the inputs
  for (int i = n - 1; i > 0; --i) { // i is number of points to collapse to
    double t = i <= nLow ? tLow : tHigh;
    for (int j = 0; j < i; ++j) { // j is index of the point to collapse to
      line(src + j, out + j, t);
    }
    src = out; // After the first loop iteration, work in place within the outputs
  }
}

inline void
bezierCurve(int n, const double* in, double* out, double t) {
  bezierBlossom(n, in, out, t, t, 0);
}

void
execute(
  const std::uint8_t* bytecode,
  const double* arguments,
  const double* constants,
  double* variables,
  double* returnValues
) {
  // Read a single byte from the bytecode,
  // and advance the bytecode pointer accordingly
  auto getByte = [&]() -> std::uint8_t {
    std::uint8_t byte = *bytecode;
    ++bytecode;
    return byte;
  };
  // Read a 16-bit unsigned integer from the bytecode
  auto getInt = [&]() -> int {
    std::uint8_t low = getByte();
    std::uint8_t high = getByte();
    return low + 256 * high;
  };
  // Get the pointer to a constant double value
  // by reading its index from the bytecode
  auto getConstant = [&]() -> const double* {
    return constants + getInt();
  };
  // Get the pointer to a local variable double value
  // by reading its index from the bytecode
  auto getVariable = [&]() -> double* {
    return variables + getInt();
  };
  // Get the pointer to an input double value
  // (which could be a variable, a constant or a function argument)
  // by reading a tag and optional index from the bytecode
  auto getInput = [&]() -> const double* {
    std::uint8_t tag = getByte();
    assert(tag < 4 && "Invalid input tag");
    if (tag == 3) {
      return getVariable();
    } else if (tag == 2) {
      return getConstant();
    } else { // 0 or 1
      return arguments + tag;
    }
  };
  while (true) {
    std::uint8_t opcode = getByte();
    switch (opcode) {
      case Return1d: {
        *returnValues = *getInput();
        return;
      }
      case Negate1d: {
        const double* input = getInput();
        double* output = getVariable();
        *output = -*input;
        break;
      }
      case Add1d: {
        const double* lhs = getInput();
        const double* rhs = getInput();
        double* output = getVariable();
        *output = *lhs + *rhs;
        break;
      }
      case Subtract1d: {
        const double* lhs = getInput();
        const double* rhs = getInput();
        double* output = getVariable();
        *output = *lhs - *rhs;
        break;
      }
      case Square1d: {
        const double* input = getInput();
        double* output = getVariable();
        *output = *input * *input;
        break;
      }
      case Multiply1d: {
        const double* lhs = getInput();
        const double* rhs = getInput();
        double* output = getVariable();
        *output = *lhs * *rhs;
        break;
      }
      case Divide1d: {
        const double* lhs = getInput();
        const double* rhs = getInput();
        double* output = getVariable();
        *output = *lhs / *rhs;
        break;
      }
      case Sqrt1d: {
        const double* input = getInput();
        double* output = getVariable();
        *output = std::sqrt(*input);
        break;
      }
      case Sin1d: {
        const double* input = getInput();
        double* output = getVariable();
        *output = std::sin(*input);
        break;
      }
      case Cos1d: {
        const double* input = getInput();
        double* output = getVariable();
        *output = std::cos(*input);
        break;
      }
      case Linear1d: {
        const double* input = getConstant();
        const double* parameter = getInput();
        double* output = getVariable();
        line(input, output, *parameter);
        break;
      }
      case Quadratic1d: {
        const double* input = getConstant();
        const double* parameter = getInput();
        double* output = getVariable();
        quadraticSpline(input, output, *parameter);
        break;
      }
      case Cubic1d: {
        const double* input = getConstant();
        const double* parameter = getInput();
        double* output = getVariable();
        cubicSpline(input, output, *parameter);
        break;
      }
      case Quartic1d: {
        const double* input = getConstant();
        const double* parameter = getInput();
        double* output = getVariable();
        quarticSpline(input, output, *parameter);
        break;
      }
      case Quintic1d: {
        const double* input = getConstant();
        const double* parameter = getInput();
        double* output = getVariable();
        quinticSpline(input, output, *parameter);
        break;
      }
      case Bezier1d: {
        int n = getInt();
        const double* input = getConstant();
        const double* parameter = getInput();
        double* output = getVariable();
        bezierCurve(n, input, output, *parameter);
        break;
      }
      default: {
        assert(false && "Unknown opcode");
        return;
      }
    }
  }
}

extern "C" {
  void
  opensolid_curve1d_value(
    const std::uint8_t* bytecode,
    double t,
    const double* constants,
    int numVariables,
    double* output
  ) {
    double* variables = static_cast<double*>(alloca(sizeof(double) * numVariables));
    execute(bytecode, &t, constants, variables, output);
  }

  void
  opensolid_surface1d_value(
    const std::uint8_t* bytecode,
    double u,
    double v,
    const double* constants,
    int numVariables,
    double* output
  ) {
    double uv[2] = {u, v};
    double* variables = static_cast<double*>(alloca(sizeof(double) * numVariables));
    execute(bytecode, uv, constants, variables, output);
  }
}
