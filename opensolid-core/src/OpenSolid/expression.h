#pragma once

#include "stdint.h"

enum Opcode {
  Return1d,
  Negate1d,
  Add1d,
  Subtract1d,
  Square1d,
  Multiply1d,
  Divide1d,
  Sqrt1d,
  Sin1d,
  Cos1d,
  Linear1d,
  Quadratic1d,
  Cubic1d,
  Quartic1d,
  Quintic1d,
  Bezier1d,
};

#ifdef __cplusplus
extern "C" {
#endif

  void
  opensolid_curve1d_value(
    const uint8_t* bytecode,
    double t,
    const double* constants,
    int numVariables,
    double* output
  );

  void
  opensolid_surface1d_value(
    const uint8_t* bytecode,
    double u,
    double v,
    const double* constants,
    int numVariables,
    double* output
  );

#ifdef __cplusplus
}
#endif
