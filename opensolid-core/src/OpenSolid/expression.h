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
    const uint8_t* bytecodePointer,
    double t,
    const double* constantsPointer,
    int numVariables,
    double* returnValuesPointer
  );

  void
  opensolid_surface1d_value(
    const uint8_t* bytecodePointer,
    double u,
    double v,
    const double* constantsPointer,
    int numVariables,
    double* returnValuesPointer
  );

#ifdef __cplusplus
}
#endif
