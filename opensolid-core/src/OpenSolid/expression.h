#pragma once

#include "stdint.h"

enum Opcode {
  Return,
  Negate1d,
  Add1d,
  AddVariableConstant1d,
  Subtract1d,
  SubtractConstantVariable1d,
  Square1d,
  Multiply1d,
  MultiplyVariableConstant1d,
  Divide1d,
  DivideConstantVariable1d,
  Sqrt1d,
  Sin1d,
  Cos1d,
  Linear1d,
  Quadratic1d,
  Cubic1d,
  Quartic1d,
  Quintic1d,
  Bezier1d,
  OPCODE_END
};

#ifdef __cplusplus
extern "C" {
#endif

  void
  opensolid_curve1d_value(
    const uint16_t* wordsPointer,
    double t,
    const double* constantsPointer,
    int numVariables,
    double* returnValuesPointer
  );

  void
  opensolid_surface1d_value(
    const uint16_t* wordsPointer,
    double u,
    double v,
    const double* constantsPointer,
    int numVariables,
    double* returnValuesPointer
  );

#ifdef __cplusplus
}
#endif
