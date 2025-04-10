#pragma once

#include "stdint.h"

enum Opcode {
  Return,
  XComponent,
  YComponent,
  ZComponent,
  Negate1d,
  Add1d,
  AddVariableConstant1d,
  Subtract1d,
  SubtractConstantVariable1d,
  Multiply1d,
  MultiplyVariableConstant1d,
  Divide1d,
  DivideConstantVariable1d,
  Square1d,
  Sqrt1d,
  Sin1d,
  Cos1d,
  Linear1d,
  Quadratic1d,
  Cubic1d,
  Quartic1d,
  Quintic1d,
  Bezier1d,
  XY2d,
  XC2d,
  CY2d,
  Negate2d,
  Add2d,
  AddVariableConstant2d,
  Subtract2d,
  SubtractConstantVariable2d,
  Multiply2d,
  MultiplyVariableConstant2d,
  MultiplyConstantVariable2d,
  Divide2d,
  DivideConstantVariable2d,
  SquaredNorm2d,
  Norm2d,
  Dot2d,
  DotVariableConstant2d,
  Cross2d,
  CrossVariableConstant2d,
  Linear2d,
  Quadratic2d,
  Cubic2d,
  Quartic2d,
  Quintic2d,
  Bezier2d,
  TransformVector2d,
  TransformPoint2d,
  XYZ3d,
  XYC3d,
  XCZ3d,
  CYZ3d,
  XCC3d,
  CYC3d,
  CCZ3d,
  Negate3d,
  Add3d,
  AddVariableConstant3d,
  Subtract3d,
  SubtractConstantVariable3d,
  Multiply3d,
  MultiplyVariableConstant3d,
  MultiplyConstantVariable3d,
  Divide3d,
  DivideConstantVariable3d,
  SquaredNorm3d,
  Norm3d,
  Dot3d,
  DotVariableConstant3d,
  Cross3d,
  CrossVariableConstant3d,
  Linear3d,
  Quadratic3d,
  Cubic3d,
  Quartic3d,
  Quintic3d,
  Bezier3d,
  TransformVector3d,
  TransformPoint3d,
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
    int numVariableComponents,
    double* returnValuesPointer
  );

  void
  opensolid_curve1d_bounds(
    const uint16_t* wordsPointer,
    double tLower,
    double tUpper,
    const double* constantsPointer,
    int numVariableComponents,
    double* returnValuesPointer
  );

  void
  opensolid_surface1d_value(
    const uint16_t* wordsPointer,
    double u,
    double v,
    const double* constantsPointer,
    int numVariableComponents,
    double* returnValuesPointer
  );

  void
  opensolid_surface1d_bounds(
    const uint16_t* wordsPointer,
    double uLower,
    double uUpper,
    double vLower,
    double vUpper,
    const double* constantsPointer,
    int numVariableComponents,
    double* returnValuesPointer
  );

  void
  opensolid_curve2d_value(
    const uint16_t* wordsPointer,
    double t,
    const double* constantsPointer,
    int numVariableComponents,
    double* returnValuesPointer
  );

  void
  opensolid_curve2d_bounds(
    const uint16_t* wordsPointer,
    double tLower,
    double tUpper,
    const double* constantsPointer,
    int numVariableComponents,
    double* returnValuesPointer
  );

#ifdef __cplusplus
}
#endif
