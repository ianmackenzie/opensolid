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
  SquaredMagnitude2d,
  Magnitude2d,
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
  ProjectVector3d,
  ProjectPoint3d,
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
  SquaredMagnitude3d,
  Magnitude3d,
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
  PlaceVector2d,
  PlacePoint2d,
  OPCODE_END
};

#ifdef __cplusplus
extern "C" {
#endif

  void
  opensolid_curve_value(const char* functionPointer, double t, double* returnValuesPointer);

  void
  opensolid_curve_bounds(
    const char* functionPointer,
    double tLower,
    double tUpper,
    double* returnValuesPointer
  );

  void
  opensolid_surface_value(
    const char* functionPointer,
    double u,
    double v,
    double* returnValuesPointer
  );

  void
  opensolid_surface_bounds(
    const char* functionPointer,
    double uLower,
    double uUpper,
    double vLower,
    double vUpper,
    double* returnValuesPointer
  );

  double
  opensolid_solve_monotonic_surface_u(
    double tol,
    const char* functionPointer,
    const char* derivativePointer,
    double uLower,
    double uUpper,
    double vValue
  );

  double
  opensolid_solve_monotonic_surface_v(
    double tol,
    const char* functionPointer,
    const char* derivativePointer,
    double uValue,
    double vLower,
    double vUpper
  );

#ifdef __cplusplus
}
#endif
