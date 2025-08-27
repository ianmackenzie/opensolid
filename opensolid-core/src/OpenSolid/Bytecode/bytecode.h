#pragma once

#include "stdint.h"

enum Opcode {
  Return = 0,
  XComponent = 1,
  YComponent = 2,
  ZComponent = 3,
  Negate1d = 4,
  Add1d = 5,
  AddVariableConstant1d = 6,
  Subtract1d = 7,
  SubtractConstantVariable1d = 8,
  Multiply1d = 9,
  MultiplyVariableConstant1d = 10,
  Divide1d = 11,
  DivideConstantVariable1d = 12,
  Square1d = 13,
  Sqrt1d = 14,
  Sin1d = 15,
  Cos1d = 16,
  Linear1d = 17,
  Quadratic1d = 18,
  Cubic1d = 19,
  Quartic1d = 20,
  Quintic1d = 21,
  Bezier1d = 22,
  XY2d = 23,
  XC2d = 24,
  CY2d = 25,
  Negate2d = 26,
  Add2d = 27,
  AddVariableConstant2d = 28,
  Subtract2d = 29,
  SubtractConstantVariable2d = 30,
  Multiply2d = 31,
  MultiplyVariableConstant2d = 32,
  MultiplyConstantVariable2d = 33,
  Divide2d = 34,
  DivideConstantVariable2d = 35,
  SquaredMagnitude2d = 36,
  Magnitude2d = 37,
  Dot2d = 38,
  DotVariableConstant2d = 39,
  Cross2d = 40,
  CrossVariableConstant2d = 41,
  Linear2d = 42,
  Quadratic2d = 43,
  Cubic2d = 44,
  Quartic2d = 45,
  Quintic2d = 46,
  Bezier2d = 47,
  TransformVector2d = 48,
  TransformPoint2d = 49,
  ProjectVector3d = 50,
  ProjectPoint3d = 51,
  XYZ3d = 52,
  XYC3d = 53,
  XCZ3d = 54,
  CYZ3d = 55,
  XCC3d = 56,
  CYC3d = 57,
  CCZ3d = 58,
  Negate3d = 59,
  Add3d = 60,
  AddVariableConstant3d = 61,
  Subtract3d = 62,
  SubtractConstantVariable3d = 63,
  Multiply3d = 64,
  MultiplyVariableConstant3d = 65,
  MultiplyConstantVariable3d = 66,
  Divide3d = 67,
  DivideConstantVariable3d = 68,
  SquaredMagnitude3d = 69,
  Magnitude3d = 70,
  Dot3d = 71,
  DotVariableConstant3d = 72,
  Cross3d = 73,
  CrossVariableConstant3d = 74,
  Linear3d = 75,
  Quadratic3d = 76,
  Cubic3d = 77,
  Quartic3d = 78,
  Quintic3d = 79,
  Bezier3d = 80,
  TransformVector3d = 81,
  TransformPoint3d = 82,
  PlaceVector2d = 83,
  PlacePoint2d = 84,
  Desingularized1d = 85,
  Desingularized2d = 86,
  Desingularized3d = 87,
  Blend1d = 88,
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
  opensolid_blend_values_1d(
    double startValue,
    int numStartDerivatives,
    double* startDerivatives,
    double endValue,
    int numEndDerivatives,
    double* endDerivatives,
    double t
  );

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
