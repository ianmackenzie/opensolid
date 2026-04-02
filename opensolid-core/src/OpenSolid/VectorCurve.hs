module OpenSolid.VectorCurve
  ( VectorCurve
  , VectorCurve2D
  , VectorCurve3D
  , Exists
  , Compiled
  , isZero
  , singular0
  , singular1
  , constant
  , new
  , zero
  , interpolateFrom
  , bezier
  , quadraticBezier
  , cubicBezier
  , arc
  , startValue
  , endValue
  , value
  , bounds
  , compiled
  , derivative
  , secondDerivative
  , derivativeValue
  , derivativeBounds
  , secondDerivativeValue
  , secondDerivativeBounds
  , squaredMagnitude_
  , squaredMagnitude
  , nondegenerate
  , magnitude
  , direction
  , directionBounds
  , quotient_
  , reverse
  , zeros
  , desingularized
  , desingularize
  , erase
  , unerase
  , coerce
  )
where

import Data.Void (Void)
import OpenSolid.Angle (Angle)
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import {-# SOURCE #-} OpenSolid.Curve (Curve2D)
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Nonzero qualified as Curve1D.Nonzero
import OpenSolid.Curve1D.Zero qualified
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import {-# SOURCE #-} OpenSolid.Curve3D (Curve3D)
import {-# SOURCE #-} OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Desingularization.Curve qualified as Desingularization.Curve
import OpenSolid.Direction (Direction)
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.DirectionBounds (DirectionBounds)
import OpenSolid.DirectionBounds qualified as DirectionBounds
import OpenSolid.DirectionCurve (DirectionCurve)
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Interval (Interval)
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson qualified as NewtonRaphson
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (IsDegenerate (IsDegenerate), Nondegenerate (Nondegenerate))
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Result qualified as Result
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds (VectorBounds)
import OpenSolid.VectorBounds qualified as VectorBounds
import OpenSolid.VectorCurve.Direction qualified as VectorCurve.Direction
import {-# SOURCE #-} OpenSolid.VectorCurve.Nondegenerate qualified as VectorCurve.Nondegenerate
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

data VectorCurve dimension units space = VectorCurve
  { compiled :: Compiled dimension units space
  , derivative :: ~(VectorCurve dimension units space)
  , startValue :: ~(Vector dimension units space)
  , endValue :: ~(Vector dimension units space)
  , maxSampledMagnitude :: ~(Quantity units)
  }

type VectorCurve2D units = VectorCurve 2 units Void

type VectorCurve3D units space = VectorCurve 3 units space

type Compiled dimension units space =
  CompiledFunction
    Number
    (Vector dimension units space)
    (Interval Unitless)
    (VectorBounds dimension units space)

class
  ( Vector.Exists dimension units space
  , VectorBounds.Exists dimension units space
  , DirectionBounds.Exists dimension space
  , Exists dimension Unitless space
  , DirectionCurve.Exists dimension space
  , Units.Coercion (VectorCurve dimension units space) (VectorCurve dimension Unitless space)
  , Units.Coercion (VectorCurve dimension Unitless space) (VectorCurve dimension units space)
  , Expression.Constant Number (Vector dimension units space)
  , Expression.Evaluation
      Number
      (Vector dimension units space)
      (Interval Unitless)
      (VectorBounds dimension units space)
  , Negation (Expression Number (Vector dimension units space))
  , Addition
      (Expression Number (Vector dimension units space))
      (Expression Number (Vector dimension units space))
      (Expression Number (Vector dimension units space))
  , Subtraction
      (Expression Number (Vector dimension units space))
      (Expression Number (Vector dimension units space))
      (Expression Number (Vector dimension units space))
  , Expression.BezierCurve (Vector dimension units space)
  , Expression.SquaredMagnitude_
      (Expression Number (Vector dimension units space))
      (Expression Number (Quantity (units ?*? units)))
  , Multiplication Number (VectorCurve dimension units space) (VectorCurve dimension units space)
  , Multiplication (VectorCurve dimension units space) Number (VectorCurve dimension units space)
  , Multiplication
      (Curve1D Unitless)
      (VectorCurve dimension units space)
      (VectorCurve dimension units space)
  , Multiplication
      (VectorCurve dimension units space)
      (Curve1D Unitless)
      (VectorCurve dimension units space)
  , Multiplication
      (Curve1D Unitless)
      (Vector dimension units space)
      (VectorCurve dimension units space)
  , Multiplication
      (Vector dimension units space)
      (Curve1D Unitless)
      (VectorCurve dimension units space)
  , Division
      (VectorCurve dimension units space)
      (Nonzero (Curve1D Unitless))
      (VectorCurve dimension units space)
  , Division
      (VectorCurve dimension units space)
      (Nonzero (Curve1D units))
      (VectorCurve dimension Unitless space)
  , Division
      (VectorCurve dimension units space)
      (Nondegenerate (Curve1D Unitless))
      (VectorCurve dimension units space)
  , Division
      (VectorCurve dimension units space)
      (Nondegenerate (Curve1D units))
      (VectorCurve dimension Unitless space)
  , DotMultiplication
      (VectorCurve dimension Unitless space)
      (VectorCurve dimension units space)
      (Curve1D units)
  , DotMultiplication
      (VectorCurve dimension units space)
      (VectorCurve dimension Unitless space)
      (Curve1D units)
  , DotMultiplication_
      (VectorCurve dimension units space)
      (VectorCurve dimension units space)
      (Curve1D (units ?*? units))
  , DotMultiplication
      (VectorCurve dimension units space)
      (Vector dimension Unitless space)
      (Curve1D units)
  , DotMultiplication
      (Vector dimension Unitless space)
      (VectorCurve dimension units space)
      (Curve1D units)
  , DotMultiplication
      (VectorCurve dimension units space)
      (Direction dimension space)
      (Curve1D units)
  , DotMultiplication
      (Direction dimension space)
      (VectorCurve dimension units space)
      (Curve1D units)
  , NewtonRaphson.Curve dimension units space
  , Desingularization.Curve
      (VectorCurve dimension units space)
      (Vector dimension units space)
      (Vector dimension units space)
  ) =>
  Exists dimension units space

instance FFI (VectorCurve2D Unitless) where
  representation = FFI.classRepresentation "UnitlessVectorCurve2D"

instance FFI (VectorCurve2D Meters) where
  representation = FFI.classRepresentation "VectorCurve2D"

instance FFI (VectorCurve3D Unitless FFI.Space) where
  representation = FFI.classRepresentation "UnitlessVectorCurve3D"

instance FFI (VectorCurve3D Meters FFI.Space) where
  representation = FFI.classRepresentation "VectorCurve3D"

instance HasUnits (VectorCurve dimension units space) units

instance Units.Coercion (VectorCurve2D units1) (VectorCurve2D units2) where
  coerce curve =
    VectorCurve
      { compiled = Units.coerce curve.compiled
      , derivative = Units.coerce curve.derivative
      , startValue = Units.coerce curve.startValue
      , endValue = Units.coerce curve.endValue
      , maxSampledMagnitude = Units.coerce curve.maxSampledMagnitude
      }

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3D units1 space1) (VectorCurve3D units2 space2)
  where
  coerce curve =
    VectorCurve
      { compiled = Units.coerce curve.compiled
      , derivative = Units.coerce curve.derivative
      , startValue = Units.coerce curve.startValue
      , endValue = Units.coerce curve.endValue
      , maxSampledMagnitude = Units.coerce curve.maxSampledMagnitude
      }

instance HasUnits (Nondegenerate (VectorCurve dimension units space)) units

instance
  Units.Coercion
    (Nondegenerate (VectorCurve2D units1))
    (Nondegenerate (VectorCurve2D units2))
  where
  coerce (Nondegenerate curve) = Nondegenerate (Units.coerce curve)

instance
  space1 ~ space2 =>
  Units.Coercion
    (Nondegenerate (VectorCurve3D units1 space1))
    (Nondegenerate (VectorCurve3D units2 space2))
  where
  coerce (Nondegenerate curve) = Nondegenerate (Units.coerce curve)

instance HasUnits (Nonzero (VectorCurve dimension units space)) units

instance
  Units.Coercion
    (Nonzero (VectorCurve2D units1))
    (Nonzero (VectorCurve2D units2))
  where
  coerce (Nonzero curve) = Nonzero (Units.coerce curve)

instance
  space1 ~ space2 =>
  Units.Coercion
    (Nonzero (VectorCurve3D units1 space1))
    (Nonzero (VectorCurve3D units2 space2))
  where
  coerce (Nonzero curve) = Nonzero (Units.coerce curve)

instance
  Exists dimension units space =>
  ApproximateEquality (VectorCurve dimension units space) (Tolerance units)
  where
  curve1 ~= curve2 = do
    let equalValuesAt t = value curve1 t ~= value curve2 t
    NonEmpty.all equalValuesAt Parameter.samples

instance
  units1 ~ units2 =>
  Intersects (VectorCurve2D units1) (Vector2D units2) (Tolerance units1)
  where
  curve `intersects` vector = Tolerance.using (Quantity.squared_ ?tolerance) do
    squaredMagnitude_ (curve - vector) `intersects` Quantity.zero

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (VectorCurve3D units1 space1) (Vector3D units2 space2) (Tolerance units1)
  where
  curve `intersects` vector = Tolerance.using (Quantity.squared_ ?tolerance) do
    squaredMagnitude_ (curve - vector) `intersects` Quantity.zero

instance
  units1 ~ units2 =>
  Intersects (Vector2D units1) (VectorCurve2D units2) (Tolerance units1)
  where
  vector `intersects` curve = curve `intersects` vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects (Vector3D units1 space1) (VectorCurve3D units2 space2) (Tolerance units1)
  where
  vector `intersects` curve = curve `intersects` vector

instance
  Exists dimension units space =>
  Negation (VectorCurve dimension units space)
  where
  negate curve = new (negate curve.compiled) (negate curve.derivative)

instance
  Exists dimension units space =>
  Multiplication Sign (VectorCurve dimension units space) (VectorCurve dimension units space)
  where
  Positive * curve = curve
  Negative * curve = -curve

instance
  Exists dimension units space =>
  Multiplication (VectorCurve dimension units space) Sign (VectorCurve dimension units space)
  where
  curve * Positive = curve
  curve * Negative = -curve

instance
  ( Exists dimension1 units1 space1
  , dimension1 ~ dimension2
  , space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (VectorCurve dimension1 units1 space1)
    (VectorCurve dimension2 units2 space2)
    (VectorCurve dimension1 units1 space1)
  where
  lhs + rhs = new (lhs.compiled + rhs.compiled) (lhs.derivative + rhs.derivative)

instance
  units1 ~ units2 =>
  Addition (VectorCurve2D units1) (Vector2D units2) (VectorCurve2D units1)
  where
  curve + vector = curve + constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition (VectorCurve3D units1 space1) (Vector3D units2 space2) (VectorCurve3D units1 space1)
  where
  curve + vector = curve + constant vector

instance
  units1 ~ units2 =>
  Addition (Vector2D units1) (VectorCurve2D units2) (VectorCurve2D units1)
  where
  vector + curve = constant vector + curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units1 space1)
  where
  vector + curve = constant vector + curve

instance
  ( Exists dimension1 units1 space1
  , dimension1 ~ dimension2
  , space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (VectorCurve dimension1 units1 space1)
    (VectorCurve dimension2 units2 space2)
    (VectorCurve dimension1 units1 space1)
  where
  lhs - rhs = new (lhs.compiled - rhs.compiled) (lhs.derivative - rhs.derivative)

instance
  units1 ~ units2 =>
  Subtraction (VectorCurve2D units1) (Vector2D units2) (VectorCurve2D units1)
  where
  curve - vector = curve - constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction (VectorCurve3D units1 space1) (Vector3D units2 space2) (VectorCurve3D units1 space1)
  where
  curve - vector = curve - constant vector

instance
  units1 ~ units2 =>
  Subtraction (Vector2D units1) (VectorCurve2D units2) (VectorCurve2D units1)
  where
  vector - curve = constant vector - curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction (Vector3D units1 space1) (VectorCurve3D units2 space2) (VectorCurve3D units1 space1)
  where
  vector - curve = constant vector - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Curve1D units1)
    (VectorCurve2D units2)
    (VectorCurve2D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Curve1D units1)
    (VectorCurve3D units2 space)
    (VectorCurve3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Curve1D units1)
    (VectorCurve2D units2)
    (VectorCurve2D (units1 ?*? units2))
  where
  lhs ?*? rhs =
    new
      (Curve1D.compiled lhs ?*? rhs.compiled)
      (Curve1D.derivative lhs ?*? rhs + lhs ?*? rhs.derivative)

instance
  Multiplication_
    (Curve1D units1)
    (VectorCurve3D units2 space)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (Curve1D.compiled lhs ?*? rhs.compiled)
      (Curve1D.derivative lhs ?*? rhs + lhs ?*? rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Quantity units1)
    (VectorCurve2D units2)
    (VectorCurve2D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Quantity units1)
    (VectorCurve3D units2 space)
    (VectorCurve3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (Quantity units1)
    (VectorCurve2D units2)
    (VectorCurve2D (units1 ?*? units2))
  where
  c1 ?*? c2 = Curve1D.constant c1 ?*? c2

instance
  Multiplication_
    (Quantity units1)
    (VectorCurve3D units2 space)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  c1 ?*? c2 = Curve1D.constant c1 ?*? c2

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorCurve2D units1)
    (Curve1D units2)
    (VectorCurve2D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorCurve3D units1 space)
    (Curve1D units2)
    (VectorCurve3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Multiplication_
    (VectorCurve2D units1)
    (Curve1D units2)
    (VectorCurve2D (units1 ?*? units2))
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? Curve1D.compiled rhs)
      (lhs.derivative ?*? rhs + lhs ?*? Curve1D.derivative rhs)

instance
  Multiplication_
    (VectorCurve3D units1 space)
    (Curve1D units2)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (lhs.compiled ?*? Curve1D.compiled rhs)
      (lhs.derivative ?*? rhs + lhs ?*? Curve1D.derivative rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorCurve2D units1)
    (Quantity units2)
    (VectorCurve2D units3)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (VectorCurve3D units1 space)
    (Quantity units2)
    (VectorCurve3D units3 space)
  where
  lhs * rhs = Units.specialize (lhs ?*? rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorCurve2D units1)
    (Quantity units2)
    (VectorCurve2D units3)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorCurve3D units1 space)
    (Quantity units2)
    (VectorCurve3D units3 space)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (VectorCurve2D units1)
    (Quantity units2)
    (VectorCurve2D (units1 ?/? units2))
  where
  curve ?/? quantity = Units.simplify (curve ?*? (1.0 ?/? quantity))

instance
  Division_
    (VectorCurve3D units1 space)
    (Quantity units2)
    (VectorCurve3D (units1 ?/? units2) space)
  where
  curve ?/? quantity = Units.simplify (curve ?*? (1.0 ?/? quantity))

instance
  Multiplication_
    (VectorCurve2D units1)
    (Quantity units2)
    (VectorCurve2D (units1 ?*? units2))
  where
  curve ?*? quantity = curve ?*? Curve1D.constant quantity

instance
  Multiplication_
    (VectorCurve3D units1 space)
    (Quantity units2)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  curve ?*? quantity = curve ?*? Curve1D.constant quantity

instance
  Units.Product units1 units2 units3 =>
  DotMultiplication
    (VectorCurve2D units1)
    (VectorCurve2D units2)
    (Curve1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (Curve1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  DotMultiplication_
    (VectorCurve2D units1)
    (VectorCurve2D units2)
    (Curve1D (units1 ?*? units2))
  where
  lhs `dot_` rhs =
    Curve1D.new
      (lhs.compiled `dot_` rhs.compiled)
      (lhs.derivative `dot_` rhs + lhs `dot_` rhs.derivative)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (Curve1D (units1 ?*? units2))
  where
  lhs `dot_` rhs =
    Curve1D.new
      (lhs.compiled `dot_` rhs.compiled)
      (lhs.derivative `dot_` rhs + lhs `dot_` rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  DotMultiplication
    (VectorCurve2D units1)
    (Vector2D units2)
    (Curve1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3D units1 space1)
    (Vector3D units2 space2)
    (Curve1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  DotMultiplication_
    (VectorCurve2D units1)
    (Vector2D units2)
    (Curve1D (units1 ?*? units2))
  where
  curve `dot_` vector = curve `dot_` constant vector

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve3D units1 space1)
    (Vector3D units2 space2)
    (Curve1D (units1 ?*? units2))
  where
  curve `dot_` vector = curve `dot_` constant vector

instance
  Units.Product units1 units2 units3 =>
  DotMultiplication (Vector2D units1) (VectorCurve2D units2) (Curve1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector3D units1 space1) (VectorCurve3D units2 space2) (Curve1D units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot_` rhs)

instance
  DotMultiplication_
    (Vector2D units1)
    (VectorCurve2D units2)
    (Curve1D (units1 ?*? units2))
  where
  vector `dot_` curve = constant vector `dot_` curve

instance
  space1 ~ space2 =>
  DotMultiplication_
    (Vector3D units1 space1)
    (VectorCurve3D units2 space2)
    (Curve1D (units1 ?*? units2))
  where
  vector `dot_` curve = constant vector `dot_` curve

instance DotMultiplication (VectorCurve2D units) Direction2D (Curve1D units) where
  lhs `dot` rhs = lhs `dot` Vector2D.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve3D units space1) (Direction3D space2) (Curve1D units)
  where
  lhs `dot` rhs = lhs `dot` Vector3D.unit rhs

instance DotMultiplication Direction2D (VectorCurve2D units) (Curve1D units) where
  lhs `dot` rhs = Vector2D.unit lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3D space1) (VectorCurve3D units space2) (Curve1D units)
  where
  lhs `dot` rhs = Vector3D.unit lhs `dot` rhs

instance
  Units.Product units1 units2 units3 =>
  CrossMultiplication
    (VectorCurve2D units1)
    (VectorCurve2D units2)
    (Curve1D units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units3 space1)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  CrossMultiplication_
    (VectorCurve2D units1)
    (VectorCurve2D units2)
    (Curve1D (units1 ?*? units2))
  where
  lhs `cross_` rhs =
    Curve1D.new
      (lhs.compiled `cross_` rhs.compiled)
      (lhs.derivative `cross_` rhs + lhs `cross_` rhs.derivative)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D (units1 ?*? units2) space1)
  where
  lhs `cross_` rhs =
    new
      (lhs.compiled `cross_` rhs.compiled)
      (lhs.derivative `cross_` rhs + lhs `cross_` rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  CrossMultiplication
    (VectorCurve2D units1)
    (Vector2D units2)
    (Curve1D units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3D units1 space1)
    (Vector3D units2 space2)
    (VectorCurve3D units3 space1)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  CrossMultiplication_
    (VectorCurve2D units1)
    (Vector2D units2)
    (Curve1D (units1 ?*? units2))
  where
  curve `cross_` vector = curve `cross_` constant vector

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorCurve3D units1 space1)
    (Vector3D units2 space2)
    (VectorCurve3D (units1 ?*? units2) space1)
  where
  curve `cross_` vector = curve `cross_` constant vector

instance
  Units.Product units1 units2 units3 =>
  CrossMultiplication
    (Vector2D units1)
    (VectorCurve2D units2)
    (Curve1D units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D units3 space1)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross_` rhs)

instance
  CrossMultiplication_
    (Vector2D units1)
    (VectorCurve2D units2)
    (Curve1D (units1 ?*? units2))
  where
  vector `cross_` curve = constant vector `cross_` curve

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (Vector3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D (units1 ?*? units2) space1)
  where
  vector `cross_` curve = constant vector `cross_` curve

instance CrossMultiplication (VectorCurve2D units) Direction2D (Curve1D units) where
  lhs `cross` rhs = lhs `cross` Vector2D.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorCurve3D units space1)
    (Direction3D space2)
    (VectorCurve3D units space1)
  where
  lhs `cross` rhs = lhs `cross` Vector3D.unit rhs

instance CrossMultiplication Direction2D (VectorCurve2D units) (Curve1D units) where
  lhs `cross` rhs = Vector2D.unit lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3D space1)
    (VectorCurve3D units space2)
    (VectorCurve3D units space1)
  where
  lhs `cross` rhs = Vector3D.unit lhs `cross` rhs

instance
  units1 ~ units2 =>
  Addition (Point2D units1) (VectorCurve2D units2) (Curve2D units1)
  where
  point + curve = Curve2D.constant point + curve

instance
  space1 ~ space2 =>
  Addition (Point3D space1) (VectorCurve3D Meters space2) (Curve3D space1)
  where
  point + curve = Curve3D.constant point + curve

instance
  units1 ~ units2 =>
  Subtraction (Point2D units1) (VectorCurve2D units2) (Curve2D units1)
  where
  point - curve = Curve2D.constant point - curve

instance
  space1 ~ space2 =>
  Subtraction (Point3D space1) (VectorCurve3D Meters space2) (Curve3D space1)
  where
  point - curve = Curve3D.constant point - curve

instance
  Exists dimension units space =>
  Composition
    (VectorCurve dimension units space)
    (Curve1D Unitless)
    (VectorCurve dimension units space)
  where
  f . g =
    new
      (f.compiled . Curve1D.compiled g)
      ((f.derivative . g) * Curve1D.derivative g)

instance
  Composition
    (VectorCurve2D units)
    (SurfaceFunction1D Unitless)
    (VectorSurfaceFunction2D units)
  where
  curve . function =
    VectorSurfaceFunction2D.new
      (curve.compiled . function.compiled)
      (\p -> (curve.derivative . function) * SurfaceFunction1D.derivative p function)

instance
  Composition
    (VectorCurve3D units space)
    (SurfaceFunction1D Unitless)
    (VectorSurfaceFunction3D units space)
  where
  curve . function =
    VectorSurfaceFunction3D.new
      (curve.compiled . function.compiled)
      (\p -> (curve.derivative . function) * SurfaceFunction1D.derivative p function)

instance
  Composition
    (VectorCurve2D units)
    SurfaceParameter
    (VectorSurfaceFunction2D units)
  where
  curve . parameter = curve . SurfaceFunction1D.parameter parameter

instance
  Composition
    (VectorCurve3D units space)
    SurfaceParameter
    (VectorSurfaceFunction3D units space)
  where
  curve . parameter = curve . SurfaceFunction1D.parameter parameter

instance
  Division_
    (VectorCurve2D units1)
    (Nonzero (Curve1D units2))
    (VectorCurve2D (units1 ?/? units2))
  where
  lhs ?/? Nonzero rhs = do
    let compiledQuotient = compiled lhs ?/? Curve1D.compiled rhs
    let quotientDerivative = Units.simplify do
          (derivative lhs ?*? rhs - lhs ?*? Curve1D.derivative rhs)
            ?/? Curve1D.Nonzero.squared_ (Nonzero rhs)
    new compiledQuotient quotientDerivative

instance
  Division_
    (VectorCurve3D units1 space)
    (Nonzero (Curve1D units2))
    (VectorCurve3D (units1 ?/? units2) space)
  where
  lhs ?/? Nonzero rhs = do
    let compiledQuotient = compiled lhs ?/? Curve1D.compiled rhs
    let quotientDerivative = Units.simplify do
          (derivative lhs ?*? rhs - lhs ?*? Curve1D.derivative rhs)
            ?/? Curve1D.Nonzero.squared_ (Nonzero rhs)
    new compiledQuotient quotientDerivative

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorCurve2D units1)
    (Nonzero (Curve1D units2))
    (VectorCurve2D units3)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorCurve3D units1 space)
    (Nonzero (Curve1D units2))
    (VectorCurve3D units3 space)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance
  Division_
    (VectorCurve2D units1)
    (Nondegenerate (Curve1D units2))
    (VectorCurve2D (units1 ?/? units2))
  where
  (?/?) = Desingularization.curveQuotient_

instance
  Division_
    (VectorCurve3D units1 space)
    (Nondegenerate (Curve1D units2))
    (VectorCurve3D (units1 ?/? units2) space)
  where
  (?/?) = Desingularization.curveQuotient_

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorCurve2D units1)
    (Nondegenerate (Curve1D units2))
    (VectorCurve2D units3)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (VectorCurve3D units1 space)
    (Nondegenerate (Curve1D units2))
    (VectorCurve3D units3 space)
  where
  lhs / rhs = Units.specialize (lhs ?/? rhs)

instance Desingularization.Curve (VectorCurve2D units) (Vector2D units) (Vector2D units) where
  value = value
  derivativeValue = derivativeValue
  secondDerivativeValue = secondDerivativeValue
  bezier = bezier
  desingularized = desingularized

instance
  Desingularization.Curve
    (VectorCurve3D units space)
    (Vector3D units space)
    (Vector3D units space)
  where
  value = value
  derivativeValue = derivativeValue
  secondDerivativeValue = secondDerivativeValue
  bezier = bezier
  desingularized = desingularized

instance Exists 2 units Void

instance Exists 3 units space

new ::
  Exists dimension units space =>
  Compiled dimension units space ->
  VectorCurve dimension units space ->
  VectorCurve dimension units space
new givenCompiled givenDerivative =
  VectorCurve
    { compiled = givenCompiled
    , derivative = givenDerivative
    , startValue = CompiledFunction.value givenCompiled 0.0
    , endValue = CompiledFunction.value givenCompiled 1.0
    , maxSampledMagnitude =
        NonEmpty.maximumOf
          (Vector.magnitude . CompiledFunction.value givenCompiled)
          Parameter.samples
    }

constant ::
  Exists dimension units space =>
  Vector dimension units space ->
  VectorCurve dimension units space
constant givenValue = new (CompiledFunction.constant givenValue) zero

zero :: Exists dimension units space => VectorCurve dimension units space
zero = constant Vector.zero

interpolateFrom ::
  Exists dimension units space =>
  Vector dimension units space ->
  Vector dimension units space ->
  VectorCurve dimension units space
interpolateFrom v1 v2 = bezier (NonEmpty.two v1 v2)

bezier ::
  Exists dimension units space =>
  NonEmpty (Vector dimension units space) ->
  VectorCurve dimension units space
bezier controlPoints = do
  let compiledBezier = CompiledFunction.concrete (Expression.bezierCurve controlPoints)
  let bezierDerivative = bezier (Bezier.derivative controlPoints)
  new compiledBezier bezierDerivative

quadraticBezier ::
  Exists dimension units space =>
  Vector dimension units space ->
  Vector dimension units space ->
  Vector dimension units space ->
  VectorCurve dimension units space
quadraticBezier v1 v2 v3 = bezier (NonEmpty.three v1 v2 v3)

cubicBezier ::
  Exists dimension units space =>
  Vector dimension units space ->
  Vector dimension units space ->
  Vector dimension units space ->
  Vector dimension units space ->
  VectorCurve dimension units space
cubicBezier v1 v2 v3 v4 = bezier (NonEmpty.four v1 v2 v3 v4)

arc ::
  Exists dimension units space =>
  Vector dimension units space ->
  Vector dimension units space ->
  Angle ->
  Angle ->
  VectorCurve dimension units space
arc vx vy theta1 theta2 = do
  let theta = Curve1D.interpolateFrom theta1 theta2
  vx * Curve1D.cos theta + vy * Curve1D.sin theta

{-# INLINE compiled #-}
compiled :: VectorCurve dimension units space -> Compiled dimension units space
compiled = (.compiled)

nondegenerate ::
  (Exists dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Result IsDegenerate (Nondegenerate (VectorCurve dimension units space))
nondegenerate curve = if isZero curve then Error IsDegenerate else Ok (Nondegenerate curve)

{-# INLINE derivative #-}
derivative ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  VectorCurve dimension units space
derivative = (.derivative)

{-# INLINE secondDerivative #-}
secondDerivative ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  VectorCurve dimension units space
secondDerivative = (.derivative.derivative)

{-# INLINE startValue #-}
startValue :: VectorCurve dimension units space -> Vector dimension units space
startValue = (.startValue)

{-# INLINE endValue #-}
endValue :: VectorCurve dimension units space -> Vector dimension units space
endValue = (.endValue)

value ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Number ->
  Vector dimension units space
value curve 0.0 = curve.startValue
value curve 1.0 = curve.endValue
value curve tValue = CompiledFunction.value curve.compiled tValue

bounds ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
bounds curve tBounds = CompiledFunction.bounds curve.compiled tBounds

derivativeValue ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Number ->
  Vector dimension units space
derivativeValue curve tValue = value (derivative curve) tValue

derivativeBounds ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
derivativeBounds curve tBounds = bounds (derivative curve) tBounds

secondDerivativeValue ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Number ->
  Vector dimension units space
secondDerivativeValue curve tValue = value (secondDerivative curve) tValue

secondDerivativeBounds ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
secondDerivativeBounds curve tBounds = bounds (secondDerivative curve) tBounds

direction ::
  (Exists dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Result IsDegenerate (DirectionCurve dimension space)
direction vectorCurve = Result.map VectorCurve.Nondegenerate.direction (nondegenerate vectorCurve)

directionBounds ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  DirectionBounds dimension space
directionBounds curve tBounds =
  VectorCurve.Direction.bounds curve tBounds (bounds curve tBounds) (derivativeBounds curve tBounds)

isZero :: Tolerance units => VectorCurve dimension units space -> Bool
isZero curve = curve.maxSampledMagnitude <= ?tolerance

singular0 :: Exists dimension units space => VectorCurve dimension units space -> Bool
singular0 curve = singularAt 0.0 curve

singular1 :: Exists dimension units space => VectorCurve dimension units space -> Bool
singular1 curve = singularAt 1.0 curve

singularAt :: Exists dimension units space => Number -> VectorCurve dimension units space -> Bool
singularAt tValue curve = do
  let singularityTolerance = Tolerance.unitless * curve.maxSampledMagnitude
  Tolerance.using singularityTolerance (value curve tValue ~= Vector.zero)

quotient_ ::
  ( Exists dimension units1 space
  , Division_
      (VectorCurve dimension units1 space)
      (Nondegenerate (Curve1D units2))
      (VectorCurve dimension (units1 ?/? units2) space)
  , Tolerance units2
  ) =>
  VectorCurve dimension units1 space ->
  Curve1D units2 ->
  Result DivisionByZero (VectorCurve dimension (units1 ?/? units2) space)
quotient_ lhs rhs
  | rhs ~= Curve1D.zero = Error DivisionByZero
  | otherwise = Ok (lhs ?/? Nondegenerate rhs)

reverse ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  VectorCurve dimension units space
reverse curve = curve . (1.0 - Curve1D.t)

zeros ::
  (Exists dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Result IsDegenerate (List Number)
zeros vectorCurve =
  case Tolerance.using (Quantity.squared_ ?tolerance) (Curve1D.zeros (squaredMagnitude_ vectorCurve)) of
    Ok zeros1D -> Ok (List.map (.location) zeros1D)
    Error Curve1D.IsZero -> Error IsDegenerate

desingularize ::
  Exists dimension units space =>
  Maybe (Vector dimension units space, Vector dimension units space) ->
  VectorCurve dimension units space ->
  Maybe (Vector dimension units space, Vector dimension units space) ->
  VectorCurve dimension units space
desingularize = Desingularization.curve

desingularized ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  VectorCurve dimension units space ->
  VectorCurve dimension units space ->
  VectorCurve dimension units space
desingularized start middle end = do
  let compiledDesingularized =
        CompiledFunction.desingularized
          (Curve1D.compiled Curve1D.t)
          start.compiled
          middle.compiled
          end.compiled
  let desingularizedDerivative = desingularized start.derivative middle.derivative end.derivative
  new compiledDesingularized desingularizedDerivative

squaredMagnitude_ ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Curve1D (units ?*? units)
squaredMagnitude_ curve = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.squaredMagnitude_
          Vector.squaredMagnitude_
          VectorBounds.squaredMagnitude_
          curve.compiled
  let squaredMagnitudeDerivative = 2.0 * curve `dot_` derivative curve
  Curve1D.new compiledSquaredMagnitude squaredMagnitudeDerivative

squaredMagnitude ::
  (Exists dimension units1 space, Units.Squared units1 units2) =>
  VectorCurve dimension units1 space ->
  Curve1D units2
squaredMagnitude curve = Units.specialize (squaredMagnitude_ curve)

magnitude ::
  (Exists dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Curve1D units
magnitude curve
  | isZero curve = Curve1D.zero
  | otherwise = Nondegenerate.unwrap (VectorCurve.Nondegenerate.magnitude (Nondegenerate curve))

erase ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  VectorCurve dimension Unitless space
erase = Units.erase

unerase ::
  Exists dimension units space =>
  VectorCurve dimension Unitless space ->
  VectorCurve dimension units space
unerase = Units.unerase

coerce ::
  (Exists dimension units1 space, Exists dimension units2 space) =>
  VectorCurve dimension units1 space ->
  VectorCurve dimension units2 space
coerce curve = unerase (erase curve)
