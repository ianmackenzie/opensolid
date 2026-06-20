module OpenSolid.VectorCurve
  ( VectorCurve
  , VectorCurve2D
  , VectorCurve3D
  , Exists
  , Compiled
  , isZero
  , hasDegenerateStart
  , hasDegenerateEnd
  , isDegenerateAt
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
  , range
  , compiled
  , derivative
  , secondDerivative
  , derivativeValue
  , derivativeRange
  , secondDerivativeValue
  , secondDerivativeRange
  , squaredMagnitude_
  , squaredMagnitude
  , nondegenerate
  , magnitude
  , direction
  , directionRange
  , quotient_
  , reverse
  , transformBy
  , zeros
  , desingularized
  , desingularize
  , erase
  , unerase
  , coerce
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Nonzero qualified as Curve1D.Nonzero
import OpenSolid.Curve1D.Zero qualified
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
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
import {-# SOURCE #-} OpenSolid.DirectionCurve (DirectionCurve)
import {-# SOURCE #-} OpenSolid.DirectionCurve qualified as DirectionCurve
import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Error (IsDegenerate (IsDegenerate))
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Interval (Interval)
import OpenSolid.List qualified as List
import OpenSolid.NewtonRaphson.Curve qualified as NewtonRaphson.Curve
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate))
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
import OpenSolid.Transform qualified as Transform
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
import OpenSolid.VectorTransform (VectorTransform)
import OpenSolid.VectorTransform qualified as VectorTransform

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
  , VectorTransform.Exists dimension space
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
  , Expression.Arc (Vector dimension units space)
  , Expression.SquaredMagnitude_
      (Expression Number (Vector dimension units space))
      (Expression Number (Quantity (units ?*? units)))
  , Expression.TransformBy
      (VectorTransform dimension Transform.Affine space)
      (Expression Number (Vector dimension units space))
      (Expression Number (Vector dimension units space))
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
  , NewtonRaphson.Curve.Solver dimension units space
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

instance FFI (VectorCurve3D Unitless Void) where
  representation = FFI.classRepresentation "UnitlessVectorCurve3D"

instance FFI (VectorCurve3D Meters Void) where
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
  negate curve = new (negate (compiled curve)) (negate (derivative curve))

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
  lhs + rhs = new (compiled lhs + compiled rhs) (derivative lhs + derivative rhs)

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
  lhs - rhs = new (compiled lhs - compiled rhs) (derivative lhs - derivative rhs)

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
      (Curve1D.compiled lhs ?*? compiled rhs)
      (Curve1D.derivative lhs ?*? rhs + lhs ?*? derivative rhs)

instance
  Multiplication_
    (Curve1D units1)
    (VectorCurve3D units2 space)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (Curve1D.compiled lhs ?*? compiled rhs)
      (Curve1D.derivative lhs ?*? rhs + lhs ?*? derivative rhs)

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
      (compiled lhs ?*? Curve1D.compiled rhs)
      (derivative lhs ?*? rhs + lhs ?*? Curve1D.derivative rhs)

instance
  Multiplication_
    (VectorCurve3D units1 space)
    (Curve1D units2)
    (VectorCurve3D (units1 ?*? units2) space)
  where
  lhs ?*? rhs =
    new
      (compiled lhs ?*? Curve1D.compiled rhs)
      (derivative lhs ?*? rhs + lhs ?*? Curve1D.derivative rhs)

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
      (compiled lhs `dot_` compiled rhs)
      (derivative lhs `dot_` rhs + lhs `dot_` derivative rhs)

instance
  space1 ~ space2 =>
  DotMultiplication_
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (Curve1D (units1 ?*? units2))
  where
  lhs `dot_` rhs =
    Curve1D.new
      (compiled lhs `dot_` compiled rhs)
      (derivative lhs `dot_` rhs + lhs `dot_` derivative rhs)

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
      (compiled lhs `cross_` compiled rhs)
      (derivative lhs `cross_` rhs + lhs `cross_` derivative rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication_
    (VectorCurve3D units1 space1)
    (VectorCurve3D units2 space2)
    (VectorCurve3D (units1 ?*? units2) space1)
  where
  lhs `cross_` rhs =
    new
      (compiled lhs `cross_` compiled rhs)
      (derivative lhs `cross_` rhs + lhs `cross_` derivative rhs)

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
      (compiled f . Curve1D.compiled g)
      ((derivative f . g) * Curve1D.derivative g)

instance
  Composition
    (VectorCurve2D units)
    (SurfaceFunction1D Unitless)
    (VectorSurfaceFunction2D units)
  where
  curve . function =
    VectorSurfaceFunction2D.new
      (compiled curve . SurfaceFunction1D.compiled function)
      (\p -> (derivative curve . function) * SurfaceFunction1D.derivative p function)

instance
  Composition
    (VectorCurve3D units space)
    (SurfaceFunction1D Unitless)
    (VectorSurfaceFunction3D units space)
  where
  curve . function =
    VectorSurfaceFunction3D.new
      (compiled curve . SurfaceFunction1D.compiled function)
      (\p -> (derivative curve . function) * SurfaceFunction1D.derivative p function)

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
  let compiledArc = CompiledFunction.concrete (Expression.arc vx vy theta1 theta2)
  let dTheta = Angle.inRadians (theta2 - theta1)
  let arcDerivative = arc (dTheta * vy) (-dTheta * vx) theta1 theta2
  new compiledArc arcDerivative

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
value curve 0.0 = startValue curve
value curve 1.0 = endValue curve
value curve tValue = CompiledFunction.value (compiled curve) tValue

range ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
range curve tRange = CompiledFunction.range (compiled curve) tRange

derivativeValue ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Number ->
  Vector dimension units space
derivativeValue curve tValue = value (derivative curve) tValue

derivativeRange ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
derivativeRange curve tRange = range (derivative curve) tRange

secondDerivativeValue ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Number ->
  Vector dimension units space
secondDerivativeValue curve tValue = value (secondDerivative curve) tValue

secondDerivativeRange ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  VectorBounds dimension units space
secondDerivativeRange curve tRange = range (secondDerivative curve) tRange

direction ::
  (Exists dimension units space, Tolerance units) =>
  VectorCurve dimension units space ->
  Result IsDegenerate (DirectionCurve dimension space)
direction vectorCurve = Result.map VectorCurve.Nondegenerate.direction (nondegenerate vectorCurve)

directionRange ::
  Exists dimension units space =>
  VectorCurve dimension units space ->
  Interval Unitless ->
  DirectionBounds dimension space
directionRange curve tRange =
  VectorCurve.Direction.range curve tRange (range curve tRange) (derivativeRange curve tRange)

{-# INLINE isZero #-}
isZero :: Tolerance units => VectorCurve dimension units space -> Bool
isZero curve = curve.maxSampledMagnitude <= ?tolerance

hasDegenerateStart :: Exists dimension units space => VectorCurve dimension units space -> Bool
hasDegenerateStart = isDegenerateAt 0.0

hasDegenerateEnd :: Exists dimension units space => VectorCurve dimension units space -> Bool
hasDegenerateEnd = isDegenerateAt 1.0

isDegenerateAt :: Exists dimension units space => Number -> VectorCurve dimension units space -> Bool
isDegenerateAt tValue curve = do
  let degeneracyTolerance = Tolerance.unitless * curve.maxSampledMagnitude
  Tolerance.using degeneracyTolerance (value curve tValue ~= Vector.zero)

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

transformBy ::
  Exists dimension units space =>
  VectorTransform dimension tag space ->
  VectorCurve dimension units space ->
  VectorCurve dimension units space
transformBy transform curve = do
  let affineTransform = VectorTransform.asAffine transform
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy affineTransform)
          (Vector.transformBy affineTransform)
          (VectorBounds.transformBy affineTransform)
          (compiled curve)
  new compiledTransformed (transformBy affineTransform (derivative curve))

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
          (compiled start)
          (compiled middle)
          (compiled end)
  let desingularizedDerivative =
        desingularized (derivative start) (derivative middle) (derivative end)
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
          (compiled curve)
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
