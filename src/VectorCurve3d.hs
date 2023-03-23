module VectorCurve3d
  ( VectorCurve3d
  , IsVectorCurve3d
  , pointOn
  , segmentBounds
  , derivative
  , zero
  , constant
  , xyz
  , squaredMagnitude
  , magnitude
  , normalize
  )
where

import CoordinateSystem (Units)
import Curve1d (Curve1d (Curve1d), IsCurve1d)
import Curve1d qualified
import Generic qualified
import OpenSolid
import Range (Range)
import Units (Unitless)
import Units qualified
import Vector3d (Vector3d (Vector3d))
import Vector3d qualified
import VectorBox3d (VectorBox3d (VectorBox3d))
import VectorBox3d qualified

class IsVectorCurve3d curve space units | curve -> space, curve -> units where
  pointOn :: curve -> Float -> Vector3d (space @ units)
  segmentBounds :: curve -> Range Unitless -> VectorBox3d (space @ units)
  derivative :: curve -> VectorCurve3d (space @ units)

data VectorCurve3d (coordinateSystem :: CoordinateSystem) where
  VectorCurve3d :: IsVectorCurve3d curve space units => curve -> VectorCurve3d (space @ units)

instance IsVectorCurve3d (VectorCurve3d (space @ units)) space units where
  pointOn (VectorCurve3d curve) = pointOn curve
  segmentBounds (VectorCurve3d curve) = segmentBounds curve
  derivative (VectorCurve3d curve) = derivative curve

instance
  (units1 ~ units1', units2 ~ units2', space ~ space')
  => Units.Coercion
      units1
      units2
      (VectorCurve3d (space @ units1'))
      (VectorCurve3d (space' @ units2'))

newtype Constant (coordinateSystem :: CoordinateSystem) = Constant (Vector3d coordinateSystem)

instance IsVectorCurve3d (Constant (space @ units)) space units where
  pointOn (Constant value) _ =
    value

  segmentBounds (Constant value) _ =
    VectorBox3d.constant value

  derivative (Constant _) =
    zero

constant :: Vector3d (space @ units) -> VectorCurve3d (space @ units)
constant vector =
  VectorCurve3d (Constant vector)

zero :: VectorCurve3d (space @ units)
zero =
  constant Vector3d.zero

instance Generic.Zero (VectorCurve3d (space @ units)) where
  zero = zero

data XYZ (coordinateSystem :: CoordinateSystem)
  = XYZ
      (Curve1d (Units coordinateSystem))
      (Curve1d (Units coordinateSystem))
      (Curve1d (Units coordinateSystem))

instance IsVectorCurve3d (XYZ (space @ units)) space units where
  pointOn (XYZ x y z) t =
    Vector3d (Curve1d.pointOn x t) (Curve1d.pointOn y t) (Curve1d.pointOn z t)

  segmentBounds (XYZ x y z) t =
    VectorBox3d (Curve1d.segmentBounds x t) (Curve1d.segmentBounds y t) (Curve1d.segmentBounds z t)

  derivative (XYZ x y z) =
    xyz (Curve1d.derivative x) (Curve1d.derivative y) (Curve1d.derivative z)

xyz :: forall space units. Curve1d units -> Curve1d units -> Curve1d units -> VectorCurve3d (space @ units)
xyz x y z = let impl :: XYZ (space @ units) = XYZ x y z in VectorCurve3d impl

newtype Negated (coordinateSystem :: CoordinateSystem) = Negated (VectorCurve3d coordinateSystem)

instance IsVectorCurve3d (Negated (space @ units)) space units where
  pointOn (Negated curve) t =
    negate (pointOn curve t)

  segmentBounds (Negated curve) t =
    negate (segmentBounds curve t)

  derivative (Negated curve) =
    negate (derivative curve)

instance Negation (VectorCurve3d (space @ units)) where
  negate curve =
    VectorCurve3d (Negated curve)

data Sum (coordinateSystem :: CoordinateSystem) = Sum (VectorCurve3d coordinateSystem) (VectorCurve3d coordinateSystem)

instance IsVectorCurve3d (Sum (space @ units)) space units where
  pointOn (Sum curve1 curve2) t =
    pointOn curve1 t + pointOn curve2 t

  segmentBounds (Sum curve1 curve2) t =
    segmentBounds curve1 t + segmentBounds curve2 t

  derivative (Sum curve1 curve2) =
    derivative curve1 + derivative curve2

instance space ~ space' => Addition (VectorCurve3d (space @ units)) (VectorCurve3d (space' @ units)) (VectorCurve3d (space @ units)) where
  curve1 + curve2 =
    VectorCurve3d (Sum curve1 curve2)

instance space ~ space' => Addition (VectorCurve3d (space @ units)) (Vector3d (space' @ units)) (VectorCurve3d (space @ units)) where
  curve + vector =
    curve + constant vector

instance space ~ space' => Addition (Vector3d (space @ units)) (VectorCurve3d (space' @ units)) (VectorCurve3d (space @ units)) where
  vector + curve =
    constant vector + curve

data Difference (coordinateSystem :: CoordinateSystem) = Difference (VectorCurve3d coordinateSystem) (VectorCurve3d coordinateSystem)

instance IsVectorCurve3d (Difference (space @ units)) space units where
  pointOn (Difference curve1 curve2) t =
    pointOn curve1 t - pointOn curve2 t

  segmentBounds (Difference curve1 curve2) t =
    segmentBounds curve1 t - segmentBounds curve2 t

  derivative (Difference curve1 curve2) =
    derivative curve1 - derivative curve2

instance space ~ space' => Subtraction (VectorCurve3d (space @ units)) (VectorCurve3d (space' @ units)) (VectorCurve3d (space @ units)) where
  curve1 - curve2 =
    VectorCurve3d (Difference curve1 curve2)

instance space ~ space' => Subtraction (VectorCurve3d (space @ units)) (Vector3d (space' @ units)) (VectorCurve3d (space @ units)) where
  curve - vector =
    curve - constant vector

instance space ~ space' => Subtraction (Vector3d (space @ units)) (VectorCurve3d (space' @ units)) (VectorCurve3d (space @ units)) where
  vector - curve =
    constant vector - curve

data Product1d3d space units1 units2 = Product1d3d (Curve1d units1) (VectorCurve3d (space @ units2))

data Product3d1d space units1 units2 = Product3d1d (VectorCurve3d (space @ units1)) (Curve1d units2)

instance Units.Product units1 units2 units3 => IsVectorCurve3d (Product3d1d space units1 units2) space units3 where
  pointOn (Product3d1d vectorCurve3d curve1d) t =
    pointOn vectorCurve3d t * Curve1d.pointOn curve1d t

  segmentBounds (Product3d1d vectorCurve3d curve1d) t =
    segmentBounds vectorCurve3d t * Curve1d.segmentBounds curve1d t

  derivative (Product3d1d vectorCurve3d curve1d) =
    derivative vectorCurve3d * curve1d + vectorCurve3d * Curve1d.derivative curve1d

instance Units.Product units1 units2 units3 => IsVectorCurve3d (Product1d3d space units1 units2) space units3 where
  pointOn (Product1d3d curve1d vectorCurve3d) t =
    Curve1d.pointOn curve1d t * pointOn vectorCurve3d t

  segmentBounds (Product1d3d curve1d vectorCurve3d) t =
    Curve1d.segmentBounds curve1d t * segmentBounds vectorCurve3d t

  derivative (Product1d3d curve1d vectorCurve3d) =
    Curve1d.derivative curve1d * vectorCurve3d + curve1d * derivative vectorCurve3d

instance Units.Product units1 units2 units3 => Multiplication (VectorCurve3d (space @ units1)) (Curve1d units2) (VectorCurve3d (space @ units3)) where
  vectorCurve3d * curve1d =
    VectorCurve3d (Product3d1d vectorCurve3d curve1d)

instance Units.Product units1 units2 units3 => Multiplication (Curve1d units1) (VectorCurve3d (space @ units2)) (VectorCurve3d (space @ units3)) where
  curve1d * vectorCurve3d =
    VectorCurve3d (Product1d3d curve1d vectorCurve3d)

instance Units.Product units1 units2 units3 => Multiplication (VectorCurve3d (space @ units1)) (Qty units2) (VectorCurve3d (space @ units3)) where
  curve * value =
    VectorCurve3d (Product3d1d curve (Curve1d.constant value))

instance Units.Product units1 units2 units3 => Multiplication (Qty units1) (VectorCurve3d (space @ units2)) (VectorCurve3d (space @ units3)) where
  value * curve =
    VectorCurve3d (Product1d3d (Curve1d.constant value) curve)

data DotProductOf space units1 units2 = DotProductOf (VectorCurve3d (space @ units1)) (VectorCurve3d (space @ units2))

instance Units.Product units1 units2 units => IsCurve1d (DotProductOf space units1 units2) units where
  pointOn (DotProductOf curve1 curve2) t =
    pointOn curve1 t <> pointOn curve2 t

  segmentBounds (DotProductOf curve1 curve2) t =
    segmentBounds curve1 t <> segmentBounds curve2 t

  derivative (DotProductOf curve1 curve2) =
    derivative curve1 <> curve2 + curve1 <> derivative curve2

instance (Units.Product units1 units2 units3, space ~ space') => DotProduct (VectorCurve3d (space @ units1)) (VectorCurve3d (space' @ units2)) (Curve1d units3) where
  curve1 <> curve2 =
    Curve1d (DotProductOf curve1 curve2)

instance (Units.Product units1 units2 units3, space ~ space') => DotProduct (VectorCurve3d (space @ units1)) (Vector3d (space' @ units2)) (Curve1d units3) where
  curve <> vector =
    Curve1d (DotProductOf curve (constant vector))

instance (Units.Product units1 units2 units3, space ~ space') => DotProduct (Vector3d (space @ units1)) (VectorCurve3d (space' @ units2)) (Curve1d units3) where
  vector <> curve =
    Curve1d (DotProductOf (constant vector) curve)

data CrossProductOf space units1 units2 = CrossProductOf (VectorCurve3d (space @ units1)) (VectorCurve3d (space @ units2))

instance Units.Product units1 units2 units3 => IsVectorCurve3d (CrossProductOf space units1 units2) space units3 where
  pointOn (CrossProductOf curve1 curve2) t =
    pointOn curve1 t >< pointOn curve2 t

  segmentBounds (CrossProductOf curve1 curve2) t =
    segmentBounds curve1 t >< segmentBounds curve2 t

  derivative (CrossProductOf curve1 curve2) =
    derivative curve1 >< curve2 + curve1 >< derivative curve2

instance (Units.Product units1 units2 units3, space ~ space') => CrossProduct (VectorCurve3d (space @ units1)) (VectorCurve3d (space' @ units2)) (VectorCurve3d (space @ units3)) where
  curve1 >< curve2 =
    VectorCurve3d (CrossProductOf curve1 curve2)

instance (Units.Product units1 units2 units3, space ~ space') => CrossProduct (Vector3d (space @ units1)) (VectorCurve3d (space' @ units2)) (VectorCurve3d (space @ units3)) where
  vector >< curve =
    VectorCurve3d (CrossProductOf (constant vector) curve)

instance (Units.Product units1 units2 units3, space ~ space') => CrossProduct (VectorCurve3d (space @ units1)) (Vector3d (space' @ units2)) (VectorCurve3d (space @ units3)) where
  curve >< vector =
    VectorCurve3d (CrossProductOf curve (constant vector))

data Quotient space units1 units2 = Quotient (VectorCurve3d (space @ units1)) (Curve1d units2)

instance Units.Quotient units1 units2 units3 => IsVectorCurve3d (Quotient space units1 units2) space units3 where
  pointOn (Quotient vectorCurve3d curve1d) t =
    pointOn vectorCurve3d t / Curve1d.pointOn curve1d t

  segmentBounds (Quotient vectorCurve3d curve1d) t =
    segmentBounds vectorCurve3d t / Curve1d.segmentBounds curve1d t

  derivative (Quotient vectorCurve3d curve1d) =
    let p = Units.generalize vectorCurve3d
        q = Units.generalize curve1d
        p' = derivative p
        q' = Curve1d.derivative q
     in Units.specialize ((p' .* q - p .* q') ./ Curve1d.squared q)

instance Units.Quotient units1 units2 units3 => Division (VectorCurve3d (space @ units1)) (Curve1d units2) (VectorCurve3d (space @ units3)) where
  vectorCurve3d / curve1d =
    VectorCurve3d (Quotient vectorCurve3d curve1d)

newtype SquaredMagnitudeOf (coordinateSystem :: CoordinateSystem) = SquaredMagnitudeOf (VectorCurve3d coordinateSystem)

instance Units.Squared units1 units2 => IsCurve1d (SquaredMagnitudeOf (space @ units1)) units2 where
  pointOn (SquaredMagnitudeOf expression) t =
    Vector3d.squaredMagnitude (pointOn expression t)

  segmentBounds (SquaredMagnitudeOf expression) t =
    VectorBox3d.squaredMagnitude (segmentBounds expression t)

  derivative (SquaredMagnitudeOf expression) =
    2.0 * expression <> derivative expression

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve3d (space @ units1) -> Curve1d units2
squaredMagnitude expression =
  Curve1d (SquaredMagnitudeOf expression)

magnitude :: VectorCurve3d (space @ units) -> Curve1d units
magnitude expression =
  Units.specialize (Curve1d.sqrt (squaredMagnitude (Units.generalize expression)))

normalize :: VectorCurve3d (space @ units) -> VectorCurve3d (space @ Unitless)
normalize expression =
  expression / magnitude expression
