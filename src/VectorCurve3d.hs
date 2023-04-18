module VectorCurve3d
  ( VectorCurve3d
  , IsVectorCurve3d
  , evaluate
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

class IsVectorCurve3d curve (coordinateSystem :: CoordinateSystem) | curve -> coordinateSystem where
  evaluateImpl :: curve -> Float -> Vector3d coordinateSystem
  segmentBoundsImpl :: curve -> Range Unitless -> VectorBox3d coordinateSystem
  derivativeImpl :: curve -> VectorCurve3d coordinateSystem

data VectorCurve3d (coordinateSystem :: CoordinateSystem) where
  VectorCurve3d :: IsVectorCurve3d curve (space @ units) => curve -> VectorCurve3d (space @ units)

instance IsVectorCurve3d (VectorCurve3d (space @ units)) (space @ units) where
  evaluateImpl = evaluate
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative

instance
  (units1 ~ units1', units2 ~ units2', space ~ space')
  => Units.Coercion
      units1
      units2
      (VectorCurve3d (space @ units1'))
      (VectorCurve3d (space' @ units2'))

newtype Constant (coordinateSystem :: CoordinateSystem) = Constant (Vector3d coordinateSystem)

instance IsVectorCurve3d (Constant (space @ units)) (space @ units) where
  evaluateImpl (Constant value) _ = value
  segmentBoundsImpl (Constant value) _ = VectorBox3d.constant value
  derivativeImpl (Constant _) = zero

constant :: Vector3d (space @ units) -> VectorCurve3d (space @ units)
constant vector = VectorCurve3d (Constant vector)

zero :: VectorCurve3d (space @ units)
zero = constant Vector3d.zero

instance Generic.Zero (VectorCurve3d (space @ units)) where
  zero = zero

data XYZ (coordinateSystem :: CoordinateSystem)
  = XYZ
      (Curve1d (Units coordinateSystem))
      (Curve1d (Units coordinateSystem))
      (Curve1d (Units coordinateSystem))

instance IsVectorCurve3d (XYZ (space @ units)) (space @ units) where
  evaluateImpl (XYZ x y z) t =
    Vector3d (Curve1d.evaluate x t) (Curve1d.evaluate y t) (Curve1d.evaluate z t)

  segmentBoundsImpl (XYZ x y z) t =
    VectorBox3d (Curve1d.segmentBounds x t) (Curve1d.segmentBounds y t) (Curve1d.segmentBounds z t)

  derivativeImpl (XYZ x y z) =
    xyz (Curve1d.derivative x) (Curve1d.derivative y) (Curve1d.derivative z)

xyz
  :: forall space units
   . Curve1d units
  -> Curve1d units
  -> Curve1d units
  -> VectorCurve3d (space @ units)
xyz x y z = let impl :: XYZ (space @ units) = XYZ x y z in VectorCurve3d impl

newtype Negated (coordinateSystem :: CoordinateSystem) = Negated (VectorCurve3d coordinateSystem)

instance IsVectorCurve3d (Negated (space @ units)) (space @ units) where
  evaluateImpl (Negated curve) t = negate (evaluate curve t)
  segmentBoundsImpl (Negated curve) t = negate (segmentBounds curve t)
  derivativeImpl (Negated curve) = negate (derivative curve)

instance Negation (VectorCurve3d (space @ units)) where
  negate curve = VectorCurve3d (Negated curve)

data Sum (coordinateSystem :: CoordinateSystem)
  = Sum (VectorCurve3d coordinateSystem) (VectorCurve3d coordinateSystem)

instance IsVectorCurve3d (Sum (space @ units)) (space @ units) where
  evaluateImpl (Sum curve1 curve2) t = evaluate curve1 t + evaluate curve2 t
  segmentBoundsImpl (Sum curve1 curve2) t = segmentBounds curve1 t + segmentBounds curve2 t
  derivativeImpl (Sum curve1 curve2) = derivative curve1 + derivative curve2

instance
  space ~ space'
  => Addition
      (VectorCurve3d (space @ units))
      (VectorCurve3d (space' @ units))
      (VectorCurve3d (space @ units))
  where
  curve1 + curve2 = VectorCurve3d (Sum curve1 curve2)

instance
  space ~ space'
  => Addition
      (VectorCurve3d (space @ units))
      (Vector3d (space' @ units))
      (VectorCurve3d (space @ units))
  where
  curve + vector = curve + constant vector

instance
  space ~ space'
  => Addition
      (Vector3d (space @ units))
      (VectorCurve3d (space' @ units))
      (VectorCurve3d (space @ units))
  where
  vector + curve = constant vector + curve

data Difference (coordinateSystem :: CoordinateSystem)
  = Difference (VectorCurve3d coordinateSystem) (VectorCurve3d coordinateSystem)

instance IsVectorCurve3d (Difference (space @ units)) (space @ units) where
  evaluateImpl (Difference curve1 curve2) t = evaluate curve1 t - evaluate curve2 t
  segmentBoundsImpl (Difference curve1 curve2) t = segmentBounds curve1 t - segmentBounds curve2 t
  derivativeImpl (Difference curve1 curve2) = derivative curve1 - derivative curve2

instance
  space ~ space'
  => Subtraction
      (VectorCurve3d (space @ units))
      (VectorCurve3d (space' @ units))
      (VectorCurve3d (space @ units))
  where
  curve1 - curve2 = VectorCurve3d (Difference curve1 curve2)

instance
  space ~ space'
  => Subtraction
      (VectorCurve3d (space @ units))
      (Vector3d (space' @ units))
      (VectorCurve3d (space @ units))
  where
  curve - vector = curve - constant vector

instance
  space ~ space'
  => Subtraction
      (Vector3d (space @ units))
      (VectorCurve3d (space' @ units))
      (VectorCurve3d (space @ units))
  where
  vector - curve = constant vector - curve

data Product1d3d space units1 units2
  = Product1d3d (Curve1d units1) (VectorCurve3d (space @ units2))

data Product3d1d space units1 units2
  = Product3d1d (VectorCurve3d (space @ units1)) (Curve1d units2)

instance
  Units.Product units1 units2 units3
  => IsVectorCurve3d (Product3d1d space units1 units2) (space @ units3)
  where
  evaluateImpl (Product3d1d vectorCurve3d curve1d) t =
    evaluate vectorCurve3d t * Curve1d.evaluate curve1d t

  segmentBoundsImpl (Product3d1d vectorCurve3d curve1d) t =
    segmentBounds vectorCurve3d t * Curve1d.segmentBounds curve1d t

  derivativeImpl (Product3d1d vectorCurve3d curve1d) =
    derivative vectorCurve3d * curve1d + vectorCurve3d * Curve1d.derivative curve1d

instance
  Units.Product units1 units2 units3
  => IsVectorCurve3d (Product1d3d space units1 units2) (space @ units3)
  where
  evaluateImpl (Product1d3d curve1d vectorCurve3d) t =
    Curve1d.evaluate curve1d t * evaluate vectorCurve3d t

  segmentBoundsImpl (Product1d3d curve1d vectorCurve3d) t =
    Curve1d.segmentBounds curve1d t * segmentBounds vectorCurve3d t

  derivativeImpl (Product1d3d curve1d vectorCurve3d) =
    Curve1d.derivative curve1d * vectorCurve3d + curve1d * derivative vectorCurve3d

instance
  Units.Product units1 units2 units3
  => Multiplication
      (VectorCurve3d (space @ units1))
      (Curve1d units2)
      (VectorCurve3d (space @ units3))
  where
  vectorCurve3d * curve1d = VectorCurve3d (Product3d1d vectorCurve3d curve1d)

instance
  Units.Product units1 units2 units3
  => Multiplication
      (Curve1d units1)
      (VectorCurve3d (space @ units2))
      (VectorCurve3d (space @ units3))
  where
  curve1d * vectorCurve3d = VectorCurve3d (Product1d3d curve1d vectorCurve3d)

instance
  Units.Product units1 units2 units3
  => Multiplication
      (VectorCurve3d (space @ units1))
      (Qty units2)
      (VectorCurve3d (space @ units3))
  where
  curve * value = VectorCurve3d (Product3d1d curve (Curve1d.constant value))

instance
  Units.Product units1 units2 units3
  => Multiplication
      (Qty units1)
      (VectorCurve3d (space @ units2))
      (VectorCurve3d (space @ units3))
  where
  value * curve = VectorCurve3d (Product1d3d (Curve1d.constant value) curve)

data DotProductOf space units1 units2
  = DotProductOf (VectorCurve3d (space @ units1)) (VectorCurve3d (space @ units2))

instance
  Units.Product units1 units2 units
  => IsCurve1d (DotProductOf space units1 units2) units
  where
  evaluateImpl (DotProductOf curve1 curve2) t =
    evaluate curve1 t <> evaluate curve2 t

  segmentBoundsImpl (DotProductOf curve1 curve2) t =
    segmentBounds curve1 t <> segmentBounds curve2 t

  derivativeImpl (DotProductOf curve1 curve2) =
    derivative curve1 <> curve2 + curve1 <> derivative curve2

instance
  (Units.Product units1 units2 units3, space ~ space')
  => DotProduct
      (VectorCurve3d (space @ units1))
      (VectorCurve3d (space' @ units2))
      (Curve1d units3)
  where
  curve1 <> curve2 = Curve1d (DotProductOf curve1 curve2)

instance
  (Units.Product units1 units2 units3, space ~ space')
  => DotProduct
      (VectorCurve3d (space @ units1))
      (Vector3d (space' @ units2))
      (Curve1d units3)
  where
  curve <> vector = Curve1d (DotProductOf curve (constant vector))

instance
  (Units.Product units1 units2 units3, space ~ space')
  => DotProduct
      (Vector3d (space @ units1))
      (VectorCurve3d (space' @ units2))
      (Curve1d units3)
  where
  vector <> curve = Curve1d (DotProductOf (constant vector) curve)

data CrossProductOf space units1 units2
  = CrossProductOf (VectorCurve3d (space @ units1)) (VectorCurve3d (space @ units2))

instance
  Units.Product units1 units2 units3
  => IsVectorCurve3d (CrossProductOf space units1 units2) (space @ units3)
  where
  evaluateImpl (CrossProductOf curve1 curve2) t =
    evaluate curve1 t >< evaluate curve2 t

  segmentBoundsImpl (CrossProductOf curve1 curve2) t =
    segmentBounds curve1 t >< segmentBounds curve2 t

  derivativeImpl (CrossProductOf curve1 curve2) =
    derivative curve1 >< curve2 + curve1 >< derivative curve2

instance
  (Units.Product units1 units2 units3, space ~ space')
  => CrossProduct
      (VectorCurve3d (space @ units1))
      (VectorCurve3d (space' @ units2))
      (VectorCurve3d (space @ units3))
  where
  curve1 >< curve2 = VectorCurve3d (CrossProductOf curve1 curve2)

instance
  (Units.Product units1 units2 units3, space ~ space')
  => CrossProduct
      (Vector3d (space @ units1))
      (VectorCurve3d (space' @ units2))
      (VectorCurve3d (space @ units3))
  where
  vector >< curve = VectorCurve3d (CrossProductOf (constant vector) curve)

instance
  (Units.Product units1 units2 units3, space ~ space')
  => CrossProduct
      (VectorCurve3d (space @ units1))
      (Vector3d (space' @ units2))
      (VectorCurve3d (space @ units3))
  where
  curve >< vector = VectorCurve3d (CrossProductOf curve (constant vector))

data Quotient space units1 units2 = Quotient (VectorCurve3d (space @ units1)) (Curve1d units2)

instance
  Units.Quotient units1 units2 units3
  => IsVectorCurve3d (Quotient space units1 units2) (space @ units3)
  where
  evaluateImpl (Quotient vectorCurve3d curve1d) t =
    evaluate vectorCurve3d t / Curve1d.evaluate curve1d t

  segmentBoundsImpl (Quotient vectorCurve3d curve1d) t =
    segmentBounds vectorCurve3d t / Curve1d.segmentBounds curve1d t

  derivativeImpl (Quotient vectorCurve3d curve1d) =
    let p = Units.generalize vectorCurve3d
        q = Units.generalize curve1d
        p' = derivative p
        q' = Curve1d.derivative q
     in Units.specialize ((p' .* q - p .* q') ./ Curve1d.squared q)

instance
  Units.Quotient units1 units2 units3
  => Division
      (VectorCurve3d (space @ units1))
      (Curve1d units2)
      (VectorCurve3d (space @ units3))
  where
  vectorCurve3d / curve1d = VectorCurve3d (Quotient vectorCurve3d curve1d)

newtype SquaredMagnitudeOf (coordinateSystem :: CoordinateSystem)
  = SquaredMagnitudeOf (VectorCurve3d coordinateSystem)

instance Units.Squared units1 units2 => IsCurve1d (SquaredMagnitudeOf (space @ units1)) units2 where
  evaluateImpl (SquaredMagnitudeOf expression) t =
    Vector3d.squaredMagnitude (evaluate expression t)

  segmentBoundsImpl (SquaredMagnitudeOf expression) t =
    VectorBox3d.squaredMagnitude (segmentBounds expression t)

  derivativeImpl (SquaredMagnitudeOf expression) =
    2.0 * expression <> derivative expression

evaluate :: VectorCurve3d (space @ units) -> Float -> Vector3d (space @ units)
evaluate (VectorCurve3d curve) t = evaluateImpl curve t

segmentBounds :: VectorCurve3d (space @ units) -> Range Unitless -> VectorBox3d (space @ units)
segmentBounds (VectorCurve3d curve) t = segmentBoundsImpl curve t

derivative :: VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)
derivative (VectorCurve3d curve) = derivativeImpl curve

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve3d (space @ units1) -> Curve1d units2
squaredMagnitude expression =
  Curve1d (SquaredMagnitudeOf expression)

magnitude :: VectorCurve3d (space @ units) -> Curve1d units
magnitude expression =
  Units.specialize (Curve1d.sqrt (squaredMagnitude (Units.generalize expression)))

normalize :: VectorCurve3d (space @ units) -> VectorCurve3d (space @ Unitless)
normalize expression =
  expression / magnitude expression
