module VectorCurve2d
  ( VectorCurve2d (..)
  , IsVectorCurve2d
  , pointOn
  , segmentBounds
  , derivative
  , zero
  , constant
  , xy
  , squaredMagnitude
  , magnitude
  , normalize
  )
where

import Curve1d (Curve1d (Curve1d), IsCurve1d)
import Curve1d qualified
import Generic qualified
import OpenSolid
import Range (Range)
import Units (HasUnits, Unitless)
import Units qualified
import Vector2d (Vector2d)
import Vector2d qualified
import VectorBox2d (VectorBox2d (VectorBox2d))
import VectorBox2d qualified

class IsVectorCurve2d curve coordinates units | curve -> units, curve -> coordinates where
  pointOn :: curve -> Float -> Vector2d coordinates units
  segmentBounds :: curve -> Range Unitless -> VectorBox2d coordinates units
  derivative :: curve -> VectorCurve2d coordinates units

type VectorCurve2d :: Type -> Type -> Type
data VectorCurve2d coordinates units where
  VectorCurve2d :: forall curve coordinates units. IsVectorCurve2d curve coordinates units => curve -> VectorCurve2d coordinates units
  Zero :: VectorCurve2d coordinates units
  Constant :: Vector2d coordinates units -> VectorCurve2d coordinates units
  XY :: Curve1d units -> Curve1d units -> VectorCurve2d coordinates units
  Negated :: VectorCurve2d coordinates units -> VectorCurve2d coordinates units
  Sum :: VectorCurve2d coordinates units -> VectorCurve2d coordinates units -> VectorCurve2d coordinates units
  Difference :: VectorCurve2d coordinates units -> VectorCurve2d coordinates units -> VectorCurve2d coordinates units
  Product1d2d :: forall units1 units2 coordinates units3. Units.Product units1 units2 units3 => Curve1d units1 -> VectorCurve2d coordinates units2 -> VectorCurve2d coordinates units3
  Product2d1d :: forall units1 units2 coordinates units3. Units.Product units1 units2 units3 => VectorCurve2d coordinates units1 -> Curve1d units2 -> VectorCurve2d coordinates units3
  Quotient :: forall units1 units2 coordinates units3. Units.Quotient units1 units2 units3 => VectorCurve2d coordinates units1 -> Curve1d units2 -> VectorCurve2d coordinates units3

instance HasUnits (VectorCurve2d coordinates)

instance IsVectorCurve2d (VectorCurve2d coordinates units) coordinates units where
  pointOn curve t =
    case curve of
      VectorCurve2d c -> pointOn c t
      Zero -> Vector2d.zero
      Constant value -> value
      XY x y -> Vector2d.xy (Curve1d.pointOn x t) (Curve1d.pointOn y t)
      Negated c -> -(pointOn c t)
      Sum c1 c2 -> pointOn c1 t + pointOn c2 t
      Difference c1 c2 -> pointOn c1 t - pointOn c2 t
      Product1d2d c1 c2 -> Curve1d.pointOn c1 t * pointOn c2 t
      Product2d1d c1 c2 -> pointOn c1 t * Curve1d.pointOn c2 t
      Quotient c1 c2 -> pointOn c1 t / Curve1d.pointOn c2 t

  segmentBounds curve t =
    case curve of
      VectorCurve2d c -> segmentBounds c t
      Zero -> VectorBox2d.constant Vector2d.zero
      Constant value -> VectorBox2d.constant value
      XY x y -> VectorBox2d (Curve1d.segmentBounds x t) (Curve1d.segmentBounds y t)
      Negated c -> -(segmentBounds c t)
      Sum c1 c2 -> segmentBounds c1 t + segmentBounds c2 t
      Difference c1 c2 -> segmentBounds c1 t - segmentBounds c2 t
      Product1d2d c1 c2 -> Curve1d.segmentBounds c1 t * segmentBounds c2 t
      Product2d1d c1 c2 -> segmentBounds c1 t * Curve1d.segmentBounds c2 t
      Quotient c1 c2 -> segmentBounds c1 t / Curve1d.segmentBounds c2 t

  derivative curve =
    case curve of
      VectorCurve2d c -> derivative c
      Zero -> Zero
      Constant _ -> Zero
      XY x y -> XY (Curve1d.derivative x) (Curve1d.derivative y)
      Negated c -> -(derivative c)
      Sum c1 c2 -> derivative c1 + derivative c2
      Difference c1 c2 -> derivative c1 - derivative c2
      Product1d2d c1 c2 -> Curve1d.derivative c1 * c2 + c1 * derivative c2
      Product2d1d c1 c2 -> derivative c1 * c2 + c1 * Curve1d.derivative c2
      Quotient c1 c2 -> derivative c1 / c2 + curve * (Curve1d.derivative c2 / c2)

instance Generic.Zero (VectorCurve2d coordinates units) where
  zero = Zero

instance Negation (VectorCurve2d coordinates units) where
  negate Zero = Zero
  negate (Constant value) = Constant -value
  negate (XY x y) = XY -x -y
  negate (Negated c) = c
  negate (Difference c1 c2) = Difference c2 c1
  negate (Product1d2d c1 c2) = Product1d2d -c1 c2
  negate (Product2d1d c1 c2) = Product2d1d c1 -c2
  negate curve = Negated curve

instance coordinates ~ coordinates' => Addition (VectorCurve2d coordinates units) (VectorCurve2d coordinates' units) (VectorCurve2d coordinates units) where
  -- TODO add special cases
  c1 + c2 = Sum c1 c2

instance coordinates ~ coordinates' => Addition (VectorCurve2d coordinates units) (Vector2d coordinates' units) (VectorCurve2d coordinates units) where
  curve + vector = curve + constant vector

instance coordinates ~ coordinates' => Addition (Vector2d coordinates units) (VectorCurve2d coordinates' units) (VectorCurve2d coordinates units) where
  vector + curve = constant vector + curve

instance coordinates ~ coordinates' => Subtraction (VectorCurve2d coordinates units) (VectorCurve2d coordinates' units) (VectorCurve2d coordinates units) where
  -- TODO add special cases
  c1 - c2 = Difference c1 c2

instance coordinates ~ coordinates' => Subtraction (VectorCurve2d coordinates units) (Vector2d coordinates' units) (VectorCurve2d coordinates units) where
  curve - vector = curve - constant vector

instance coordinates ~ coordinates' => Subtraction (Vector2d coordinates units) (VectorCurve2d coordinates' units) (VectorCurve2d coordinates units) where
  vector - curve = constant vector + curve

instance Units.Product units1 units2 units3 => Multiplication (Curve1d units1) (VectorCurve2d coordinates units2) (VectorCurve2d coordinates units3) where
  -- TODO add special cases
  c1 * c2 = Product1d2d c1 c2

instance Units.Product units1 units2 units3 => Multiplication (Qty units1) (VectorCurve2d coordinates units2) (VectorCurve2d coordinates units3) where
  c1 * c2 = Curve1d.constant c1 * c2

instance Units.Product units1 units2 units3 => Multiplication (VectorCurve2d coordinates units1) (Curve1d units2) (VectorCurve2d coordinates units3) where
  -- TODO add special cases
  c1 * c2 = Product2d1d c1 c2

instance Units.Product units1 units2 units3 => Multiplication (VectorCurve2d coordinates units1) (Qty units2) (VectorCurve2d coordinates units3) where
  curve * value = curve * Curve1d.constant value

instance Units.Quotient units1 units2 units3 => Division (VectorCurve2d coordinates units1) (Curve1d units2) (VectorCurve2d coordinates units3) where
  -- TODO add special cases
  c1 / c2 = Quotient c1 c2

instance Units.Quotient units1 units2 units3 => Division (VectorCurve2d coordinates units1) (Qty units2) (VectorCurve2d coordinates units3) where
  curve / value = curve / Curve1d.constant value

data DotProductOf units1 coordinates units2 = DotProductOf (VectorCurve2d coordinates units1) (VectorCurve2d coordinates units2)

instance Units.Product units1 units2 units3 => IsCurve1d (DotProductOf units1 coordinates units2) units3 where
  pointOn (DotProductOf c1 c2) t = pointOn c1 t <> pointOn c2 t
  segmentBounds (DotProductOf c1 c2) t = segmentBounds c1 t <> segmentBounds c2 t
  derivative (DotProductOf c1 c2) = derivative c1 <> c2 + c1 <> derivative c2

instance (Units.Product units1 units2 units3, coordinates ~ coordinates') => DotProduct (VectorCurve2d coordinates units1) (VectorCurve2d coordinates' units2) (Curve1d units3) where
  -- TODO add special cases
  curve1 <> curve2 = Curve1d (DotProductOf curve1 curve2)

instance (Units.Product units1 units2 units3, coordinates ~ coordinates') => DotProduct (VectorCurve2d coordinates units1) (Vector2d coordinates' units2) (Curve1d units3) where
  curve <> vector = curve <> constant vector

instance (Units.Product units1 units2 units3, coordinates ~ coordinates') => DotProduct (Vector2d coordinates units1) (VectorCurve2d coordinates' units2) (Curve1d units3) where
  vector <> curve = constant vector <> curve

zero :: VectorCurve2d coordinates units
zero = Zero

constant :: Vector2d coordinates units -> VectorCurve2d coordinates units
constant = Constant

xy :: Curve1d units -> Curve1d units -> VectorCurve2d coordinates units
xy = XY

newtype MagnitudeOf coordinates units = MagnitudeOf (VectorCurve2d coordinates units)

instance IsCurve1d (MagnitudeOf coordinates units) units where
  pointOn (MagnitudeOf curve) t = Vector2d.magnitude (pointOn curve t)
  segmentBounds (MagnitudeOf curve) t = VectorBox2d.magnitude (segmentBounds curve t)
  derivative (MagnitudeOf curve) = derivative curve <> normalize curve

newtype SquaredMagnitudeOf coordinates units = SquaredMagnitudeOf (VectorCurve2d coordinates units)

instance Units.Squared units1 units2 => IsCurve1d (SquaredMagnitudeOf coordinates units1) units2 where
  pointOn (SquaredMagnitudeOf curve) t = Vector2d.squaredMagnitude (pointOn curve t)
  segmentBounds (SquaredMagnitudeOf curve) t = VectorBox2d.squaredMagnitude (segmentBounds curve t)
  derivative (SquaredMagnitudeOf curve) = 2.0 * curve <> derivative curve

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve2d coordinates units1 -> Curve1d units2
squaredMagnitude curve = Curve1d (SquaredMagnitudeOf curve)

magnitude :: VectorCurve2d coordinates units -> Curve1d units
magnitude curve = Curve1d (MagnitudeOf curve)

normalize :: VectorCurve2d coordinates units -> VectorCurve2d coordinates Unitless
normalize curve = curve / magnitude curve
