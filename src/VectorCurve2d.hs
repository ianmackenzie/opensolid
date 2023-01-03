module VectorCurve2d (
    VectorCurve2d (..),
    IsVectorCurve2d,
    pointOn,
    segmentBounds,
    derivative,
    zero,
    constant,
    xy,
    squaredMagnitude,
    magnitude,
    normalize,
) where

import Curve1d (Curve1d (Curve1d), IsCurve1d)
import Curve1d qualified
import Data.Kind (Type)
import Generic qualified
import OpenSolid
import Range (Range)
import Units qualified
import Vector2d (Vector2d)
import Vector2d qualified
import VectorBox2d (VectorBox2d (VectorBox2d))
import VectorBox2d qualified

class IsVectorCurve2d curve units coordinates | curve -> units, curve -> coordinates where
    pointOn :: curve -> Float -> Vector2d units coordinates
    segmentBounds :: curve -> Range Unitless -> VectorBox2d units coordinates
    derivative :: curve -> VectorCurve2d units coordinates

type VectorCurve2d :: Type -> Type -> Type
data VectorCurve2d units coordinates where
    VectorCurve2d :: forall curve units coordinates. IsVectorCurve2d curve units coordinates => curve -> VectorCurve2d units coordinates
    Zero :: VectorCurve2d units coordinates
    Constant :: Vector2d units coordinates -> VectorCurve2d units coordinates
    XY :: Curve1d units -> Curve1d units -> VectorCurve2d units coordinates
    Negated :: VectorCurve2d units coordinates -> VectorCurve2d units coordinates
    Sum :: VectorCurve2d units coordinates -> VectorCurve2d units coordinates -> VectorCurve2d units coordinates
    Difference :: VectorCurve2d units coordinates -> VectorCurve2d units coordinates -> VectorCurve2d units coordinates
    Product1d2d :: forall units1 units2 units3 coordinates. Multiplication (Qty units1) (Qty units2) (Qty units3) => Curve1d units1 -> VectorCurve2d units2 coordinates -> VectorCurve2d units3 coordinates
    Product2d1d :: forall units1 units2 units3 coordinates. Multiplication (Qty units1) (Qty units2) (Qty units3) => VectorCurve2d units1 coordinates -> Curve1d units2 -> VectorCurve2d units3 coordinates
    Quotient :: forall units1 units2 units3 coordinates. Division (Qty units1) (Qty units2) (Qty units3) => VectorCurve2d units1 coordinates -> Curve1d units2 -> VectorCurve2d units3 coordinates

instance Units.Coercion (VectorCurve2d units coordinates) (VectorCurve2d Unitless coordinates)

instance IsVectorCurve2d (VectorCurve2d units coordinates) units coordinates where
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
            Quotient c1 c2 ->
                let p = Units.drop c1
                    q = Units.drop c2
                    p' = derivative p
                    q' = Curve1d.derivative q
                 in Units.add ((p' * q - p * q') / Curve1d.squared q)

instance Generic.Zero (VectorCurve2d units) where
    zero = Zero

instance Negation (VectorCurve2d units coordinates) where
    negate Zero = Zero
    negate (Constant value) = Constant -value
    negate (XY x y) = XY -x -y
    negate (Negated c) = c
    negate (Difference c1 c2) = Difference c2 c1
    negate (Product1d2d c1 c2) = Product1d2d -c1 c2
    negate (Product2d1d c1 c2) = Product2d1d c1 -c2
    negate curve = Negated curve

instance Addition (VectorCurve2d units) (VectorCurve2d units) (VectorCurve2d units) where
    -- TODO add special cases
    c1 + c2 = Sum c1 c2

instance Addition (VectorCurve2d units) (Vector2d units) (VectorCurve2d units) where
    curve + vector = curve + constant vector

instance Addition (Vector2d units) (VectorCurve2d units) (VectorCurve2d units) where
    vector + curve = constant vector + curve

instance Subtraction (VectorCurve2d units) (VectorCurve2d units) (VectorCurve2d units) where
    -- TODO add special cases
    c1 - c2 = Difference c1 c2

instance Subtraction (VectorCurve2d units) (Vector2d units) (VectorCurve2d units) where
    curve - vector = curve - constant vector

instance Subtraction (Vector2d units) (VectorCurve2d units) (VectorCurve2d units) where
    vector - curve = constant vector + curve

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (VectorCurve2d units2 coordinates) (VectorCurve2d units3 coordinates) where
    -- TODO add special cases
    c1 * c2 = Product1d2d c1 c2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (VectorCurve2d units2 coordinates) (VectorCurve2d units3 coordinates) where
    c1 * c2 = Curve1d.constant c1 * c2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (VectorCurve2d units1 coordinates) (Curve1d units2) (VectorCurve2d units3 coordinates) where
    -- TODO add special cases
    c1 * c2 = Product2d1d c1 c2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (VectorCurve2d units1 coordinates) (Qty units2) (VectorCurve2d units3 coordinates) where
    curve * value = curve * Curve1d.constant value

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (VectorCurve2d units1 coordinates) (Curve1d units2) (VectorCurve2d units3 coordinates) where
    -- TODO add special cases
    c1 / c2 = Quotient c1 c2

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (VectorCurve2d units1 coordinates) (Qty units2) (VectorCurve2d units3 coordinates) where
    curve / value = curve / Curve1d.constant value

data DotProductOf units1 units2 coordinates = DotProductOf (VectorCurve2d units1 coordinates) (VectorCurve2d units2 coordinates)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => IsCurve1d (DotProductOf units1 units2 coordinates) units3 where
    pointOn (DotProductOf c1 c2) t = pointOn c1 t <> pointOn c2 t
    segmentBounds (DotProductOf c1 c2) t = segmentBounds c1 t <> segmentBounds c2 t
    derivative (DotProductOf c1 c2) = derivative c1 <> c2 + c1 <> derivative c2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (VectorCurve2d units1) (VectorCurve2d units2) (Curve1d units3) where
    -- TODO add special cases
    curve1 <> curve2 = Curve1d (DotProductOf curve1 curve2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (VectorCurve2d units1) (Vector2d units2) (Curve1d units3) where
    curve <> vector = curve <> constant vector

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (Vector2d units1) (VectorCurve2d units2) (Curve1d units3) where
    vector <> curve = constant vector <> curve

zero :: VectorCurve2d units coordinates
zero = Zero

constant :: Vector2d units coordinates -> VectorCurve2d units coordinates
constant = Constant

xy :: Curve1d units -> Curve1d units -> VectorCurve2d units coordinates
xy = XY

newtype SquaredMagnitudeOf units coordinates = SquaredMagnitudeOf (VectorCurve2d units coordinates)

instance Squared (Qty units1) (Qty units2) => IsCurve1d (SquaredMagnitudeOf units1 coordinates) units2 where
    pointOn (SquaredMagnitudeOf expression) t = Vector2d.squaredMagnitude (pointOn expression t)
    segmentBounds (SquaredMagnitudeOf expression) t = VectorBox2d.squaredMagnitude (segmentBounds expression t)
    derivative (SquaredMagnitudeOf expression) = 2.0 * expression <> derivative expression

squaredMagnitude :: Squared (Qty units1) (Qty units2) => VectorCurve2d units1 coordinates -> Curve1d units2
squaredMagnitude expression = Curve1d (SquaredMagnitudeOf expression)

magnitude :: VectorCurve2d units coordinates -> Curve1d units
magnitude expression =
    let f = Units.drop expression
     in Units.add (Curve1d.sqrt (squaredMagnitude f))

normalize :: VectorCurve2d units coordinates -> VectorCurve2d Unitless coordinates
normalize expression = expression / magnitude expression
