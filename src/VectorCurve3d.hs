module VectorCurve3d (
    VectorCurve3d,
    IsVectorCurve3d,
    pointOn,
    segmentBounds,
    derivative,
    zero,
    constant,
    xyz,
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
import Vector3d (Vector3d (Vector3d))
import Vector3d qualified
import VectorBox3d (VectorBox3d (VectorBox3d))
import VectorBox3d qualified

class IsVectorCurve3d curve units coordinates | curve -> units, curve -> coordinates where
    pointOn :: curve -> Float -> Vector3d units coordinates
    segmentBounds :: curve -> Range Unitless -> VectorBox3d units coordinates
    derivative :: curve -> VectorCurve3d units coordinates

type VectorCurve3d :: Type -> Type -> Type
data VectorCurve3d units coordinates = forall curve. IsVectorCurve3d curve units coordinates => VectorCurve3d curve

instance IsVectorCurve3d (VectorCurve3d units coordinates) units coordinates where
    pointOn (VectorCurve3d curve) = pointOn curve
    segmentBounds (VectorCurve3d curve) = segmentBounds curve
    derivative (VectorCurve3d curve) = derivative curve

instance Units.Coercion (VectorCurve3d units coordinates) (VectorCurve3d Unitless coordinates)

newtype Constant units coordinates = Constant (Vector3d units coordinates)

instance IsVectorCurve3d (Constant units coordinates) units coordinates where
    pointOn (Constant value) _ =
        value

    segmentBounds (Constant value) _ =
        VectorBox3d.constant value

    derivative (Constant _) =
        zero

constant :: Vector3d units coordinates -> VectorCurve3d units coordinates
constant vector =
    VectorCurve3d (Constant vector)

zero :: VectorCurve3d units coordinates
zero =
    constant Vector3d.zero

instance Generic.Zero (VectorCurve3d units) where
    zero = zero

data XYZ units coordinates = XYZ (Curve1d units) (Curve1d units) (Curve1d units)

instance IsVectorCurve3d (XYZ units coordinates) units coordinates where
    pointOn (XYZ x y z) t =
        Vector3d (Curve1d.pointOn x t) (Curve1d.pointOn y t) (Curve1d.pointOn z t)

    segmentBounds (XYZ x y z) t =
        VectorBox3d (Curve1d.segmentBounds x t) (Curve1d.segmentBounds y t) (Curve1d.segmentBounds z t)

    derivative (XYZ x y z) =
        xyz (Curve1d.derivative x) (Curve1d.derivative y) (Curve1d.derivative z)

xyz :: forall units coordinates. Curve1d units -> Curve1d units -> Curve1d units -> VectorCurve3d units coordinates
xyz x y z =
    let impl :: XYZ units coordinates = XYZ x y z
     in VectorCurve3d impl

newtype Negated units coordinates = Negated (VectorCurve3d units coordinates)

instance IsVectorCurve3d (Negated units coordinates) units coordinates where
    pointOn (Negated curve) t =
        negate (pointOn curve t)

    segmentBounds (Negated curve) t =
        negate (segmentBounds curve t)

    derivative (Negated curve) =
        negate (derivative curve)

instance Negation (VectorCurve3d units coordinates) where
    negate curve =
        VectorCurve3d (Negated curve)

data Sum units coordinates = Sum (VectorCurve3d units coordinates) (VectorCurve3d units coordinates)

instance IsVectorCurve3d (Sum units coordinates) units coordinates where
    pointOn (Sum curve1 curve2) t =
        pointOn curve1 t + pointOn curve2 t

    segmentBounds (Sum curve1 curve2) t =
        segmentBounds curve1 t + segmentBounds curve2 t

    derivative (Sum curve1 curve2) =
        derivative curve1 + derivative curve2

instance Addition (VectorCurve3d units) (VectorCurve3d units) (VectorCurve3d units) where
    curve1 + curve2 =
        VectorCurve3d (Sum curve1 curve2)

instance Addition (VectorCurve3d units) (Vector3d units) (VectorCurve3d units) where
    curve + vector =
        curve + constant vector

instance Addition (Vector3d units) (VectorCurve3d units) (VectorCurve3d units) where
    vector + curve =
        constant vector + curve

data Difference units coordinates = Difference (VectorCurve3d units coordinates) (VectorCurve3d units coordinates)

instance IsVectorCurve3d (Difference units coordinates) units coordinates where
    pointOn (Difference curve1 curve2) t =
        pointOn curve1 t - pointOn curve2 t

    segmentBounds (Difference curve1 curve2) t =
        segmentBounds curve1 t - segmentBounds curve2 t

    derivative (Difference curve1 curve2) =
        derivative curve1 - derivative curve2

instance Subtraction (VectorCurve3d units) (VectorCurve3d units) (VectorCurve3d units) where
    curve1 - curve2 =
        VectorCurve3d (Difference curve1 curve2)

instance Subtraction (VectorCurve3d units) (Vector3d units) (VectorCurve3d units) where
    curve - vector =
        curve - constant vector

instance Subtraction (Vector3d units) (VectorCurve3d units) (VectorCurve3d units) where
    vector - curve =
        constant vector - curve

data Product1d3d units1 units2 coordinates = Product1d3d (Curve1d units1) (VectorCurve3d units2 coordinates)

data Product3d1d units1 units2 coordinates = Product3d1d (VectorCurve3d units1 coordinates) (Curve1d units2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => IsVectorCurve3d (Product3d1d units1 units2 coordinates) units3 coordinates where
    pointOn (Product3d1d vectorCurve3d curve1d) t =
        pointOn vectorCurve3d t * Curve1d.pointOn curve1d t

    segmentBounds (Product3d1d vectorCurve3d curve1d) t =
        segmentBounds vectorCurve3d t * Curve1d.segmentBounds curve1d t

    derivative (Product3d1d vectorCurve3d curve1d) =
        derivative vectorCurve3d * curve1d + vectorCurve3d * Curve1d.derivative curve1d

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => IsVectorCurve3d (Product1d3d units1 units2 coordinates) units3 coordinates where
    pointOn (Product1d3d curve1d vectorCurve3d) t =
        Curve1d.pointOn curve1d t * pointOn vectorCurve3d t

    segmentBounds (Product1d3d curve1d vectorCurve3d) t =
        Curve1d.segmentBounds curve1d t * segmentBounds vectorCurve3d t

    derivative (Product1d3d curve1d vectorCurve3d) =
        Curve1d.derivative curve1d * vectorCurve3d + curve1d * derivative vectorCurve3d

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (VectorCurve3d units1 coordinates) (Curve1d units2) (VectorCurve3d units3 coordinates) where
    vectorCurve3d * curve1d =
        VectorCurve3d (Product3d1d vectorCurve3d curve1d)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (VectorCurve3d units2 coordinates) (VectorCurve3d units3 coordinates) where
    curve1d * vectorCurve3d =
        VectorCurve3d (Product1d3d curve1d vectorCurve3d)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (VectorCurve3d units1 coordinates) (Qty units2) (VectorCurve3d units3 coordinates) where
    curve * value =
        VectorCurve3d (Product3d1d curve (Curve1d.constant value))

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Qty units1) (VectorCurve3d units2 coordinates) (VectorCurve3d units3 coordinates) where
    value * curve =
        VectorCurve3d (Product1d3d (Curve1d.constant value) curve)

data DotProductOf units1 units2 coordinates = DotProductOf (VectorCurve3d units1 coordinates) (VectorCurve3d units2 coordinates)

instance Multiplication (Qty units1) (Qty units2) (Qty units) => IsCurve1d (DotProductOf units1 units2 coordinates) units where
    pointOn (DotProductOf curve1 curve2) t =
        pointOn curve1 t <> pointOn curve2 t

    segmentBounds (DotProductOf curve1 curve2) t =
        segmentBounds curve1 t <> segmentBounds curve2 t

    derivative (DotProductOf curve1 curve2) =
        derivative curve1 <> curve2 + curve1 <> derivative curve2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (VectorCurve3d units1) (VectorCurve3d units2) (Curve1d units3) where
    curve1 <> curve2 =
        Curve1d (DotProductOf curve1 curve2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (VectorCurve3d units1) (Vector3d units2) (Curve1d units3) where
    curve <> vector =
        Curve1d (DotProductOf curve (constant vector))

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (Vector3d units1) (VectorCurve3d units2) (Curve1d units3) where
    vector <> curve =
        Curve1d (DotProductOf (constant vector) curve)

data CrossProductOf units1 units2 coordinates = CrossProductOf (VectorCurve3d units1 coordinates) (VectorCurve3d units2 coordinates)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => IsVectorCurve3d (CrossProductOf units1 units2 coordinates) units3 coordinates where
    pointOn (CrossProductOf curve1 curve2) t =
        pointOn curve1 t >< pointOn curve2 t

    segmentBounds (CrossProductOf curve1 curve2) t =
        segmentBounds curve1 t >< segmentBounds curve2 t

    derivative (CrossProductOf curve1 curve2) =
        derivative curve1 >< curve2 + curve1 >< derivative curve2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => CrossProduct (VectorCurve3d units1) (VectorCurve3d units2) (VectorCurve3d units3) where
    curve1 >< curve2 =
        VectorCurve3d (CrossProductOf curve1 curve2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => CrossProduct (Vector3d units1) (VectorCurve3d units2) (VectorCurve3d units3) where
    vector >< curve =
        VectorCurve3d (CrossProductOf (constant vector) curve)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => CrossProduct (VectorCurve3d units1) (Vector3d units2) (VectorCurve3d units3) where
    curve >< vector =
        VectorCurve3d (CrossProductOf curve (constant vector))

data Quotient units1 units2 coordinates = Quotient (VectorCurve3d units1 coordinates) (Curve1d units2)

instance Division (Qty units1) (Qty units2) (Qty units3) => IsVectorCurve3d (Quotient units1 units2 coordinates) units3 coordinates where
    pointOn (Quotient vectorCurve3d curve1d) t =
        pointOn vectorCurve3d t / Curve1d.pointOn curve1d t

    segmentBounds (Quotient vectorCurve3d curve1d) t =
        segmentBounds vectorCurve3d t / Curve1d.segmentBounds curve1d t

    derivative (Quotient vectorCurve3d curve1d) =
        let p = Units.drop vectorCurve3d
            q = Units.drop curve1d
            p' = derivative p
            q' = Curve1d.derivative q
         in Units.add ((p' * q - p * q') / Curve1d.squared q)

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (VectorCurve3d units1 coordinates) (Curve1d units2) (VectorCurve3d units3 coordinates) where
    vectorCurve3d / curve1d =
        VectorCurve3d (Quotient vectorCurve3d curve1d)

newtype SquaredMagnitudeOf units coordinates = SquaredMagnitudeOf (VectorCurve3d units coordinates)

instance Squared (Qty units1) (Qty units2) => IsCurve1d (SquaredMagnitudeOf units1 coordinates) units2 where
    pointOn (SquaredMagnitudeOf expression) t =
        Vector3d.squaredMagnitude (pointOn expression t)

    segmentBounds (SquaredMagnitudeOf expression) t =
        VectorBox3d.squaredMagnitude (segmentBounds expression t)

    derivative (SquaredMagnitudeOf expression) =
        2.0 * expression <> derivative expression

squaredMagnitude :: Squared (Qty units1) (Qty units2) => VectorCurve3d units1 coordinates -> Curve1d units2
squaredMagnitude expression =
    Curve1d (SquaredMagnitudeOf expression)

magnitude :: VectorCurve3d units coordinates -> Curve1d units
magnitude expression =
    let f = Units.drop expression
     in Units.add (Curve1d.sqrt (squaredMagnitude f))

normalize :: VectorCurve3d units coordinates -> VectorCurve3d Unitless coordinates
normalize expression =
    expression / magnitude expression
