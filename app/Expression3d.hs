module Expression3d (
    Expression3d,
    evaluate,
    bounds,
    derivative,
    zero,
    constant,
    xyz,
    squaredMagnitude,
    magnitude,
    normalize,
) where

import Curve1d (Curve1d (Curve1d), IsCurve1d)
import qualified Curve1d
import OpenSolid hiding (zero)
import Range (Range)
import qualified Units
import Vector3d (Vector3d (Vector3d))
import qualified Vector3d
import VectorBox3d (VectorBox3d (..))
import qualified VectorBox3d

data Expression3d units coordinates = Expression3d
    { evaluate :: !(Float -> Vector3d units coordinates)
    , bounds :: !(Range Unitless -> VectorBox3d units coordinates)
    , derivative :: ~(Expression3d units coordinates)
    }

instance Units.Coercion (Expression3d units coordinates) (Expression3d Unitless coordinates)

zero :: Expression3d units coordinates
zero =
    constant Vector3d.zero

constant :: Vector3d units coordinates -> Expression3d units coordinates
constant vector =
    Expression3d
        { evaluate = always vector
        , bounds = always (VectorBox3d.constant vector)
        , derivative = constant Vector3d.zero
        }

xyz :: Curve1d units -> Curve1d units -> Curve1d units -> Expression3d units coordinates
xyz x y z =
    Expression3d
        { evaluate = \t -> Vector3d (Curve1d.pointOn x t) (Curve1d.pointOn y t) (Curve1d.pointOn z t)
        , bounds = \t -> VectorBox3d (Curve1d.segmentBounds x t) (Curve1d.segmentBounds y t) (Curve1d.segmentBounds z t)
        , derivative = xyz (Curve1d.derivative x) (Curve1d.derivative y) (Curve1d.derivative z)
        }

instance Negation (Expression3d units coordinates) where
    negate expression =
        Expression3d
            { evaluate = evaluate expression >> negate
            , bounds = bounds expression >> negate
            , derivative = negate (derivative expression)
            }

instance Addition (Expression3d units) (Expression3d units) (Expression3d units) where
    expression1 + expression2 =
        Expression3d
            { evaluate = \t -> evaluate expression1 t + evaluate expression2 t
            , bounds = \t -> bounds expression1 t + bounds expression2 t
            , derivative = derivative expression1 + derivative expression2
            }

instance Subtraction (Expression3d units) (Expression3d units) (Expression3d units) where
    expression1 - expression2 =
        Expression3d
            { evaluate = \t -> evaluate expression1 t - evaluate expression2 t
            , bounds = \t -> bounds expression1 t - bounds expression2 t
            , derivative = derivative expression1 - derivative expression2
            }

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Curve1d units1) (Expression3d units2 coordinates) (Expression3d units3 coordinates) where
    curve1d * expression3d =
        Expression3d
            { evaluate = \t -> Curve1d.pointOn curve1d t * evaluate expression3d t
            , bounds = \t -> Curve1d.segmentBounds curve1d t * bounds expression3d t
            , derivative = Curve1d.derivative curve1d * expression3d + curve1d * derivative expression3d
            }

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => Multiplication (Expression3d units1 coordinates) (Curve1d units2) (Expression3d units3 coordinates) where
    expression3d * curve1d =
        Expression3d
            { evaluate = \t -> evaluate expression3d t * Curve1d.pointOn curve1d t
            , bounds = \t -> bounds expression3d t * Curve1d.segmentBounds curve1d t
            , derivative = expression3d * Curve1d.derivative curve1d + derivative expression3d * curve1d
            }

data DotProductOf units1 units2 coordinates = DotProductOf (Expression3d units1 coordinates) (Expression3d units2 coordinates)

instance Multiplication (Qty units1) (Qty units2) (Qty units) => IsCurve1d (DotProductOf units1 units2 coordinates) units where
    pointOn (DotProductOf expression1 expression2) t =
        evaluate expression1 t <> evaluate expression2 t

    segmentBounds (DotProductOf expression1 expression2) t =
        bounds expression1 t <> bounds expression2 t

    derivative (DotProductOf expression1 expression2) =
        derivative expression1 <> expression2 + expression1 <> derivative expression2

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => DotProduct (Expression3d units1) (Expression3d units2) (Curve1d units3) where
    expression1 <> expression2 =
        Curve1d (DotProductOf expression1 expression2)

instance Multiplication (Qty units1) (Qty units2) (Qty units3) => CrossProduct (Expression3d units1) (Expression3d units2) (Expression3d units3) where
    expression1 >< expression2 =
        Expression3d
            (\t -> evaluate expression1 t >< evaluate expression2 t)
            (\t -> bounds expression1 t >< bounds expression2 t)
            (derivative expression1 >< expression2 + expression1 >< derivative expression2)

instance Division (Qty units1) (Qty units2) (Qty units3) => Division (Expression3d units1 coordinates) (Curve1d units2) (Expression3d units3 coordinates) where
    expression3d / curve1d =
        Expression3d
            (\t -> evaluate expression3d t / Curve1d.pointOn curve1d t)
            (\t -> bounds expression3d t / Curve1d.segmentBounds curve1d t)
            ( let p = Units.drop expression3d
                  q = Units.drop curve1d
                  p' = derivative p
                  q' = Curve1d.derivative q
               in Units.add ((p' * q - p * q') / Curve1d.squared q)
            )

newtype SquaredMagnitudeOf units coordinates = SquaredMagnitudeOf (Expression3d units coordinates)

instance Multiplication (Qty units1) (Qty units1) (Qty units2) => IsCurve1d (SquaredMagnitudeOf units1 coordinates) units2 where
    pointOn (SquaredMagnitudeOf expression) t =
        Vector3d.squaredMagnitude (evaluate expression t)

    segmentBounds (SquaredMagnitudeOf expression) t =
        VectorBox3d.squaredMagnitude (bounds expression t)

    derivative (SquaredMagnitudeOf expression) =
        2.0 * expression <> derivative expression

squaredMagnitude :: Multiplication (Qty units1) (Qty units1) (Qty units2) => Expression3d units1 coordinates -> Curve1d units2
squaredMagnitude expression =
    Curve1d (SquaredMagnitudeOf expression)

magnitude :: Expression3d units coordinates -> Curve1d units
magnitude expression =
    let f = Units.drop expression
     in Units.add (Curve1d.sqrt (squaredMagnitude f))

normalize :: Expression3d units coordinates -> Expression3d Unitless coordinates
normalize expression =
    expression / magnitude expression
