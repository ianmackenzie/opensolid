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

import Expression1d (Expression1d (Expression1d))
import qualified Expression1d
import OpenSolid
import Range (Range)
import qualified Units
import Vector3d (Vector3d (Vector3d))
import qualified Vector3d
import VectorBox3d (VectorBox3d (..))
import qualified VectorBox3d

data Expression3d scalar coordinates = Expression3d
    { evaluate :: !(Float -> Vector3d scalar coordinates)
    , bounds :: !(Range Float -> VectorBox3d scalar coordinates)
    , derivative :: ~(Expression3d scalar coordinates)
    }

instance Units.Coercion (Expression3d scalar coordinates) (Expression3d Float coordinates)

zero :: Scalar scalar => Expression3d scalar coordinates
zero =
    constant Vector3d.zero

constant :: Scalar scalar => Vector3d scalar coordinates -> Expression3d scalar coordinates
constant vector =
    Expression3d
        { evaluate = always vector
        , bounds = always (VectorBox3d.constant vector)
        , derivative = constant Vector3d.zero
        }

xyz :: Scalar scalar => Expression1d scalar -> Expression1d scalar -> Expression1d scalar -> Expression3d scalar coordinates
xyz x y z =
    Expression3d
        { evaluate = \t -> Vector3d (Expression1d.evaluate x t) (Expression1d.evaluate y t) (Expression1d.evaluate z t)
        , bounds = \t -> VectorBox3d (Expression1d.bounds x t) (Expression1d.bounds y t) (Expression1d.bounds z t)
        , derivative = xyz (Expression1d.derivative x) (Expression1d.derivative y) (Expression1d.derivative z)
        }

instance Scalar scalar => Negation (Expression3d scalar coordinates) where
    negate expression =
        Expression3d
            { evaluate = evaluate expression >> negate
            , bounds = bounds expression >> negate
            , derivative = negate (derivative expression)
            }

instance Scalar scalar => Addition (Expression3d scalar coordinates) where
    expression1 + expression2 =
        Expression3d
            { evaluate = \t -> evaluate expression1 t + evaluate expression2 t
            , bounds = \t -> bounds expression1 t + bounds expression2 t
            , derivative = derivative expression1 + derivative expression2
            }

instance Scalar scalar => Subtraction (Expression3d scalar coordinates) where
    expression1 - expression2 =
        Expression3d
            { evaluate = \t -> evaluate expression1 t - evaluate expression2 t
            , bounds = \t -> bounds expression1 t - bounds expression2 t
            , derivative = derivative expression1 - derivative expression2
            }

instance Scalar scalar => Multiplication (Expression1d Float) (Expression3d scalar coordinates) (Expression3d scalar coordinates) where
    expression1d * expression3d =
        Expression3d
            { evaluate = \t -> Expression1d.evaluate expression1d t * evaluate expression3d t
            , bounds = \t -> Expression1d.bounds expression1d t * bounds expression3d t
            , derivative = Expression1d.derivative expression1d * expression3d + expression1d * derivative expression3d
            }

instance Scalar scalar => Multiplication (Expression3d scalar coordinates) (Expression1d Float) (Expression3d scalar coordinates) where
    expression3d * expression1d =
        Expression3d
            { evaluate = \t -> Expression1d.evaluate expression1d t * evaluate expression3d t
            , bounds = \t -> Expression1d.bounds expression1d t * bounds expression3d t
            , derivative = Expression1d.derivative expression1d * expression3d + expression1d * derivative expression3d
            }

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => Multiplication (Expression1d (Quantity units)) (Expression3d scalar coordinates) (Expression3d result coordinates) where
    expression1d * expression3d =
        Expression3d
            { evaluate = \t -> Expression1d.evaluate expression1d t * evaluate expression3d t
            , bounds = \t -> Expression1d.bounds expression1d t * bounds expression3d t
            , derivative = Expression1d.derivative expression1d * expression3d + expression1d * derivative expression3d
            }

instance (Scalar scalar, Scalar result, Multiplication (Quantity units) scalar result) => Multiplication (Expression3d scalar coordinates) (Expression1d (Quantity units)) (Expression3d result coordinates) where
    expression3d * expression1d =
        Expression3d
            { evaluate = \t -> Expression1d.evaluate expression1d t * evaluate expression3d t
            , bounds = \t -> Expression1d.bounds expression1d t * bounds expression3d t
            , derivative = Expression1d.derivative expression1d * expression3d + expression1d * derivative expression3d
            }

instance (Scalar scalar1, Scalar scalar2, Scalar result, Multiplication scalar1 scalar2 result) => DotProduct (Expression3d scalar1) (Expression3d scalar2) (Expression1d result) where
    expression1 <> expression2 =
        Expression1d
            (\t -> evaluate expression1 t <> evaluate expression2 t)
            (\t -> bounds expression1 t <> bounds expression2 t)
            (derivative expression1 <> expression2 + expression1 <> derivative expression2)

instance (Scalar scalar1, Scalar scalar2, Scalar result, Multiplication scalar1 scalar2 result) => CrossProduct (Expression3d scalar1) (Expression3d scalar2) (Expression3d result) where
    expression1 >< expression2 =
        Expression3d
            (\t -> evaluate expression1 t >< evaluate expression2 t)
            (\t -> bounds expression1 t >< bounds expression2 t)
            (derivative expression1 >< expression2 + expression1 >< derivative expression2)

instance (Scalar scalar1, Scalar scalar2, Scalar result, Division scalar1 scalar2 result) => Division (Expression3d scalar1 coordinates) (Expression1d scalar2) (Expression3d result coordinates) where
    expression1 / expression2 =
        Expression3d
            (\t -> evaluate expression1 t / Expression1d.evaluate expression2 t)
            (\t -> bounds expression1 t / Expression1d.bounds expression2 t)
            ( let p = Units.drop expression1
                  q = Units.drop expression2
                  p' = derivative p
                  q' = Expression1d.derivative q
               in Units.add ((p' * q - p * q') / Expression1d.squared q)
            )

squaredMagnitude :: (Scalar scalar, Scalar squaredScalar, Multiplication scalar scalar squaredScalar) => Expression3d scalar coordinates -> Expression1d squaredScalar
squaredMagnitude expression =
    Expression1d
        (evaluate expression >> Vector3d.squaredMagnitude)
        (bounds expression >> VectorBox3d.squaredMagnitude)
        (Expression1d.constant 2.0 * expression <> derivative expression)

magnitude :: Scalar scalar => Expression3d scalar coordinates -> Expression1d scalar
magnitude expression =
    let f = Units.drop expression
     in Units.add (Expression1d.sqrt (squaredMagnitude f))

normalize :: Scalar scalar => Expression3d scalar coordinates -> Expression3d Float coordinates
normalize expression =
    expression / magnitude expression
