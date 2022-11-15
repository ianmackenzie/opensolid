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

data Expression3d qty coordinates = Expression3d
    { evaluate :: !(Float -> Vector3d qty coordinates)
    , bounds :: !(Range Float -> VectorBox3d qty coordinates)
    , derivative :: ~(Expression3d qty coordinates)
    }

instance Units.Coercion (Expression3d (Qty a) coordinates) (Expression3d Float coordinates)

zero :: Expression3d (Qty a) coordinates
zero =
    constant Vector3d.zero

constant :: Vector3d (Qty a) coordinates -> Expression3d (Qty a) coordinates
constant vector =
    Expression3d
        { evaluate = always vector
        , bounds = always (VectorBox3d.constant vector)
        , derivative = constant Vector3d.zero
        }

xyz :: Expression1d (Qty a) -> Expression1d (Qty a) -> Expression1d (Qty a) -> Expression3d (Qty a) coordinates
xyz x y z =
    Expression3d
        { evaluate = \t -> Vector3d (Expression1d.evaluate x t) (Expression1d.evaluate y t) (Expression1d.evaluate z t)
        , bounds = \t -> VectorBox3d (Expression1d.bounds x t) (Expression1d.bounds y t) (Expression1d.bounds z t)
        , derivative = xyz (Expression1d.derivative x) (Expression1d.derivative y) (Expression1d.derivative z)
        }

instance Negation (Expression3d (Qty a) coordinates) where
    negate expression =
        Expression3d
            { evaluate = evaluate expression >> negate
            , bounds = bounds expression >> negate
            , derivative = negate (derivative expression)
            }

instance Addition (Expression3d (Qty a)) (Expression3d (Qty a)) (Expression3d (Qty a)) coordinates where
    expression1 + expression2 =
        Expression3d
            { evaluate = \t -> evaluate expression1 t + evaluate expression2 t
            , bounds = \t -> bounds expression1 t + bounds expression2 t
            , derivative = derivative expression1 + derivative expression2
            }

instance Subtraction (Expression3d (Qty a)) (Expression3d (Qty a)) (Expression3d (Qty a)) coordinates where
    expression1 - expression2 =
        Expression3d
            { evaluate = \t -> evaluate expression1 t - evaluate expression2 t
            , bounds = \t -> bounds expression1 t - bounds expression2 t
            , derivative = derivative expression1 - derivative expression2
            }

instance Multiplication (Qty a) (Qty b) (Qty c) => Multiplication (Expression1d (Qty a)) (Expression3d (Qty b) coordinates) (Expression3d (Qty c) coordinates) where
    expression1d * expression3d =
        Expression3d
            { evaluate = \t -> Expression1d.evaluate expression1d t * evaluate expression3d t
            , bounds = \t -> Expression1d.bounds expression1d t * bounds expression3d t
            , derivative = Expression1d.derivative expression1d * expression3d + expression1d * derivative expression3d
            }

instance Multiplication (Qty a) (Qty b) (Qty c) => Multiplication (Expression3d (Qty a) coordinates) (Expression1d (Qty b)) (Expression3d (Qty c) coordinates) where
    expression3d * expression1d =
        Expression3d
            { evaluate = \t -> evaluate expression3d t * Expression1d.evaluate expression1d t
            , bounds = \t -> bounds expression3d t * Expression1d.bounds expression1d t
            , derivative = expression3d * Expression1d.derivative expression1d + derivative expression3d * expression1d
            }

instance Multiplication (Qty a) (Qty b) (Qty c) => DotProduct (Expression3d (Qty a)) (Expression3d (Qty b)) (Expression1d (Qty c)) coordinates where
    expression1 <> expression2 =
        Expression1d
            (\t -> evaluate expression1 t <> evaluate expression2 t)
            (\t -> bounds expression1 t <> bounds expression2 t)
            (derivative expression1 <> expression2 + expression1 <> derivative expression2)

instance Multiplication (Qty a) (Qty b) (Qty c) => CrossProduct (Expression3d (Qty a)) (Expression3d (Qty b)) (Expression3d (Qty c)) coordinates where
    expression1 >< expression2 =
        Expression3d
            (\t -> evaluate expression1 t >< evaluate expression2 t)
            (\t -> bounds expression1 t >< bounds expression2 t)
            (derivative expression1 >< expression2 + expression1 >< derivative expression2)

instance Division (Qty a) (Qty b) (Qty c) => Division (Expression3d (Qty a) coordinates) (Expression1d (Qty b)) (Expression3d (Qty c) coordinates) where
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

squaredMagnitude :: Multiplication (Qty a) (Qty a) (Qty b) => Expression3d (Qty a) coordinates -> Expression1d (Qty b)
squaredMagnitude expression =
    Expression1d
        (evaluate expression >> Vector3d.squaredMagnitude)
        (bounds expression >> VectorBox3d.squaredMagnitude)
        (2.0 * expression <> derivative expression)

magnitude :: Expression3d (Qty a) coordinates -> Expression1d (Qty a)
magnitude expression =
    let f = Units.drop expression
     in Units.add (Expression1d.sqrt (squaredMagnitude f))

normalize :: Expression3d (Qty a) coordinates -> Expression3d Float coordinates
normalize expression =
    expression / magnitude expression
