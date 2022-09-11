module Expression1d (
    Expression1d (Expression1d),
    bounds,
    derivative,
    zero,
    constant,
    parameter,
    squared,
) where

import Interval (Interval)
import qualified Interval
import OpenSolid
import qualified Quantity
import qualified Units

data Expression1d units = Expression1d
    { bounds :: Interval Unitless -> Interval units
    , derivative :: Expression1d units
    }

zero :: Expression1d units
zero =
    constant Quantity.zero

constant :: Quantity units -> Expression1d units
constant value =
    Expression1d
        { bounds = always (Interval.singleton value)
        , derivative = constant Quantity.zero
        }

parameter :: Expression1d Unitless
parameter =
    Expression1d
        { bounds = identity
        , derivative = constant 1.0
        }

instance Negation (Expression1d units) where
    negate expression =
        Expression1d
            { bounds = \t -> - (bounds expression t)
            , derivative = - (derivative expression)
            }

instance Addition Expression1d Quantity where
    type Sum Expression1d Quantity = Expression1d
    expression + quantity =
        Expression1d
            { bounds = \t -> bounds expression t + quantity
            , derivative = derivative expression
            }

instance Addition Quantity Expression1d where
    type Sum Quantity Expression1d = Expression1d
    quantity + expression =
        expression + quantity

instance Addition Expression1d Expression1d where
    type Sum Expression1d Expression1d = Expression1d
    expression1 + expression2 =
        Expression1d
            { bounds = \t -> bounds expression1 t + bounds expression2 t
            , derivative = derivative expression1 + derivative expression2
            }

instance Subtraction Expression1d Quantity where
    type Difference Expression1d Quantity = Expression1d
    expression - quantity =
        Expression1d
            { bounds = \t -> bounds expression t - quantity
            , derivative = derivative expression
            }

instance Subtraction Quantity Expression1d where
    type Difference Quantity Expression1d = Expression1d
    quantity - expression =
        Expression1d
            { bounds = \t -> quantity - bounds expression t
            , derivative = - (derivative expression)
            }

instance Subtraction Expression1d Expression1d where
    type Difference Expression1d Expression1d = Expression1d
    expression1 - expression2 =
        Expression1d
            { bounds = \t -> bounds expression1 t - bounds expression2 t
            , derivative = derivative expression1 - derivative expression2
            }

instance Units.Multiplication units1 units2 => Multiplication (Quantity units1) (Expression1d units2) where
    type Product (Quantity units1) (Expression1d units2) = Expression1d (Units.Product units1 units2)
    quantity * expression =
        Expression1d
            { bounds = \t -> quantity * bounds expression t
            , derivative = quantity * derivative expression
            }

instance Units.Multiplication units1 units2 => Multiplication (Expression1d units1) (Quantity units2) where
    type Product (Expression1d units1) (Quantity units2) = Expression1d (Units.Product units1 units2)
    expression * quantity =
        Expression1d
            { bounds = \t -> bounds expression t * quantity
            , derivative = derivative expression * quantity
            }

instance Units.Multiplication units1 units2 => Multiplication (Expression1d units1) (Expression1d units2) where
    type Product (Expression1d units1) (Expression1d units2) = Expression1d (Units.Product units1 units2)
    expression1 * expression2 =
        Expression1d
            { bounds = \t -> bounds expression1 t * bounds expression2 t
            , derivative = derivative expression1 * expression2 + expression1 * derivative expression2
            }

squared :: Units.Multiplication units units => Expression1d units -> Expression1d (Units.Product units units)
squared expression =
    Expression1d
        { bounds = \t -> let x = bounds expression t in Interval.squared x
        , derivative = 2.0 * expression * derivative expression
        }
