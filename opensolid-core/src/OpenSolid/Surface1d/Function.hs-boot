module OpenSolid.Surface1d.Function
  ( Interface (..)
  , Function (Parametric)
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import OpenSolid.Expression (Expression)
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.SurfaceParameter (SurfaceParameter, UvBounds, UvPoint)
import OpenSolid.Units (Radians)
import OpenSolid.Units qualified as Units

class
  Show function =>
  Interface function units
    | function -> units
  where
  evaluateImpl :: function -> UvPoint -> Qty units
  evaluateBoundsImpl :: function -> UvBounds -> Range units
  derivativeImpl :: SurfaceParameter -> function -> Function units

type role Function nominal

data Function units where
  Function ::
    Interface function units =>
    function ->
    Function units
  Parametric ::
    Expression UvPoint (Qty units) ->
    Function units
  Negated ::
    Function units ->
    Function units
  Sum ::
    Function units ->
    Function units ->
    Function units
  Difference ::
    Function units ->
    Function units ->
    Function units
  Product' ::
    Function units1 ->
    Function units2 ->
    Function (units1 :*: units2)
  Quotient' ::
    Function units1 ->
    Function units2 ->
    Function (units1 :/: units2)
  Squared' ::
    Function units ->
    Function (units :*: units)
  SquareRoot' ::
    Function (units :*: units) ->
    Function units
  Sin ::
    Function Radians ->
    Function Unitless
  Cos ::
    Function Radians ->
    Function Unitless
  Coerce ::
    Function units1 ->
    Function units2

instance Show (Function units)

instance Negation (Function units)

instance Division' (Function units1) (Function units2) (Function (units1 :/: units2))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function units1) (Function units2) (Function units3)

evaluate :: Function units -> UvPoint -> Qty units
evaluateBounds :: Function units -> UvBounds -> Range units
derivative :: SurfaceParameter -> Function units -> Function units
