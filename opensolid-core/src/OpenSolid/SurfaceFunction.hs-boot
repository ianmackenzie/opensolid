module OpenSolid.SurfaceFunction
  ( Interface (..)
  , SurfaceFunction (Parametric)
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
  derivativeImpl :: SurfaceParameter -> function -> SurfaceFunction units

type role SurfaceFunction nominal

data SurfaceFunction units where
  SurfaceFunction ::
    Interface function units =>
    function ->
    SurfaceFunction units
  Parametric ::
    Expression UvPoint (Qty units) ->
    SurfaceFunction units
  Negated ::
    SurfaceFunction units ->
    SurfaceFunction units
  Sum ::
    SurfaceFunction units ->
    SurfaceFunction units ->
    SurfaceFunction units
  Difference ::
    SurfaceFunction units ->
    SurfaceFunction units ->
    SurfaceFunction units
  Product' ::
    SurfaceFunction units1 ->
    SurfaceFunction units2 ->
    SurfaceFunction (units1 :*: units2)
  Quotient' ::
    SurfaceFunction units1 ->
    SurfaceFunction units2 ->
    SurfaceFunction (units1 :/: units2)
  Squared' ::
    SurfaceFunction units ->
    SurfaceFunction (units :*: units)
  SquareRoot' ::
    SurfaceFunction (units :*: units) ->
    SurfaceFunction units
  Sin ::
    SurfaceFunction Radians ->
    SurfaceFunction Unitless
  Cos ::
    SurfaceFunction Radians ->
    SurfaceFunction Unitless
  Coerce ::
    SurfaceFunction units1 ->
    SurfaceFunction units2

instance Show (SurfaceFunction units)

instance Negation (SurfaceFunction units)

instance
  Division'
    (SurfaceFunction units1)
    (SurfaceFunction units2)
    (SurfaceFunction (units1 :/: units2))

instance
  Units.Quotient units1 units2 units3 =>
  Division (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units3)

evaluate :: SurfaceFunction units -> UvPoint -> Qty units
evaluateBounds :: SurfaceFunction units -> UvBounds -> Range units
derivative :: SurfaceParameter -> SurfaceFunction units -> SurfaceFunction units
