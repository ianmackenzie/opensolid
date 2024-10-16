module Surface1d.Function (Interface (..), Function, unwrap, evaluate, bounds) where

import Curve1d (Curve1d)
import Curve2d (Curve2d)
import Function qualified
import OpenSolid
import Range (Range)
import {-# SOURCE #-} Surface1d.Function.Symbolic (Symbolic)
import Units qualified
import Uv (Parameter)
import Uv qualified

class
  Show function =>
  Interface function units
    | function -> units
  where
  evaluateImpl :: function -> Uv.Point -> Qty units
  boundsImpl :: function -> Uv.Bounds -> Range units
  derivativeImpl :: Parameter -> function -> Function units
  expressionImpl :: function -> Maybe (Function.Function Uv.Point (Qty units))

type role Function nominal

type Function :: Type -> Type
data Function units

instance Show (Function units)

instance Negation (Function units)

instance Division' (Function units1) (Function units2)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function units1) (Function units2) (Function units3)

instance Composition (Curve2d Uv.Coordinates) (Function units) (Curve1d units)

evaluate :: Function units -> Uv.Point -> Qty units
bounds :: Function units -> Uv.Bounds -> Range units
unwrap :: Function units -> Symbolic units
