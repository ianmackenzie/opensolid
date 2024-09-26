module Surface1d.Function (Interface(..), Function, unwrap, evaluate, bounds) where

import Curve1d (Curve1d)
import Curve2d (Curve2d)
import OpenSolid
import {-# SOURCE #-} Surface1d.Function.Expression (Expression)
import Range (Range)
import Units qualified
import Uv qualified
import Jit qualified
import Uv (Parameter)

class Known function => Interface function units | function -> units where
  evaluateImpl :: function -> Uv.Point -> Qty units
  boundsImpl :: function -> Uv.Bounds -> Range units
  derivativeImpl :: Parameter -> function -> Function units
  toAstImpl :: function -> Jit.Ast Uv.Point Float

type role Function nominal

type Function :: Type -> Type
data Function units

instance Show (Function units)

instance Eq (Function units)

instance Known units => Negation (Function units)

instance
  (Known units1, Known units2) =>
  Division' (Function units1) (Function units2)

instance
  (Known units1, Known units2, Known units3, Units.Quotient units1 units2 units3) =>
  Division (Function units1) (Function units2) (Function units3)

instance
  Known units =>
  Composition (Curve2d Uv.Coordinates) (Function units) (Curve1d units)

evaluate :: Function units -> Uv.Point -> Qty units
bounds :: Function units -> Uv.Bounds -> Range units
unwrap :: Function units -> Expression units
