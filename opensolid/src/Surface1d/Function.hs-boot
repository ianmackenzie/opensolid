module Surface1d.Function (Function, evaluate, bounds) where

import Curve1d (Curve1d)
import Curve2d (Curve2d)
import OpenSolid
import Range (Range)
import Units qualified
import Uv qualified

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
