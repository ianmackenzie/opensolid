module Surface1d.Function (Interface (..), Function, evaluate, evaluateBounds) where

import Curve1d (Curve1d)
import Curve2d (Curve2d)
import OpenSolid
import Range (Range)
import SurfaceParameter (SurfaceParameter, UvBounds, UvCoordinates, UvPoint)
import Units qualified

class
  Show function =>
  Interface function units
    | function -> units
  where
  evaluateImpl :: function -> UvPoint -> Qty units
  evaluateBoundsImpl :: function -> UvBounds -> Range units
  derivativeImpl :: SurfaceParameter -> function -> Function units

type role Function nominal

type Function :: Type -> Type
data Function units

instance Show (Function units)

instance Negation (Function units)

instance Division' (Function units1) (Function units2)

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function units1) (Function units2) (Function units3)

instance Composition (Curve2d UvCoordinates) (Function units) (Curve1d units)

evaluate :: Function units -> UvPoint -> Qty units
evaluateBounds :: Function units -> UvBounds -> Range units
