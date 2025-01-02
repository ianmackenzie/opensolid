module OpenSolid.Surface1d.Function (Interface (..), Function, evaluate, evaluateBounds) where

import OpenSolid.Curve1d (Curve1d)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.SurfaceParameter (SurfaceParameter, UvBounds, UvCoordinates, UvPoint)
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

type Function :: Type -> Type
data Function units

instance Show (Function units)

instance Negation (Function units)

instance Division' (Function units1) (Function units2) (Function (units1 :/: units2))

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function units1) (Function units2) (Function units3)

instance Composition (Curve2d UvCoordinates) (Function units) (Curve1d units)

evaluate :: Function units -> UvPoint -> Qty units
evaluateBounds :: Function units -> UvBounds -> Range units
