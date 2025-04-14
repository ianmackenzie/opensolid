module OpenSolid.SurfaceFunction
  ( SurfaceFunction
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.SurfaceParameter (SurfaceParameter, UvBounds, UvPoint)
import OpenSolid.Units qualified as Units

type role SurfaceFunction nominal

type SurfaceFunction :: Type -> Type
data SurfaceFunction units

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
