module OpenSolid.SurfaceFunction
  ( SurfaceFunction
  , Compiled
  , compiled
  , evaluate
  , evaluateBounds
  , derivative
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (SurfaceParameter, UvBounds, UvPoint)
import OpenSolid.Units qualified as Units

type role SurfaceFunction nominal

type SurfaceFunction :: Type -> Type
data SurfaceFunction units

type Compiled units = CompiledFunction UvPoint (Qty units) UvBounds (Bounds units)

instance Negation (SurfaceFunction units)

instance
  Division'
    (SurfaceFunction units1)
    (SurfaceFunction units2)
    (SurfaceFunction (units1 :/: units2))

instance
  Units.Quotient units1 units2 units3 =>
  Division (SurfaceFunction units1) (SurfaceFunction units2) (SurfaceFunction units3)

compiled :: SurfaceFunction units -> Compiled units
evaluate :: SurfaceFunction units -> UvPoint -> Qty units
evaluateBounds :: SurfaceFunction units -> UvBounds -> Bounds units
derivative :: SurfaceParameter -> SurfaceFunction units -> SurfaceFunction units
