module OpenSolid.SurfaceFunction2D
  ( SurfaceFunction2D
  , Compiled
  , derivative
  , new
  , xy
  )
where

import GHC.Records (HasField)
import {-# SOURCE #-} OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds2D, Point2D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import {-# SOURCE #-} OpenSolid.UvBounds (UvBounds)
import {-# SOURCE #-} OpenSolid.UvPoint (UvPoint)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)

type role SurfaceFunction2D nominal

type SurfaceFunction2D :: Type -> Type
data SurfaceFunction2D units

type Compiled units =
  CompiledFunction UvPoint (Point2D units) UvBounds (Bounds2D units)

instance HasField "compiled" (SurfaceFunction2D units) (Compiled units)

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction2D units1)
    (VectorSurfaceFunction2D units2)
    (SurfaceFunction2D units1)

instance
  units1 ~ units2 =>
  Subtraction
    (SurfaceFunction2D units1)
    (SurfaceFunction2D units2)
    (VectorSurfaceFunction2D units1)

instance
  unitless ~ Unitless =>
  Composition (SurfaceFunction2D units) (Curve2D unitless) (Curve2D units)

instance
  unitless ~ Unitless =>
  Composition (SurfaceFunction1D units) (SurfaceFunction2D unitless) (SurfaceFunction1D units)

instance
  unitless ~ Unitless =>
  Composition
    (VectorSurfaceFunction2D units)
    (SurfaceFunction2D unitless)
    (VectorSurfaceFunction2D units)

instance
  unitless ~ Unitless =>
  Composition
    (VectorSurfaceFunction3D units space)
    (SurfaceFunction2D unitless)
    (VectorSurfaceFunction3D units space)

new ::
  Compiled units ->
  (SurfaceParameter -> VectorSurfaceFunction2D units) ->
  SurfaceFunction2D units
xy :: SurfaceFunction1D units -> SurfaceFunction1D units -> SurfaceFunction2D units
derivative :: SurfaceParameter -> SurfaceFunction2D units -> VectorSurfaceFunction2D units
