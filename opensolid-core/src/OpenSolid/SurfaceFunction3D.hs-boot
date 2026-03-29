module OpenSolid.SurfaceFunction3D
  ( SurfaceFunction3D
  , Compiled
  , new
  , constant
  , point
  , bounds
  , derivative
  , transformBy
  )
where

import GHC.Records (HasField)
import {-# SOURCE #-} OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds3D, Point3D)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Transform3D (Transform3D)
import {-# SOURCE #-} OpenSolid.UvBounds (UvBounds)
import {-# SOURCE #-} OpenSolid.UvPoint (UvPoint)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)

type role SurfaceFunction3D nominal

type SurfaceFunction3D :: Type -> Type
data SurfaceFunction3D space

instance
  HasField
    "du"
    (SurfaceFunction3D space)
    (VectorSurfaceFunction3D Meters space)

instance
  HasField
    "dv"
    (SurfaceFunction3D space)
    (VectorSurfaceFunction3D Meters space)

type Compiled space =
  CompiledFunction UvPoint (Point3D space) UvBounds (Bounds3D space)

instance HasField "compiled" (SurfaceFunction3D space) (Compiled space)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (SurfaceFunction3D space1)
    (VectorSurfaceFunction3D meters space2)
    (SurfaceFunction3D space1)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (SurfaceFunction3D space1)
    (VectorSurfaceFunction3D meters space2)
    (SurfaceFunction3D space1)

new ::
  Compiled space ->
  (SurfaceParameter -> VectorSurfaceFunction3D Meters space) ->
  SurfaceFunction3D space
constant :: Point3D space -> SurfaceFunction3D space
point :: SurfaceFunction3D space -> UvPoint -> Point3D space
bounds :: SurfaceFunction3D space -> UvBounds -> Bounds3D space
derivative ::
  SurfaceParameter ->
  SurfaceFunction3D space ->
  VectorSurfaceFunction3D Meters space
transformBy :: Transform3D tag space -> SurfaceFunction3D space -> SurfaceFunction3D space
