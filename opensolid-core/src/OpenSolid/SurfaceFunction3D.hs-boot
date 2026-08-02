module OpenSolid.SurfaceFunction3D
  ( SurfaceFunction3D
  , Compiled
  , new
  , constant
  , pointAt
  , pointOn
  , range
  , compiled
  , partialDerivatives
  , transformBy
  )
where

import {-# SOURCE #-} OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds3D, Point3D)
import OpenSolid.Transform3D (Transform3D)
import {-# SOURCE #-} OpenSolid.UvBounds (UvBounds)
import {-# SOURCE #-} OpenSolid.UvPoint (UvPoint)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)

type role SurfaceFunction3D nominal

type SurfaceFunction3D :: Type -> Type
data SurfaceFunction3D space

type Compiled space =
  CompiledFunction UvPoint (Point3D space) UvBounds (Bounds3D space)

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
  (VectorSurfaceFunction3D Meters space, VectorSurfaceFunction3D Meters space) ->
  SurfaceFunction3D space
constant :: Point3D space -> SurfaceFunction3D space
pointAt :: UvPoint -> SurfaceFunction3D space -> Point3D space
pointOn :: SurfaceFunction3D space -> UvPoint -> Point3D space
range :: UvBounds -> SurfaceFunction3D space -> Bounds3D space
compiled :: SurfaceFunction3D space -> Compiled space
partialDerivatives ::
  SurfaceFunction3D space ->
  (VectorSurfaceFunction3D Meters space, VectorSurfaceFunction3D Meters space)
transformBy :: Transform3D tag space -> SurfaceFunction3D space -> SurfaceFunction3D space
