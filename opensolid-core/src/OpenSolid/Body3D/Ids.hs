module OpenSolid.Body3D.Ids (SurfaceId (SurfaceId), BoundaryId (BoundaryId), CurveId (CurveId)) where

import Data.Hashable (Hashable)
import OpenSolid.Prelude
import OpenSolid.Set3D (Set3D)
import OpenSolid.Surface3D (Surface3D)
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.SurfaceCurve3D (SurfaceCurve3D)

-- | ID of a boundary surface within a body
newtype SurfaceId = SurfaceId Int
  deriving newtype (Eq, Ord, Show)
  deriving newtype (Hashable)

-- | ID of a surface boundary (curve loop) within a boundary surface
newtype BoundaryId = BoundaryId Int
  deriving (Eq, Ord, Show)
  deriving newtype (Hashable)

-- | ID of a boundary curve within a surface boundary
newtype CurveId = CurveId Int
  deriving (Eq, Ord, Show)
  deriving newtype (Hashable)

instance Indexed (Set3D space (Surface3D space)) SurfaceId (Surface3D space) where
  surfaces !! SurfaceId index = surfaces !! index

instance Indexed (Surface3D space) BoundaryId (Surface3D.Boundary space) where
  surface !! BoundaryId index = Surface3D.boundaries surface !! index

instance Indexed (Surface3D.Boundary space) CurveId (SurfaceCurve3D space) where
  boundary !! CurveId index = boundary !! index
