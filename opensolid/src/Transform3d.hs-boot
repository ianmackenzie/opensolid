module Transform3d (Transform3d) where

import OpenSolid

type role Transform3d phantom nominal

type Transform3d :: Type -> CoordinateSystem -> Type
data Transform3d tag (coordinateSystem :: CoordinateSystem)
