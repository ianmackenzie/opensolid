module Transform2d (Transform2d) where

import OpenSolid.Prelude

type role Transform2d phantom nominal

type Transform2d :: Type -> CoordinateSystem -> Type
data Transform2d tag (coordinateSystem :: CoordinateSystem)
