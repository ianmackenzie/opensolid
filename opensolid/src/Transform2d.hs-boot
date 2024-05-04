module Transform2d (Transform2d, IsRigid) where

import OpenSolid

type role Transform2d phantom nominal

type Transform2d :: Type -> CoordinateSystem -> Type
data Transform2d a (coordinateSystem :: CoordinateSystem)

class IsRigid a
