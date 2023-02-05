module Point2d (Point2d) where

import OpenSolid

type role Point2d nominal nominal

type Point2d :: Type -> Type -> Type
data Point2d units coordinates
