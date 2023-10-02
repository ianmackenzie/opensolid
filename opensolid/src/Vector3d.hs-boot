module Vector3d
  ( Vector3d
  , xyz
  )
where

import OpenSolid

type role Vector3d nominal

data Vector3d (coordinateSystem :: CoordinateSystem)

xyz :: Qty units -> Qty units -> Qty units -> Vector3d (space @ units)
