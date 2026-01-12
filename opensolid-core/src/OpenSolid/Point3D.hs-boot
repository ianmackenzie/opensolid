module OpenSolid.Point3D
  ( coerce
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Primitives (Frame3D, Point3D, Transform3D)

coerce :: Point3D space1 -> Point3D space2
transformBy :: Transform3D tag space -> Point3D space -> Point3D space
placeIn :: Frame3D global local -> Point3D local -> Point3D global
relativeTo :: Frame3D global local -> Point3D global -> Point3D local
