module OpenSolid.Point3d
  ( coerce
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Frame3d, Point3d, Transform3d)

coerce :: Point3d space1 -> Point3d space2
transformBy :: Transform3d tag space -> Point3d space -> Point3d space
placeIn :: Frame3d global (Defines local) -> Point3d local -> Point3d global
relativeTo :: Frame3d global (Defines local) -> Point3d global -> Point3d local
