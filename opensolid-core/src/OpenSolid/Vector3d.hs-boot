module OpenSolid.Vector3d
  ( coerce
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Prelude
import OpenSolid.Primitives (Frame3d, Transform3d, Vector3d)

coerce :: Vector3d space1 units1 -> Vector3d space2 units2
transformBy :: Transform3d tag space -> Vector3d space units -> Vector3d space units
placeIn :: Frame3d global (Defines local) -> Vector3d local units -> Vector3d global units
relativeTo :: Frame3d global (Defines local) -> Vector3d global units -> Vector3d local units
