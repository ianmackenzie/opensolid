module OpenSolid.Vector3d
  ( coerce
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Primitives (Frame3d, Transform3d, Vector3d)

coerce :: Vector3d units1 space1 -> Vector3d units2 space2
transformBy :: Transform3d tag space -> Vector3d units space -> Vector3d units space
placeIn :: Frame3d global local -> Vector3d units local -> Vector3d units global
relativeTo :: Frame3d global local -> Vector3d units global -> Vector3d units local
