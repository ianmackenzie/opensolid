module OpenSolid.Vector3D
  ( Vector3D
  , zero
  , coerce
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Primitives (Frame3D, Transform3D, Vector3D)

zero :: Vector3D units space
coerce :: Vector3D units1 space1 -> Vector3D units2 space2
transformBy :: Transform3D tag space -> Vector3D units space -> Vector3D units space
placeIn :: Frame3D global local -> Vector3D units local -> Vector3D units global
relativeTo :: Frame3D global local -> Vector3D units global -> Vector3D units local
