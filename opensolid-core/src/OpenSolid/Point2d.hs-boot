module OpenSolid.Point2d
  ( origin
  , coerce
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Primitives (Frame2d, Point2d, Transform2d)

origin :: Point2d units space
coerce :: Point2d units1 space1 -> Point2d units2 space2
transformBy :: Transform2d tag units space -> Point2d units space -> Point2d units space
placeIn :: Frame2d units global local -> Point2d units local -> Point2d units global
relativeTo :: Frame2d units global local -> Point2d units global -> Point2d units local
