module OpenSolid.Point2D
  ( origin
  , coerce
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Primitives (Frame2d, Point2D, Transform2d)

origin :: Point2D units space
coerce :: Point2D units1 space1 -> Point2D units2 space2
transformBy :: Transform2d tag units space -> Point2D units space -> Point2D units space
placeIn :: Frame2d units global local -> Point2D units local -> Point2D units global
relativeTo :: Frame2d units global local -> Point2D units global -> Point2D units local
