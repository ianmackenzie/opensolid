module OpenSolid.Point2D
  ( origin
  , coerce
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Primitives (Frame2D, Point2D, Transform2D)

origin :: Point2D units space
coerce :: Point2D units1 space1 -> Point2D units2 space2
transformBy :: Transform2D tag units space -> Point2D units space -> Point2D units space
placeIn :: Frame2D units global local -> Point2D units local -> Point2D units global
relativeTo :: Frame2D units global local -> Point2D units global -> Point2D units local
