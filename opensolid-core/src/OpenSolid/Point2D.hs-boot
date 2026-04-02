module OpenSolid.Point2D
  ( Point2D
  , origin
  , coerce
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Primitives (Frame2D, Point2D, Transform2D)

origin :: Point2D units
coerce :: Point2D units1 -> Point2D units2
transformBy :: Transform2D tag units -> Point2D units -> Point2D units
placeIn :: Frame2D units -> Point2D units -> Point2D units
relativeTo :: Frame2D units -> Point2D units -> Point2D units
