module OpenSolid.Point2d
  ( origin
  , coerce
  , transformBy
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Primitives (Frame2d, Point2d, Transform2d)

origin :: Point2d space units
coerce :: Point2d space1 units1 -> Point2d space2 units2
transformBy :: Transform2d tag space units -> Point2d space units -> Point2d space units
placeIn :: Frame2d global units local -> Point2d local units -> Point2d global units
relativeTo :: Frame2d global units local -> Point2d global units -> Point2d local units
