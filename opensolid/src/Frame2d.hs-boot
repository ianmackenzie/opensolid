module Frame2d
  ( Frame2d
  , originPoint
  , xDirection
  , yDirection
  )
where

import CoordinateSystem (Space)
import {-# SOURCE #-} Direction2d (Direction2d)
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)

type role Frame2d phantom phantom

type Frame2d :: CoordinateSystem -> LocalSpace -> Type
data Frame2d coordinateSystem defines
  = Frame2d
      (Point2d coordinateSystem)
      (Direction2d (Space coordinateSystem))
      (Direction2d (Space coordinateSystem))

originPoint :: Frame2d (space @ units) defines -> Point2d (space @ units)
xDirection :: Frame2d (space @ units) defines -> Direction2d space
yDirection :: Frame2d (space @ units) defines -> Direction2d space
