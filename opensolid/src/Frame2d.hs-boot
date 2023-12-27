module Frame2d
  ( Frame2d
  , originPoint
  , xDirection
  , yDirection
  )
where

import {-# SOURCE #-} Direction2d (Direction2d)
import OpenSolid
import {-# SOURCE #-} Point2d (Point2d)

type role Frame2d nominal nominal

type Frame2d :: CoordinateSystem -> LocalSpace -> Type
data Frame2d coordinateSystem defines where
  Frame2d ::
    { originPoint :: Point2d (space @ units)
    , xDirection :: Direction2d space
    , yDirection :: Direction2d space
    } ->
    Frame2d (space @ units) defines
