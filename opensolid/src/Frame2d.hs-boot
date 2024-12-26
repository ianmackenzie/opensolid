module Frame2d
  ( Frame2d
  , originPoint
  , basis
  , xDirection
  , yDirection
  )
where

import {-# SOURCE #-} Basis2d (Basis2d)
import {-# SOURCE #-} Direction2d (Direction2d)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Point2d (Point2d)

type role Frame2d nominal nominal

type Frame2d :: CoordinateSystem -> LocalSpace -> Type
data Frame2d coordinateSystem defines where
  Frame2d ::
    { originPoint :: Point2d (space @ units)
    , basis :: Basis2d space defines
    } ->
    Frame2d (space @ units) defines

xDirection :: Frame2d (space @ units) defines -> Direction2d space
yDirection :: Frame2d (space @ units) defines -> Direction2d space
