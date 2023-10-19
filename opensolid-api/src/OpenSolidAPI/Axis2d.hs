module OpenSolidAPI.Axis2d (axis2d) where

import Axis2d qualified
import Internal (Class, cls, method, static)
import OpenSolid

axis2d :: Class
axis2d =
  cls ''Axis2d.Axis2d ['Axis2d.originPoint, 'Axis2d.direction] $
    [ method 'Axis2d.originPoint ["axis"]
    , method 'Axis2d.direction ["axis"]
    , static 'Axis2d.x []
    , static 'Axis2d.y []
    , static 'Axis2d.through ["point", "direction"]
    ]
