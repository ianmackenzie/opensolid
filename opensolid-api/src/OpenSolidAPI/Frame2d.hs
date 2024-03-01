module OpenSolidAPI.Frame2d (frame2d) where

import Frame2d qualified
import Internal (Class, cls, method, static)
import OpenSolid

frame2d :: Class
frame2d =
  cls ''Frame2d.Frame2d ['Frame2d.originPoint, 'Frame2d.xDirection, 'Frame2d.yDirection] [] <|
    [ static 'Frame2d.xy []
    , static 'Frame2d.withOriginPoint ["point"]
    , method 'Frame2d.originPoint ["frame"]
    , method 'Frame2d.xDirection ["frame"]
    , method 'Frame2d.yDirection ["frame"]
    , method 'Frame2d.xAxis ["point"]
    , method 'Frame2d.yAxis ["point"]
    , static 'Frame2d.withXDirection ["direction", "point"]
    , static 'Frame2d.withYDirection ["direction", "point"]
    , static 'Frame2d.fromXAxis ["axis"]
    , static 'Frame2d.fromYAxis ["axis"]
    ]
