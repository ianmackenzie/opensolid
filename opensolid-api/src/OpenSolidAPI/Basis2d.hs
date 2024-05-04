module OpenSolidAPI.Basis2d (basis2d) where

import Basis2d qualified
import Internal (Class, cls, method, static)
import OpenSolid

basis2d :: Class
basis2d =
  cls ''Basis2d.Basis2d ['Basis2d.xDirection, 'Basis2d.yDirection] [] $
    [ static 'Basis2d.xy []
    , method 'Basis2d.xDirection ["basis"]
    , method 'Basis2d.yDirection ["basis"]
    , static 'Basis2d.fromXDirection ["direction"]
    , static 'Basis2d.fromYDirection ["direction"]
    ]
