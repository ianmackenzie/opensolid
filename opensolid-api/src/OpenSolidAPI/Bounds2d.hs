module OpenSolidAPI.Bounds2d (bounds2d) where

import Bounds2d qualified
import Internal (Class, cls, method, static)
import OpenSolid

bounds2d :: Class
bounds2d =
  cls ''Bounds2d.Bounds2d ['Bounds2d.xCoordinate, 'Bounds2d.yCoordinate] [] $
    [ method 'Bounds2d.xCoordinate ["bounds"]
    , method 'Bounds2d.yCoordinate ["bounds"]
    , static 'Bounds2d.constant ["point"]
    , static 'Bounds2d.hull2 ["p1", "p2"]
    , static 'Bounds2d.hull3 ["p1", "p2", "p3"]
    , static 'Bounds2d.hull4 ["p1", "p2", "p3", "p4"]
    , static 'Bounds2d.aggregate2 ["bounds1", "bounds2"]
    , method 'Bounds2d.intersection ["bounds1", "bounds2"]
    , static 'Bounds2d.interpolate ["bounds", "u", "v"]
    ]
