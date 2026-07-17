module OpenSolid.Bag2D (Bag2D) where

import {-# SOURCE #-} OpenSolid.Bag (Bag)
import OpenSolid.Bounds2D (Bounds2D)

type Bag2D units item = Bag (Bounds2D units) item
