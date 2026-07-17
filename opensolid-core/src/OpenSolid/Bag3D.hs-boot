module OpenSolid.Bag3D (Bag3D) where

import {-# SOURCE #-} OpenSolid.Bag (Bag)
import OpenSolid.Bounds3D (Bounds3D)

type Bag3D space item = Bag (Bounds3D space) item
