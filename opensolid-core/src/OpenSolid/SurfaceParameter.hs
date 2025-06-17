module OpenSolid.SurfaceParameter (SurfaceParameter (U, V)) where

import OpenSolid.Prelude

data SurfaceParameter = U | V deriving (Eq, Show, Ord)
