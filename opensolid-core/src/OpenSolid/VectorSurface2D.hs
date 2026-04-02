module OpenSolid.VectorSurface2D
  ( VectorSurface2D
  , parametric
  , function
  )
where

import OpenSolid.UvRegion (UvRegion)
import OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)

data VectorSurface2D units where
  Parametric :: VectorSurfaceFunction2D units -> UvRegion -> VectorSurface2D units

parametric :: VectorSurfaceFunction2D units -> UvRegion -> VectorSurface2D units
parametric = Parametric

function :: VectorSurface2D units -> VectorSurfaceFunction2D units
function (Parametric f _) = f
