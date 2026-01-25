module OpenSolid.VectorSurface2D
  ( VectorSurface2D
  , parametric
  , function
  )
where

import OpenSolid.UvRegion (UvRegion)
import OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)

data VectorSurface2D units space where
  Parametric :: VectorSurfaceFunction2D units space -> UvRegion -> VectorSurface2D units space

parametric :: VectorSurfaceFunction2D units space -> UvRegion -> VectorSurface2D units space
parametric = Parametric

function :: VectorSurface2D units space -> VectorSurfaceFunction2D units space
function (Parametric f _) = f
