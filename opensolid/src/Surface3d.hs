module Surface3d
  ( Surface3d
  , Function
  , parametric
  , function
  )
where

import Region2d (Region2d)
import Surface3d.Function (Function)
import Uv qualified

data Surface3d units where
  Parametric :: Function units -> Region2d Uv.Coordinates -> Surface3d units

parametric :: Function units -> Region2d Uv.Coordinates -> Surface3d units
parametric = Parametric

function :: Surface3d units -> Function units
function (Parametric f _) = f
