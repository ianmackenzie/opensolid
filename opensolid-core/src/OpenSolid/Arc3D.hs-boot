module OpenSolid.Arc3D (Arc3D, with) where

import OpenSolid.Angle (Angle)
import OpenSolid.Length (Length)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Prelude

type role Arc3D nominal

data Arc3D (space :: Type)

with ::
  ( "plane" ::: Plane3D space
  , "radius" ::: Length
  , "startAngle" ::: Angle
  , "endAngle" ::: Angle
  ) ->
  Arc3D space
