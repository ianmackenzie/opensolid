module OpenSolid.Ap242.Surface (plane3D, cylinder3D) where

import OpenSolid.Ap242 qualified as Ap242
import OpenSolid.Axis3D (Axis3D)
import OpenSolid.Axis3D qualified as Axis3D
import OpenSolid.Length (Length)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Prelude
import OpenSolid.Step qualified as Step

plane3D :: Plane3D space -> Step.Entity
plane3D plane = Step.entity "PLANE" [Step.text "", Step.referenceTo (Ap242.axisPlacement3D plane)]

cylinder3D :: "axis" ::: Axis3D space -> "radius" ::: Length -> Step.Entity
cylinder3D ("axis" ::: axis) ("radius" ::: radius) =
  Step.entity "CYLINDRICAL_SURFACE" $
    [ Step.text ""
    , Step.referenceTo (Ap242.axisPlacement3D (Axis3D.normalPlane axis))
    , Ap242.length radius
    ]
