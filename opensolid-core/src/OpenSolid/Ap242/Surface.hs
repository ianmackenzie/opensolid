module OpenSolid.Ap242.Surface (plane3D, cylinder3D) where

import OpenSolid.Ap242 qualified as Ap242
import OpenSolid.Axis3D (Axis3D)
import OpenSolid.Length (Length)
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Prelude
import OpenSolid.Step qualified as Step

plane3D :: Plane3D space -> Step.Entity
plane3D plane = Step.entity "PLANE" [Step.text "", Step.referenceTo (Ap242.axisPlacement3D plane)]

cylinder3D :: Axis3D space -> ("radius" ::: Length) -> Step.Entity
cylinder3D axis ("radius" ::: radius) =
  Step.entity "CYLINDRICAL_SURFACE" $
    [ Step.text ""
    , Step.referenceTo (Ap242.axisPlacement3D (Plane3D.fromNormalAxis axis))
    , Ap242.length radius
    ]
