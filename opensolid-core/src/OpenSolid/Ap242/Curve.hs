module OpenSolid.Ap242.Curve (line2D, line3D, circle2D, circle3D) where

import OpenSolid.Ap242 qualified as Ap242
import OpenSolid.Circle2D (Circle2D)
import OpenSolid.Circle2D qualified as Circle2D
import OpenSolid.Circle3D (Circle3D)
import OpenSolid.Circle3D qualified as Circle3D
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Length (Length)
import OpenSolid.Line2D (Line2D, pattern Line2D)
import OpenSolid.Line3D (Line3D, pattern Line3D)
import OpenSolid.Prelude
import OpenSolid.Step qualified as Step

line :: Step.Entity -> Step.Entity -> Step.Entity
line givenPoint givenVector =
  Step.entity "LINE" [Step.text "", Step.referenceTo givenPoint, Step.referenceTo givenVector]

line2D :: Tolerance Meters => Line2D Meters space -> Step.Entity
line2D (Line2D p1 p2) = line (Ap242.point2D p1) (Ap242.vector2D (p2 - p1))

line3D :: Tolerance Meters => Line3D space -> Step.Entity
line3D (Line3D p1 p2) = line (Ap242.point3D p1) (Ap242.vector3D (p2 - p1))

circle :: Step.Entity -> Length -> Step.Entity
circle placement radius =
  Step.entity "CIRCLE" [Step.text "", Step.referenceTo placement, Ap242.length radius]

circle2D :: Circle2D Meters space -> Step.Entity
circle2D givenCircle = do
  let frame = Frame2D.atPoint (Circle2D.centerPoint givenCircle)
  circle (Ap242.axisPlacement2D frame) (Circle2D.radius givenCircle)

circle3D :: Circle3D space -> Step.Entity
circle3D givenCircle =
  circle (Ap242.axisPlacement3D (Circle3D.plane givenCircle)) (Circle3D.radius givenCircle)
