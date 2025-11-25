module OpenSolid.Frame3d
  ( Frame3d
  , forwardAxis
  , backwardAxis
  , rightwardAxis
  , leftwardAxis
  , upwardAxis
  , downwardAxis
  , frontPlane
  , backPlane
  , leftPlane
  , rightPlane
  , topPlane
  , bottomPlane
  )
where

import OpenSolid.Primitives (Axis3d, Frame3d, Plane3d)

forwardAxis :: Frame3d space defines -> Axis3d space
backwardAxis :: Frame3d space defines -> Axis3d space
rightwardAxis :: Frame3d space defines -> Axis3d space
leftwardAxis :: Frame3d space defines -> Axis3d space
upwardAxis :: Frame3d space defines -> Axis3d space
downwardAxis :: Frame3d space defines -> Axis3d space
frontPlane :: Frame3d space defines1 -> Plane3d space defines2
backPlane :: Frame3d space defines1 -> Plane3d space defines2
topPlane :: Frame3d space defines1 -> Plane3d space defines2
bottomPlane :: Frame3d space defines1 -> Plane3d space defines2
leftPlane :: Frame3d space defines1 -> Plane3d space defines2
rightPlane :: Frame3d space defines1 -> Plane3d space defines2
