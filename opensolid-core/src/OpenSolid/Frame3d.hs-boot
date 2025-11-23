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

forwardAxis :: Frame3d space units defines -> Axis3d space units
backwardAxis :: Frame3d space units defines -> Axis3d space units
rightwardAxis :: Frame3d space units defines -> Axis3d space units
leftwardAxis :: Frame3d space units defines -> Axis3d space units
upwardAxis :: Frame3d space units defines -> Axis3d space units
downwardAxis :: Frame3d space units defines -> Axis3d space units
frontPlane :: Frame3d space units defines1 -> Plane3d space units defines2
backPlane :: Frame3d space units defines1 -> Plane3d space units defines2
topPlane :: Frame3d space units defines1 -> Plane3d space units defines2
bottomPlane :: Frame3d space units defines1 -> Plane3d space units defines2
leftPlane :: Frame3d space units defines1 -> Plane3d space units defines2
rightPlane :: Frame3d space units defines1 -> Plane3d space units defines2
