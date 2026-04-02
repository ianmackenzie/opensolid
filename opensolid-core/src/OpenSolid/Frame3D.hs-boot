module OpenSolid.Frame3D
  ( Frame3D
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

import OpenSolid.Primitives (Axis3D, Frame3D, Plane3D)

forwardAxis :: Frame3D global local -> Axis3D global
backwardAxis :: Frame3D global local -> Axis3D global
rightwardAxis :: Frame3D global local -> Axis3D global
leftwardAxis :: Frame3D global local -> Axis3D global
upwardAxis :: Frame3D global local -> Axis3D global
downwardAxis :: Frame3D global local -> Axis3D global
frontPlane :: Frame3D global local -> Plane3D global
backPlane :: Frame3D global local -> Plane3D global
topPlane :: Frame3D global local -> Plane3D global
bottomPlane :: Frame3D global local -> Plane3D global
leftPlane :: Frame3D global local -> Plane3D global
rightPlane :: Frame3D global local -> Plane3D global
