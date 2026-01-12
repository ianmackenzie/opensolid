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
frontPlane :: Frame3D global local1 -> Plane3D global local2
backPlane :: Frame3D global local1 -> Plane3D global local2
topPlane :: Frame3D global local1 -> Plane3D global local2
bottomPlane :: Frame3D global local1 -> Plane3D global local2
leftPlane :: Frame3D global local1 -> Plane3D global local2
rightPlane :: Frame3D global local1 -> Plane3D global local2
