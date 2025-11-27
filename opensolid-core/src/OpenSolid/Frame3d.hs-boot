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

forwardAxis :: Frame3d global local -> Axis3d global
backwardAxis :: Frame3d global local -> Axis3d global
rightwardAxis :: Frame3d global local -> Axis3d global
leftwardAxis :: Frame3d global local -> Axis3d global
upwardAxis :: Frame3d global local -> Axis3d global
downwardAxis :: Frame3d global local -> Axis3d global
frontPlane :: Frame3d global local1 -> Plane3d global local2
backPlane :: Frame3d global local1 -> Plane3d global local2
topPlane :: Frame3d global local1 -> Plane3d global local2
bottomPlane :: Frame3d global local1 -> Plane3d global local2
leftPlane :: Frame3d global local1 -> Plane3d global local2
rightPlane :: Frame3d global local1 -> Plane3d global local2
