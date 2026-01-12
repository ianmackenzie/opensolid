module OpenSolid.Orientation3D
  ( Orientation3D
  , rightwardDirection
  , leftwardDirection
  , forwardDirection
  , backwardDirection
  , upwardDirection
  , downwardDirection
  , rightPlaneOrientation
  , leftPlaneOrientation
  , frontPlaneOrientation
  , backPlaneOrientation
  , topPlaneOrientation
  , bottomPlaneOrientation
  )
where

import OpenSolid.Primitives (Direction3D, Orientation3D, PlaneOrientation3D)

rightwardDirection :: Orientation3D space -> Direction3D space
leftwardDirection :: Orientation3D space -> Direction3D space
forwardDirection :: Orientation3D space -> Direction3D space
backwardDirection :: Orientation3D space -> Direction3D space
upwardDirection :: Orientation3D space -> Direction3D space
downwardDirection :: Orientation3D space -> Direction3D space
rightPlaneOrientation :: Orientation3D space -> PlaneOrientation3D space
leftPlaneOrientation :: Orientation3D space -> PlaneOrientation3D space
frontPlaneOrientation :: Orientation3D space -> PlaneOrientation3D space
backPlaneOrientation :: Orientation3D space -> PlaneOrientation3D space
topPlaneOrientation :: Orientation3D space -> PlaneOrientation3D space
bottomPlaneOrientation :: Orientation3D space -> PlaneOrientation3D space
