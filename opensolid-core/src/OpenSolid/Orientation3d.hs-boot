module OpenSolid.Orientation3d
  ( Orientation3d
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

import OpenSolid.Primitives (Direction3d, Orientation3d, PlaneOrientation3d)

rightwardDirection :: Orientation3d space -> Direction3d space
leftwardDirection :: Orientation3d space -> Direction3d space
forwardDirection :: Orientation3d space -> Direction3d space
backwardDirection :: Orientation3d space -> Direction3d space
upwardDirection :: Orientation3d space -> Direction3d space
downwardDirection :: Orientation3d space -> Direction3d space
rightPlaneOrientation :: Orientation3d space -> PlaneOrientation3d space
leftPlaneOrientation :: Orientation3d space -> PlaneOrientation3d space
frontPlaneOrientation :: Orientation3d space -> PlaneOrientation3d space
backPlaneOrientation :: Orientation3d space -> PlaneOrientation3d space
topPlaneOrientation :: Orientation3d space -> PlaneOrientation3d space
bottomPlaneOrientation :: Orientation3d space -> PlaneOrientation3d space
