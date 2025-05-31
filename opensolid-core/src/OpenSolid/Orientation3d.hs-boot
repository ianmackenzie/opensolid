module OpenSolid.Orientation3d
  ( Orientation3d
  , world
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

import OpenSolid.Prelude
import OpenSolid.Primitives (Orientation3d, Direction3d, PlaneOrientation3d)

world :: Orientation3d space (Defines space)
rightwardDirection :: Orientation3d space defines -> Direction3d space
leftwardDirection :: Orientation3d space defines -> Direction3d space
forwardDirection :: Orientation3d space defines -> Direction3d space
backwardDirection :: Orientation3d space defines -> Direction3d space
upwardDirection :: Orientation3d space defines -> Direction3d space
downwardDirection :: Orientation3d space defines -> Direction3d space
rightPlaneOrientation :: Orientation3d space defines1 -> PlaneOrientation3d space defines2
leftPlaneOrientation :: Orientation3d space defines1 -> PlaneOrientation3d space defines2
frontPlaneOrientation :: Orientation3d space defines1 -> PlaneOrientation3d space defines2
backPlaneOrientation :: Orientation3d space defines1 -> PlaneOrientation3d space defines2
topPlaneOrientation :: Orientation3d space defines1 -> PlaneOrientation3d space defines2
bottomPlaneOrientation :: Orientation3d space defines1 -> PlaneOrientation3d space defines2
