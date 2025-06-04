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
import OpenSolid.Primitives (Direction3d, Orientation3d, PlaneOrientation3d)

world :: Orientation3d space (Defines space)
rightwardDirection :: Orientation3d space defines -> Direction3d space
leftwardDirection :: Orientation3d space defines -> Direction3d space
forwardDirection :: Orientation3d space defines -> Direction3d space
backwardDirection :: Orientation3d space defines -> Direction3d space
upwardDirection :: Orientation3d space defines -> Direction3d space
downwardDirection :: Orientation3d space defines -> Direction3d space
rightPlaneOrientation ::
  Orientation3d space (Defines local) ->
  PlaneOrientation3d space (Defines (RightPlane local))
leftPlaneOrientation ::
  Orientation3d space (Defines local) ->
  PlaneOrientation3d space (Defines (LeftPlane local))
frontPlaneOrientation ::
  Orientation3d space (Defines local) ->
  PlaneOrientation3d space (Defines (FrontPlane local))
backPlaneOrientation ::
  Orientation3d space (Defines local) ->
  PlaneOrientation3d space (Defines (BackPlane local))
topPlaneOrientation ::
  Orientation3d space (Defines local) ->
  PlaneOrientation3d space (Defines (TopPlane local))
bottomPlaneOrientation ::
  Orientation3d space (Defines local) ->
  PlaneOrientation3d space (Defines (BottomPlane local))
