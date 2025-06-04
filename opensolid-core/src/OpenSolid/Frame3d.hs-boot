module OpenSolid.Frame3d
  ( Frame3d
  , world
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

import OpenSolid.Prelude
import OpenSolid.Primitives (Axis3d, Frame3d, Plane3d)

world :: Frame3d (space @ units) (Defines space)
forwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
backwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
rightwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
leftwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
upwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
downwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
frontPlane ::
  Frame3d (space @ units) (Defines local) ->
  Plane3d (space @ units) (Defines (FrontPlane local))
backPlane ::
  Frame3d (space @ units) (Defines local) ->
  Plane3d (space @ units) (Defines (BackPlane local))
topPlane ::
  Frame3d (space @ units) (Defines local) ->
  Plane3d (space @ units) (Defines (TopPlane local))
bottomPlane ::
  Frame3d (space @ units) (Defines local) ->
  Plane3d (space @ units) (Defines (BottomPlane local))
leftPlane ::
  Frame3d (space @ units) (Defines local) ->
  Plane3d (space @ units) (Defines (LeftPlane local))
rightPlane ::
  Frame3d (space @ units) (Defines local) ->
  Plane3d (space @ units) (Defines (RightPlane local))
