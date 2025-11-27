module OpenSolid.SurfaceFunction.HorizontalCurve
  ( new
  , monotonic
  , bounded
  )
where

import OpenSolid.Axis2d (Axis2d)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.UvBounds (UvBounds)

new ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2d Unitless UvSpace
monotonic ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2d Unitless UvSpace
bounded ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Frame2d Unitless UvSpace local ->
  List (Axis2d Unitless UvSpace) ->
  Curve2d Unitless UvSpace
