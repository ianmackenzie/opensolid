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
  Float ->
  Float ->
  NonEmpty UvBounds ->
  Curve2d UvCoordinates
monotonic ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Float ->
  Float ->
  NonEmpty UvBounds ->
  Curve2d UvCoordinates
bounded ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Float ->
  Float ->
  NonEmpty UvBounds ->
  Frame2d UvCoordinates defines ->
  List (Axis2d UvCoordinates) ->
  Curve2d UvCoordinates
