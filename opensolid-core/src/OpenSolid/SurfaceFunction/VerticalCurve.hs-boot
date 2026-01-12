module OpenSolid.SurfaceFunction.VerticalCurve
  ( new
  , monotonic
  , bounded
  )
where

import OpenSolid.Axis2D (Axis2D)
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import OpenSolid.Frame2D (Frame2D)
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
  Curve2D Unitless UvSpace
monotonic ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2D Unitless UvSpace
bounded ::
  Tolerance units =>
  SurfaceFunction units ->
  SurfaceFunction Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Frame2D Unitless UvSpace local ->
  List (Axis2D Unitless UvSpace) ->
  Curve2D Unitless UvSpace
