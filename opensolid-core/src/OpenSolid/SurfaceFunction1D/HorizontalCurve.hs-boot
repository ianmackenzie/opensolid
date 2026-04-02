module OpenSolid.SurfaceFunction1D.HorizontalCurve
  ( new
  , monotonic
  , bounded
  )
where

import OpenSolid.Axis2D (Axis2D)
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.UvBounds (UvBounds)

new ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2D Unitless
monotonic ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2D Unitless
bounded ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Frame2D Unitless ->
  List (Axis2D Unitless) ->
  Curve2D Unitless
