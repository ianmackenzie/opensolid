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
import OpenSolid.UvSpace (UvSpace)

new ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2D Unitless UvSpace
monotonic ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Curve2D Unitless UvSpace
bounded ::
  Tolerance units =>
  SurfaceFunction1D units ->
  SurfaceFunction1D Unitless ->
  Number ->
  Number ->
  NonEmpty UvBounds ->
  Frame2D Unitless UvSpace local ->
  List (Axis2D Unitless UvSpace) ->
  Curve2D Unitless UvSpace
