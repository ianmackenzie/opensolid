module OpenSolid.Surface1d.Function.VerticalCurve
  ( new
  , monotonic
  , bounded
  )
where

import OpenSolid.Axis2d (Axis2d)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import {-# SOURCE #-} OpenSolid.Surface1d.Function (Function)
import OpenSolid.SurfaceParameter (UvCoordinates)
import OpenSolid.Uv.Derivatives (Derivatives)

new ::
  Tolerance units =>
  Derivatives (Function units) ->
  Function Unitless ->
  Range Unitless ->
  Float ->
  Float ->
  Curve2d UvCoordinates
monotonic ::
  Tolerance units =>
  Derivatives (Function units) ->
  Function Unitless ->
  Range Unitless ->
  Float ->
  Float ->
  Curve2d UvCoordinates
bounded ::
  Tolerance units =>
  Derivatives (Function units) ->
  Function Unitless ->
  Range Unitless ->
  Float ->
  Float ->
  Frame2d UvCoordinates defines ->
  List (Axis2d UvCoordinates) ->
  Curve2d UvCoordinates
