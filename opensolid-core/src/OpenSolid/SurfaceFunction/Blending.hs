module OpenSolid.SurfaceFunction.Blending
  ( Blendable
  , desingularize
  )
where

import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import {-# SOURCE #-} OpenSolid.SurfaceFunction2d (SurfaceFunction2d)
import {-# SOURCE #-} OpenSolid.SurfaceFunction2d qualified as SurfaceFunction2d

type Blendable function =
  ( Multiplication (SurfaceFunction Unitless) function function
  , Multiplication (Qty Unitless) function function
  , Addition function function function
  , Composition (SurfaceFunction2d UvCoordinates) function function
  , HasField "du" function function
  , HasField "dv" function function
  )

desingularize ::
  Blendable function =>
  (SurfaceFunction Unitless -> function -> function -> function -> function) ->
  ( "function" ::: function
  , "singularityU0" ::: Maybe (function, function)
  , "singularityU1" ::: Maybe (function, function)
  , "singularityV0" ::: Maybe (function, function)
  , "singularityV1" ::: Maybe (function, function)
  ) ->
  function
desingularize desingularized args = do
  let f = args.function
  case (args.singularityU0, args.singularityU1, args.singularityV0, args.singularityV1) of
    (Nothing, Nothing, Nothing, Nothing) -> args.function
    (Just singularityU0, Nothing, Nothing, Nothing) ->
      desingularized SurfaceFunction.u (blendU0 singularityU0 f) f f
    (Nothing, Just singularityU1, Nothing, Nothing) ->
      desingularized SurfaceFunction.u f f (blendU1 f singularityU1)
    (Just singularityU0, Just singularityU1, Nothing, Nothing) ->
      desingularized SurfaceFunction.u (blendU0 singularityU0 f) f (blendU1 f singularityU1)
    (Nothing, Nothing, Just singularityV0, Nothing) ->
      desingularized SurfaceFunction.v (blendV0 singularityV0 f) f f
    (Nothing, Nothing, Nothing, Just singularityV1) ->
      desingularized SurfaceFunction.v f f (blendV1 f singularityV1)
    (Nothing, Nothing, Just singularityV0, Just singularityV1) ->
      desingularized SurfaceFunction.v (blendV0 singularityV0 f) f (blendV1 f singularityV1)
    _ -> exception "Unsupported combination of singularities for surface function: singularities may only be in U or V direction, not both"

blendU0 ::
  Blendable function =>
  (function, function) ->
  function ->
  function
blendU0 (f0, dfdu0) f = do
  let t0 = Desingularization.t0
  let u0 = vParameterizationU0
  let uT0 = vParameterizationUT0
  blend
    (f . uT0, -t0 * f.du . uT0, t0 * t0 * f.du.du . uT0)
    (f0 . u0, -t0 * dfdu0 . u0)
    ((t0 - SurfaceFunction.u) / t0)

blendU1 ::
  Blendable function =>
  function ->
  (function, function) ->
  function
blendU1 f (f1, dfdu1) = do
  let t0 = Desingularization.t0
  let t1 = Desingularization.t1
  let uT1 = vParameterizationUT1
  let u1 = vParameterizationU1
  blend
    (f . uT1, t0 * f.du . uT1, t0 * t0 * f.du.du . uT1)
    (f1 . u1, t0 * dfdu1 . u1)
    ((SurfaceFunction.u - t1) / t0)

blendV0 ::
  Blendable function =>
  (function, function) ->
  function ->
  function
blendV0 (f0, dfdv0) f = do
  let t0 = Desingularization.t0
  let v0 = uParameterizationV0
  let vT0 = uParameterizationVT0
  blend
    (f . vT0, -t0 * f.dv . vT0, t0 * t0 * f.dv.dv . vT0)
    (f0 . v0, -t0 * dfdv0 . v0)
    ((t0 - SurfaceFunction.v) / t0)

blendV1 ::
  Blendable function =>
  function ->
  (function, function) ->
  function
blendV1 f (f1, dfdv1) = do
  let t0 = Desingularization.t0
  let t1 = Desingularization.t1
  let uT1 = uParameterizationVT1
  let u1 = uParameterizationV1
  blend
    (f . uT1, t0 * f.dv . uT1, t0 * t0 * f.dv.dv . uT1)
    (f1 . u1, t0 * dfdv1 . u1)
    ((SurfaceFunction.v - t1) / t0)

blend ::
  Blendable function =>
  (function, function, function) ->
  (function, function) ->
  SurfaceFunction Unitless ->
  function
blend (f00, f01, f02) (f10, f11) t =
  b00 t * f00 + b01 t * f01 + b02 t * f02 + b10 t * f10 + b11 t * f11

b00 :: SurfaceFunction Unitless -> SurfaceFunction Unitless
b00 t = 1.0 - SurfaceFunction.cubed t * (4.0 - 3.0 * t)

b01 :: SurfaceFunction Unitless -> SurfaceFunction Unitless
b01 t = t * (1.0 + SurfaceFunction.squared t * (2.0 * t - 3.0))

b02 :: SurfaceFunction Unitless -> SurfaceFunction Unitless
b02 t = SurfaceFunction.squared t * (0.5 + t * (0.5 * t - 1.0))

b10 :: SurfaceFunction Unitless -> SurfaceFunction Unitless
b10 t = SurfaceFunction.cubed t * (4.0 - 3.0 * t)

b11 :: SurfaceFunction Unitless -> SurfaceFunction Unitless
b11 t = SurfaceFunction.cubed t * (t - 1.0)

uParameterization :: Float -> SurfaceFunction2d UvCoordinates
uParameterization vValue = SurfaceFunction2d.xy SurfaceFunction.u (SurfaceFunction.constant vValue)

uParameterizationV0 :: SurfaceFunction2d UvCoordinates
uParameterizationV0 = uParameterization 0.0

uParameterizationVT0 :: SurfaceFunction2d UvCoordinates
uParameterizationVT0 = uParameterization Desingularization.t0

uParameterizationVT1 :: SurfaceFunction2d UvCoordinates
uParameterizationVT1 = uParameterization Desingularization.t1

uParameterizationV1 :: SurfaceFunction2d UvCoordinates
uParameterizationV1 = uParameterization 1.0

vParameterization :: Float -> SurfaceFunction2d UvCoordinates
vParameterization uValue = SurfaceFunction2d.xy (SurfaceFunction.constant uValue) SurfaceFunction.v

vParameterizationU0 :: SurfaceFunction2d UvCoordinates
vParameterizationU0 = vParameterization 0.0

vParameterizationUT0 :: SurfaceFunction2d UvCoordinates
vParameterizationUT0 = vParameterization Desingularization.t0

vParameterizationUT1 :: SurfaceFunction2d UvCoordinates
vParameterizationUT1 = vParameterization Desingularization.t1

vParameterizationU1 :: SurfaceFunction2d UvCoordinates
vParameterizationU1 = vParameterization 1.0
