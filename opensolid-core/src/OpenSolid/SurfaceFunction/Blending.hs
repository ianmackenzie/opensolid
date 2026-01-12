module OpenSolid.SurfaceFunction.Blending
  ( Blendable
  , desingularize
  )
where

import GHC.Records (HasField)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D qualified as SurfaceFunction2D
import Prelude qualified

type Blendable function =
  ( Multiplication (SurfaceFunction Unitless) function function
  , Multiplication (Quantity Unitless) function function
  , Addition function function function
  , Composition (SurfaceFunction2D Unitless UvSpace) function function
  , HasField "du" function function
  , HasField "dv" function function
  )

data UnsupportedCombinationOfSingularities
  = UnsupportedCombinationOfSingularities
  deriving (Exception)

instance Show UnsupportedCombinationOfSingularities where
  show UnsupportedCombinationOfSingularities =
    "Unsupported combination of singularities for surface function: singularities may only be in U or V direction, not both"

desingularize ::
  Blendable function =>
  (SurfaceFunction Unitless -> function -> function -> function -> function) ->
  function ->
  "singularityU0" ::: Maybe (function, function) ->
  "singularityU1" ::: Maybe (function, function) ->
  "singularityV0" ::: Maybe (function, function) ->
  "singularityV1" ::: Maybe (function, function) ->
  function
desingularize
  desingularized
  function
  (Named maybeSingularityU0)
  (Named maybeSingularityU1)
  (Named maybeSingularityV0)
  (Named maybeSingularityV1) =
    case (maybeSingularityU0, maybeSingularityU1, maybeSingularityV0, maybeSingularityV1) of
      (Nothing, Nothing, Nothing, Nothing) -> function
      (Just singularityU0, Nothing, Nothing, Nothing) ->
        desingularized
          SurfaceFunction.u
          (blendU0 singularityU0 function)
          function
          function
      (Nothing, Just singularityU1, Nothing, Nothing) ->
        desingularized
          SurfaceFunction.u
          function
          function
          (blendU1 function singularityU1)
      (Just singularityU0, Just singularityU1, Nothing, Nothing) ->
        desingularized
          SurfaceFunction.u
          (blendU0 singularityU0 function)
          function
          (blendU1 function singularityU1)
      (Nothing, Nothing, Just singularityV0, Nothing) ->
        desingularized
          SurfaceFunction.v
          (blendV0 singularityV0 function)
          function
          function
      (Nothing, Nothing, Nothing, Just singularityV1) ->
        desingularized
          SurfaceFunction.v
          function
          function
          (blendV1 function singularityV1)
      (Nothing, Nothing, Just singularityV0, Just singularityV1) ->
        desingularized
          SurfaceFunction.v
          (blendV0 singularityV0 function)
          function
          (blendV1 function singularityV1)
      _ -> throw UnsupportedCombinationOfSingularities

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
    (f `compose` uT0, negative t0 .*. f.du `compose` uT0, t0 .*. t0 .*. f.du.du `compose` uT0)
    (f0 `compose` u0, negative t0 .*. dfdu0 `compose` u0)
    ((t0 .-. SurfaceFunction.u) ./. t0)

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
    (f `compose` uT1, t0 .*. f.du `compose` uT1, t0 .*. t0 .*. f.du.du `compose` uT1)
    (f1 `compose` u1, t0 .*. dfdu1 `compose` u1)
    ((SurfaceFunction.u .-. t1) ./. t0)

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
    (f `compose` vT0, negative t0 .*. f.dv `compose` vT0, t0 .*. t0 .*. f.dv.dv `compose` vT0)
    (f0 `compose` v0, negative t0 .*. dfdv0 `compose` v0)
    ((t0 .-. SurfaceFunction.v) ./. t0)

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
    (f `compose` uT1, t0 .*. f.dv `compose` uT1, t0 .*. t0 .*. f.dv.dv `compose` uT1)
    (f1 `compose` u1, t0 .*. dfdv1 `compose` u1)
    ((SurfaceFunction.v .-. t1) ./. t0)

blend ::
  Blendable function =>
  (function, function, function) ->
  (function, function) ->
  SurfaceFunction Unitless ->
  function
blend (f00, f01, f02) (f10, f11) t = do
  let b00 = Curve.b00 `compose` t
  let b01 = Curve.b01 `compose` t
  let b02 = Curve.b02 `compose` t
  let b10 = Curve.b10 `compose` t
  let b11 = Curve.b11 `compose` t
  b00 .*. f00 .+. b01 .*. f01 .+. b02 .*. f02 .+. b10 .*. f10 .+. b11 .*. f11

uParameterization :: Number -> SurfaceFunction2D Unitless UvSpace
uParameterization vValue = SurfaceFunction2D.xy SurfaceFunction.u (SurfaceFunction.constant vValue)

uParameterizationV0 :: SurfaceFunction2D Unitless UvSpace
uParameterizationV0 = uParameterization 0

uParameterizationVT0 :: SurfaceFunction2D Unitless UvSpace
uParameterizationVT0 = uParameterization Desingularization.t0

uParameterizationVT1 :: SurfaceFunction2D Unitless UvSpace
uParameterizationVT1 = uParameterization Desingularization.t1

uParameterizationV1 :: SurfaceFunction2D Unitless UvSpace
uParameterizationV1 = uParameterization 1

vParameterization :: Number -> SurfaceFunction2D Unitless UvSpace
vParameterization uValue = SurfaceFunction2D.xy (SurfaceFunction.constant uValue) SurfaceFunction.v

vParameterizationU0 :: SurfaceFunction2D Unitless UvSpace
vParameterizationU0 = vParameterization 0

vParameterizationUT0 :: SurfaceFunction2D Unitless UvSpace
vParameterizationUT0 = vParameterization Desingularization.t0

vParameterizationUT1 :: SurfaceFunction2D Unitless UvSpace
vParameterizationUT1 = vParameterization Desingularization.t1

vParameterizationU1 :: SurfaceFunction2D Unitless UvSpace
vParameterizationU1 = vParameterization 1
