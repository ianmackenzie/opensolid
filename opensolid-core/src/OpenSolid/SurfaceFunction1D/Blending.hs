module OpenSolid.SurfaceFunction1D.Blending (desingularize) where

import OpenSolid.CoordinateSystem (VectorSurfaceFunction)
import OpenSolid.CoordinateSystem qualified as CoordinateSystem
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Desingularization qualified as Desingularization
import OpenSolid.Differentiable (Differentiable (derivative))
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction2D qualified as SurfaceFunction2D
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import Prelude qualified

data UnsupportedCombinationOfSingularities
  = UnsupportedCombinationOfSingularities
  deriving (Exception)

instance Show UnsupportedCombinationOfSingularities where
  show UnsupportedCombinationOfSingularities =
    "Unsupported combination of singularities for surface function: singularities may only be in U or V direction, not both"

type Singularity dimension units space =
  ( VectorSurfaceFunction dimension units space
  , VectorSurfaceFunction dimension units space
  )

desingularize ::
  CoordinateSystem.Generic dimension units space =>
  ( SurfaceFunction1D Unitless ->
    VectorSurfaceFunction dimension units space ->
    VectorSurfaceFunction dimension units space ->
    VectorSurfaceFunction dimension units space ->
    VectorSurfaceFunction dimension units space
  ) ->
  VectorSurfaceFunction dimension units space ->
  "singularityU0" ::: Maybe (Singularity dimension units space) ->
  "singularityU1" ::: Maybe (Singularity dimension units space) ->
  "singularityV0" ::: Maybe (Singularity dimension units space) ->
  "singularityV1" ::: Maybe (Singularity dimension units space) ->
  VectorSurfaceFunction dimension units space
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
          SurfaceFunction1D.u
          (blendU0 singularityU0 function)
          function
          function
      (Nothing, Just singularityU1, Nothing, Nothing) ->
        desingularized
          SurfaceFunction1D.u
          function
          function
          (blendU1 function singularityU1)
      (Just singularityU0, Just singularityU1, Nothing, Nothing) ->
        desingularized
          SurfaceFunction1D.u
          (blendU0 singularityU0 function)
          function
          (blendU1 function singularityU1)
      (Nothing, Nothing, Just singularityV0, Nothing) ->
        desingularized
          SurfaceFunction1D.v
          (blendV0 singularityV0 function)
          function
          function
      (Nothing, Nothing, Nothing, Just singularityV1) ->
        desingularized
          SurfaceFunction1D.v
          function
          function
          (blendV1 function singularityV1)
      (Nothing, Nothing, Just singularityV0, Just singularityV1) ->
        desingularized
          SurfaceFunction1D.v
          (blendV0 singularityV0 function)
          function
          (blendV1 function singularityV1)
      _ -> throw UnsupportedCombinationOfSingularities

blendU0 ::
  CoordinateSystem.Generic dimension units space =>
  Singularity dimension units space ->
  VectorSurfaceFunction dimension units space ->
  VectorSurfaceFunction dimension units space
blendU0 (f0, dfdu0) f = do
  let t0 = Desingularization.t0
  let u0 = vParameterizationU0
  let uT0 = vParameterizationUT0
  blend
    ( f `compose` uT0
    , negative t0 .*. derivative U f `compose` uT0
    , t0 .*. t0 .*. derivative U (derivative U f) `compose` uT0
    )
    ( f0 `compose` u0
    , negative t0 .*. dfdu0 `compose` u0
    )
    ((t0 .-. SurfaceFunction1D.u) ./. t0)

blendU1 ::
  CoordinateSystem.Generic dimension units space =>
  VectorSurfaceFunction dimension units space ->
  Singularity dimension units space ->
  VectorSurfaceFunction dimension units space
blendU1 f (f1, dfdu1) = do
  let t0 = Desingularization.t0
  let t1 = Desingularization.t1
  let uT1 = vParameterizationUT1
  let u1 = vParameterizationU1
  blend
    ( f `compose` uT1
    , t0 .*. derivative U f `compose` uT1
    , t0 .*. t0 .*. derivative U (derivative U f) `compose` uT1
    )
    ( f1 `compose` u1
    , t0 .*. dfdu1 `compose` u1
    )
    ((SurfaceFunction1D.u .-. t1) ./. t0)

blendV0 ::
  CoordinateSystem.Generic dimension units space =>
  Singularity dimension units space ->
  VectorSurfaceFunction dimension units space ->
  VectorSurfaceFunction dimension units space
blendV0 (f0, dfdv0) f = do
  let t0 = Desingularization.t0
  let v0 = uParameterizationV0
  let vT0 = uParameterizationVT0
  blend
    ( f `compose` vT0
    , negative t0 .*. derivative V f `compose` vT0
    , t0 .*. t0 .*. derivative V (derivative V f) `compose` vT0
    )
    ( f0 `compose` v0
    , negative t0 .*. dfdv0 `compose` v0
    )
    ((t0 .-. SurfaceFunction1D.v) ./. t0)

blendV1 ::
  CoordinateSystem.Generic dimension units space =>
  VectorSurfaceFunction dimension units space ->
  Singularity dimension units space ->
  VectorSurfaceFunction dimension units space
blendV1 f (f1, dfdv1) = do
  let t0 = Desingularization.t0
  let t1 = Desingularization.t1
  let uT1 = uParameterizationVT1
  let u1 = uParameterizationV1
  blend
    ( f `compose` uT1
    , t0 .*. derivative V f `compose` uT1
    , t0 .*. t0 .*. derivative V (derivative V f) `compose` uT1
    )
    ( f1 `compose` u1
    , t0 .*. dfdv1 `compose` u1
    )
    ((SurfaceFunction1D.v .-. t1) ./. t0)

blend ::
  CoordinateSystem.Generic dimension units space =>
  ( VectorSurfaceFunction dimension units space
  , VectorSurfaceFunction dimension units space
  , VectorSurfaceFunction dimension units space
  ) ->
  ( VectorSurfaceFunction dimension units space
  , VectorSurfaceFunction dimension units space
  ) ->
  SurfaceFunction1D Unitless ->
  VectorSurfaceFunction dimension units space
blend (f00, f01, f02) (f10, f11) t = do
  let b00 = Curve1D.b00 `compose` t
  let b01 = Curve1D.b01 `compose` t
  let b02 = Curve1D.b02 `compose` t
  let b10 = Curve1D.b10 `compose` t
  let b11 = Curve1D.b11 `compose` t
  b00 .*. f00 .+. b01 .*. f01 .+. b02 .*. f02 .+. b10 .*. f10 .+. b11 .*. f11

uParameterization :: Number -> SurfaceFunction2D Unitless UvSpace
uParameterization vValue =
  SurfaceFunction2D.xy SurfaceFunction1D.u (SurfaceFunction1D.constant vValue)

uParameterizationV0 :: SurfaceFunction2D Unitless UvSpace
uParameterizationV0 = uParameterization 0

uParameterizationVT0 :: SurfaceFunction2D Unitless UvSpace
uParameterizationVT0 = uParameterization Desingularization.t0

uParameterizationVT1 :: SurfaceFunction2D Unitless UvSpace
uParameterizationVT1 = uParameterization Desingularization.t1

uParameterizationV1 :: SurfaceFunction2D Unitless UvSpace
uParameterizationV1 = uParameterization 1

vParameterization :: Number -> SurfaceFunction2D Unitless UvSpace
vParameterization uValue =
  SurfaceFunction2D.xy (SurfaceFunction1D.constant uValue) SurfaceFunction1D.v

vParameterizationU0 :: SurfaceFunction2D Unitless UvSpace
vParameterizationU0 = vParameterization 0

vParameterizationUT0 :: SurfaceFunction2D Unitless UvSpace
vParameterizationUT0 = vParameterization Desingularization.t0

vParameterizationUT1 :: SurfaceFunction2D Unitless UvSpace
vParameterizationUT1 = vParameterization Desingularization.t1

vParameterizationU1 :: SurfaceFunction2D Unitless UvSpace
vParameterizationU1 = vParameterization 1
