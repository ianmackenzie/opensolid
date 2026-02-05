module OpenSolid.SurfaceFunction1D.Quotient (impl) where

import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceFunction1D.Desingularization qualified as SurfaceFunction1D.Desingularization
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))

type Desingularize function =
  function ->
  "singularityU0" ::: Maybe (function, function) ->
  "singularityU1" ::: Maybe (function, function) ->
  "singularityV0" ::: Maybe (function, function) ->
  "singularityV1" ::: Maybe (function, function) ->
  function

impl ::
  Tolerance units =>
  (function -> SurfaceFunction1D units -> quotient) ->
  (SurfaceParameter -> (quotient, quotient)) ->
  Desingularize quotient ->
  function ->
  SurfaceFunction1D units ->
  Result DivisionByZero quotient
impl unsafeQuotient lhopital desingularize numerator denominator
  | denominator ~= SurfaceFunction1D.zero = Error DivisionByZero
  | otherwise = do
      let maybeSingularity parameter value
            | SurfaceFunction1D.Desingularization.isZero parameter value denominator = do
                let denominator' = SurfaceFunction1D.derivative parameter denominator
                if denominator' ~= SurfaceFunction1D.zero -- TODO switch to "if SurfaceFunction1D.hasZero denominator'"
                  then Error DivisionByZero
                  else Ok (Just (lhopital parameter))
            | otherwise = Ok Nothing
      maybeSingularityU0 <- maybeSingularity U 0.0
      maybeSingularityU1 <- maybeSingularity U 1.0
      maybeSingularityV0 <- maybeSingularity V 0.0
      maybeSingularityV1 <- maybeSingularity V 1.0
      Ok $
        desingularize
          (unsafeQuotient numerator denominator)
          (#singularityU0 maybeSingularityU0)
          (#singularityU1 maybeSingularityU1)
          (#singularityV0 maybeSingularityV0)
          (#singularityV1 maybeSingularityV1)
