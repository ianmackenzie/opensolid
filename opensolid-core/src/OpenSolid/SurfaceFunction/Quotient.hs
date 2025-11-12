module OpenSolid.SurfaceFunction.Quotient (impl) where

import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Desingularization qualified as SurfaceFunction.Desingularization
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
  (function -> SurfaceFunction units -> quotient) ->
  (SurfaceParameter -> (quotient, quotient)) ->
  Desingularize quotient ->
  function ->
  SurfaceFunction units ->
  Result DivisionByZero quotient
impl unsafeQuotient lhopital desingularize numerator denominator
  | denominator ~= SurfaceFunction.zero = Error DivisionByZero
  | otherwise = do
      let maybeSingularity parameter value
            | SurfaceFunction.Desingularization.isZero parameter value denominator = do
                let denominator' = SurfaceFunction.derivative parameter denominator
                if denominator' ~= SurfaceFunction.zero -- TODO switch to "if SurfaceFunction.hasZero denominator'"
                  then Error DivisionByZero
                  else Ok (Just (lhopital parameter))
            | otherwise = Ok Nothing
      maybeSingularityU0 <- maybeSingularity U 0
      maybeSingularityU1 <- maybeSingularity U 1
      maybeSingularityV0 <- maybeSingularity V 0
      maybeSingularityV1 <- maybeSingularity V 1
      Ok $
        desingularize
          (unsafeQuotient numerator denominator)
          (#singularityU0 maybeSingularityU0)
          (#singularityU1 maybeSingularityU1)
          (#singularityV0 maybeSingularityV0)
          (#singularityV1 maybeSingularityV1)
