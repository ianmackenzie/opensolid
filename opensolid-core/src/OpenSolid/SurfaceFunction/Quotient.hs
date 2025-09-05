module OpenSolid.SurfaceFunction.Quotient (impl) where

import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Result qualified as Result
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceFunction.Desingularization qualified as SurfaceFunction.Desingularization
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))

type Desingularize function =
  ( "function" ::: function
  , "singularityU0" ::: Maybe (function, function)
  , "singularityU1" ::: Maybe (function, function)
  , "singularityV0" ::: Maybe (function, function)
  , "singularityV1" ::: Maybe (function, function)
  ) ->
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
  | denominator ~= Qty.zero = Failure DivisionByZero
  | otherwise = Result.do
      let singularityIf zeroCheck direction
            | zeroCheck denominator = do
                let denominator' = SurfaceFunction.derivative direction denominator
                if denominator' ~= Qty.zero -- TODO switch to "if SurfaceFunction.hasZero denominator'"
                  then Failure DivisionByZero
                  else Success (Just (lhopital direction))
            | otherwise = Success Nothing
      maybeSingularityU0 <- singularityIf SurfaceFunction.Desingularization.zeroU0 U
      maybeSingularityU1 <- singularityIf SurfaceFunction.Desingularization.zeroU1 U
      maybeSingularityV0 <- singularityIf SurfaceFunction.Desingularization.zeroV0 V
      maybeSingularityV1 <- singularityIf SurfaceFunction.Desingularization.zeroV1 V
      Success $ desingularize do
        #function (unsafeQuotient numerator denominator)
        #singularityU0 maybeSingularityU0
        #singularityU1 maybeSingularityU1
        #singularityV0 maybeSingularityV0
        #singularityV1 maybeSingularityV1
