module OpenSolid.SurfaceFunction.Quotient (impl) where

import OpenSolid.DivisionByZero (DivisionByZero (DivisionByZero))
import OpenSolid.List qualified as List
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Result qualified as Result
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
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
impl unsafeQuotient' lhopital desingularize numerator denominator
  | denominator ~= Qty.zero = Failure DivisionByZero
  | otherwise = Result.do
      let singularity direction = do
            let denominator' = SurfaceFunction.derivative direction denominator
            if denominator' ~= Qty.zero
              then Failure DivisionByZero -- TODO switch to "if SurfaceFunction.hasZero denominator'"
              else Success (Just (lhopital direction))
      singularityU0 <- if zeroU0 denominator then singularity U else Success Nothing
      singularityU1 <- if zeroU1 denominator then singularity U else Success Nothing
      singularityV0 <- if zeroV0 denominator then singularity V else Success Nothing
      singularityV1 <- if zeroV1 denominator then singularity V else Success Nothing
      Success $ desingularize do
        #function (unsafeQuotient' numerator denominator)
        #singularityU0 singularityU0
        #singularityU1 singularityU1
        #singularityV0 singularityV0
        #singularityV1 singularityV1

zeroU0 :: Tolerance units => SurfaceFunction units -> Bool
zeroU0 function =
  List.allTrue
    [SurfaceFunction.evaluate function (Point2d 0.0 v) ~= Qty.zero | v <- Parameter.samples]

zeroU1 :: Tolerance units => SurfaceFunction units -> Bool
zeroU1 function =
  List.allTrue
    [SurfaceFunction.evaluate function (Point2d 1.0 v) ~= Qty.zero | v <- Parameter.samples]

zeroV0 :: Tolerance units => SurfaceFunction units -> Bool
zeroV0 function =
  List.allTrue
    [SurfaceFunction.evaluate function (Point2d u 0.0) ~= Qty.zero | u <- Parameter.samples]

zeroV1 :: Tolerance units => SurfaceFunction units -> Bool
zeroV1 function =
  List.allTrue
    [SurfaceFunction.evaluate function (Point2d u 1.0) ~= Qty.zero | u <- Parameter.samples]
