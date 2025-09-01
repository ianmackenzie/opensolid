module OpenSolid.Desingularization
  ( t0
  , t1
  , value
  , bounds
  , continuity
  , blendValues1d
  , blendBounds1d
  )
where

import Foreign (Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.IO qualified as IO
import OpenSolid.Prelude
import OpenSolid.Qty (Qty (Qty))
import System.IO.Unsafe qualified
import Prelude (Double)

t0 :: Float
-- Should be kept in sync with T0 in bytecode.cpp
t0 = 0.001

t1 :: Float
-- Should be kept in sync with CUTOFF_1 in bytecode.cpp
t1 = 0.999

value :: Float -> a -> a -> a -> a
value t start middle end
  | t <= t0 = start
  | t >= t1 = end
  | otherwise = middle

bounds :: Bounds Unitless -> a -> a -> a -> a
bounds t start middle end
  | t.upper <= t0 = start
  | t.lower >= t1 = end
  | otherwise = middle

{-| The order of continuity to use when joining a synthetic curve to a base curve
in order to 'desingularize' the base curve.
-}
continuity :: Int
continuity = 2

blendValues1d ::
  Qty units ->
  List (Qty units) ->
  Qty units ->
  List (Qty units) ->
  Float ->
  Qty units
blendValues1d
  (Qty startValue)
  startDerivatives
  (Qty endValue)
  endDerivatives
  (Qty parameterValue) = do
    let numStartDerivatives = startDerivatives.length
    let numEndDerivatives = endDerivatives.length
    System.IO.Unsafe.unsafeDupablePerformIO $
      Foreign.Marshal.Alloc.allocaBytes (8 * numStartDerivatives) \startDerivativesPtr ->
        Foreign.Marshal.Alloc.allocaBytes (8 * numEndDerivatives) \endDerivativesPtr ->
          IO.do
            IO.forEachWithIndex startDerivatives $ \index (Qty startDerivative) ->
              Foreign.pokeElemOff startDerivativesPtr index startDerivative
            IO.forEachWithIndex endDerivatives $ \index (Qty endDerivative) ->
              Foreign.pokeElemOff endDerivativesPtr index endDerivative
            IO.succeed $
              Qty $
                opensolid_blend_values_1d
                  startValue
                  numStartDerivatives
                  startDerivativesPtr
                  endValue
                  numEndDerivatives
                  endDerivativesPtr
                  parameterValue

blendBounds1d ::
  Bounds units ->
  List (Bounds units) ->
  Bounds units ->
  List (Bounds units) ->
  Bounds Unitless ->
  Bounds units
blendBounds1d
  (Bounds (Qty startValueLower) (Qty startValueUpper))
  startDerivatives
  (Bounds (Qty endValueLower) (Qty endValueUpper))
  endDerivatives
  (Bounds (Qty parameterValueLower) (Qty parameterValueUpper)) = do
    let numStartDerivatives = startDerivatives.length
    let numEndDerivatives = endDerivatives.length
    System.IO.Unsafe.unsafeDupablePerformIO $
      Foreign.Marshal.Alloc.allocaBytes (16 * numStartDerivatives) \startDerivativesPtr ->
        Foreign.Marshal.Alloc.allocaBytes (16 * numEndDerivatives) \endDerivativesPtr ->
          Foreign.Marshal.Alloc.allocaBytes 16 \returnValuesPtr ->
            IO.do
              IO.forEachWithIndex startDerivatives $ \index startDerivative -> IO.do
                let Bounds (Qty startDerivativeLower) (Qty startDerivativeUpper) = startDerivative
                Foreign.pokeElemOff startDerivativesPtr (2 * index) startDerivativeLower
                Foreign.pokeElemOff startDerivativesPtr (2 * index + 1) startDerivativeUpper
              IO.forEachWithIndex endDerivatives $ \index endDerivative -> IO.do
                let Bounds (Qty endDerivativeLower) (Qty endDerivativeUpper) = endDerivative
                Foreign.pokeElemOff endDerivativesPtr (2 * index) endDerivativeLower
                Foreign.pokeElemOff endDerivativesPtr (2 * index + 1) endDerivativeUpper
              opensolid_blend_bounds_1d
                startValueLower
                startValueUpper
                numStartDerivatives
                startDerivativesPtr
                endValueLower
                endValueUpper
                numEndDerivatives
                endDerivativesPtr
                parameterValueLower
                parameterValueUpper
                returnValuesPtr
              lower <- Foreign.peekElemOff returnValuesPtr 0
              upper <- Foreign.peekElemOff returnValuesPtr 1
              IO.succeed (Bounds (Qty lower) (Qty upper))

foreign import ccall unsafe "bytecode.h opensolid_blend_values_1d"
  opensolid_blend_values_1d ::
    Double -> Int -> Ptr Double -> Double -> Int -> Ptr Double -> Double -> Double

foreign import ccall unsafe "bytecode.h opensolid_blend_bounds_1d"
  opensolid_blend_bounds_1d ::
    Double ->
    Double ->
    Int ->
    Ptr Double ->
    Double ->
    Double ->
    Int ->
    Ptr Double ->
    Double ->
    Double ->
    Ptr Double ->
    IO ()
