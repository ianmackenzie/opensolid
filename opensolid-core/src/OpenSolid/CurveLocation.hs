module OpenSolid.CurveLocation
  ( CurveLocation (..)
  , data Boundary
  , isBoundary
  , fromParameterValue
  , toParameterValue
  )
where

import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance

data CurveLocation
  = Start
  | End
  | Interior Number
  deriving (Eq, Show)

instance Ord CurveLocation where
  {-# INLINE compare #-}
  compare first second = compare (toParameterValue first) (toParameterValue second)

{-# INLINE Boundary #-}
pattern Boundary :: Number -> CurveLocation
pattern Boundary tValue <- (boundaryParameterValue -> Just tValue)

{-# INLINE boundaryParameterValue #-}
boundaryParameterValue :: CurveLocation -> Maybe Number
boundaryParameterValue Start = Just 0.0
boundaryParameterValue End = Just 1.0
boundaryParameterValue (Interior _) = Nothing

{-# INLINE isBoundary #-}
isBoundary :: CurveLocation -> Bool
isBoundary Start = True
isBoundary End = True
isBoundary (Interior _) = False

{-# INLINE toParameterValue #-}
toParameterValue :: CurveLocation -> Number
toParameterValue Start = 0.0
toParameterValue End = 1.0
toParameterValue (Interior tValue) = tValue

{-# INLINE fromParameterValue #-}
fromParameterValue :: Number -> CurveLocation
fromParameterValue tValue =
  Tolerance.using Tolerance.unitless do
    if
      | tValue ~= 0.0 -> Start
      | tValue ~= 1.0 -> End
      | otherwise -> Interior tValue
