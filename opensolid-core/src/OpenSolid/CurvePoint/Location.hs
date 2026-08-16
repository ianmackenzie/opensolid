module OpenSolid.CurvePoint.Location
  ( Location (..)
  , data Boundary
  , isBoundary
  , fromParameterValue
  , toParameterValue
  )
where

import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance

data Location
  = Start
  | End
  | Interior Number
  deriving (Eq, Show)

instance Ord Location where
  {-# INLINE compare #-}
  compare first second = compare (toParameterValue first) (toParameterValue second)

{-# INLINE Boundary #-}
pattern Boundary :: Number -> Location
pattern Boundary tValue <- (boundaryParameterValue -> Just tValue)

{-# INLINE boundaryParameterValue #-}
boundaryParameterValue :: Location -> Maybe Number
boundaryParameterValue Start = Just 0.0
boundaryParameterValue End = Just 1.0
boundaryParameterValue Interior{} = Nothing

{-# INLINE isBoundary #-}
isBoundary :: Location -> Bool
isBoundary Start = True
isBoundary End = True
isBoundary Interior{} = False

{-# INLINE toParameterValue #-}
toParameterValue :: Location -> Number
toParameterValue Start = 0.0
toParameterValue End = 1.0
toParameterValue (Interior tValue) = tValue

{-# INLINE fromParameterValue #-}
fromParameterValue :: Number -> Location
fromParameterValue tValue =
  Tolerance.using Tolerance.unitless do
    if
      | tValue ~= 0.0 -> Start
      | tValue ~= 1.0 -> End
      | otherwise -> Interior tValue
