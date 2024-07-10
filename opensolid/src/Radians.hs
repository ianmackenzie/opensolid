module Radians (toUnitless, fromUnitless) where

import Basics
import Units (Erase, Radians, Units)
import Units qualified

toUnitless ::
  ( Units a ~ Radians
  , Units.Coercion a (Units.Erase a)
  ) =>
  a ->
  Erase a
toUnitless = Units.erase

fromUnitless ::
  ( Units.Coercion (Units.Erase a) a
  , Units a ~ Radians
  ) =>
  Units.Erase a ->
  a
fromUnitless = Units.coerce
